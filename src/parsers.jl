""" Parsers

Test case displaying parsing of ACSetSpecs using PEG.jl.
This module also includes basic lexing rules as seen in
SyntacticModels.jl.

Tasks Left to Complete:

- Add more test cases for the parser
- Call parser from ADTs.jl acsetspec function
- Add error handling for invalid input
- Enable extraction from files?

"""

module Parsers

using Reexport
using ACSets, ACSets.ADTs

@reexport using PEG

#Export lexing rules
export ws, eq, lparen, rparen, comma, EOL, colon, identifier
#Export parsing rules
export body, block, line, statement, args, arg

# Basic Lexing rules for scanning sting of characters
# Breaks up words/structures into tokens

@rule ws = r"[^\S\r\n]*"
@rule eq = r"="p
@rule lparen = r"\("
@rule rparen = r"\)"
@rule comma = r","p
@rule EOL = "\n" , ";"
@rule colon = r":"p
@rule identifier = r"[^:{}→\n;=,\(\)\s]+"

# Core Parsing rules for ACSetSpecs
# ACSetSpec Structure:
# acsetspec(head, body)
# Example:
#
# acsetspec(:(LabeledGraph{Symbol}),quote
#       V(label=a)
#       V(label=b)
#       V(label=c)
#       E(src=1,tgt=3)
#       E(src=2,tgt=3)
#   end
#   )
#
# This PEG.jl based parser takes from the recursive decent
# parser in ACSets.jl/ADTs.jl and parses the body in "acsetspec(head, body)"

@rule body = r"quote\s*"p & block & r"end"p |> v -> v[2]
# Block contains one or more lines of statements
@rule block = line[*] & r"\n?"p |> v -> v[1]
# Line contians a statement followed by a new line or ";"
@rule line  = ws & statement & ws & EOL |> v -> v[2]
# Statement contains a call followed by arguments in parenthesis: "identifier(args)"
@rule statement = identifier & lparen & args & rparen |> v -> Statement(Symbol(v[1]), v[3])
# args contains one or more arguments separated by commas
@rule args = (arg & comma)[*] & arg |> v -> collect_args(v)
# arg can be a list of further arguments, a key-value pair, or a single value
@rule arg = ((lparen & args & rparen) |> v -> v[2]), 
            ((identifier & eq & arg) |> v -> Kwarg(Symbol(v[1]), v[3])), 
            (identifier |> v -> parse_identifier(v[1]))            

#Collects and flattens arguments into a single list
collect_args(v::Vector{Any}) = begin
    output = first.(v[1])
    push!(output, last(v))
end

#Parses an identifier into a number or symbol
parse_identifier(x) = begin 
    try
        return Value(parse(Int, x))
    catch
        return Value(Symbol(x))
    end
end

end