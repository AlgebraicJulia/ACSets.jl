""" Parsers

Test case displaying parsing of ACSetSpecs using PEG.jl.
This module also includes basic lexing rules as seen in
SyntacticModels.jl.

Tasks Left to Complete:

- Add more test cases for the parser
- Call parser from ADTs.jl acsetspec function
- Add error handling for invalid input?
- Enable extraction from files?

"""

module Parsers

using Reexport
using ACSets, ACSets.ADTs

@reexport using PEG

#Export macro
export @acsetspec_str
#Export lexing rules
export ws, eq, lparen, rparen, comma, EOL, colon, identifier
#Export parsing rules
export acset_spec, block, line, statement, args, arg

# Basic Lexing rules for scanning sting of characters
# Breaks up words/structures into tokens

@rule ws = r"\s*"
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
# acsetspec"""
# LabeledGraph{Symbol}
# begin
#        V(label=a)
#        V(label=b)
#        V(label=c)
#        E(src=1,tgt=3)
#        E(src=2,tgt=3)
# end
# """
#

# This PEG.jl based parser takes from the recursive decent
# parser in ACSets.jl/ADTs.jl and parses "acsetspec(head, body)"

#acset_spec takes in head and body args
@rule acset_spec = ws & head & r"begin"p & block & r"end"p |> v -> ACSetSpec(v[2], v[4])
#Head contains ACsetSpec Type: As acetspec(), the PEG parser, really does not parse the head.
#It just ensure's that it exists.
@rule head = r"\S*"p |> v -> Symbol(v)
# Block contains one or more lines of statements
@rule block = line[*] & r"\n?"p |> v -> v[1]
# Line contians a statement followed by a new line or ";"
@rule line  = ws & statement & r"[^\S\r\n]*" & EOL |> v -> v[2]
# Statement contains a call followed by arguments in parenthesis: "identifier(args)"
@rule statement = identifier & lparen & args & rparen |> v -> Statement(Symbol(v[1]), v[3])
# args contains one or more arguments separated by commas
@rule args = (arg & ws & comma)[*] & arg |> v -> collect_args(v)
# arg can be a list of further arguments, a key-value pair, or a single value
@rule arg = ((lparen & args & rparen) |> v -> v[2]), 
            ((identifier & eq & arg) |> v ->  parse_assignment(v)),
            (identifier |> v -> parse_identifier(v[1]))            

#Collects and flattens arguments into a single list
# If a type conversion error occurs, simply casts entire vector to closest superType
collect_args(v::Vector{Any}) = begin
    try
        output = first.(v[1])
        push!(output, last(v))
    catch error
        if isa(error, MethodError)
            #Grabs types that are failing to convert
            #For some reason arg1 is the type while arg2 is the failing object
            error_type_1 = error.args[1] 
            println("error type 1: ", error_type_1)
            error_type_2 = typeof(error.args[2])
            println("error type 2: ", error_type_2)
            #Checks if each type is of closest supertype
            if (error_type_1 <: Kwarg &&  error_type_2 <: Kwarg)
                output = Vector{Kwarg}(first.(v[1]))
                println("kwarg casted")
            elseif (error_type_1 <: Value &&  error_type_2 <: Value)
                output = Vector{Value}(first.(v[1]))
                println("value casted")
            else
                output = Vector{Args}(first.(v[1]))
                println("arg casted")
            end
            push!(output, last(v))
        else
            rethrow(e)
        end
    end
end

#Parses an identifier into a number or symbol
parse_identifier(v) = begin 
    try
        return Value(parse(Int, v))
    catch
        return Value(Symbol(v))
    end
end

#Parses an assignment statement - Kwarg only takes type Value for args. 
#Other cases than vector should have already been wrapped in "parse_identifier" 
parse_assignment(v) = begin
    if isa(v[3], Vector)
        return Kwarg(Symbol(v[1]), Value(v[3])) 
    else
        return Kwarg(Symbol(v[1]), v[3])
    end
end

#Creates a string macro to parse/create acsetspec
macro acsetspec_str(x::String) parse_whole(acset_spec, x) end

end