""" Parsers

Parsing ACSetSpecs using PEG.jl. This module allows you to build custom
grammars that represent the models in strings that aren't tied to any particular
programming language syntax. Specifically functional for parsing and constructing
ACSetSpecs.
"""

module Parsers

using Reexport
using ACSets, ACSets.ADTs

@reexport using PEG

# Export macro
export @acsetspec_str

# Export lexing rules
export ws, eq, lparen, rparen, comma, EOL, colon, identifier

# Export parsing rules
export acset_spec, block, line, statement, args, arg

# Basic Lexing rules for scanning string of characters
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
#
# Example:
#
# acsetspec"""
#  LabeledGraph{Symbol}
#  begin
#    V(label=a)
#    V(label=b)
#    V(label=c)
#    E(src=1,tgt=3)
#    E(src=2,tgt=3)
#  end
# """
#

# This PEG.jl-based parser takes from the recursive decent
# parser in ACSets.jl/ADTs.jl and parses "acsetspec(head, body)"

# acset_spec takes in head and body args
@rule acset_spec = ws & head & r"begin"p & block & r"end"p |> v -> ACSetSpec(v[2], v[4])
# Ensures "head" exists but does not check type
@rule head = r"\S*"p |> v -> Symbol(v)
# Block contains one or more lines of statements
@rule block = line[*] & r"\n?"p |> v -> v[1]
# Line contains a statement followed by a new line or ";"
@rule line = ws & statement & r"[^\S\r\n]*" & EOL |> v -> v[2]
# Statement contains a call followed by arguments in parenthesis: "identifier(args)"
@rule statement = identifier & lparen & ws & args & ws & rparen |> v -> Statement(Symbol(v[1]), v[4])
# args contains one or more arguments separated by commas
@rule args = (arg & ws & comma)[*] & arg |> v -> collect_args(v)
# arg can be a list of further arguments, a key-value pair, or a single value
@rule arg = ((lparen & args & rparen) |> v -> v[2]), 
      ((identifier & eq & arg) |> v -> parse_assignment(v)),
      (identifier |> v -> parse_identifier(v))

# Collects and flattens arguments into a single list
collect_args(v::Vector{Any}) = begin
  output = Vector{Args}(first.(v[1]))
  push!(output, last(v))
end

# Parses an identifier into a symbol/integer
parse_identifier(v) = begin
  v_parsed = tryparse(Int, v)
  
  if isnothing(v_parsed)
    return Value(Symbol(v))
  else
    return Value(v_parsed)
  end
end

# Parses an assignment statement
# Vectors wrapped as Value
# Ensures singular Values are not wrapped twice
parse_assignment(v) = begin
  if isa(v[3], Vector)
    return Kwarg(Symbol(v[1]), Value(v[3])) 
  else
    return Kwarg(Symbol(v[1]), v[3])
  end
end

# Creates a string macro to parse/create acsetspec
macro acsetspec_str(x::String) parse_whole(acset_spec, x) end

end