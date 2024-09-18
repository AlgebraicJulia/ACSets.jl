""" Parsers

Test case displaying parsing of ACSetSpecs using PEG.jl.
This module also includes basic lexing rules as seen in
SyntacticModels.jl.

"""

module Parsers

using Reexport
@reexport using PEG

#Export lexing rules
export ws, eq, lparen, rparen, comma, EOL, colon, identifier
#Export parsing rules
export body, block, line, statement, args, arg

# Basic Lexing rules for scanning sting of characters
# Breaks up words/structures into tokens

@rule ws = r"\s*"
@rule eq = r"="p
@rule lparen = r"\("
@rule rparen = r"\)"
@rule comma = r","p
@rule EOL = "\n" , ";"
@rule colon = r":"p
@rule identifier = r"[^:{}→\n;=,\(\)]*"

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

@rule body = r"quote" & block & r"end"p
@rule block = line[*] & r"\n?"p
@rule line  = ws & statement & ws & EOL
@rule statement = identifier & lparen & args & rparen
@rule args = (arg & comma)[*] & arg
@rule arg = (identifier & eq & args) , identifier

#Deleted args rule for stack overflow problem
# @rule arg = args ,  (identifier & eq & args) , identifier creates stack overflow



# Body contains "qoute ... end", containing a block of code
# Block contains one or more lines of statements
# Line contians a statement followed by a new line or ";"
# Statement contains a call followed by arguments in parenthesis: "identifier(args)"
# args contains one or more arguments separated by commas
# arg contains multiple possibilites:
#   - A list of further arguments
#   - A key followed by an equals sign and a value: "identifier = args"
#   - A single value

end