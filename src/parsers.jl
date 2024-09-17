""" Parsers

Test case displaying parsing of ACSetSpecs using PEG.jl.
This module also includes basic lexing rules as seen in
SyntacticModels.jl.

"""

module Parsers

@reexport using PEG

# Basic Lexing rules for scanning sting of characters
# Breaks up words/structures into tokens

@rule ws = r"\s*"
@rule eq = r"="p
@rule lparen = r"\("
@rule rparen = r"\)"
@rule comma = r","p
@rule EOL = "\n" , ";"
@rule colon = r":"p
@rule keyWord = r"[^:{}→\n;=,\(\)]*"

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
@rule statement = keyWord & lparen & args & rparen
@rule args = (arg & comma)[*] & arg
@rule arg = args , (keyWord & eq & args) , keyWord


end