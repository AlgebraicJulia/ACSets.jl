# # Specifying acsets using Algebraic Data Types

# ACSets are an extremely flexible data representation that can store anything you
# can put in a database. But in order to construct them, you might want
# something that feels more like a custom programming language.
# The Graphviz software comes with a custom language called dot files for specifying
# the data of a graph that Graphviz will draw. In order to make implementing 
# these linguisting interfaces easier, ACSets.jl supports an Algebraic Data Types approach
# to specification of ACSets.

using ACSets, ACSets.ADTs
using MLStyle
using Test
import ACSets.ADTs: symb2string

# Our schema will be labeled graphs. These are graphs with vertex labels.

SchLabeledGraph = BasicSchema([:E,:V], [(:src,:E,:V),(:tgt,:E,:V)],
                          [:L], [(:label,:V,:L)])

@acset_type LabeledGraph(SchLabeledGraph, index=[:src,:tgt])

# The basic principle is a nested expression syntax for specifying the ACSet.

s = Statement(:E, [Value(2), Value(3)])

# You can extract information from an expression with pattern matching from MLStyle.jl

get_table(s) = @match s begin
    Statement(t, e) => t
    _ => nothing
end
get_arg1(s) = @match s begin
    Statement(t, e) => e[1]
end 

@test get_table(s) == :E
@test get_arg1(s) == Value(2)

# These statements can be grouped together into a list an tagged with the type of ACSet you want to make.

gspec = ACSetSpec(
    :(LabeledGraph{Symbol}),
    [
        Statement(:V, [Kwarg(:label, Value(:a))])
        Statement(:V, [Kwarg(:label, Value(:b))])
        Statement(:V, [Kwarg(:label, Value(:c))])
        Statement(:E, [Value(1), Value(3)])
        Statement(:E, [Value(2), Value(3)])
    ]
)

# These expressions can be serialized as strings:

sprint(show, gspec)

# Or as Julia code:

generate_expr(gspec)

# From the ACSetSpec you can construct the ACSet that it specifies.

gspec = ACSetSpec(
    :(LabeledGraph{Symbol}),
    [
        Statement(:V, [Kwarg(:label, Value(:a))])
        Statement(:V, [Kwarg(:label, Value(:b))])
        Statement(:V, [Kwarg(:label, Value(:c))])
        Statement(:E, [Kwarg(:src, Value(1)), Kwarg(:tgt, Value(3))])
        Statement(:E, [Kwarg(:src, Value(2)), Kwarg(:tgt, Value(3))])
    ]
)
g = construct(LabeledGraph{Symbol}, gspec)

# There is an embedding of `ACSetSpec` into `Expr`:

hspec = acsetspec(:(LabeledGraph{Symbol}),quote
    V(label=a)
    V(label=b)
    V(label=c)
    E(src=1,tgt=3)
    E(src=2,tgt=3)
end
)

# The `acsetspec` function is a good example of embedding your custom language into Julia syntax
# That save you the trouble of writing your own lexer and parser for your custom language.

construct(LabeledGraph{Symbol}, hspec) == construct(LabeledGraph{Symbol}, gspec)

# You can export your specification to a dictionary and put that dictionary into a JSON document
# this gives you a nice way of serializing the ACSet that is machine readable and row oriented.
# The ACSet serialization is by column oriented which might be inconvenient for your consumers.

to_dict(gspec)
