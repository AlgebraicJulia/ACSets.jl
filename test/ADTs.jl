using ACSets
using ACSets.ADTs
using MLStyle
using Test

import ACSets.ADTs: symb2string



SchLabeledGraph = BasicSchema([:E,:V], [(:src,:E,:V),(:tgt,:E,:V)],
                          [:L], [(:label,:V,:L)])

@acset_type LabeledGraph(SchLabeledGraph, index=[:src,:tgt])

@testset "Basic Constructions" begin
  s = Statement(:E, [Value(2), Value(3)])
  get_table(s) = @match s begin
      Statement(t, e) => t
      _ => nothing
  end
  get_arg1(s) = @match s begin
      Statement(t, e) => e[1]
  end 
  @test get_table(s) == :E
  @test get_arg1(s) == Value(2)
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
  @test sprint(show, gspec) isa String
  @test sprint(show, gspec) != ""
  @test !isnothing(sprint(show, gspec))
  @test sprint(show, gspec) == "LabeledGraph{Symbol} begin \n  V(label=a)\n  V(label=b)\n  V(label=c)\n  E(1,3)\n  E(2,3)\n end"
  @test generate_expr(gspec) isa Expr
end



@testset "Constructing ACSets from Specs" begin
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
  @test nparts(g, :V) == 3
  @test nparts(g, :E) == 2

  hspec = acsetspec(:(LabeledGraph{Symbol}),quote
      V(label=a)
      V(label=b)
      V(label=c)
      E(src=1,tgt=3)
      E(src=2,tgt=3)
  end
  )
  @test construct(LabeledGraph{Symbol}, hspec) == construct(LabeledGraph{Symbol}, gspec)
  cspec = acsetspec(:SemiSimplicialSet,quote
      V(label=:a, pos=(0,0))
      V(label=:b, pos=(1,0))
      V(label=:c, pos=(0,1))
      E(1,2)
      E(2,3)
      E(3,1)
      T(1,2,3)
  end) # can't test this without CombinatorialSpaces.jl
end

using JSON
@testset "to_dict" begin
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

  @test to_dict(gspec) isa AbstractDict
  @test JSON.json(to_dict(gspec)) isa String
  @test JSON.Parser.parse(JSON.json(to_dict(gspec))) isa AbstractDict
  @test JSON.Parser.parse(JSON.json(to_dict(gspec))) == symb2string(to_dict(gspec))
end

@testset "Labeled Vertices" begin
  gspec = ACSetSpec(
  :(LabeledGraph{Symbol}),
  [
      Statement(:V, [Kwarg(:label, Value(:a))])
      Statement(:V, [Kwarg(:label, Value(:b))])
      Statement(:V, [Kwarg(:label, Value(:c))])
      Statement(:E, [Kwarg(:src, Value(:a)), Kwarg(:tgt, Value(:c))])
      Statement(:E, [Kwarg(:src, Value(:b)), Kwarg(:tgt, Value(:c))])
  ])
  @test label2index(gspec) isa ACSetSpec
  gl = construct(LabeledGraph{Symbol}, label2index(gspec))
  @test gl[:label] == [:a, :b, :c]
  @test gl[:src] == [1,2]
  @test gl[:tgt] == [3,3]
  
  gspec_bad_labels = ACSetSpec(
    :(LabeledGraph{Symbol}),
    [
      Statement(:V, [Kwarg(:label, Value(:a))])
      Statement(:V, [Kwarg(:label, Value(:b))])
      Statement(:V, [Kwarg(:label, Value(:a))])
      Statement(:E, [Kwarg(:src, Value(:a)), Kwarg(:tgt, Value(:c))])
      Statement(:E, [Kwarg(:src, Value(:b)), Kwarg(:tgt, Value(:c))])
    ])
  @test_throws ErrorException label2index(gspec_bad_labels)
end