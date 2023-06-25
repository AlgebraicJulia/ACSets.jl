using ACSets
using ACSets.ADTs
using MLStyle
using Catlab
using Catlab.CategoricalAlgebra
using Catlab.Graphs
using Catlab.Graphs.BasicGraphs
using Test

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
  println(to_string(gspec))
  @show generate_expr(gspec)
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

  println("h is ")
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
  JSON.json(to_dict(gspec))
  #JSON.print(to_dict(gspec), 2)
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
  println(to_string(gspec))
  label2index(gspec) |> to_string |> println
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