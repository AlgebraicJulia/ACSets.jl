using Test

SchDDS = BasicSchema([:X], [(:Φ,:X,:X)])

@abstract_acset_type AbstractDDS
@acset_type DDS(SchDDS, index=[:Φ]) <: AbstractDDS

DDS(i::Int) = DDS(rand(1:i, i))
function DDS(v::Vector{Int})
  x = DDS()
  add_parts!(x, :X, length(v))
  set_subpart!(x, :Φ, v)
  x
end

d = DDS(5)

q = From(:X)
@test q(d) == parts(d, :X)

q = From(:X) |> Select(:X)
@test q(d) == parts(d, :X)

q = From(:X) |> Select(:Φ)
@test q(d) == subpart(d, :Φ)

SchDecGraph = BasicSchema([:E,:V], [(:src,:E,:V),(:tgt,:E,:V)],
                          [:X], [(:dec,:E,:X)])

@acset_type DecGraph(SchDecGraph, index=[:src,:tgt])

g = @acset DecGraph{String} begin
  V = 4
  E = 4
  src = [1,2,3,4]
  tgt = [2,3,4,1]
  dec = ["a","b","c","d"]
end

q = From(:V) |> Where(:dec, x -> String([x]) != "a") |> Select(:dec)
@test q(g) == ["b", "c", "d"]

q = From(:V) |> Where(:src, 1) & Where(:tgt, 2) |> Select(:dec)
@test q(g) == ["a"]
