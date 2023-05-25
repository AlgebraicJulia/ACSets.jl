module TestJSONACSets
using Test

import JSONSchema

using ACSets

# Acset serialization
#####################

function roundtrip_json_acset(x::T) where T <: ACSet
  mktempdir() do dir
    path = joinpath(dir, "acset.json")
    write_json_acset(x, path)
    read_json_acset(T, path)
  end
end

SchGraph = BasicSchema([:V,:E], [(:src,:E,:V),(:tgt,:E,:V)])
@acset_type Graph(SchGraph, index=[:src,:tgt])

g = Graph()
add_parts!(g, :V, 5)
add_parts!(g, :E, 5, src=[1,2,3,4,5], tgt=[2,3,4,5,1])
@test roundtrip_json_acset(g) == g
json = generate_json_acset(g)
@test all(row -> haskey(row, :_id), json[:V])

SchWeightedGraph = BasicSchema([:V,:E], [(:src,:E,:V),(:tgt,:E,:V)],
                               [:Weight], [(:weight,:E,:Weight)])
@acset_type WeightedGraph(SchWeightedGraph, index=[:src,:tgt])

g = WeightedGraph{Float64}()
add_parts!(g, :V, 3)
add_parts!(g, :E, 2, src=[1,2], tgt=[2,3], weight=[0.5,1.5])
@test roundtrip_json_acset(g) == g

SchLabeledDDS = BasicSchema([:X], [(:Φ,:X,:X)], [:Label], [(:label,:X,:Label)])
@acset_type LabeledDDS(SchLabeledDDS, index=[:Φ])

ldds = LabeledDDS{Symbol}()
add_parts!(ldds, :Label, 2)
add_parts!(ldds, :X, 4, Φ=[2,3,4,1], label=[AttrVar(1), :a, :b, AttrVar(2)])
@test roundtrip_json_acset(ldds) == ldds
json = generate_json_acset(ldds)
@test all(row -> haskey(row, :_id), json[:Label])

# Schema serialization
######################

function roundtrip_json_acset_schema(schema::Schema)
  mktempdir() do dir
    path = joinpath(dir, "schema.json")
    write_json_acset_schema(schema, path)
    read_json_acset_schema(BasicSchema, path)
  end
end

json_schema = JSONSchema.Schema(acset_schema_json_schema())

for schema in [SchGraph, SchWeightedGraph, SchLabeledDDS]
  schema_dict = generate_json_acset_schema(schema)
  @test isnothing(JSONSchema.validate(json_schema, schema_dict))
  @test roundtrip_json_acset_schema(schema) == schema
end

end
