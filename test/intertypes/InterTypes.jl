module TestInterTypes

using ACSets
using ACSets.InterTypes

using Test
using OrderedCollections
import JSON
import JSON3
import JSONSchema
using JavaCall

function testjson(x::T) where {T}
  (x == JSON3.read(JSON3.write(x), T))
end

vals = Any[
  Int32(5),
  UInt32(5),
  Int64(5),
  hash("hello"),
  "hello",
  :hello,
  UInt8[5, 3, 8],
  true,
  ["python"],
  OrderedDict(:jl => :py),
  (name=:jake,),
  (:finn,)
]

for val in vals
  @test testjson(val)
end

@intertypes "simpleast.it" module simpleast end

using .simpleast

t = Plus([Constant(ConstInt(1)), Constant(ConstInt(2))])

s = JSON3.write(t)

@test s isa String

@test JSON3.read(s, Term) == t
@test JSON3.read(s, Plus) == t

generate_module(simpleast, JSONTarget)

simpleast_schema = JSONSchema.Schema(read("simpleast_schema.json", String))

@test JSONSchema._validate(simpleast_schema, JSON.parse(s), "Term") === nothing

@intertypes "model.it" module model
  import ..simpleast
end

using .model

e = Equation(t, t)

m = Model([:x], [e])

@test testjson(m)

@intertypes "wgraph.it" module wgraph end

using .wgraph

g = EDWeightedGraph()
add_parts!(g, :V, 2)
add_part!(g, :E, src=1, tgt=2, weight=EdgeData(:mass_ave, 42))

@test testjson(g)

sg = WeightedGraph{Symbol}()

add_parts!(sg, :V, 2)
add_part!(sg, :E, src=1, tgt=2, weight=:mass_ave)

@test testjson(sg)

generate_module(wgraph, JSONTarget)

wgraph_schema = JSONSchema.Schema(read("wgraph_schema.json", String))

@test JSONSchema._validate(wgraph_schema, JSON.parse(JSON3.write(g)), "EDWeightedGraph") === nothing

@intertypes "objects.it" module objects end

using .objects

md = Metadata("lion", Object{String}(:fierceness => "high"))

@test md.name == "lion"
@test md.attributes[:fierceness] == "high"

@test testjson(md)

generate_module(objects, JSONTarget)

objects_schema = JSONSchema.Schema(read("objects_schema.json", String))

@test JSONSchema._validate(objects_schema, JSON.parse(JSON3.write(md)), "Metadata") === nothing

@intertypes "optionals.it" module optionals end

using .optionals

x = NullableInt(nothing)

@test isnothing(x.value)

@test testjson(x)

y = NullableInt(5)

@test y.value == 5

@test testjson(y)

generate_module(optionals, JSONTarget)

optionals_schema = JSONSchema.Schema(read("optionals_schema.json", String))

@test JSONSchema._validate(optionals_schema, JSON.parse(JSON3.write(x)), "NullableInt") === nothing
@test JSONSchema._validate(optionals_schema, JSON.parse(JSON3.write(y)), "NullableInt") === nothing

# Python Integration Tests

@static if !Sys.iswindows()
  using CondaPkg
  using PythonCall

  CondaPkg.add("pydantic")
  CondaPkg.add_pip("acsets")

  dir = @__DIR__
  write(dir * "/intertypes.py", InterTypes.INTERTYPE_PYTHON_MODULE)
  generate_module(simpleast, PydanticTarget, dir)
  generate_module(model, PydanticTarget, dir)
  generate_module(wgraph, PydanticTarget, dir)
  generate_module(objects, PydanticTarget, dir)
  generate_module(optionals, PydanticTarget, dir)

  pushfirst!(PyList(pyimport("sys")."path"), Py(dir))

  pyast = pyimport("simpleast")
  pymodel = pyimport("model")
  pywgraph = pyimport("wgraph")
  pyobjects = pyimport("objects")
  pyoptionals = pyimport("optionals")
  pyjson = pyimport("json")

  function python_roundtrip(pythontype, val)
    py_val = pythontype.model_validate_json(Py(JSON3.write(val)))
    py_val_str = string(py_val.model_dump_json())

    JSON3.read(py_val_str, typeof(val)) == val
  end

  @test python_roundtrip(pymodel.Model, m)
  @test python_roundtrip(pyobjects.Metadata, md)
  @test python_roundtrip(pyoptionals.NullableInt, x)
  @test python_roundtrip(pyoptionals.NullableInt, y)

  py_g = pywgraph.EDWeightedGraph.read_json(Py(JSON3.write(g)))
  py_g_str = string(py_g.to_json_str())

  @test JSON3.read(py_g_str, EDWeightedGraph) == g
end

# Java Integration Tests

java_dir = joinpath(@__DIR__, "java/lib/src/main/java")
generate_module(simpleast, JacksonTarget, java_dir)
generate_module(model, JacksonTarget, java_dir)

cd("java")
run(`sh gradlew build`)
cd("..")

push!(JavaCall.cp, joinpath(@__DIR__, "java/lib/build/libs/lib.jar"))

JavaCall.init()

ObjectMapper = @jimport com.fasterxml.jackson.databind.ObjectMapper
om = ObjectMapper(())

function java_roundtrip(javatype, val)
  java_val = jcall(om, "readValue", JObject, (JString, JClass), JSON3.write(val), classforname(javatype))
  java_val_str = jcall(om, "writeValueAsString", JString, (JObject,), java_val)

  JSON3.read(java_val_str, typeof(val)) == val
end

@test java_roundtrip("simpleast.Term", t)

end
