module TestInterTypes

using ACSets
using ACSets.InterTypes

using Test
using OrderedCollections
import JSON
import JSON3
import JSONSchema

function testjson(x::T) where {T}
  (x == jsonread(jsonwrite(x), T))
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

s = jsonwrite(t)

@test s isa String

@test jsonread(s, Term) == t

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

generate_module(wgraph, JSONTarget)

wgraph_schema = JSONSchema.Schema(read("wgraph_schema.json", String))

@test JSONSchema._validate(wgraph_schema, JSON.parse(jsonwrite(g)), "EDWeightedGraph") === nothing

@intertypes "objects.it" module objects end

using .objects

md = Metadata("lion", Object{String}(:fierceness => "high"))

@test md.name == "lion"
@test md.attributes[:fierceness] == "high"

@test testjson(md)

generate_module(objects, JSONTarget)

objects_schema = JSONSchema.Schema(read("objects_schema.json", String))

@test JSONSchema._validate(objects_schema, JSON.parse(jsonwrite(md)), "Metadata") === nothing

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

@test JSONSchema._validate(optionals_schema, JSON.parse(jsonwrite(x)), "NullableInt") === nothing
@test JSONSchema._validate(optionals_schema, JSON.parse(jsonwrite(y)), "NullableInt") === nothing

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
    py_val = pythontype.model_validate_json(Py(jsonwrite(val)))
    py_val_str = string(py_val.model_dump_json())

    jsonread(py_val_str, typeof(val)) == val
  end

  @test python_roundtrip(pymodel.Model, m)
  @test python_roundtrip(pyobjects.Metadata, md)
  @test python_roundtrip(pyoptionals.NullableInt, x)
  @test python_roundtrip(pyoptionals.NullableInt, y)

  py_g = pywgraph.EDWeightedGraph.read_json(Py(jsonwrite(g)))
  py_g_str = string(py_g.to_json_str())

  @test jsonread(py_g_str, EDWeightedGraph) == g
end

end
