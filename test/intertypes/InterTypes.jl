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

@test testjson(m)

generate_module(wgraph, JSONTarget)

wgraph_schema = JSONSchema.Schema(read("wgraph_schema.json", String))

@test JSONSchema._validate(wgraph_schema, JSON.parse(jsonwrite(g)), "EDWeightedGraph") === nothing

@intertypes "objects.it" module objects end

using .objects

m = Metadata("lion", Object{String}(:fierceness => "high"))

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

  pushfirst!(PyList(pyimport("sys")."path"), Py(dir))

  pyast = pyimport("simpleast")
  pymodel = pyimport("model")
  pywgraph = pyimport("wgraph")
  pyjson = pyimport("json")

  py_m = pymodel.Model.model_validate_json(Py(jsonwrite(m)))
  py_m_str = string(py_m.model_dump_json())

  @test jsonread(py_m_str, Model) == m

  py_g = pywgraph.EDWeightedGraph.read_json(Py(jsonwrite(g)))
  py_g_str = string(py_g.to_json_str())

  @test jsonread(py_g_str, EDWeightedGraph) == g
end

end
