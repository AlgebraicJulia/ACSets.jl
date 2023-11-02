module TestInterTypes

using ACSets
using ACSets.InterTypes
using Test
using OrderedCollections
import JSON3

function testjson(x::T) where {T}
  (x == jsonread(jsonwrite(x), T))
end

vals = Any[
  Int32(5),
  UInt32(5),
  Int64(5),
  UInt64(5),
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
add_part!(g, :E, src=1, tgt=2, weight=EdgeData(:mass_ave, 42.0))

@test jsonwrite(g) == JSON3.write(generate_json_acset(g))

@static if !Sys.iswindows()
  using CondaPkg
  using PythonCall

  CondaPkg.add("pydantic")

  dir = @__DIR__
  write(dir * "/intertypes.py", InterTypes.INTERTYPE_PYTHON_MODULE)
  generate_python_module(simpleast, dir)
  generate_python_module(model, dir)

  pushfirst!(PyList(pyimport("sys")."path"), Py(dir))

  pyast = pyimport("simpleast")
  pymodel = pyimport("model")
  pyjson = pyimport("json")

  py_m = pymodel.Model.model_validate_json(Py(jsonwrite(m)))
  s′ = string(py_m.model_dump_json())

  @test jsonread(s′, Model) == m
end

end
