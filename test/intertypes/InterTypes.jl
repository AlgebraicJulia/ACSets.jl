module TestInterTypes

using ACSets.InterTypes
using Test
using OrderedCollections
import JSON3


include(as_intertypes(), "ast.it")

@test intertype(Term) isa InterType

@test intertype_to_jsonschema(intertype(Term)) isa InterTypes.Object

function testjson(x::T) where {T}
  roundtrip = (x == jsonread(jsonwrite(x), T))
  schema = intertype_to_jsonschema(intertype(T)) isa InterTypes.Object
  roundtrip && schema
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

t = Plus([Constant(ConstInt(1)), Constant(ConstInt(2))])

@test jsonwrite(t) isa String

s = jsonwrite(t)

@test jsonread(s, Term) == t


@static if !Sys.iswindows()
  using CondaPkg
  using PythonCall

  CondaPkg.add("pydantic")

  dir = @__DIR__
  generate_python_classes(dir * "/ast.it", dir * "/ast_generated.py")

  pushfirst!(PyList(pyimport("sys")."path"), Py(dir))

  pyast = pyimport("ast_generated")
  pyjson = pyimport("json")

  py_t = pyast.term_adapter.validate_python(pyjson.loads(Py(s)))
  s′ = string(py_t.model_dump_json())

  @test jsonread(s′, Term) == t

  rm(dir * "/ast_generated.py")
end

end
