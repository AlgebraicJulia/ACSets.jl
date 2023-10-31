module TestInterTypes

using ACSets.InterTypes
using Test
using CondaPkg
using PythonCall

CondaPkg.add("pydantic")

include(as_intertypes(), "ast.it")

@test intertype(Term) isa InterType

t = Plus([Constant(ConstInt(1)), Constant(ConstInt(2))])

@test jsonwrite(t) isa String

s = jsonwrite(t)

@test jsonread(s, Term) == t

dir = @__DIR__
generate_python_classes(dir * "/ast.it", dir * "/ast_generated.py")

pushfirst!(PyList(pyimport("sys")."path"), Py(dir))

pyast = pyimport("ast_generated")
pyjson = pyimport("json")

py_t = pyast.term_adapter.validate_python(pyjson.loads(Py(s)))
s′ = string(py_t.model_dump_json())

@test jsonread(s′, Term) == t

rm(dir * "ast_generated.py")

end
