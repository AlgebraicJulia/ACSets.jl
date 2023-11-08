export generate_python_module

function topy(intertype::InterType; forward_ref=true)
  @match intertype begin
    I32 => "int"
    U32 => "int"
    I64 => "SafeInt"
    U64 => "SafeInt"
    F64 => "float"
    Boolean => "bool"
    Str => "str"
    Sym => "str"
    Binary => "str"
    List(elemtype) => "list[$(topy(elemtype))]"
    Map(keytype, valuetype) => "OrderedDict[$(topy(keytype)), $(topy(valuetype))]"
    Record(_) => error("no native record type for python")
    Sum(_) => error("no native sum type for python")
    Annot(_, type) => topy(type)
    TypeRef(to) =>
      if forward_ref
        "\"$(string(toexpr(to)))\""
      else
        string(toexpr(to))
      end
  end
end

indent = "    "

function python_class(io::IO, name, fields)
  println(io, "class ", name, "(InterTypeBase):")
  for (name, typestr) in fields
    println(io, indent, name, ": ", typestr)
  end
end

function python_sumtype(io::IO, name, variants)
  for variant in variants
    regularfields = [(field.name, topy(field.type)) for field in variant.fields]
    tagstr = "\"$(variant.tag)\""
    tagfield = ("tag", "Literal[$tagstr] = Field(default=$tagstr, alias=\"_type\", repr=False)")
    python_class(io, variant.tag, [tagfield, regularfields...])
    print(io, "\n\n")
  end
  print(io, name, " = Annotated[")
  join(io, nameof.(variants), " | ")
  print(io, ", Field(discriminator=\"tag\")]")
  println(io)
  print(io, """
        $(lowercase(string(name)))_adapter: TypeAdapter[$name] = TypeAdapter($name)
        """)
  print(io, "\n\n")
end

function python_schema(io::IO, name, schema::TypedSchema{Symbol, InterType})
  println(io, "class $(name):")
  for x in objects(schema)
    println(io, indent, "$x = Ob(name=\"$x\")")
  end
  for (f, d, c) in homs(schema)
    println(io, indent, "$f = Hom(name=\"$f\", dom=$d, codom=$c)")
  end
  for T in attrtypes(schema)
    println(io, indent, "$T = AttrType(name=\"$T\", ty=$(topy(schema.typing[T]; forward_ref=false)))")
  end
  for (a, d, c) in attrs(schema)
    println(io, indent, "$a = Attr(name=\"$a\", dom=$d, codom=$d)")
  end
  println(
    io,
    """
        schema = Schema(
            name=\"$name\",
            obs=[$(join(objects(schema), ", "))],
            homs=[$(join(homs(schema; just_names=true), ", "))],
            attrtypes=[$(join(attrtypes(schema), ", "))],
            attrs=[$(join(attrs(schema; just_names=true), ", "))]
        )
    """)
  print(io, "\n\n")
end

function python_abstract_acset(io::IO, name, parent)
  parent = if !isnothing(parent)
    parent
  else
    :ACSet
  end
  println(io, "class $(name)($parent):")
  println(io, indent, "pass")
  print(io, "\n\n")
end

function python_named_acset_type(io::IO, name, spec)
  abstract_type = if !isnothing(spec.abstract_type)
    spec.abstract_type
  else
    :ACSet
  end
  println(
    io,
    """
    class $(name)($abstract_type):

        def __init__(self, name=\"$name\", schema=$(spec.schemaname).schema):
            super($name, self).__init__(name, schema)

        @classmethod
        def import_pydantic(cls, d: Any):
            return ACSet.import_pydantic(cls, \"$name\", $(spec.schemaname).schema, d)

        @classmethod
        def read_json(cls, s: str):
            return ACSet.read_json(cls, \"$name\", $(spec.schemaname).schema, s)
    """
  )
  print(io, "\n\n")
end

function topy(io::IO, name, decl::InterTypeDecl)
  @match decl begin
    Alias(type) => begin
      println(io, "$name = $(topy(type))\n")
      print(io, "\n\n")
    end
    Struct(fields) => begin
      python_class(io, name, [(field.name, topy(field.type)) for field in fields])
      print(io, "\n\n")
    end
    SumType(variants) => python_sumtype(io, name, variants)
    VariantOf(_) => nothing
    SchemaDecl(schema) => python_schema(io, name, schema)
    AbstractACSetType(parent) => python_abstract_acset(io, name, parent)
    NamedACSetType(spec) => python_named_acset_type(io, name, spec)
  end
end

PYTHON_PREAMBLE = """
from typing import Literal, Annotated

from pydantic import Field, TypeAdapter

from intertypes import SafeInt, InterTypeBase
"""

INTERTYPE_PYTHON_MODULE = """
from typing import Annotated

from pydantic import BaseModel, PlainSerializer

SafeInt = Annotated[
    int, PlainSerializer(lambda x: str(x), return_type=str, when_used="json")
]


class InterTypeBase(BaseModel):
    def model_dump_json(self, *args, **kwargs):
        return super().model_dump_json(*args, **kwargs, by_alias=True)
"""

function generate_python_module(jmod::Module, outdir)
  mod = jmod.Meta
  outfile = outdir * "/" * string(mod.name) * ".py"
  open(outfile, "w") do io
    print(io, PYTHON_PREAMBLE)
    for (name, importedmod) in mod.imports
      if name != importedmod.name
        println(io, "import $(importedmod.name) as $name")
      else
        println(io, "import $name")
      end
    end
    print(io, "\n\n")
    for (name, decl) in mod.declarations
      topy(io, name, decl)
    end
  end
end
