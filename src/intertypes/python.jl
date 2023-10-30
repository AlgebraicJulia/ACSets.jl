export generate_python_classes

function topy(intertype::InterType)
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
    TypeRef(to) => "\"$(string(to))\""
  end
end

function python_class(io::IO, name, fields)
  println(io, "class ", name, "(InterTypeBase):")
  indent = "    "
  for (name, typestr) in fields
    println(io, indent, name, ": ", typestr)
  end
end

function topy(io::IO, decl::InterTypeDecl)
  @match decl begin
    Alias(name, type) =>
      println(io, "$name = $(topy(type))\n")
    Struct(name, fields) =>
      python_class(io, name, [(field.name, topy(field.type)) for field in fields])
    SumType(name, variants) => begin
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
    end
  end
end

PYTHON_PREAMBLE = """
from typing import Annotated, Literal

from pydantic import BaseModel, Field, PlainSerializer, TypeAdapter

SafeInt = Annotated[
    int, PlainSerializer(lambda x: str(x), return_type=str, when_used="json")
]


class InterTypeBase(BaseModel):
    def model_dump_json(self, *args, **kwargs):
        return super().model_dump_json(*args, **kwargs, by_alias=True)
"""

function generate_python_classes(infile, outfile)
  in = Meta.parseall(Base.read(infile, String))
  Base.remove_linenums!(in)
  in = macroexpand(InterTypeDeclImplPrivate, in)
  decls = parse_intertype_decls(in.args)
  open(outfile, "w") do io
    print(io, PYTHON_PREAMBLE)
    print(io, "\n\n")
    for decl in decls
      topy(io, decl)
      print(io, "\n\n")
    end
  end
end
