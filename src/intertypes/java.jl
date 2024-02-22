export JacksonTarget

struct JavaFile
  name::String # without .java extension
  content::String # without "package xxxx" at the top
end

function tojava(intertype::InterType)
  @match intertype begin
    I32 => "int"
    U32 => "int"
    I64 => "long"
    U64 => "long"
    F64 => "double"
    Boolean => "boolean"
    Str => "String"
    Sym => "String"
    Binary => "String"
    OptionalType(elemtype) => "Optional<$(tojava(elemtype))>"
    ObjectType(elemtype) => "LinkedHashMap<String, $(tojava(elemtype))>"
    List(elemtype) => "ArrayList<$(tojava(elemtype))>"
    Map(keytype, valuetype) => "LinkedHashMap<$(tojava(keytype)), $(tojava(elemtype))>"
    Record(_) => error("no native record type for java")
    Sum(_) => error("no native sum type for java")
    Annot(_, type) => tojava(type)
    TypeRef(to) => string(toexpr(to))
  end
end

function separate(f, io, xs; separator=", ")
  sep = false
  for x in xs
    if sep
      print(io, separator)
    end
    sep = true
    f(io, x)
  end
end

RECORD_IMPORTS = """
import java.util.Optional;
import java.util.ArrayList;
import java.util.LinkedHashMap;
"""

function java_record!(io::IO, name, fields; implements=nothing, typename=false)
  println(io, RECORD_IMPORTS);
  if typename
    println(io, "import com.fasterxml.jackson.annotation.JsonTypeName;")
    println(io)
    println(io, "@JsonTypeName(\"$(name)\")")
  end
  print(io, "public record ", name, "(")
  separate(io, fields) do io, field
    print(io, tojava(field.type), " ", field.name)
  end
  print(io, ")")
  if !isnothing(implements)
    print(io, " implements ", implements)
  end
  print(io, " {}")
end

function java_record(name, fields; implements=nothing, typename=false)
  b = IOBuffer()
  java_record!(b, name, fields; implements, typename)
  JavaFile(string(name), String(take!(b)))
end

function java_sumtype_parent!(io, name, variants)
  println(io, "import com.fasterxml.jackson.annotation.*;")
  println(io, "import com.fasterxml.jackson.annotation.JsonTypeInfo.*;")
  println(io)
  println(io, "@JsonTypeInfo(use=Id.NAME, include=As.PROPERTY, property=\"_type\")")
  println(io, "@JsonSubTypes({")
  separate(io, variants; separator=",\n") do io, v
    print(io, "    @JsonSubTypes.Type(value = $(v.tag).class, name = \"$(v.tag)\")")
  end
  println(io, "\n})")
  print(io, "public sealed interface ", name, " permits ")
  separate(io, variants) do io, v
    print(io, v.tag)
  end
  print(io, " {}")
end

function java_sumtype(name, variants)
  parent_buffer = IOBuffer()
  java_sumtype_parent!(parent_buffer, name, variants)
  parentfile = JavaFile("$(name)", String(take!(parent_buffer)))
  variantfiles = map(variants) do v
    java_record(v.tag, v.fields; implements=name, typename=true)
  end
  [parentfile; variantfiles]
end

function tojava(name, decl::InterTypeDecl)
  @match decl begin
    Struct(fields) => [java_record(name, fields)]
    SumType(variants) => java_sumtype(name, variants)
    VariantOf(_) => []
    _ => error("unsupported declaration for java")
  end
end

"""
    JacksonTarget

Targets the creation of a directory full of `.java` files that use the
Jackson JSON library.
"""
struct JacksonTarget <: LanguageTarget end

function generate_module(mod::InterTypeModule, ::Type{JacksonTarget}, path)
  files = JavaFile[]
  for (name, decl) in mod.declarations
    append!(files, tojava(name, decl))
  end
  outdir = joinpath(path, string(mod.name))
  mkpath(outdir)
  for file in files
    open(joinpath(outdir, file.name * ".java"), "w") do io
      println(io, "package $(mod.name);")
      println(io)
      print(io, file.content)
    end
  end
end
