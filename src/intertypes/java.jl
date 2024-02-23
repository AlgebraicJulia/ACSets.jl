export JacksonTarget

struct JavaFile
  name::String # without .java extension
  content::String # without "package xxxx" at the top
end

function tojava(intertype::InterType)
  @match intertype begin
    I32 => "Integer"
    U32 => "Integer"
    I64 => "Long"
    U64 => "Long"
    F64 => "Double"
    Boolean => "Boolean"
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

function java_list_literal(f, io, xs)
  print(io, "new ArrayList<>(Arrays.asList(")
  separate(f, io, xs)
  print(io, "))")
end

function java_schema(name, schema::TypedSchema{Symbol, InterType})
  io = IOBuffer()
  println(io, """
    import acsets4j.*;

    import java.util.ArrayList;
    import java.util.Arrays;

    public class $name {
      public static Schema schema = new Schema(
    """)
  java_list_literal(io, objects(schema)) do io, ob
    print(io, "new Ob(\"$ob\")")
  end
  println(io, ",")
  java_list_literal(io, homs(schema)) do io, (f, d, c)
    print(io, "new Hom(\"$f\", \"$d\", \"$c\")")
  end
  println(io, ",")
  java_list_literal(io, attrtypes(schema)) do io, T
    print(io, "new AttrType(\"$T\", $(tojava(schema.typing[T])).class)")
  end
  println(io, ",")
  java_list_literal(io, attrs(schema)) do io, (f, d, c)
    print(io, "new Attr(\"$f\", \"$d\", \"$c\")")
  end
  println(io, ");\n}")
  [JavaFile(string(name), String(take!(io)))]
end

function java_abstract_acset(name, parent)
  parent = if !isnothing(parent)
    parent
  else
    :ACSet
  end
  content = """
    import acsets4j.*;

    public abstract class $(name) extends $parent {}
  """
  [JavaFile(string(name), content)]
end

function java_named_acset_type(name, spec)
  abstract_type = if !isnothing(spec.abstract_type)
    spec.abstract_type
  else
    :ACSet
  end
  serializername = string(name, "Serializer")
  serializer_content = """
    import acsets4j.*;

    public class $serializername extends ACSetSerializer<$(name)> { }
  """
  serializer = JavaFile(serializername, serializer_content)
  deserializername = string(name, "Deserializer")
  deserializer_content = """
    import java.io.IOException;

    import acsets4j.*;

    import com.fasterxml.jackson.core.JsonParser;
    import com.fasterxml.jackson.core.JsonProcessingException;
    import com.fasterxml.jackson.databind.DeserializationContext;

    public class $deserializername extends ACSetDeserializer<$(name)> {
      public $name deserialize(JsonParser jp, DeserializationContext ctxt) 
          throws IOException, JsonProcessingException {
            $(name) acs = new $(name)();
            deserializeInto(acs, jp, ctxt);
            return acs;
        }
    }
  """
  deserializer = JavaFile(deserializername, deserializer_content)
  content = """
    import acsets4j.*;

    import com.fasterxml.jackson.databind.annotation.JsonDeserialize;
    import com.fasterxml.jackson.databind.annotation.JsonSerialize;

    @JsonSerialize(using = $(serializername).class)
    @JsonDeserialize(using = $(deserializername).class)
    public class $name extends $abstract_type {
        public static Schema schema = $(spec.schemaname).schema;

        @Override
        public Schema schema() {
            return schema;
        }
    }
  """
  class = JavaFile(string(name), content)
  [class, serializer, deserializer]
end

function tojava(name, decl::InterTypeDecl)
  @match decl begin
    Struct(fields) => [java_record(name, fields)]
    SumType(variants) => java_sumtype(name, variants)
    VariantOf(_) => []
    SchemaDecl(schema) => java_schema(name, schema)
    AbstractACSetType(parent) => java_abstract_acset(name, parent)
    NamedACSetType(spec) => java_named_acset_type(name, spec)
    _ => error("unsupported declaration for java: $decl")
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
      for (name, importedmod) in mod.imports
        if name != importedmod.name
          error("java does not support import aliasing")
        end
      end
      print(io, file.content)
    end
  end
end
