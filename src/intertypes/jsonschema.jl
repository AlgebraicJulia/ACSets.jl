export tojsonschema, JSONTarget

using JSON3

function fieldproperties(fields::Vector{Field{InterType}})
  map(fields) do field
    field.name => tojsonschema(field.type)
  end
end

"""
    tojsonschema(type::InterType)

Convert an InterType to a JSONSchema representation.

TODO: We could use multiple dispatch instead of the `@match` here, which might
be cleaner
"""
function tojsonschema(type::InterType)
  @match type begin
    I32 => Object(
      :type => "integer",
      Symbol("\$comment") => "I32",
      :minimum => typemin(Int32),
      :maximum => typemax(Int32)
    )
    U32 => Object(
      :type => "integer",
      Symbol("\$comment") => "U32",
      :minimum => typemin(UInt32),
      :maximum => typemax(UInt32)
    )
    I64 => Object(
      :type => "string",
      Symbol("\$comment") => "I64"
    )
    U64 => Object(
      :type => "string",
      Symbol("\$comment") => "U64"
    )
    F64 => Object(
      :type => "number",
      Symbol("\$comment") => "F64"
    )
    Boolean => Object(
      :type => "boolean",
      Symbol("\$comment") => "Boolean"
    )
    Str => Object(
      :type => "string",
      Symbol("\$comment") => "Str"
    )
    Sym => Object(
      :type => "string",
      Symbol("\$comment") => "Sym"
    )
    Binary => Object(
      :type => "string",
      :contentEncoding => "base64",
      Symbol("\$comment") => "Binary"
    )
    OptionalType(elemtype) => begin
      schema = tojsonschema(elemtype)
      schema[:type] = [schema[:type], "null"]
      schema
    end
    ObjectType(elemtype) => Object(
      :type => "object",
      :additionalProperties => tojsonschema(elemtype)
    )
    List(elemtype) => Object(
      :type => "array",
      :items => tojsonschema(elemtype)
    )
    Map(keytype, valuetype) => Object(
      :type => "array",
      :items => Object(
        :type => "object",
        :properties => Object(
          :key => tojsonschema(keytype),
          :value => tojsonschema(valuetype)
        )
      )
    )
    Record(fields) => recordtype(fields)
    Sum(variants) => Object(
      "oneOf" => Vector{Object}(varianttype.(variants))
    )
    Annot(desc, innertype) => begin
      innerschematype = tojsonschema(innertype)
      innerschematype["description"] = desc
      innerschematype
    end
    TypeRef(to) => reftype(string(toexpr(to)))
  end
end

reftype(name) = Object(
  Symbol("\$ref") => "#/\$defs/$(name)"
)

recordtype(fields) = Object(
  :type => "object",
  :properties => Object(fieldproperties(fields)...),
  :required => string.(nameof.(fields))
)

varianttype(variant) = Object(
  :type => "object",
  :properties => Object(
    :tag => Object(
      :const => string(variant.tag)
    ),
    fieldproperties(variant.fields)...
  ),
  :required => string.(nameof.(variant.fields))
)

function acsettype(spec)
  tablespecs = map(objects(spec.schema)) do ob
    idfield = Field{InterType}(:_id, U32)
    homfields = map(homs(spec.schema; from=ob, just_names=true)) do f
      Field{InterType}(f, U32)
    end
    attrfields = map(attrs(spec.schema; from=ob)) do (f, _, t)
      Field{InterType}(f, spec.schema.typing[t])
    end
    Field{InterType}(ob, List(Record([idfield; homfields; attrfields])))
  end
  Object(
    :type => "object",
    :properties => recordtype(tablespecs)
  )
end

"""
    JSONTarget  

Specifies a serialization target of JSON Schema when
generating a module.

TODO: This should really be called something like JSONSchemaTarget.
"""
struct JSONTarget <: SerializationTarget end

# TODO: Should this be ::JSONTarget instead of ::Type{JSONTarget} so
# that we pass in `JSONTarget()` instead of `JSONTarget`?
function generate_module(
  mod::InterTypeModule, ::Type{JSONTarget}, path
  ;ac=JSON3.AlignmentContext(indent=2)
)
  defs = Pair{Symbol, Object}[]
  for (name, decl) in mod.declarations
    @match decl begin
      Alias(type) => push!(defs, name => tojsonschema(type))
      Struct(fields) => push!(defs, name => recordtype(fields))
      VariantOf(parent) => begin
        sum = mod.declarations[parent]
        variant = only(filter(v -> v.tag == name, sum.variants))
        push!(defs, name => varianttype(variant))
      end
      SumType(variants) => 
        push!(defs, name => Object(:oneOf => reftype.([v.tag for v in variants])))
      NamedACSetType(spec) => 
        push!(defs, name => acsettype(spec))
      _ => nothing
    end
  end
  schema = Object(
    Symbol("\$schema") => "http://json-schema.org/draft-07/schema#",
    Symbol("\$defs") => Object(defs...)
  )
  schema_filepath = joinpath(path, string(mod.name)*"_schema.json") 
  open(schema_filepath, "w") do io
    JSON3.pretty(io, schema, ac)
  end
end
