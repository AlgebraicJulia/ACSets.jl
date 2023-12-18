export JSONTarget, tojsonschema, jsonwrite, jsonread

using OrderedCollections
using Base64
using MLStyle
import JSON3
using CompTime

using ..ACSetInterface
using ..DenseACSets

struct JSONFormat
end

struct InterTypeConversionError <: Exception
  expected::InterType
  got::Any
end

function read(format, type::Type, x)
  throw(InterTypeConversionError(intertype(type), x))
end

function joinwith(io::IO, f, xs, separator)
  for x in xs[1:end-1]
    f(io, x)
    print(io, separator)
  end
  f(io, xs[end])
end

jsonwrite(x) = sprint(jsonwrite, x)
jsonwrite(io::IO, x) = write(io, JSONFormat(), x)
function jsonread(s::String, ::Type{T}) where {T}
  json = JSON3.read(s)
  read(JSONFormat(), T, json)
end

intertype(::Type{Nothing}) = Unit
read(::JSONFormat, ::Type{Nothing}, ::Nothing) = nothing
write(io::IO, ::JSONFormat, ::Nothing) = print(io, "null")

intertype(::Type{Int32}) = I32
read(::JSONFormat, ::Type{Int32}, s::Real) = Int32(s)
write(io::IO, ::JSONFormat, d::Int32) = print(io, d)

intertype(::Type{UInt32}) = U32
read(::JSONFormat, ::Type{UInt32}, s::Real) = UInt32(s)
write(io::IO, ::JSONFormat, d::UInt32) = print(io, d)

intertype(::Type{Int64}) = I64
read(::JSONFormat, ::Type{Int64}, s::String) = parse(Int64, s)
read(::JSONFormat, ::Type{Int64}, s::Integer) = Int64(s)
write(io::IO, ::JSONFormat, d::Int64) = print(io, "\"", d, "\"")

intertype(::Type{UInt64}) = U64
read(::JSONFormat, ::Type{UInt64}, s::String) = parse(UInt64, s)
read(::JSONFormat, ::Type{UInt64}, s::Integer) = UInt64(s)
write(io::IO, ::JSONFormat, d::UInt64) = print(io, "\"", d, "\"")

intertype(::Type{Float64}) = F64
read(::JSONFormat, ::Type{Float64}, s::Real) = Float64(s)
write(io::IO, ::JSONFormat, d::Float64) = print(io, d)

intertype(::Type{Bool}) = Boolean
read(::JSONFormat, ::Type{Bool}, s::Bool) = s
write(io::IO, ::JSONFormat, d::Bool) = print(io, d)

intertype(::Type{String}) = Str
read(::JSONFormat, ::Type{String}, s::String) = s
write(io::IO, ::JSONFormat, d::String) = JSON3.write(io, d)

intertype(::Type{Symbol}) = Sym
read(::JSONFormat, ::Type{Symbol}, s::String) = Symbol(s)
write(io::IO, ::JSONFormat, d::Symbol) = JSON3.write(io, string(d))

intertype(::Type{Vector{UInt8}}) = Binary
read(::JSONFormat, ::Type{Vector{UInt8}}, s::String) = base64decode(s)
function write(io::IO, ::JSONFormat, d::Vector{UInt8})
  print(io, "\"")
  Base.write(io, base64encode(d))
  print(io, "\"")
end

intertype(::Type{Vector{T}}) where {T} = List(intertype(T))
function read(format::JSONFormat, ::Type{Vector{T}}, s::JSON3.Array) where {T}
  res = T[]
  for elt in s
    push!(res, read(format, T, elt))
  end
  res
end
function write(io::IO, format::JSONFormat, d::Vector{T}) where {T}
  print(io, "[")
  joinwith(io, (io, x) -> write(io, format, x), d, ",")
  print(io, "]")
end

intertype(::Type{OrderedDict{K,V}}) where {K,V} = Map(intertype(K), intertype(V))
function read(format::JSONFormat, ::Type{OrderedDict{K, V}}, s::JSON3.Array) where {K, V}
  res = OrderedDict{K, V}()
  for elt in s
    (;key, value) = read(format, NamedTuple{(:key, :value), Tuple{K, V}}, elt)
    res[key] = value
  end
  res
end
function write(io::IO, format::JSONFormat, d::OrderedDict{K, V}) where {K, V}
  print(io, "[")
  joinwith(io, (io, x) -> write(io, format, (key=x[1], value=x[2])), collect(pairs(d)), ",")
  print(io, "]")
end

function intertype(::Type{T}) where {T<:Tuple}
  types = T.parameters
  Record(map(enumerate(types)) do (i, type)
    Field{InterType}(Symbol("_", i), intertype(type))
  end)
end
function read(format::JSONFormat, ::Type{T}, s::JSON3.Object) where {T<:Tuple}
  keys = Tuple([Symbol("_", i) for i in 1:length(T.parameters)])
  Tuple(read(format, NamedTuple{keys, T}, s))
end
function write(io::IO, format::JSONFormat, d::T) where {T<:Tuple}
  keys = Tuple([Symbol("_", i) for i in 1:length(T.parameters)])
  write(io, format, NamedTuple{keys, T}(d))
end

function intertype(::Type{NamedTuple{names, T}}) where {names, T<:Tuple}
  types = T.parameters
  Record([Field{InterType}(name, intertype(type)) for (name, type) in zip(names, (types))])
end
# TODO: comptime this
function read(format::JSONFormat, ::Type{NamedTuple{names, T}}, s::JSON3.Object) where {names, T<:Tuple}
  keys(s) == Set(names) || error("wrong keys: expected $names got $(keys(s))")
  vals = Any[]
  for (name, type) in zip(names, T.parameters)
    push!(vals, read(format, type, s[name]))
  end
  NamedTuple{names, T}(vals)
end
function write(io::IO, format::JSONFormat, d::NamedTuple{names, T}) where {names, T<:Tuple}
  writeobject(io) do next
    for p in pairs(d)
      next()
      writekv(io, p)
    end
  end
end

function read(format::JSONFormat, ::Type{T}, s::JSON3.Object) where {S, Ts, T <: StructACSet{S, Ts}}
  schema = Schema(S)
  acs = T()
  for ob in objects(schema)
    add_parts!(acs, ob, length(s[ob]))
  end
  for at in attrtypes(schema)
    if haskey(s, at)
      add_parts!(acs, at, length(s[at]))
    end
  end
  typing = Dict{Symbol, Type}(zip(attrtypes(schema), Ts.parameters))
  for ob in objects(schema)
    for jsonobject in s[ob]
      i = jsonobject[:_id]
      for f in homs(schema; from=ob, just_names=true)
        acs[i, f] = read(format, Int, jsonobject[f])
      end
      for (f, _, t) in attrs(schema; from=ob)
        acs[i, f] = read(format, typing[t], jsonobject[f])
      end
    end
  end
  acs
end

writekey(io::IO, key) = print(io, "\"", key, "\":")

function writekv(io, kv::Pair{Symbol, T}) where {T}
  (k, v) = kv
  writekey(io, k)
  write(io, JSONFormat(), v)
end

function writeitems(f, io)
  first = true
  function next()
    if !first
      print(io, ",")
    end
    first = false
  end
  f(next)
end

function writeobject(f, io)
  print(io, "{")
  writeitems(f, io)
  print(io, "}")
end

function writearray(f, io)
  print(io, "[")
  writeitems(f, io)
  print(io, "]")
end

function write(io::IO, format::JSONFormat, acs::ACSet)
  schema = acset_schema(acs)
  writeobject(io) do next
    for ob in objects(schema)
      next()
      writekey(io, ob)
      writearray(io) do next
        for i in parts(acs, ob)
          next()
          writeobject(io) do next
            next()
            writekv(io, :_id => Int32(i))
            for f in homs(schema; from=ob, just_names=true)
              next()
              writekv(io, f => Int32(acs[i, f]))
            end
            for f in attrs(schema; from=ob, just_names=true)
              next()
              writekv(io, f => acs[i, f])
            end
          end
        end
      end
    end
    for at in attrtypes(schema)
      next()
      writekey(io, at)
      writearray(io) do next
        for i in parts(acs, at)
          next()
          write(io, format, (:_id, i))
        end
      end
    end
  end
end

const Object = OrderedDict{String, Any}

function fieldproperties(fields::Vector{Field{InterType}})
  map(fields) do field
    string(field.name) => tojsonschema(field.type)
  end
end

function tojsonschema(type::InterType)
  @match type begin
    I32 => Object(
      "type" => "integer",
      "\$comment" => "I32",
      "minimum" => typemin(Int32),
      "maximum" => typemax(Int32)
    )
    U32 => Object(
      "type" => "integer",
      "\$comment" => "U32",
      "minimum" => typemin(UInt32),
      "maximum" => typemax(UInt32)
    )
    I64 => Object(
      "type" => "string",
      "\$comment" => "I64"
    )
    U64 => Object(
      "type" => "string",
      "\$comment" => "U64"
    )
    F64 => Object(
      "type" => "number",
      "\$comment" => "F64"
    )
    Boolean => Object(
      "type" => "boolean",
      "\$comment" => "Boolean"
    )
    Str => Object(
      "type" => "string",
      "\$comment" => "Str"
    )
    Sym => Object(
      "type" => "string",
      "\$comment" => "Sym"
    )
    Binary => Object(
      "type" => "string",
      "contentEncoding" => "base64",
      "\$comment" => "Binary"
    )
    List(elemtype) => Object(
      "type" => "array",
      "items" => tojsonschema(elemtype)
    )
    Map(keytype, valuetype) => Object(
      "type" => "array",
      "items" => Object(
        "type" => "object",
        "properties" => Object(
          "key" => tojsonschema(keytype),
          "value" => tojsonschema(valuetype)
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
  "\$ref" => "#/\$defs/$(name)"
)

recordtype(fields) = Object(
  "type" => "object",
  "properties" => Object(fieldproperties(fields)),
  "required" => string.(nameof.(fields))
)

varianttype(variant) = Object(
  "type" => "object",
  "properties" => Object(
    "tag" => Object(
      "const" => string(variant.tag)
    ),
    fieldproperties(variant.fields)...
  ),
  "required" => string.(nameof.(variant.fields))
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
    "type" => "object",
    "properties" => recordtype(tablespecs)
  )
end

"""
    JSONTarget  

Specifies a serialization target of JSON Schema when
generating a module.
"""
struct JSONTarget <: SerializationTarget end

function generate_module(
  mod::InterTypeModule, ::Type{JSONTarget}, path
  ;ac=JSON3.AlignmentContext(indent=2)
)
  defs = Pair{String, Object}[]
  for (name, decl) in mod.declarations
    sname = string(name)
    @match decl begin
      Alias(type) => push!(defs, sname => tojsonschema(type))
      Struct(fields) => push!(defs, sname => recordtype(fields))
      VariantOf(parent) => begin
        sum = mod.declarations[parent]
        variant = only(filter(v -> v.tag == name, sum.variants))
        push!(defs, sname => varianttype(variant))
      end
      SumType(variants) => 
        push!(defs, sname => Object("oneOf" => reftype.([v.tag for v in variants])))
      NamedACSetType(spec) => 
        push!(defs, sname => acsettype(spec))
      _ => nothing
    end
  end
  schema = Object(
    "\$schema" => "http://json-schema.org/draft-07/schema#",
    "\$defs" => Object(defs)
  )
  schema_filepath = joinpath(path, string(mod.name)*"_schema.json") 
  open(schema_filepath, "w") do io
    JSON3.pretty(io, schema, ac)
  end
end
