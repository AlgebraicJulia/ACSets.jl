module InterTypes
export InterType, InterTypeDecl, Object, Optional,
  LanguageTarget, SerializationTarget, generate_module, intertype, @intertypes

using MLStyle
using OrderedCollections
using ..Schemas
import ..Schemas: toexpr

struct Field{T}
  name::Symbol
  type::T
end

Base.nameof(field::Field) = field.name

struct Variant{T}
  tag::Symbol
  fields::Vector{Field{T}}
end

Base.nameof(variant::Variant) = variant.tag

@data RefPath begin
  RefHere(name::Symbol)
  RefThere(mod::RefPath, name::Symbol)
end

function RefPath(s::Symbol)
  RefHere(s)
end

function RefPath(e::Expr)
  @match e begin
    Expr(:(.), rest, QuoteNode(name)) => RefThere(RefPath(rest), name)
    _ => error("could not parse refpath from $e")
  end
end

function toexpr(p::RefPath)
  @match p begin
    RefHere(name) => name
    RefThere(mod, name) => Expr(:(.), toexpr(mod), QuoteNode(name))
  end
end

@data InterType begin
  Unit
  I32
  U32
  I64
  U64
  F64
  Boolean
  Str
  Sym
  Binary
  List(elemtype::InterType)
  Map(keytype::InterType, valuetype::InterType)
  ObjectType(elemtype::InterType)
  OptionalType(elemtype::InterType)
  Record(fields::Vector{Field{InterType}})
  Sum(variants::Vector{Variant{InterType}})
  ACSetInterType(schema::TypedSchema{InterType})
  Annot(desc::String, type::InterType)
  TypeRef(to::RefPath)
end

struct ACSetTypeSpec
  genericname::Union{Symbol, Nothing}
  abstract_type::Union{Symbol, Nothing}
  schemaname::Symbol
  schema::TypedSchema{Symbol, InterType}
  index::Vector{Symbol}
  unique_index::Vector{Symbol}
end

@data InterTypeDecl begin
  Alias(type::InterType)
  SumType(variants::Vector{Variant{InterType}})
  VariantOf(parent::Symbol)
  Struct(fields::Vector{Field{InterType}})
  SchemaDecl(schema::TypedSchema{Symbol, InterType})
  AbstractACSetType(parent::Union{Symbol, Nothing})
  NamedACSetType(typespec::ACSetTypeSpec)
end

function hashdecls end

struct InterTypeModule
  name::Symbol
  imports::OrderedDict{Symbol, InterTypeModule}
  declarations::OrderedDict{Symbol, InterTypeDecl}
  hash::String
  function InterTypeModule(
    name::Symbol,
    imports::OrderedDict{Symbol, InterTypeModule}=OrderedDict{Symbol, InterTypeModule}(),
    declarations::OrderedDict{Symbol, InterTypeDecl}=OrderedDict{Symbol, InterTypeDecl}()
  )
    new(name, imports, declarations, hashdecls(declarations))
  end
end

function intertype end

abstract type ExportTarget end
abstract type LanguageTarget <: ExportTarget end
abstract type SerializationTarget <: ExportTarget end

"""
    generate_module(mod::Module, target::Type{<:ExportTarget}, path="."; target_specific_args...)
  
Generate files that define the intertypes for the specified target. 
"""
function generate_module(mod::Module, target::Type{<:ExportTarget}, path="."; target_specific_args...)
  generate_module(mod.Meta, target, path; target_specific_args...)
end

module InterTypeSupport
using OrderedCollections
using StructEquality
import StructTypes
export Object, Optional

@struct_hash_equal struct Object{T}
  fields::OrderedDict{Symbol, T}
end

function Object{T}(pairs::(Pair{Symbol, S} where {S<:T})...) where {T}
  Object{T}(OrderedDict{Symbol, T}(pairs...))
end

function Object(pairs::(Pair{Symbol, T} where {T})...)
  Object{Any}(pairs...)
end

Base.getindex(obj::Object, key::Symbol) = obj.fields[key]
Base.setindex!(obj::Object, x, key::Symbol) = (obj.fields[key] = x)

Base.pairs(obj::Object) = pairs(obj.fields)

StructTypes.StructType(::Type{<:Object}) = StructTypes.DictType()

const Optional{T} = Union{T, Nothing}
end

using .InterTypeSupport

include("json.jl")
include("sexp.jl")
include("julia.jl")
include("python.jl")

end
