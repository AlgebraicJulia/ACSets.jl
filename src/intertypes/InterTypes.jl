module InterTypes
export InterType, InterTypeDecl, Object, Optional,
  LanguageTarget, SerializationTarget, generate_module, intertype, @intertypes

using MLStyle
using OrderedCollections
using ..Schemas
import ..Schemas: toexpr

# InterType Definitions
#######################

"""
A field of a struct. Used in [`Variant`](@ref) and [`Record`](@ref).

The `T` parameter will always be [`InterType`](@ref), but this is mutually-recursive
with `InterType` so we have to be generic here.
"""
struct Field{T}
  name::Symbol
  type::T
end

Base.nameof(field::Field) = field.name

"""
One of the summands of a sum type.

Like [`Field`](@ref), the `T` parameter will always be [`InterType`](@ref), but
this is mutually-recursive with `InterType` so we have to be generic here.
"""
struct Variant{T}
  tag::Symbol
  fields::Vector{Field{T}}
end

Base.nameof(variant::Variant) = variant.tag

@data RefPath begin
  RefHere(name::Symbol)
  RefThere(mod::RefPath, name::Symbol)
end

@doc """
A non-empty linked list of symbols representing something like `foo.bar.baz`.
""" RefPath

@doc """E.g. mod.name""" RefThere


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
  ACSetInterType(schema::TypedSchema{Symbol, InterType})
  Annot(desc::String, type::InterType)
  TypeRef(to::RefPath)
end

@doc """
An intertype expression representing a type.

TODO: Generic types
TODO: Remove anonymous sums, anonymous products
TODO: Separate out primitives, so that this is something like

```julia
@data InterType begin
  PrimType(prim::InterTypePrimitive)
  TypeRef(path::RefPath)
  TypeApp(type::InterType, args::Vector{InterType})
end
```
""" InterType

"""
A specification for the type of an acset.

## Fields
- `genericname::Union{Symbol, Nothing}`: The name for the generic version of the acset, with type parameters.

  Note that the name assigned to this in the declaration is the name *with*
  type parameters pre-specified.

  If there are no attribute types, then this is nothing.
- `abstract_type::Union{Symbol, Nothing}`: The parent abstract type for the acset.
- `schemaname::Symbol`
- `schema::TypedSchema{Symbol, InterType}`
- `index::Vector{Symbol}`
- `unique_index::Vector{Symbol}`
"""
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

@doc """
An intertype declaration.

Does not include the name of the declaration.
""" InterTypeDecl

@doc """An alias for an existing type""" Alias

@doc """A sum type, also known as a tagged union.""" SumType

@doc """
A variant of a sum type, i.e. one of the summands. These are implicitly
produced when declaring a sum type, and the data of the variant (i.e. the
fields) are in the parent sum type.
""" VariantOf

@doc """A struct type, also known as a product type or record type.""" Struct

@doc """
A schema for acsets. Does not declare the acset type yet, however, that is
done by [`NamedACSetType`](@ref).
""" SchemaDecl

@doc """
An abstract acset type which ACSets can subtype. Mostly used for backwards
compatibility with AlgebraicJulia code.
""" AbstractACSetType

@doc """
An acset type, with customizations like choice of indices, etc.
""" NamedACSetType

exports(::InterTypeDecl) = Symbol[]

function exports(acset_type::NamedACSetType)
  if !isnothing(acset_type.typespec.genericname)
    [acset_type.typespec.genericname]
  else
    []
  end
end

function hashdecls end

"""
A collection of intertype declarations packaged together. May refer to other
InterTypeModules via the `imports` field.
"""
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

function exports(mod::InterTypeModule)
  export_list = Symbol[]
  for (name, decl) in pairs(mod.declarations)
    push!(export_list, name)
    append!(export_list, exports(decl))
  end
  export_list
end

function intertype end

abstract type ExportTarget end
abstract type LanguageTarget <: ExportTarget end
abstract type SerializationTarget <: ExportTarget end

abstract type SerializationFormat end

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

include("jsonschema.jl")
include("sexp.jl")
include("julia.jl")
include("python.jl")

end
