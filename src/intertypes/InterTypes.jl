module InterTypes
export InterType, InterTypeDecl, Binary, intertype, @intertypes

using MLStyle
using OrderedCollections
using ..Schemas

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
  Record(fields::Vector{Field{InterType}})
  Sum(variants::Vector{Variant{InterType}})
  ACSetInterType(schema::TypedSchema{InterType})
  Annot(desc::String, type::InterType)
  TypeRef(to::RefPath)
end

@data InterTypeDecl begin
  Alias(type::InterType)
  SumType(variants::Vector{Variant{InterType}})
  VariantOf(parent::Symbol)
  Struct(fields::Vector{Field{InterType}})
  SchemaDecl(schema::TypedSchema{Symbol, InterType})
  NamedACSetType(schemaname::Symbol)
end

struct InterTypeModule
  name::Symbol
  imports::OrderedDict{Symbol, InterTypeModule}
  declarations::OrderedDict{Symbol, InterTypeDecl}
  function InterTypeModule(
    name::Symbol,
    imports::OrderedDict{Symbol, InterTypeModule}=OrderedDict{Symbol, InterTypeModule}(),
    declarations::OrderedDict{Symbol, InterTypeDecl}=OrderedDict{Symbol, InterTypeDecl}()
  )
    new(name, imports, declarations)
  end
end


function intertype end

include("json.jl")
include("julia.jl")
include("python.jl")

end
