module InterTypes
export InterType, InterTypeDecl, Binary, intertype, as_intertypes

using ..InterSchemas
import ..InterSchemas: Ob as IOb, Hom as IHom, Attr as IAttr

using MLStyle

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
  # ACSetInterType(schema::InterSchema{InterType})
  Annot(desc::String, type::InterType)
  TypeRef(to::Symbol)
end

@data InterTypeDecl begin
  Alias(name::Symbol, type::InterType)
  SumType(name::Symbol, variants::Vector{Variant{InterType}})
  Struct(name::Symbol, fields::Vector{Field{InterType}})
  SchemaDecl(name::Symbol, schema::InterSchema{InterType})
  # NamedACSetType(name::Symbol, schemaname:::Symbol)
end

Base.nameof(decl::InterTypeDecl) = @match decl begin
  Alias(name, _) => name
  SumType(name, _) => name
  Struct(name, _) => name
end

function intertype end

include("json.jl")
include("julia.jl")
include("python.jl")

end
