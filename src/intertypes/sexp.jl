using SHA
using Random

"""
A very simple s-expression data structure.

We use this to write the intertype schema to a string and then hash it to get a
version identifier.
"""
struct SExp
  args::Vector{Union{Symbol, SExp}}
end

function SExp(args...)
  SExp(Union{Symbol, SExp}[args...])
end

function SExp(arg::Symbol)
  SExp(Union{Symbol, SExp}[arg])
end

function Base.print(io::IO, s::SExp)
  print(io, "(")
  join(io, s.args, " ")
  print(io, ")")
end

fieldsexps(fields) =
  [SExp(field.name, tosexp(field.type)) for field in fields]

variantsexps(variants) =
  [SExp(variant.tag, fieldsexps(variant.fields)...) for variant in variants] 

function schemasexp(tschema::TypedSchema{Symbol, InterType})
  schema = tschema.schema
  args = Union{Symbol, SExp}[]
  for ob in sort(schema.obs)
    push!(args, SExp(:Ob, ob))
  end
  for (f, c, d) in sort(schema.homs)
    push!(args, SExp(:Hom, f, c, d))
  end
  for t in sort(schema.attrtypes)
    push!(args, SExp(:AttrType, t, tosexp(tschema.typing[t])))
  end
  for (f, c, d) in sort(schema.attrs)
    push!(args, SExp(:Attrs, f, c, d))
  end
  args
end

function tosexp(t::InterType)
  @match t begin
    I32 => :I32
    U32 => :U32
    I64 => :I64
    U64 => :U64
    F64 => :F64
    Boolean => :Boolean
    Str => :Str
    Sym => :Sym
    Binary => :Binary
    List(elemtype) =>
      SExp(:List, tosexp(elemtype))
    Map(keytype, valuetype) =>
      SExp(:Map, tosexp(keytype), tosexp(valuetype))
    ObjectType(elemtype) =>
      SExp(:Object, tosexp(elemtype))
    OptionalType(elemtype) =>
      SExp(:Optional, tosexp(elemtype))
    Record(fields) => SExp(:Record, fieldsexps(fields)...)
    Sum(variants) => SExp(:Sum, variantsexps(variants)...)
    ACSetInterType(schema) => :ACset
    Annot(desc::Symbol, type::InterType) => SExp(:Annot, desc, tosexp(type))
    TypeRef(to::RefPath) => Symbol(string(toexpr(to)))
  end
end

function tosexp(d::InterTypeDecl)
  @match d begin
    Alias(type) => SExp(:Alias, tosexp(type))
    SumType(variants) => SExp(:SumType, variantsexps(variants)...)
    VariantOf(parent) => SExp(:VariantOf, parent)
    Struct(fields) => SExp(:Struct, fieldsexps(fields)...)
    SchemaDecl(schema) => SExp(:SchemaDecl, schemasexp(schema)...)
    AbstractACSetType(parent) => SExp(:AbstractACSetType, Symbol(parent))
    NamedACSetType(spec) =>
      SExp(:NamedACSetType, spec.schemaname)
  end
end

function tosexp(decls::OrderedDict{Symbol, InterTypeDecl})
  SExp(map(collect(pairs(decls))) do (name, decl)
    SExp(name, tosexp(decl))
  end)
end

WORD_LIST = readlines(@__DIR__() * "/words.txt")

function hashdecls(decls::OrderedDict{Symbol, InterTypeDecl})
  sexpr = sprint(print, tosexp(decls))
  rng = MersenneTwister(Vector(reinterpret(UInt32, sha256(sexpr))))
  join([rand(rng, WORD_LIST) for _ in 1:4], " ")
end
