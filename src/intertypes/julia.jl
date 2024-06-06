using ...ACSets
using StructTypes

function parse_fields(fieldexprs; mod)
  map(enumerate(fieldexprs)) do (i, fieldexpr)
    @match fieldexpr begin
      Expr(:(::), name, type) => Field{InterType}(name, parse_intertype(type; mod))
      Expr(:(::), type) => Field{InterType}(Symbol("_", i), parse_intertype(type; mod))
    end
  end
end

function parse_variants(variantexprs; mod)
  map(variantexprs) do vexpr
    @match vexpr begin
      tag::Symbol => Variant{InterType}(tag, Field{InterType}[])
      Expr(:call, tag, fieldexprs...) => Variant{InterType}(tag, parse_fields(fieldexprs; mod))
      _ => error("could not parse variant from $vexpr")
    end
  end
end

function parse_typeref(p::RefPath; mod)
  check_typeref(p; mod)
  TypeRef(p)
end

function check_typeref(p::RefPath; mod)
  @match p begin
    RefHere(name) =>
      if !haskey(mod.declarations, name)
        error("name $name not found in module $(mod.name)")
      end
    RefThere(RefHere(extern), name) =>
      if haskey(mod.imports, extern)
        parse_typeref(RefHere(name); mod=mod.imports[extern])
      else
        error("module $mod is not an import of module $(mod.name)")
      end
    _ => error("nested references not supported yet")
  end
end

function parse_typedschema!(e, tschema::TypedSchema{Symbol, InterType}; mod::InterTypeModule)
  schema = tschema.schema
  typing = tschema.typing
  for line in e.args
    @match line begin
      Expr(:(::), names, schtype) => begin
        names = @match names begin
          Expr(:tuple, names...) => [names...]
          name::Symbol => [name]
          _ => error("expected a symbol or a tuple, got $names")
        end
        @match schtype begin
          :Ob => append!(schema.obs, names)
          :(Hom($A, $B)) => append!(schema.homs, map(name -> (name, A, B), names))
          :(AttrType($type)) => begin
            type = parse_intertype(type; mod)
            for name in names
              push!(schema.attrtypes, name)
              typing[name] = type
            end
          end
          :(Attr($A, $X)) => append!(schema.attrs, map(name -> (name, A, X), names))
        end
      end
    end
  end
  tschema
end

unquote(q::QuoteNode) = q.value

function extract_vect(vectexpr)
  @match vectexpr begin
    Expr(:vect, args...) => Vector(unquote.(args))
    _ => error("expected a vector expression, got: $vectexpr")
  end
end

function NamedACSetType(abstract_type, schemaname::Symbol, kwargs; mod)
  kwargs = map(kwargs) do kv
    @match kv begin
      Expr(:kw, k, v) => (k => v)
      _ => error("got unexpected argument to @acset_type: $kv")
    end
  end |> Dict
  genericname = get(kwargs, :generic, nothing)
  index = extract_vect(get(kwargs, :index, :([])))
  unique_index = extract_vect(get(kwargs, :unique_index, :([])))
  schema = mod.declarations[schemaname].schema
  spec = ACSetTypeSpec(genericname, abstract_type, schemaname, schema, index, unique_index)
  NamedACSetType(spec)
end

function parse_intertype(e; mod::InterTypeModule)
  @match e begin
    :Nothing => InterTypes.Unit
    :Int32 => InterTypes.I32
    :UInt32 => InterTypes.U32
    :Int64 => InterTypes.I64
    :UInt64 => InterTypes.U64
    :Float64 => InterTypes.F64
    :Bool => InterTypes.Boolean
    :String => InterTypes.Str
    :Symbol => InterTypes.Sym
    :Binary => InterTypes.Binary
    T::Symbol => parse_typeref(RefPath(T); mod)
    Expr(:(.), args...) => parse_typeref(RefPath(e); mod)
    Expr(:curly, :Vector, elemtype) =>
      InterTypes.List(parse_intertype(elemtype; mod))
    Expr(:curly, :OrderedDict, keytype, valuetype) =>
      InterTypes.Map(parse_intertype(keytype; mod), parse_intertype(valuetype; mod))
    Expr(:curly, :Object, elemtype) =>
      InterTypes.ObjectType(parse_intertype(elemtype; mod))
    Expr(:curly, :Optional, elemtype) =>
      InterTypes.OptionalType(parse_intertype(elemtype; mod))
    Expr(:curly, :Record, fieldexprs...) => begin
      InterTypes.Record(parse_fields(fieldexprs; mod))
    end
    Expr(:curly, :Sum, variantexprs...) => begin
      InterTypes.Sum(parse_variants(variantexprs; mod))
    end
    _ => error("could not parse intertype from $e")
  end
end

function extract_variant_names(variantexprs)
  map(variantexprs) do vexpr
    @match vexpr begin
      tag::Symbol => tag
      Expr(:call, tag, fieldexprs...) => tag
      _ => error("could not parse variant from $vexpr")
    end
  end
end

function parse_intertype_decl(e; mod::InterTypeModule)
  @match e begin
    Expr(:const, Expr(:(=), name::Symbol, type)) => Pair(name, Alias(parse_intertype(type; mod)))
    Expr(:struct, _, name::Symbol, body) => begin
      Base.remove_linenums!(body)
      # this is a hack so we can have recursive data types
      mod.declarations[name] = Alias(TypeRef(RefPath(:nothing)))
      ret = Pair(name, Struct(parse_fields(body.args; mod)))
      delete!(mod.declarations, name)
      ret
    end
    Expr(:sum, name::Symbol, body) => begin
      Base.remove_linenums!(body)
      mod.declarations[name] = Alias(TypeRef(RefPath(:nothing)))
      variant_names = extract_variant_names(body.args)
      for vname in variant_names
        mod.declarations[vname] = Alias(TypeRef(RefPath(:nothing)))
      end
      ret = Pair(name, SumType(parse_variants(body.args; mod)))
      delete!(mod.declarations, name)
      for vname in variant_names
        delete!(mod.declarations, vname)
      end
      ret
    end
    Expr(:schema, head, body) => begin
      (name, tschema) = @match head begin
        Expr(:(<:), name, parent) => (name, copy(mod.declarations[parent].schema))
        name::Symbol => (name, TypedSchema{Symbol, InterType}())
        _ => error("expected schema head of the form `\$SchemaName` or `\$SchemaName <: Parent`.")
      end
      Base.remove_linenums!(body)
      Pair(name, SchemaDecl(parse_typedschema!(body, tschema; mod)))
    end
    Expr(:abstract_acset_type, Expr(:(<:), name, parent)) =>
      Pair(name, AbstractACSetType(parent))
    Expr(:abstract_acset_type, name) =>
      Pair(name, AbstractACSetType(nothing))
    Expr(:acset_type, :($name($schemaname, $(kwargs...)))) =>
      Pair(name, NamedACSetType(nothing, schemaname, kwargs; mod))
    Expr(:acset_type, Expr(:(<:), :($name($schemaname, $(kwargs...))), abstract_type)) =>
      Pair(name, NamedACSetType(abstract_type, schemaname, kwargs; mod))
    _ => error("could not parse intertype declaration from $e")
  end
end

"""
We use these macros so that we don't have to, for instance, pattern match on
`Expr(:macrocall, var"@sum", _, args...)`, and can instead pattern match on
`Expr(:sum, args...)`.
"""
module InterTypeDeclImplPrivate

macro sum(head, body)
  esc(Expr(:sum, head, body))
end

macro schema(head, body)
  esc(Expr(:schema, head, body))
end

macro acset_type(head)
  esc(Expr(:acset_type, head))
end

macro abstract_acset_type(head)
  esc(Expr(:abstract_acset_type, head))
end
end

"""
    function toexpr(x) end

Used to convert intertype data types to `Expr`.

TODO: Should we unify [`tojsonschema`](@ref), [`toexpr`](@ref), and
[`topy`](@ref) into a single function with an extra argument to control
dispatch?
"""
function toexpr end

function toexpr(field::Field)
  Expr(:(::), field.name, toexpr(field.type))
end

function toexpr(variant::Variant)
  Expr(:call, variant.tag, toexpr.(variant.fields)...)
end

function toexpr(intertype::InterType)
  @match intertype begin
    Unit => :Nothing
    I32 => :Int32
    U32 => :UInt32
    I64 => :Int64
    U64 => :UInt64
    F64 => :Float64
    Boolean => :Bool
    Str => :String
    Sym => :Symbol
    Binary => :(Vector{UInt8})
    List(elemtype) => Expr(:curly, :Vector, toexpr(elemtype))
    Map(keytype, valuetype) => Expr(:curly, :OrderedDict, toexpr(keytype), toexpr(valuetype))
    ObjectType(elemtype) => Expr(:curly, InterTypeSupport.Object, toexpr(elemtype))
    OptionalType(elemtype) => Expr(:curly, InterTypeSupport.Optional, toexpr(elemtype))
    Record(fields) =>
      Expr(:curly, :Record, toexpr.(fields)...)
    Sum(variants) =>
      Expr(:curly, :Sum, toexpr.(variants)...)
    Annot(desc, innertype) => toexpr(innertype)
    TypeRef(to) => toexpr(to)
  end
end

Base.show(io::IO, intertype::InterType) = print(io, toexpr(intertype))


function acset_type_decl(spec, name)
  callexpr = Expr(
    :call,
    name, spec.schemaname,
    Expr(:kw, :index, spec.index),
    Expr(:kw, :unique_index, spec.unique_index)
  )
  if !isnothing(spec.abstract_type)
    callexpr = Expr(:(<:), callexpr, spec.abstract_type)
  end
  Expr(
    :macrocall,
    GlobalRef(ACSets, :(var"@acset_type")),
    nothing,
    callexpr
  )
end

function toexpr(name::Symbol, decl::InterTypeDecl; show=false)
  @match decl begin
    Alias(type) => :(const $name = $type)
    Struct(fields) =>
      Expr(:macrocall,
        GlobalRef(MLStyle, :(var"@as_record")),
        nothing,
        Expr(:struct,
          false,
          name,
          Expr(:block, toexpr.(fields)...)
        )
      )
    SumType(variants) =>
      Expr(:macrocall,
        GlobalRef(MLStyle, :(var"@data")),
        nothing, name,
        Expr(:block, toexpr.(variants)...)
      )
    SchemaDecl(schema) =>
      :(const $name = $schema)
    AbstractACSetType(parent) => begin
      body = if isnothing(parent)
        name
      else
        Expr(:(<:), name, parent)
      end
      Expr(
        :macrocall,
        GlobalRef(ACSets, :(var"@abstract_acset_type")),
        nothing,
        body
      )
    end
    NamedACSetType(spec) => begin
      if isnothing(spec.genericname)
        acset_type_decl(spec, name)
      else
        types = [toexpr(spec.schema.typing[at]) for at in attrtypes(spec.schema)]
        quote
          $(acset_type_decl(spec, spec.genericname))
          const $name = $(spec.genericname){$(types...)}
        end
      end
    end
  end
end

function Base.show(io::IO, declpair::Pair{Symbol, InterTypeDecl})
  (name, decl) = declpair
  print(io, toexpr(name, decl; show=true))
end

function as_intertypes(mod::InterTypeModule)
  function parse(in::Expr)
    Base.remove_linenums!(in)
    in = macroexpand(InterTypeDeclImplPrivate, in)
    (name, decl) = parse_intertype_decl(in; mod)
    mod.declarations[name] = decl
    out = Expr(:block)
    push!(out.args, toexpr(name, decl))
    @match decl begin
      SumType(variants) => begin
        for variant in variants
          mod.declarations[variant.tag] = InterTypes.VariantOf(name)
        end
      end
      _ => nothing
    end
    push!(out.args, eqmethods(name, decl))
    push!(out.args, structtypesfor(name, decl))
    push!(out.args, constructorsfor(name, decl))
    out
  end
end

function include_intertypes(into::Module, file::String, imports::AbstractVector)
  endswith(file, ".it") || error("expected a file ending in \".it\"")
  name = Symbol(basename(chop(file; tail=3)))
  mod = InterTypeModule(name, OrderedDict{Symbol, InterTypeModule}(imports))
  into.include(as_intertypes(mod), file)
  # recompute the hash
  mod = InterTypeModule(name, mod.imports, mod.declarations)
  into.eval(Expr(:export, exports(mod)...))
  mod
end

"""
    @intertypes "file.it" module modulename end

Used to import an intertypes file into Julia.

TODO: maybe we should just build a .jl file from an .it file directly.
"""
macro intertypes(file, modexpr)
  name, imports = @match modexpr begin
    Expr(:module, _, name, body) => begin
      imports = Symbol[]
      for line in body.args
        @match line begin
          Expr(:import, Expr(:(.), :(.), :(.), name)) => push!(imports, name)
          _ => nothing
        end
      end
      (name, imports)
    end
    _ => error("expected a module expression, got $modexpr")
  end
  push!(modexpr.args[3].args, :(global Meta))
  imports = Expr(:vect, [:($(Expr(:quote, name)) => $name.Meta) for name in imports]...)
  Expr(
    :toplevel,
    esc(modexpr),
    :($(esc(name)).Meta = include_intertypes($(esc(name)), $file, $(esc(imports)))),
    esc(name),
  )
end

function eqmethod(name::Symbol, fields::Vector{Field{InterType}})
  quote
    function Base.:(==)(a::$name, b::$name)
      Base.all([$([:(a.$x == b.$x) for x in nameof.(fields)]...)])
    end
  end
end

eqmethods(name, decl::InterTypeDecl) = nothing

eqmethods(name, decl::Struct) = eqmethod(name, decl.fields)
eqmethods(name, decl::SumType) =
  Expr(:block, map(decl.variants) do variant
    eqmethod(variant.tag, variant.fields)
  end...)

structtypesfor(name, decl::InterTypeDecl) = nothing

function structtypesfor(name, decl::Struct)
  :($(GlobalRef(StructTypes, :StructType))(::Type{$name}) =
      $(GlobalRef(StructTypes, :Struct))())
end

function structtypesfor(variant::Variant)
  lowertype = :(NamedTuple{
    $(Expr(
      :tuple,
      Expr(:quote, :_type),
      [Expr(:quote, f.name) for f in variant.fields]...
    )),
    $(Expr(
      :curly,
      :Tuple,
      Symbol,
      [toexpr(f.type) for f in variant.fields]...
    ))
  })
  quote
    $(GlobalRef(StructTypes, :StructType))(::Type{$(variant.tag)}) =
      $(GlobalRef(StructTypes, :CustomStruct))()
    $(GlobalRef(StructTypes, :lower))(x::$(variant.tag)) =
      $(Expr(:tuple,
             Expr(:(=), :_type, Expr(:quote, variant.tag)),
             [Expr(:(=), f.name, Expr(:(.), :x, Expr(:quote, f.name))) for f in variant.fields]...))
    $(GlobalRef(StructTypes, :lowertype))(::Type{$(variant.tag)}) = $lowertype
    $(GlobalRef(StructTypes, :construct))(::Type{$(variant.tag)}, x) =
      $(Expr(
        :call,
        variant.tag,
        [Expr(:(.), :x, Expr(:quote, f.name)) for f in variant.fields]...
      ))
  end
end

function structtypesfor(name, decl::SumType)
  quote
    $(Expr(:block, [constructorsfor(v.tag, v) for v in decl.variants]...))
    $(Expr(:block, [structtypesfor(v) for v in decl.variants]...))
    $(GlobalRef(StructTypes, :StructType))(::Type{$name}) =
      $(GlobalRef(StructTypes, :AbstractType))()
    $(GlobalRef(StructTypes, :subtypekey))(::Type{$name}) = :_type
    $(GlobalRef(StructTypes, :subtypes))(::Type{$name}) =
      $(Expr(:tuple, [Expr(:(=), v.tag, v.tag) for v in decl.variants]...))
  end
end

constructorsfor(name, decl::InterTypeDecl) = nothing

function constructorsfor(name, decl::Union{Struct, Variant})
  quote
    $(GlobalRef(StructTypes, :construct))(::Type{$name}, x::Union{NamedTuple, Dict{Symbol}}) =
    $name(
      $([Expr(
        :call,
        GlobalRef(StructTypes, :construct),
        toexpr(f.type),
        :(x[$(Expr(:quote, f.name))])
      ) for f in decl.fields]...)
    )
    $(GlobalRef(StructTypes, :construct))(::Type{$name}, x::Dict{String}) =
    $name(
      $([Expr(
        :call,
        GlobalRef(StructTypes, :construct),
        toexpr(f.type),
        :(x[$(string(f.name))])
      ) for f in decl.fields]...)
    )
  end
end
