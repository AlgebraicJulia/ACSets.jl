function parse_fields(fieldexprs; defined_types)
  map(enumerate(fieldexprs)) do (i, fieldexpr)
    @match fieldexpr begin
      Expr(:(::), name, type) => Field{InterType}(name, parse_intertype(type; defined_types))
      Expr(:(::), type) => Field{InterType}(Symbol("_", i), parse_intertype(type; defined_types))
    end
  end
end

function parse_variants(variantexprs; defined_types)
  map(variantexprs) do vexpr
    @match vexpr begin
      tag::Symbol => Variant{InterType}(tag, Field{InterType}[])
      Expr(:call, tag, fieldexprs...) => Variant{InterType}(tag, parse_fields(fieldexprs; defined_types))
      _ => error("could not parse variant from $vexpr")
    end
  end
end

function parse_intertype(e; defined_types::AbstractSet{Symbol}=Set{Symbol}())
  @match e begin
    :Int32 => InterTypes.I32
    :UInt32 => InterTypes.U32
    :Int64 => InterTypes.I64
    :UInt64 => InterTypes.U64
    :Float64 => InterTypes.F64
    :Bool => InterTypes.Boolean
    :String => InterTypes.Str
    :Symbol => InterTypes.Sym
    :Binary => InterTypes.Binary
    T::Symbol && if T ∈ defined_types end => InterTypes.TypeRef(T)
    Expr(:curly, :Vector, elemtype) =>
      InterTypes.List(parse_intertype(elemtype; defined_types))
    Expr(:curly, :OrderedDict, keytype, valuetype) =>
      InterTypes.Map(parse_intertype(keytype; defined_types), parse_intertype(valuetype; defined_types))
    Expr(:curly, :Record, fieldexprs...) => begin
      InterTypes.Record(parse_fields(fieldexprs; defined_types))
    end
    Expr(:curly, :Sum, variantexprs...) => begin
      InterTypes.Sum(parse_variants(variantexprs; defined_types))
    end
    _ => error("could not parse intertype from $e")
  end
end

function parse_interschema(block::Expr; defined_types::AbstractSet{Symbol}=Set{Symbol}())
  objects = OrderedDict{Symbol, IOb}()
  homs = OrderedDict{Symbol, IHom}()
  attrs = OrderedDict{Symbol, IAttr{InterType}}()
  for line in block.args
    @match line begin
      :($ob::Ob) => (objects[ob] = IOb())
      :($f::Hom($x, $y)) => (homs[f] = IHom(x, y))
      :($a::Attr($x, $y)) => (attrs[a] = IAttr{InterType}(x, parse_intertype(y; defined_types)))
    end
  end
  InterSchema(objects, homs, attrs)
end

function parse_intertype_decl(e; defined_types::AbstractSet{Symbol}=Set{Symbol}())
  @match e begin
    Expr(:const, Expr(:(=), name::Symbol, type)) => InterTypes.Alias(name, parse_intertype(type; defined_types))
    Expr(:struct, _, name::Symbol, body) => begin
      Base.remove_linenums!(body)
      InterTypes.Struct(name, parse_fields(body.args; defined_types = Set([defined_types..., name])))
    end
    Expr(:sum_type, name::Symbol, body) => begin
      Base.remove_linenums!(body)
      InterTypes.SumType(name, parse_variants(body.args; defined_types = Set([defined_types..., name])))
    end
    Expr(:acset_schema, name::Symbol, body) => begin
      Base.remove_linenums!(body)
      InterTypes.SchemaDecl(name, parse_interschema(body; defined_types))
    end
    _ => error("could not parse intertype declaration from $e")
  end
end

function parse_intertype_decls(exprs)
  defined_types = Set{Symbol}()
  map(filter(e -> !(e isa LineNumberNode), exprs)) do expr
    decl = parse_intertype_decl(expr; defined_types)
    push!(defined_types, nameof(decl))
    decl
  end
end

module InterTypeDeclImplPrivate
macro sum(head, body)
  esc(Expr(:sum_type, head, body))
end

macro schema(head, body)
  esc(Expr(:acset_schema, head, body))
end
end

macro intertype_decls(e)
  e.head == :block || error("expected a block as argument to @intertype_decls")
  e = macroexpand(InterTypeDeclImplPrivate, e)
  Base.remove_linenums!(e)
  parse_intertype_decls(e.args)
end

function toexpr(field::Field)
  Expr(:(::), field.name, toexpr(field.type))
end

function toexpr(variant::Variant)
  Expr(:call, variant.tag, toexpr.(variant.fields)...)
end

function toexpr(intertype::InterType)
  @match intertype begin
    I32 => :Int32
    U32 => :UInt32
    I64 => :Int64
    U64 => :UInt64
    F64 => :Float64
    Boolean => :Bool
    Str => :String
    Sym => :Symbol
    Binary => :Binary
    List(elemtype) => Expr(:curly, :Vector, toexpr(elemtype))
    Map(keytype, valuetype) => Expr(:curly, :OrderedDict, toexpr(keytype), toexpr(valuetype))
    Record(fields) =>
      Expr(:curly, :Record, toexpr.(fields)...)
    Sum(variants) =>
      Expr(:curly, :Sum, toexpr.(variants)...)
    Annot(desc, innertype) => toexpr(innertype)
    TypeRef(to) => to
  end
end

Base.show(io::IO, intertype::InterType) = print(io, toexpr(intertype))

function toexpr(intertype::InterTypeDecl; show=false)
  @match intertype begin
    Alias(name, type) => :(const $name = $type)
    Struct(name, fields) =>
      Expr(:macrocall,
        GlobalRef(MLStyle, :(var"@as_record")),
        nothing,
        Expr(:struct,
          false,
          name,
          Expr(:block, toexpr.(fields)...)
        )
      )
    SumType(name, variants) =>
      Expr(:macrocall,
        GlobalRef(MLStyle, :(var"@data")),
        nothing, name,
        Expr(:block, toexpr.(variants)...)
      )
    SchemaDecl(name, schema) =>
      if show
        Expr(:macrocall,
          GlobalRef(InterSchemas, :(var"@interschema")),
          nothing, name,
          toexpr(schema)
        )
      else
        :(const $name = $schema)
      end
  end
end

function toexpr(schema::InterSchema{InterType})
  lines = Expr[]
  for (X, _) in schema.objects
    push!(lines, :($X::Ob))
  end
  for (f, hom) in schema.homs
    push!(lines, :($f::Hom($(hom.dom), $(hom.codom))))
  end
  for (a, attr) in schema.attrs
    push!(lines, :($a::Attr($(attr.dom), $(toexpr(attr.codom)))))
  end
  Expr(:block, lines...)
end

function Base.show(io::IO, decl::InterTypeDecl)
  print(io, toexpr(decl; show=true))
end

as_intertypes() = as_intertypes(OrderedDict{Symbol, InterType}())

function as_intertypes(context::OrderedDict{Symbol, InterType})
  function parse(in::Expr)
    Base.remove_linenums!(in)
    in = macroexpand(InterTypeDeclImplPrivate, in)
    decl = parse_intertype_decl(in; defined_types=keys(context))
    out = Expr(:block)
    push!(out.args, toexpr(decl))
    @match decl begin
      Alias(name, type) => begin
        context[name] = type
      end
      Struct(name, fields) => begin
        type = InterTypes.Record(fields)
        context[name] = type
        push!(out.args, :($(GlobalRef(InterTypes, :intertype))(::Type{$name}) = $type))
      end
      SumType(name, variants) => begin
        type = InterTypes.Sum(variants)
        context[name] = type
        push!(out.args,
          :($(GlobalRef(InterTypes, :intertype))(::Type{$name}) = $type)
        )
      end
      _ => println(decl)
    end
    push!(out.args, reader(decl))
    push!(out.args, :(eval($(Expr(:quote, writer(decl))))))
    out
  end
end

function variantreader(name::Symbol, fields::Vector{Field{InterType}})
  fieldreads = map(fields) do field
    :($(read)(format, $(toexpr(field.type)), s[$(Expr(:quote, field.name))]))
  end
  :($name($(fieldreads...)))
end

function makeifs(branches)
  makeifs(branches[1:end-1], branches[end][2])
end

function makeifs(branches, elsebody)
  expr = elsebody
  if length(branches) == 0
    return expr
  end
  for (cond, body) in Iterators.reverse(branches[2:end])
    expr = Expr(:elseif, cond, body, expr)
  end
  (cond, body) = branches[1]
  Expr(:if, cond, body, expr)
end

function reader(decl::InterTypeDecl)
  body = @match decl begin
    Alias(_, _) => nothing
    Struct(name, fields) => variantreader(name, fields)
    SumType(name, variants) => begin
      tag = gensym(:tag)
      ifs = makeifs(map(variants) do variant
        (
          :($tag == $(string(variant.tag))),
          variantreader(variant.tag, variant.fields)
        )
      end)
      quote
        $tag = s[:tag]
        $ifs
      end
    end
    SchemaDecl(_, _) => nothing
  end
  if !isnothing(body)
    :(function $(GlobalRef(InterTypes, :read))(format::$(JSONFormat), ::Type{$(nameof(decl))}, s::$(JSON3.Object))
      $body
    end)
  else
    nothing
  end
end

function writejsonfield(io, name, value, comma=true)
  print(io, "\"", string(name), "\":")
  write(io, JSONFormat(), value)
  if comma
    print(io, ",")
  end
end

function fieldwriters(fields)
  map(enumerate(fields)) do (i, field)
    (name, expr) = field
    comma = i != length(fields)
    :($(writejsonfield)(io, $(string(name)), $expr, $comma))
  end
end

function objectwriter(fields)
  quote
    print(io, "{")
    $(fieldwriters(fields)...)
    print(io, "}")
  end
end

function writer(decl::InterTypeDecl)
  body = @match decl begin
    Alias(_, _) => nothing
    Struct(_, fields) => begin
      objectwriter([(field.name, :(d.$(field.name))) for field in fields])
    end
    SumType(name, variants) => begin
      variantlines = map(variants) do variant
        fieldnames = nameof.(variant.fields)
        fieldvars = gensym.(fieldnames)
        Expr(
          :call, :(=>),
          :($(variant.tag)($(fieldvars...))),
          Expr(
            :block,
            fieldwriters([(:_type, string(variant.tag)), zip(fieldnames, fieldvars)...])...
          )
        )
      end
      quote
        print(io, "{")
        $(Expr(
          :macrocall, GlobalRef(MLStyle, :(var"@match")), nothing, :d,
          Expr(:block, variantlines...)
        ))
        print(io, "}")
      end
    end
    _ => nothing
  end
  if !isnothing(body)
    :(function $(GlobalRef(InterTypes, :write))(io::IO, format::$(JSONFormat), d::$(nameof(decl)))
      $body
    end)
  else
    nothing
  end
end
