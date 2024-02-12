"""
These are ACSets where the set associated to each object is of the form `1:n`
"""
module DenseACSets
export @acset_type, @abstract_acset_type, StructACSet, StructCSet,
  DynamicACSet, SimpleACSet, AnonACSet, ACSetTableType, AnonACSetType,
  IntParts, BitSetParts, sparsify, densify, idx

using StructEquality
using MLStyle: @match
using CompTime
import Tables
import Base: parent

using ..Columns, ..ColumnImplementations
using ..ACSetInterface, ..Schemas

# Parts 
#######

""" Part IDs are contiguous integers from 1 to n.
"""
@struct_hash_equal mutable struct IntParts <: DenseParts{Int,Int}
  val::Int 
  IntParts(n::Int=0) = new(n)
end

""" Parts IDs are a subset of contiguous integers from 1 to n.
"""
struct BitSetParts <: MarkAsDeleted{BitSet,Int}
  val::BitSet
  next::Ref{Int}
  BitSetParts(n::Int=0) = new(BitSet(1:n),n)
end

Base.:(==)(x::BitSetParts,y::BitSetParts) = x.val == y.val && x.next.x == y.next.x
Base.hash(x::BitSetParts, h::UInt64) = hash(x.val, hash(x.next.x, h))

ACSetInterface.default_parts_type(::Type{DenseParts}) = IntParts
ACSetInterface.default_parts_type(::Type{MarkAsDeleted}) = BitSetParts


Base.length(s::IntParts) = s.val
Base.length(s::BitSetParts) = length(s.val)
Base.iterate(s::IntParts) = iterate(1:s.val)  
Base.iterate(s::IntParts,i) = iterate(1:s.val,i)  
Base.iterate(s::BitSetParts,) = iterate(s.val)  
Base.iterate(s::BitSetParts,i) = iterate(s.val,i)
Base.eltype(::IntParts) = Int
Base.eltype(::BitSetParts) = Int

function gc!(b::BitSetParts, n::Int) 
  for i in b.val 
    if i > n delete!(b.val, i) end
  end
  push!(b.val, Base.OneTo(n)...) 
  b.next.x = n
end
ACSetInterface.nparts(p::IntParts) = p.val 
ACSetInterface.nparts(p::BitSetParts) = length(p.val)
ACSetInterface.maxpart(p::IntParts) = p.val 
ACSetInterface.maxpart(p::BitSetParts) = p.next[]

@inline ACSetInterface.parts(p::IntParts) = 1:p.val
@inline ACSetInterface.parts(p::BitSetParts) = collect(p.val)

@inline function ACSetInterface.add_parts!(m::IntParts, n::Int)
  nparts = m.val + n
  newparts = (m.val+1):nparts
  m.val = nparts
  newparts
end

@inline function ACSetInterface.add_parts!(m::BitSetParts, n::Int)
  nparts = m.next.x + n
  newparts = (m.next.x+1):nparts
  m.next.x = nparts
  for p in newparts 
    push!(m.val, p) 
  end
  newparts
end

# StructACSet Struct Generation
###############################

""" A `SimpleACSet` is an abstract type for any acset that has a certain layout

Specifically, subtypes of `SimpleACSet` are expected to have a `parts` field
which is a mapping from symbols to ints, and a `subparts` field which is a
mapping from symbols to columns, which are any data structure that
satisfies the interface given in Columns.jl.
"""
abstract type SimpleACSet{PT} <: ACSet{PT} end

""" A `StructACSet` is a SimpleACSet where the schema and the types assigned
to the attrtypes are available in the type.
"""
abstract type StructACSet{S<:TypeLevelSchema{Symbol},Ts<:Tuple,PT} <: SimpleACSet{PT} end

""" A special case where there are no attributes.
"""
const StructCSet{S,PT} = StructACSet{S,Tuple{},PT}

""" Creates a named tuple type
"""
function pi_type(types::Vector{Tuple{Symbol, Type}})
  NamedTuple{Tuple(map(t -> t[1], types)), Tuple{map(t -> t[2], types)...}}
end

""" Creates a quoted element of a named tuple
"""
function pi_type_elt(exprs::Vector{Tuple{Symbol, Expr}})
  Expr(:tuple, Expr(:parameters, [Expr(:kw, f, e) for (f,e) in exprs]...))
end

"""
The type variables that we have generated might not match up with the type
variables that are created as generic parameters to the struct acset, this is a
way of making the two line up
"""
function genericize(T::Type, tvars::Vector{TypeVar})
  occuring_variables = []
  cur = T
  for tvar in reverse(tvars)
    next = UnionAll(tvar, cur)
    if typeof(next) == UnionAll && next.var == tvar
      push!(occuring_variables, tvar)
      cur = next
    end
  end
  if length(occuring_variables) > 0
    :($cur{$([tvar.name for tvar in reverse(occuring_variables)]...)})
  else
    cur
  end
end

make_parts(s::Schema{Symbol}, part_type=IntParts) =  
  NamedTuple{Tuple(types(s)), Tuple{fill(part_type,length(types(s)))...}}
  
function make_columns(s::Schema{Symbol}, index, unique_index, Tvars)
  vcat(
    Tuple{Symbol,Type}[
      (f,column_type(HomChoice, indexchoice(f, index, unique_index)))
      for f in homs(s; just_names=true)
    ],
    Tuple{Symbol,Type}[
      (f,column_type(AttrChoice(Tvars[c]), indexchoice(f, index, unique_index)))
      for (f,_,c) in attrs(s)
    ]
  )
end

""" Create the struct declaration for a `StructACSet` from a Presentation
"""
function struct_acset(name::Symbol, parent, s::Schema{Symbol};
                      index::Vector=[], unique_index::Vector=[], 
                      part_type::Type{<:PartsType}=IntParts)
  Tvars = Dict(at => TypeVar(at) for at in attrtypes(s))
  parameterized_type, new_call = if length(attrtypes(s)) > 0
    (:($name{$(attrtypes(s)...)}), :(new{$(attrtypes(s)...)}))
  else
    name, :new
  end
  schema_type = typelevel(s)
  columns = make_columns(s, index, unique_index, Tvars)
  part_type = ACSetInterface.default_parts_type(part_type)
  Parts = make_parts(s, part_type)
  Subparts = genericize(pi_type(columns), TypeVar[values(Tvars)...])
  quote
    struct $parameterized_type <: $parent{$schema_type, Tuple{$(attrtypes(s)...)},$part_type}
      parts::$Parts
      subparts::$Subparts
      function $parameterized_type() where {$(attrtypes(s)...)}
        $new_call(
          $Parts(([$part_type() for _ in 1:$(length(types(s)))])),
          $(pi_type_elt([(f,:($(genericize(T, TypeVar[values(Tvars)...]))())) for (f,T) in columns]))
        )
      end
      function $parameterized_type(parts::$Parts, subparts::$Subparts) where {$(attrtypes(s)...)}
        $new_call(parts, subparts)
      end
    end
  end
end

unquote(x::QuoteNode) = x.value

""" This macro creates custom structs that subclass `StructACSet{S}` for specific `S`.
These are used for acsets whose schema is known at compile time.
"""
macro acset_type(head)
  head, parent = @match head begin
    Expr(:(<:), h, p) => (h,p)
    _ => (head, GlobalRef(DenseACSets, :StructACSet))
  end
  name, schema, idx_args = @match head begin
    Expr(:call, name, schema, idx_args...) => (name, schema, idx_args)
    _ => error("Unsupported head for @acset_type")
  end
  quote
    $(esc(:eval))($(GlobalRef(DenseACSets, :struct_acset))(
      $(Expr(:quote, name)), $(Expr(:quote, parent)), $(esc(schema));
      $((esc(arg) for arg in idx_args)...)))
    Core.@__doc__ $(esc(name))
  end
end

""" We want control over the type class hierarchy of acsets; this allows us
to create abstract types that subtype StructACSet. For instance, we might have
an `AbstractGraph` type, and then assume (this is not enforced) that any
subtype of `AbstractGraph` has `E,V,src,tgt` in its schema.
"""
macro abstract_acset_type(head)
  type, parent = @match head begin
    Expr(:(<:), h, p) => (h,p)
    _ => (head, GlobalRef(DenseACSets, :StructACSet))
  end
  esc(quote
    abstract type $type{S,Ts,P} <: $parent{S,Ts,P} end
  end)
end

""" This is a SimpleACSet which has the schema as a field value rather
than as a type parameter.
"""
struct DynamicACSet{PT} <: SimpleACSet{PT}
  name::String
  schema::Schema{Symbol}
  type_assignment::Dict{Symbol,Type}
  parts::Dict{Symbol,<:PartsType}
  subparts::Dict{Symbol,Column}
end

function DynamicACSet(
  name::String,
  s::Schema{Symbol};
  type_assignment=Dict{Symbol,Type}(),
  index::Vector=[],
  unique_index::Vector=[],
  part_type::Type{<:PartsType}=IntParts
)
  part_type = ACSetInterface.default_parts_type(part_type)
  DynamicACSet{part_type}(
    name,
    s,
    type_assignment,
    Dict(ob => part_type() for ob in types(s)),
    Dict([
      [f => column_type(HomChoice, indexchoice(f,index,unique_index))()
       for f in homs(s; just_names=true)];
      [f => column_type(AttrChoice(type_assignment[c]), indexchoice(f,index,unique_index))()
       for (f,_,c) in attrs(s)]
    ])
  )
end

attrtype_type(x::DynamicACSet, D::Symbol) = x.type_assignment[D]
attr_type(x::DynamicACSet, f::Symbol) = attrtype_type(x,codom(x.schema, f))
datatypes(x::DynamicACSet) = x.type_assignment

function ACSetInterface.constructor(X::DynamicACSet{PT}; type_assignment=nothing,
    index=nothing, unique_index=nothing, part_type=nothing) where PT
  type_assignment = isnothing(type_assignment) ? X.type_assignment : type_assignment
  index = isnothing(index) ? indices(X) : index 
  unique_index = isnothing(unique_index) ? unique_indices(X) : unique_index 
  part_type = isnothing(part_type) ? PT : part_type
  () -> DynamicACSet(X.name,X.schema,type_assignment=type_assignment, 
                  index=index, unique_index=unique_index, part_type=part_type)
end

function ACSetInterface.subpart_type(x::DynamicACSet,s::Symbol) 
  try
    if s in [a[1] for a in x.schema.attrs]
      attr_type(x,s)
    else
      attrtype_type(x,s)
    end
  catch e
    error("$s is neither a valid attr nor attrtype.")
  end
end

"""Cast StructACSet into a DynamicACSet"""
function DynamicACSet(X::StructACSet{S}) where S 
  Y = DynamicACSet(string(typeof(X).name.name), Schema(S); type_assignment=datatypes(X))
  copy_parts!(Y,X, NamedTuple(Dict(k=>parts(X,k) for k in types(S))))
  return Y
end



""" This works the same as something made with `@acset_type`, only the types of the
parts and subparts are stored as type parameters. Thus, this can be used with any schema.
"""
struct AnonACSet{S,Ts,Parts,Subparts,PT} <: StructACSet{S,Ts, PT}
  parts::Parts
  subparts::Subparts
end

function AnonACSet{S,Ts,Parts,Subparts,PT}() where {S,Ts,Parts,Subparts,PT}
  AnonACSet{S,Ts,Parts,Subparts,PT}(
    Parts([PT() for _ in 1:length(types(S))]),
    Subparts(T() for T in Subparts.parameters[2].parameters)
  )
end

function AnonACSet(
  s::Schema{Symbol};
  type_assignment=Dict{Symbol,Type}(),
  index::Vector{Symbol}=Symbol[],
  unique_index::Vector{Symbol}=Symbol[],
  part_type::Type{<:PartsType}=IntParts
)
  T = AnonACSetType(s; type_assignment, index=index, unique_index=unique_index,
                    part_type=part_type)
  T()
end

""" This can be used to fill out the type parameters to an AnonACSet ahead of time.
"""
function AnonACSetType(
  s::Schema;
  type_assignment::Dict{Symbol, Type}=Dict{Symbol,Type}(),
  index::Vector=[],
  unique_index::Vector=[],
  union_all::Bool=false,
  part_type::Type{<:PartsType}=IntParts
)
  (!union_all || isempty(type_assignment)) || error("If union_all is true, then attrtypes must be empty")
  S = typelevel(s)
  if union_all
    Tvars = Dict(at => TypeVar(at) for at in attrtypes(s))
  else
    Tvars = type_assignment
  end
  Ts = Tuple{(Tvars[at] for at in attrtypes(s))...}
  columns = make_columns(s, index, unique_index, Tvars)
  part_type = ACSetInterface.default_parts_type(part_type)
  Parts = make_parts(s, part_type)
  Subparts = pi_type(columns)
  T = AnonACSet{S,Ts,Parts,Subparts,part_type}
  if union_all
    foldr(UnionAll, [Tvars[at] for at in attrtypes(s)]; init=T)
  else
    T
  end
end

attrtype_type(::StructACSet{S,Ts}, D::Symbol) where {S,Ts} = attrtype_instantiation(S, Ts, D)
attr_type(X::StructACSet{S}, f::Symbol) where {S} = attrtype_type(X, codom(S, f))
datatypes(::StructACSet{S,Ts}) where {S,Ts} = Dict{Symbol,Type}(zip(attrtypes(S),Ts.parameters))
function ACSetInterface.constructor(X::StructACSet{S,Ts,PT};
    index=nothing, type_assignment=nothing, unique_index=nothing, 
    part_type=nothing) where {S,Ts,PT}
  if all(isnothing, [index, type_assignment, unique_index, part_type]) 
    return typeof(X)
  else 
    type_assignment = isnothing(type_assignment) ? datatypes(X) : type_assignment
    index = isnothing(index) ? indices(X) : index 
    unique_index = isnothing(unique_index) ? unique_indices(X) : unique_index 
    part_type = isnothing(part_type) ? PT : part_type
    return () -> AnonACSet(Schema(S), type_assignment=type_assignment, index=index, 
                           unique_index=unique_index, part_type=part_type)
  end
end

function ACSetInterface.subpart_type(x::StructACSet{S}, s::Symbol) where {S}
  try 
    if s in [a[1] for a in Schema(S).attrs]
      attr_type(x,s)
    else
      attrtype_type(x,s)
    end
  catch e
    error("$s is not a valid attr/attrtype.")
  end
end

function ACSetTableSchema(s::Schema{Symbol}, ob::Symbol)
  attrs = filter(Schemas.attrs(s)) do (f,d,c)
    d == ob
  end
  BasicSchema{Symbol}([ob], [], attrtypes(s), attrs)
end

function ACSetTableDataType(::Type{<:StructACSet{S,Ts}}, ob::Symbol) where {S,Ts}
  s = Schema(S)
  s′ = ACSetTableSchema(s,ob)
  type_assignment = Dict{Symbol, Type}(a => T for (a,T) in zip(attrtypes(s), Ts.parameters))
  AnonACSetType(s′; type_assignment)
end

function ACSetTableUnionAll(::Type{<:StructACSet{S}}, ob::Symbol) where {S}
  s′ = ACSetTableSchema(Schema(S),ob)
  AnonACSetType(s′;union_all=true)
end

""" This takes an ACSet type, and produces an AnonACSet which represents an
acset with just the object passed in, and then all of the attributes of that
object.

TODO: rename this to be less confusing with ACSetTable. Maybe ASet (attributed
set)
"""
function ACSetTableType(X::Type, ob::Symbol; union_all::Bool=false)
  (union_all ? ACSetTableUnionAll : ACSetTableDataType)(X, ob)
end

Base.copy(acs::DynamicACSet{PT}) where PT =
  DynamicACSet{PT}(
    acs.name,
    acs.schema,
    acs.type_assignment,
    deepcopy(acs.parts),
    typeof(acs.subparts)(k => copy(v) for (k,v) in pairs(acs.subparts))
  )

indices(acs::ACSet) = 
  Symbol[k for (k,v) in pairs(acs.subparts) if v.pc isa StoredPreimageCache]
unique_indices(acs::ACSet) = 
  Symbol[k for (k,v) in pairs(acs.subparts) if v.pc isa InjectiveCache]

Base.copy(acs::T) where {T <: StructACSet} =
  T(deepcopy(acs.parts), map(copy, acs.subparts))

Base.:(==)(acs1::T, acs2::T) where {T <: SimpleACSet} =
  acs1.parts == acs2.parts && acs1.subparts == acs2.subparts

ACSetInterface.acset_schema(acs::StructACSet{S}) where {S} = Schema(S)
ACSetInterface.acset_schema(acs::DynamicACSet) = acs.schema

add_parts_with_indices!(acs::SimpleACSet, ob::Symbol, n::Int, index_sizes::NamedTuple) =
  add_parts!(acs, ob, n)

Base.hash(x::T, h::UInt) where T <: SimpleACSet =
  hash(x.parts, hash(x.subparts, h))

@inline ACSetInterface.add_parts!(acs::SimpleACSet, ob::Symbol, n::Int) = 
  ACSetInterface.add_parts!(acs.parts[ob], n)


@inline ACSetInterface.nparts(acs::SimpleACSet, type::Symbol) = nparts(acs.parts[type])
@inline ACSetInterface.maxpart(acs::SimpleACSet, type::Symbol) = maxpart(acs.parts[type])

ACSetInterface.has_part(::StructACSet{S}, ob::Symbol) where {S} =
  _has_part(Val{S}, Val{ob})

ACSetInterface.has_part(acs::DynamicACSet, ob::Symbol) =
  runtime(_has_part, acs.schema, ob)

@ct_enable function _has_part(@ct(S), @ct(ob))
  @ct s = Schema(S)
  @ct ob ∈ types(s)
end

outgoing(acs::StructACSet{S}, ob::Symbol) where {S} = _outgoing(Val{S}, Val{ob})

outgoing(acs::DynamicACSet, ob::Symbol) = runtime(_outgoing, acs.schema, ob)

@ct_enable function _outgoing(@ct(S), @ct(ob))
  @ct s = Schema(S)
  @ct Tuple(arrows(s; from=ob, just_names=true))
end

@inline default_value(acs::StructACSet{S}, f::Symbol) where {S} = _default_value(Val{S}, Val{f})
@inline default_value(acs::DynamicACSet, f::Symbol) = runtime(_default_value, acs.schema, f)

@ct_enable function _default_value(@ct(S), @ct(f))
  @ct begin
    s = Schema(S)
    if f ∈ homs(s; just_names=true)
      0
    elseif f ∈ attrs(s; just_names=true)
      nothing
    else
      error("$f not in schema")
    end
  end
end

Base.view(acs::SimpleACSet, part, f) = view_with_default(acs.subparts[f], part, default_value(acs, f))
Base.view(acs::SimpleACSet, part::AbstractVector{Bool}, f) = view(acs, f)[part]
Base.view(acs::SimpleACSet, ::Colon, f) = view(acs, dom_parts(acs,f), f)

@inline Base.view(acs::SimpleACSet, f::Symbol) =
  view_with_default(acs.subparts[f], dom_parts(acs, f), default_value(acs, f))

@inline ACSetInterface.subpart(acs::SimpleACSet, f::Symbol) = subpart(acs, dom_parts(acs, f), f)
@inline ACSetInterface.subpart(acs::SimpleACSet, f::Vector{Symbol}) = subpart(acs, dom_parts(acs, first(f)), f)
@inline ACSetInterface.subpart(acs::ACSet, part::Union{Colon,AbstractVector}, name::Symbol) =
  collect_column(view(acs, part, name))

@inline ACSetInterface.subpart(acs::SimpleACSet, f::Tuple{Vararg{Symbol,1}}) = subpart(acs, dom_parts(acs, only(f)), only(f))
@inline ACSetInterface.subpart(acs::SimpleACSet, part, f::Tuple{Vararg{Symbol,1}}) = subpart(acs, part, only(f))

@inline ACSetInterface.subpart(acs::SimpleACSet, names::Tuple{Vararg{Symbol}}) = subpart(acs, dom_parts(acs, first(names)), names)

ACSetInterface.subpart(acs::StructACSet{S}, part, names::Tuple{Vararg{Symbol}}) where {S} = _subpart(acs, part, Val{S}, Val{names})
ACSetInterface.subpart(acs::DynamicACSet, part, names::Tuple{Vararg{Symbol}}) = runtime(_subpart, acs, part, acs.schema, names)
  
@ct_enable function _subpart(acs::SimpleACSet, part, @ct(S), @ct(names))
  @ct s = Schema(S)
  out = ACSetInterface.collect_or_id(subpart(acs, part, @ct first(names)))
  @ct_ctrl for i in 2:length(names)
    @ct begin
      codom(s, names[i-1]) == dom(s, names[i]) || error("morphisms $(names[i-1]) and $(names[i]) are not composable")
    end
    out = ACSetInterface.collect_or_id(subpart(acs, out, @ct names[i]))
  end
  return out
end

function collect_column(x::AbstractVector)
  if isempty(x)
    Base.typesplit(eltype(x), AttrVar)[]
  else
    map(identity, x)
  end
end

@inline ACSetInterface.subpart(acs::SimpleACSet, part::Int, f::Symbol) =
  get(acs.subparts[f], part, default_value(acs, f))

@inline ACSetInterface.has_subpart(::StructACSet{S}, f::Symbol) where {S} =
  _has_subpart(Val{S}, Val{f})

ACSetInterface.has_subpart(acs::DynamicACSet, f::Symbol) =
  runtime(_has_subpart, acs.schema, f)

@ct_enable function _has_subpart(@ct(S), @ct(f))
  @ct s = Schema(S)
  @ct f ∈ arrows(s; just_names=true)
end

@inline ACSetInterface.dom_parts(acs::StructACSet{S}, f::Symbol) where {S} = _dom_parts(acs, Val{S}, Val{f})
@inline ACSetInterface.dom_parts(acs::DynamicACSet, f::Symbol) = runtime(_dom_parts, acs, acs.schema, f)

@ct_enable function _dom_parts(acs, @ct(S), @ct(f))
  @ct s = Schema(S)
  parts(acs, @ct dom(s, f))
end

@inline ACSetInterface.codom_parts(acs::StructACSet{S}, f::Symbol) where {S} = _codom_parts(acs, Val{S}, Val{f})
@inline ACSetInterface.codom_parts(acs::DynamicACSet, f::Symbol) = runtime(_codom_parts, acs, acs.schema, f)

@ct_enable function _codom_parts(acs, @ct(S), @ct(f))
  @ct s = Schema(S)
  parts(acs, @ct codom(s, f))
end

@inline function ACSetInterface.incident(acs::SimpleACSet, part, f::Symbol)
  preimage(dom_parts(acs, f), acs.subparts[f], part)
end 

"""
Calling incident on a range of values, e.g. `incident(G, 1:2, :src)` is 
equivalent to concatenating the results of incident on each part, i.e. 
`[incident(G,1,:src), incident(G,2,:src)]`.
"""
@inline function ACSetInterface.incident(acs::SimpleACSet, 
    parts::Union{AbstractVector,UnitRange}, f::Symbol)
  # FIXME: update preimage_multi to work on attrs for better performance
  AbstractVector{Int}[incident(acs, part, f) for part in parts]
end 

@inline ACSetInterface.incident(acs::StructACSet{S}, ::Colon, f::Symbol) where {S} =
  _incident(acs, Val{S}, :, Val{f})

ACSetInterface.incident(acs::DynamicACSet, ::Colon, f::Symbol) =
  runtime(_incident, acs, acs.schema, :, f)

@ct_enable function _incident(acs::SimpleACSet, @ct(S), ::Colon, @ct(f))
  @ct s = Schema(S)
  incident(acs, parts(acs, @ct(codom(s, f))), @ct(f))
end

@inline ACSetInterface.set_subpart!(acs::StructACSet{S,Ts}, part::Int, f::Symbol, subpart) where {S,Ts} =
  _set_subpart!(acs, Val{S}, Val{Ts}, part, Val{f}, subpart)

ACSetInterface.set_subpart!(acs::DynamicACSet, part::Int, f::Symbol, subpart) =
  runtime(_set_subpart!, acs, acs.schema, 
    Tuple{[acs.type_assignment[t] for t in acs.schema.attrtypes]...}, 
    part, f, subpart)

@ct_enable function _set_subpart!(acs::SimpleACSet, @ct(S), @ct(Ts), part, @ct(f), subpart)
  @ct s = Schema(S)
  @ct_ctrl if f ∈ homs(s; just_names=true)
    @assert 0 <= subpart <= ACSetInterface.nparts(acs,@ct codom(s, f))
  end
  acs.subparts[@ct f][part] = subpart
end

@inline ACSetInterface.clear_subpart!(acs::SimpleACSet, part::Int, f::Symbol) =
  delete!(acs.subparts[f], part)

@inline ACSetInterface.rem_part!(acs::StructACSet{S}, type::Symbol, part::Int) where {S} =
  _rem_part!(acs, Val{S}, Val{type}, part, acs.parts[type])

ACSetInterface.rem_part!(acs::DynamicACSet, type::Symbol, part::Int) =
  runtime(_rem_part!, acs, acs.schema, type, part, acs.parts[type])

@ct_enable function _rem_part!(acs::SimpleACSet, @ct(S), @ct(ob), part, ::DenseParts)
  @ct s = Schema(S)
  @ct in_homs = homs(s; to=ob, just_names=true)
  @ct in_attrs = attrs(s; to=ob, just_names=true)
  @ct out_homs = homs(s; from=ob, just_names=true)
  @ct out_attrs = attrs(s; from=ob, just_names=true)

  last_part = acs.parts[@ct ob].val

  @ct_ctrl for hom in in_homs
    incoming_to_part = copy(incident(acs, part, @ct hom))
    clear_subpart!(acs, incoming_to_part, @ct hom)

    incoming_to_last_part = copy(incident(acs, last_part, @ct hom))
    set_subpart!(acs, incoming_to_last_part, (@ct hom), part)
  end

  @ct_ctrl for hom in in_attrs
    incoming_to_part = copy(incident(acs, AttrVar(part), @ct hom))
    clear_subpart!(acs, incoming_to_part, @ct hom)

    incoming_to_last_part = copy(incident(acs, AttrVar(last_part), @ct hom))
    set_subpart!(acs, incoming_to_last_part, (@ct hom), [AttrVar(part)])
  end

  @ct_ctrl for f in [out_homs; out_attrs]
    if haskey(acs.subparts[@ct f], last_part) && part != last_part
      last_part_f = subpart(acs, last_part, @ct f)      
      clear_subpart!(acs, last_part, @ct f)
      set_subpart!(acs, part, (@ct f), last_part_f)
    else
      clear_subpart!(acs, last_part, @ct f)
    end    
  end
  acs.parts[@ct ob].val -= 1
end

@ct_enable function _rem_part!(acs::SimpleACSet, @ct(S), @ct(ob), part, ::MarkAsDeleted)
  @ct s = Schema(S)
  @ct out_homs = homs(s; from=ob, just_names=true)
  @ct out_attrs = attrs(s; from=ob, just_names=true)

  @ct_ctrl for f in [out_homs; out_attrs]
    clear_subpart!(acs, part, @ct f)
  end

  delete!(acs.parts[ob].val, part)
end

"""
Identify which parts of an ACSet need to be deleted if some initial collection 
of parts is to be deleted. E.g. deleting a vertex deletes its edge
"""
function delete_subobj(X::ACSet, delparts)
  S = acset_schema(X)
  delparts = Dict([k=>Set{Int}(get(delparts, k, [])) for k in ob(S)])
  change = true
  while change
    change = false
    for (f,c,d) in homs(S)
      for c_part in setdiff(parts(X,c),delparts[c])
        if X[c_part,f] ∈ delparts[d]
          change = true
          push!(delparts[c], c_part)
        end
      end
    end
  end
  return Dict([k => sort(collect(v)) for (k,v) in pairs(delparts)])
end

"""
Return a mapping of from parts of updated X to the old X

Note: the correctness is dependent on the implementation details of `rem_parts!`
"""
function delete_subobj!(X::ACSet, delparts)
  dels = delete_subobj(X, delparts) # find all of the things that need to go
  return NamedTuple(map(ob(acset_schema(X))) do o
    map_to_old = collect(parts(X, o)) # injective map from curr to original X
    for delᵢ in reverse(dels[o])
      currᵢ = findfirst(==(delᵢ), map_to_old) # where to delete in curr acset
      rem_part!(X, o, currᵢ) # remove the part
      map_to_old[currᵢ] = map_to_old[end] # update map to original X
      pop!(map_to_old) # (assumes rem_part! popped and swapped from the end)
    end
    o => map_to_old
  end)
end

ACSetInterface.cascading_rem_parts!(acs::ACSet, type, parts) =
  delete_subobj!(acs, Dict(type=>parts))

function ACSetInterface.undefined_subparts(acs::SimpleACSet{<:DenseParts}, f::Symbol)
  findall([!haskey(acs.subparts[f],i) for i in dom_parts(acs,f)])
end

function ACSetInterface.undefined_subparts(acs::SimpleACSet{<:MarkAsDeleted}, f::Symbol)
  codom_ids = codom_parts(acs,f)
  findall([acs.subparts[f][i] ∉ codom_ids for i in dom_parts(acs,f)])
end

# Copy Parts
############

@ct_enable function common_objects(@ct(S), @ct(S′))
  @ct s,s′ = Schema(S), Schema(S′)
  @ct Tuple(intersect(types(s), types(s′)))
end

ACSetInterface.copy_parts!(to::StructACSet{S}, from::StructACSet{S′}) where {S,S′} =
  copy_parts!(to, from, common_objects(Val{S}, Val{S′}))

ACSetInterface.copy_parts!(to::DynamicACSet, from::DynamicACSet) =
  copy_parts!(to, from, runtime(common_objects, to.schema, from.schema))

ACSetInterface.copy_parts!(to::SimpleACSet, from::SimpleACSet; kw...) =
  copy_parts!(to, from, (;kw...))

ACSetInterface.copy_parts!(to::SimpleACSet, from::SimpleACSet, obs::Tuple) =
  copy_parts!(to, from, NamedTuple{obs}((:) for ob in obs))

ACSetInterface.copy_parts!(to::StructACSet{S}, from::StructACSet{S′}, parts::NamedTuple) where {S,S′} =
  _copy_parts!(to, Val{S}, from, Val{S′}, replace_colons(from, parts))

ACSetInterface.copy_parts!(to::ACSet, from::ACSet, parts::NamedTuple) =
  runtime(_copy_parts!, to, acset_schema(to), from, acset_schema(from), replace_colons(from, parts))

@ct_enable function _copy_parts!(
  to::SimpleACSet, @ct(S),
  from::SimpleACSet, @ct(S′),
  parts::NamedTuple{obs}
) where {obs}
  @ct begin
    s, s′ = Schema(S), Schema(S′)
    @assert obs ⊆ intersect(types(s), types(s′))
    common_homs = intersect(homs(s), homs(s′))
    relevant_homs = [(f,d,c) for (f,d,c) in common_homs if d ∈ obs && c ∈ obs]
    in_obs = unique!([c for (_,_,c) in relevant_homs])
  end

  newparts = copy_parts_only!(to, from, parts)
  partmaps = NamedTuple{@ct Tuple(in_obs)}(((@ct_ctrl (
    Dict{Int,Int}(zip(parts[@ct type], newparts[@ct type]))
    for type in in_obs
  )...),))

  @ct_ctrl for (f, d, c) in relevant_homs
    for (p, newp) in zip(parts[@ct d], newparts[@ct d])
      q = subpart(from, p, @ct f)
      newq = get(partmaps[@ct c], q, nothing)
      if !isnothing(newq)
        set_subpart!(to, newp, @ct(f), newq)
      end
    end
  end

  newparts
end

ACSetInterface.copy_parts_only!(to::SimpleACSet, from::SimpleACSet; kw...) =
  copy_parts!(to, from, (;kw...))

ACSetInterface.copy_parts_only!(to::StructACSet{S}, from::StructACSet{S′}) where {S,S′} =
  copy_parts_only!(to, from, common_objects(Val{S}, Val{S′}))

ACSetInterface.copy_parts_only!(to::SimpleACSet, from::SimpleACSet, obs::Tuple) =
  copy_parts_only!(to, from, NamedTuple{obs}((:) for ob in obs))

ACSetInterface.copy_parts_only!(to::StructACSet{S}, from::StructACSet{S′}, parts::NamedTuple) where {S,S′}=
  _copy_parts_only!(to, Val{S}, from, Val{S′}, replace_colons(from, parts))

ACSetInterface.copy_parts_only!(to::ACSet, from::ACSet, parts::NamedTuple) =
  runtime(_copy_parts_only!, to, acset_schema(to), from, acset_schema(from), replace_colons(from, parts))

@ct_enable function _copy_parts_only!(
  to::SimpleACSet, @ct(S),
  from::SimpleACSet, @ct(S′),
  parts::NamedTuple{obs}
) where {obs}

  @ct begin
    s, s′ = Schema(S), Schema(S′)
    @assert obs ⊆ intersect(types(s), types(s′))
    common_attrs = intersect(attrs(s), attrs(s′))
    relevant_attrs = [(f,d,c) for (f,d,c) in common_attrs if d ∈ obs]
  end

  newparts = NamedTuple{@ct obs}((@ct_ctrl(
    (add_parts!(to, @ct(ob), length(parts[@ct ob])) for ob in obs)...
  ),))

  @ct_ctrl for (a,d,c) in relevant_attrs
    for (part, val) in zip(newparts[@ct d], subpart(from, parts[@ct d], @ct(a)))
      if !(val isa AttrVar)
        set_subpart!(to, part, @ct(a), val)
      else
        newindex = findfirst(==(val.val), get(parts, @ct(c), []))
        if !isnothing(newindex)
          set_subpart!(to, part, @ct(a), AttrVar(newparts[@ct c][newindex]))
        end
      end
    end
  end

  newparts
end

function replace_colons(acs::ACSet, parts::NamedTuple{types}) where {types}
  NamedTuple{types}(map(types, parts) do type, part
    part == (:) ? collect(acs.parts[type]) : part
  end)
end

# Garbage collection 
####################

"""
Reindex the parts of the acset such that there are no gaps between the indices.
Return a vector for each part mapping the new parts into the old parts. 
"""
function ACSetInterface.gc!(X::ACSet{<:MarkAsDeleted})
  S = acset_schema(X)
  μ = Dict(map(types(S)) do o 
    p    = X.parts[o]
    m    = collect(p.val)
    m⁻¹  = Vector{Union{Int,Nothing}}(fill(nothing, p.next.x))
    for (i, v) in enumerate(m)  
      m⁻¹[v] = i 
    end
    return o => (m, m⁻¹)
  end)
  # Update homs and attrs
  for (h, a, b) in arrows(S)
    μᵦ = μ[b][2]
    if h ∈ homs(S; just_names=true)
      X[h] = [μᵦ[X[a,h]] for a in μ[a][1]]
    else 
      X[h] = map(μ[a][1]) do p
        p′ = X[p, h]
        p′ isa AttrVar ? AttrVar(μᵦ[p′.val]) : p′
      end
    end
    for i in (nparts(X,a)+1) : X.parts[a].next.x
      clear_subpart!(X, i, h)
    end
  end

  for o in types(S)
    gc!(X.parts[o], nparts(X,o))
  end
  return Dict([o=>μ[o][1] for o in types(S)])
end

function ACSetInterface.gc!(X::ACSet{<:DenseParts})
  Dict(o=>1:nparts(X,o) for o in types(acset_schema(X)))
end

sparsify(X::ACSet{<:MarkAsDeleted}) = X

function sparsify(X::ACSet{<:DenseParts})
  Y = constructor(X, part_type=BitSetParts)()
  copy_parts!(Y, X)
  Y
end

densify(X::ACSet{<:DenseParts}) = X

function densify(X::ACSet{<:MarkAsDeleted})
  Y = constructor(X; part_type=IntParts)()
  X = deepcopy(X)
  m = ACSetInterface.gc!(X)
  copy_parts!(Y, X)
  Y, m
end

# Type modification
###################

function empty_with_types(acs::SA, type_assignment) where {S, SA <: StructACSet{S}}
  s = acset_schema(acs)
  (SA.name.wrapper){[type_assignment[d] for d in attrtypes(s)]...}()
end

function empty_with_types(acs::DynamicACSet, type_assignment)
  DynamicACSet(acs.name, acs.schema, type_assignment)
end

function get_type_assignment(acs::StructACSet{S,Ts}) where {S,Ts}
  Dict(d => Ts.parameters[i] for (i,d) in enumerate(attrtypes(S)))
end

get_type_assignment(acs::DynamicACSet) = acs.type_assignment

# Printing
##########

ACSetInterface.acset_name(x::StructACSet) = sprint(show, typeof(x))

function ACSetInterface.acset_name(x::DynamicACSet)
  s = x.schema
  if length(attrtypes(s)) == 0
    x.name
  else
    Ts = join([x.type_assignment[at] for at in attrtypes(s)], ",")
    "$(x.name){$(Ts)}"
  end
end

function Base.show(io::IO, acs::SimpleACSet)
  s = acset_schema(acs)
  if get(io, :compact, false)
    print(io, acset_name(acs))
    print(io, " {")
    join(io, ("$(ob):$(nparts(acs,ob))" for ob in types(s)), ", ")
    print(io, "}")
  else
    print(io, acset_name(acs))
    println(io, ":")
    join(io, Iterators.flatten((
      ("  $ob = $(parts(acs,ob))" for ob in types(s)),
      ("  $f : $d → $c = $(subpart(acs,f))"
       for (f,d,c) in homs(s)),
      ("  $a : $d → $c = $(subpart(acs,a))"
       for (a,d,c) in attrs(s)),
    )), "\n")
  end
end

# Tables
########

struct ACSetTable{T<:ACSet, ob} <: Tables.AbstractColumns
  parent::T
end

ACSetTable(acs::ACSet, ob::Symbol) = ACSetTable{typeof(acs), ob}(acs)

ACSetInterface.tables(acs::StructACSet{<:TypeLevelBasicSchema{Name, obs}}) where {Name, obs} =
  NamedTuple([ob => ACSetTable(acs, ob) for ob in obs.parameters])

ACSetInterface.tables(acs::DynamicACSet) =
  NamedTuple([ob => ACSetTable(acs, ob) for ob in objects(acs.schema)])

""" Get parent acset.

Given a `ACSetTable` or `ACSetRow` object from the Tables.jl interface,
return the parent acset the object was derived from.
"""
parent(sat::ACSetTable) = getfield(sat, :parent)

struct ACSetRow{T<:ACSet, ob} <: Tables.AbstractRow
  parent::T
  idx::Int
end

parent(row::ACSetRow) = getfield(row, :parent)

""" Get index of row in parent acset.

Given an `ACSetRow` object from the Tables.jl interface,
return the ID of the correspond part in the parent acset the row was
derived from.
"""
idx(row::ACSetRow) = getfield(row, :idx)

# - Tables.jl interface

Tables.istable(sat::ACSetTable) = true

Tables.columnaccess(sat::ACSetTable) = true
Tables.columns(sat::ACSetTable) = sat

Tables.getcolumn(sat::ACSetTable{T,ob}, i::Int) where {T,ob} =
  Base.getproperty(sat, outgoing(parent(sat), ob)[i])

Base.getproperty(sat::ACSetTable, nm::Symbol) = subpart(parent(sat), :, nm)

Tables.getcolumn(sat::ACSetTable, nm::Symbol) = Base.getproperty(sat, nm)

Base.propertynames(sat::ACSetTable{T,ob}) where {T,ob} = outgoing(parent(sat), ob)

Tables.columnnames(sat::ACSetTable) = Base.propertynames(sat)

Tables.rowaccess(sat::ACSetTable) = true
Tables.rows(sat::ACSetTable{T,ob}) where {T,ob} =
  ACSetRow{T,ob}.(Ref(parent(sat)), parts(parent(sat), ob))

Tables.getcolumn(row::ACSetRow{T,ob}, i::Int) where {T,ob} =
  Base.getproperty(row, outgoing(parent(row), ob)[i])

Base.getproperty(row::ACSetRow, nm::Symbol) = subpart(parent(row), idx(row), nm)

Tables.getcolumn(row::ACSetRow, nm::Symbol) = Base.getproperty(row, nm)

Base.propertynames(row::ACSetRow{T,ob}) where {T,ob} = outgoing(parent(row), ob)

Tables.columnnames(row::ACSetRow) = Base.propertynames(row)

# Acset macro
#############

@ct_enable function _make_acset(@ct(S), T, rows::NamedTuple{names}) where {names}
  @ct begin
    s = Schema(S)
  end
  acs = T()
  @ct_ctrl for ob in intersect(types(s), names)
    add_parts!(acs, @ct(ob), rows[@ct ob])
  end
  @ct_ctrl for f in intersect(arrows(s; just_names=true), names)
    set_subpart!(acs, :, @ct(f), rows[@ct f])
  end
  acs
end

ACSetInterface.make_acset(T::Type{<:StructACSet{S}}, rows::NamedTuple) where {S} =
  _make_acset(Val{S}, T, rows)

# TODO: Support dynamic acsets with `@acset!` macro?

# Mapping
#########

function Base.map(acs::ACSet; kwargs...)
  s = acset_schema(acs)
  fns = (;kwargs...)

  mapped_attrs = intersect(attrs(s; just_names=true), keys(fns))
  mapped_attrtypes = intersect(attrtypes(s), keys(fns))
  mapped_attrs_from_attrtypes = [a for (a,d,c) in attrs(s) if c ∈ mapped_attrtypes]
  attrs_accounted_for = unique!(sort!(Symbol[mapped_attrs; mapped_attrs_from_attrtypes]))

  affected_attrtypes = unique!(sort!(map(a -> codom(s,a), attrs_accounted_for)))
  needed_attrs = sort!([a for (a,d,c) in attrs(s) if c ∈ affected_attrtypes])

  unnaccounted_for_attrs = filter(a -> a ∉ attrs_accounted_for, needed_attrs)
  unnaccounted_for_attrs == [] ||
    error("not enough functions provided to fully transform ACSet, need functions for: $(unnaccounted_for_attrs)")

  new_subparts = Dict(
    f => (f ∈ keys(fns) ? fns[f] : fns[codom(s, f)]).(subpart(acs, f))
    for f in needed_attrs)

  type_assignments = get_type_assignment(acs)

  new_type_assignments = Dict(map(enumerate(attrtypes(s))) do (i,d)
    if d ∈ affected_attrtypes
      d => mapreduce(eltype, typejoin, [new_subparts[f] for f in attrs(s, to=d, just_names=true)])
    else
      d => type_assignments[d]
    end
  end...)

  new_acs = empty_with_types(acs, new_type_assignments)

  for ob in objects(s)
    add_parts!(new_acs, ob, nparts(acs, ob))
  end

  for f in homs(s; just_names=true)
    set_subpart!(new_acs, :, f, subpart(acs, f))
  end

  for f in attrs(s; just_names=true)
    if f ∈ keys(new_subparts)
      set_subpart!(new_acs, :, f, new_subparts[f])
    else
      set_subpart!(new_acs, :, f, subpart(acs, f))
    end
  end

  new_acs
end

end
