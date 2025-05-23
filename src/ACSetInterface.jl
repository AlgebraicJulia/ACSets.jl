module ACSetInterface
export ACSet, acset_schema, acset_name, dom_parts, codom_parts, subpart_type,
  nparts, maxpart, parts, has_part, has_subpart, subpart, incident,
  add_part!, add_parts!, set_subpart!, set_subparts!, clear_subpart!,
  rem_part!, rem_parts!, cascading_rem_part!, cascading_rem_parts!, gc!,
  copy_parts!, copy_parts_only!, disjoint_union, tables, pretty_tables,
  @acset, constructor, undefined_subparts, PartsType, DenseParts, MarkAsDeleted,
  rem_free_vars!, parts_type, ensure_size!

using MLStyle: @match
using StaticArrays: StaticArray
using Tables
using PrettyTables: pretty_table

using ..ColumnImplementations: AttrVar

using ..Schemas: types, attrs, attrtypes

# Parts types
#############

""" Type of part IDs to use in an acset.

The choice of parts type does not alter the mathematical model but it does
affect the performance tradeoffs of the acset data structure, the assumptions
that can be made about the part IDs, and whether garbage collection
([`gc!`](@ref)) is relevant.

Type parameter `S` is the collection type (`Int`, `BitSet`, etc) and type
parameter `T` is the element type (`Int`, `Symbol`, etc), mirroring the two type
parameters of `FinSet` in Catlab.

The default choice of parts type is [`DenseParts`](@ref).
"""
abstract type PartsType{S,T} end

""" Part IDs are densely packed without gaps.

Mutations are eager and garbage collection is a no-op. Deletion or
identification of parts may invalidate external references to particular parts.
"""
abstract type DenseParts{S,T} <: PartsType{S,T} end

""" Mark parts as deleted when they are removed.

Deletions are lazy and arrays are not resized until garbage collection. Parts
can be deleted without invalidating external references to other parts.
"""
abstract type MarkAsDeleted{S,T} <: PartsType{S,T} end

""" Allow distinct part IDs to refer to the same logical part.

Implemented using union-find. Garbage collection is an operation that makes
sense to perform. Parts can be identified with each other without invalidating
external references to particular parts.
"""
abstract type UnionFind{S,T} <: PartsType{S,T} end

""" Combination of [`MarkAsDeleted`](@ref) and [`UnionFind`](@ref).
"""
abstract type MarkAsDeletedUnionFind{S,T} <: PartsType{S,T} end

function default_parts_type(::Type{PT}) where PT <: PartsType
  @assert !isabstracttype(PT)
  return PT
end

# Core interface
################

"""
Abstract base type for acsets, static or dynamic.
"""
abstract type ACSet{PT<:PartsType}  end

"""
Get the schema of an acset at runtime.
"""
function acset_schema end

"""
Get the name of an acset at runtime
"""
function acset_name end

""" Number of parts of given type in an acset.
"""
function nparts end

""" Maximum possible part value of given type in an acset.
"""
function maxpart end

""" Parts of given type in an acset.
"""
@inline parts(acs, type) = parts(acs.parts[type])

""" Whether an acset has a part with the given name.
"""
function has_part end

@inline has_part(acs, type::Symbol, part::Int) = 1 <= part <= nparts(acs, type)
@inline has_part(acs, type::Symbol, part::AbstractVector{Int}) =
  let n=nparts(acs, type); [ 1 <= x <= n for x in part ] end

function has_subpart end

"""
Get the parts of the domain of a morphism in an acset

dom_parts(acs, f) == parts(acs, X)

where X is the dom of the f in the schema
"""
function dom_parts end

"""
Get the parts of the codomain of a morphism in an acset

dom_parts(acs, f) == parts(acs, Y)

where Y is the codom of the f in the schema
"""
function codom_parts end

"""
Get the type assigned to a subpart in an acset, i.e.

subpart_type(acs::WeightedGraph{T}, :weight) == T
"""
function subpart_type end

""" Get subpart of part in acset.

Both single and vectorized access are supported, with a view of the underlying
data being returned in the latter case. Chaining, or composition, of subparts is
also supported. For example, given a vertex-attributed graph `g`,

```
subpart(g, e, [:src, :vattr])
```

returns the vertex attribute of the source vertex of the edge `e`. As a
shorthand, subparts can also be accessed by indexing:

```
g[e, :src] == subpart(g, e, :src)
```

If the chaining of subparts is given as a tuple (e.g.; (:src, :vattr)), then
code generation is used to check that the subparts and their order is a valid
composition and to improve performance.

Be warned that indexing with lists of subparts works just like `subpart`:
`g[e,[:src,:vattr]]` is equivalent to `subpart(g, e, [:src,:vattr])`. This
convention differs from DataFrames but note that the alternative interpretation
of `[:src,:vattr]` as two independent columns does not even make sense, since
they have different domains (belong to different tables).
"""
function subpart end

function view_or_slice end
@inline view_or_slice(x::AbstractVector, i::Union{Integer,StaticArray}) = x[i]
@inline view_or_slice(x::AbstractVector, ::Colon) = x
@inline Base.@propagate_inbounds view_or_slice(x::AbstractVector, i) = @view x[i]

function subpart(acs, part, names::AbstractVector{Symbol})
  foldl(names, init=part) do part, name
    subpart(acs, part, name)
  end
end

@inline Base.getindex(acs::ACSet, part, name) = subpart(acs, part, name)
@inline Base.getindex(acs::ACSet, name) = subpart(acs, name)

""" Get superparts incident to part in acset.

If the subpart is indexed, this takes constant time; otherwise, it takes linear
time. As with [`subpart`](@ref), both single and vectorized access, as well as
chained access, are supported. Note that sequences of morphisms are supplied in
the usual left-to-right order, so that

```
incident(g, x, [:src, :vattr])
```

returns the list of all edges whose source vertex has vertex attribute `x`.

If the chaining of subparts is given as a tuple (e.g.; (:src, :vattr)), then
code generation is used to check that the subparts and their order is a valid
composition and to improve performance.

Note that when the subpart is indexed, this function returns a view of the
underlying index, which should not be mutated. To ensure that a fresh copy is
returned, regardless of whether indexing is enabled, set the keyword argument
`copy=true`.
"""
function incident end

function incident(acs, part, names::AbstractVector{Symbol};
                  copy::Bool=false)
  # Don't need to pass `copy` because copy will be made regardless.
  foldr(names, init=part) do name, part
    reduce(vcat, collect.(incident(acs, part, name)), init=Int[])
  end
end


@inline add_part!(acs, type; kw...) = add_part!(acs, type, (;kw...))

""" Add part of given type to acset, optionally setting its subparts.

Returns the ID of the added part.

See also: [`add_parts!`](@ref).
"""
@inline function add_part!(acs::ACSet , type::Symbol, kw)
  part = only(add_parts!(acs,type,1))
  try
    set_subparts!(acs, part, kw)
  catch e
    rem_part!(acs, type, part)
    rethrow(e)
  end
  part
end

""" Add parts of given type to acset, optionally setting their subparts.

Returns the range of IDs for the added parts.

See also: [`add_part!`](@ref).
"""
function add_parts! end

@inline add_parts!(acs, type::Symbol, n::Int; kw...) = add_parts!(acs, type, n, (;kw...))

@inline function add_parts!(acs::ACSet, type::Symbol, n::Int, kw)
  parts = add_parts!(acs, type, n)
  try
    set_subparts!(acs, parts, kw)
  catch e
    rem_parts!(acs, type, parts)
    rethrow(e)
  end
  parts
end


""" Mutate subpart of a part in a C-set.

Both single and vectorized assignment are supported.

See also: [`set_subparts!`](@ref).
"""
function set_subpart! end
@inline set_subpart!(acs, name, vals) = set_subpart!(acs, :, name, vals)

@inline set_subpart!(acs, ::Colon, name, vals) =
  set_subpart!(acs, 1:length(subpart(acs,name)), name, vals)

# Inlined for the same reason as `subpart`.
@inline set_subpart!(acs::ACSet , parts::Union{AbstractVector{Int}, AbstractSet{Int}}, name, vals) = 
  set_subpart!(acs , parts, Val{name}, vals)

@inline function set_subpart!(acs::ACSet , parts::Union{AbstractVector{Int}, AbstractSet{Int}}, ::Type{Val{name}}, vals) where name
  broadcast(parts, vals) do part, val
    set_subpart!(acs, part, name, val)
  end
end

""" Mutate subparts of a part in a C-set.

Both single and vectorized assignment are supported.

See also: [`set_subpart!`](@ref).
"""
@inline @generated function set_subparts!(acs::ACSet, part, kw::NamedTuple{keys}) where {keys}
  Expr(:block,[:(set_subpart!(acs, part, $(Expr(:quote, name)), kw.$name)) for name in keys]...)
end

@inline set_subparts!(acs, part; kw...) = set_subparts!(acs, part, (;kw...))

@inline Base.setindex!(acs::ACSet, val, part, name) = set_subpart!(acs, part, name, val)
@inline Base.setindex!(acs::ACSet, vals, name) = set_subpart!(acs, name, vals)


"""Fill an ACSet

Adds parts for a given part `part` up to a given number `n`. Unlike `add_part!`, this is idempotent.

Example:

Consider a cycle graph `g` with 3 vertices and 3 edges. Then
```
ensure_size!(g, :V, 5) # = 4:5
``` 
adds vertices 4 and 5. Suppose instead we added an edge,
```
ensure_size!(g, :E, 4) # = 4:4
```
Repeating this operation, we see that no new edges are added.
```
ensure_size!(g, :E, 4) # = 1:0
```
"""
function ensure_size!(acs::ACSet, part::Symbol, n::Integer)
    if nparts(acs, part) < n
        add_parts!(acs, part, n - nparts(acs, part))
    else
        1:0 # type stability
    end
end

"""Clear a subpart in a C-set

If the subpart is a hom, this is equivalent to setting it to 0
If the subpart is an attr, then if the type has nothing as a subtype, it
sets value to nothing. If the type doesn't have nothing as a subtype, then
it does not change the value, but still unsets the index, so this can
potentially cause an inconsistent acset if used without caution.
"""

function clear_subpart! end

function clear_subpart!(acs::ACSet, parts::Union{AbstractVector{Int}, AbstractSet{Int}}, name)
  for part in parts
    clear_subpart!(acs, part, name)
  end
end

""" Remove part from a C-set.

The part is removed using the "pop and swap" strategy familiar from
[Graphs.jl](https://github.com/JuliaGraphs/Graphs.jl), where the "removed" part
is actually replaced by the last part, which is then deleted. This strategy has
important performance benefits since only the last part must be assigned a new
ID, as opposed to assigning new IDs to *every* part following the removed part.

The removal operation is *not* recursive. When a part is deleted, any superparts
incident to it are retained, but their subparts become undefined (equal to the
integer zero). For example, in a graph, if you call `rem_part!` on a vertex, the
edges incident the `src` and/or `tgt` vertices of the edge become undefined but
the edge itself is not deleted.

Indexing has both positive and negative impacts on performance. On the one hand,
indexing reduces the cost of finding affected superparts from linear time to
constant time. On the other hand, the indices of subparts must be updated when
the part is removed. For example, in a graph, indexing `src` and `tgt` makes
removing vertices faster but removing edges (slightly) slower.

See also: [`rem_parts!`](@ref).
"""
function rem_part! end

""" Remove part and all parts incident to it, recursively.

Cf. [`rem_part!`](@ref), which is not recursive.
"""
function cascading_rem_part!(acset::ACSet, type, part)
  cascading_rem_parts!(acset, type, [part])
end

""" Remove parts from a C-set.

The parts must be supplied in sorted order, without duplicates.

See also: [`rem_part!`](@ref).
"""
@inline function rem_parts!(acs::ACSet, type, parts)
  issorted(parts) || error("Parts to be removed must be in sorted order")
  for part in Iterators.reverse(parts)
    rem_part!(acs, type, part)
  end
end

@inline function rem_parts!(acs::ACSet, type, ::Colon)
  rem_parts!(acs, type, parts(acs, type))
end

""" Remove parts and all parts incident to them, recursively.

The parts may be supplied in any order and may include duplicates.

Cf. [`rem_parts!`](@ref), which is not recursive.
"""
function cascading_rem_parts! end

""" Copy parts from a C-set to a C′-set.

The selected parts must belong to both schemas. All subparts common to the
selected parts, including data attributes, are preserved. Thus, if the selected
parts form a sub-C-set, then the whole sub-C-set is preserved. On the other
hand, if the selected parts do *not* form a sub-C-set, then some copied parts
will have undefined subparts.

TODO: handle colons
"""
function copy_parts! end

copy_parts!(to::ACSet, from::ACSet , obs::Tuple) =
  copy_parts!(to, from, NamedTuple{obs}((:) for ob in obs))

copy_parts!(to::ACSet, from::ACSet; kw...) = copy_parts!(to, from, (;kw...))

""" Copy parts from a C-set to a C′-set, ignoring all non-data subparts.

The selected parts must belong to both schemas. Attributes common to both
schemas are also copied, but no other subparts are copied.

See also: [`copy_parts!`](@ref).
"""
function copy_parts_only! end

function disjoint_union(acs1, acs2)
  acs = copy(acs1)
  copy_parts!(acs, acs2)
  acs
end

Base.isempty(X::ACSet) = all(o->nparts(X,o)==0, types(acset_schema(X)))

"""
Get a named tuple of Tables.jl-compatible tables from an acset
"""
function tables end

"""
Garbage collect in an acset.

For some choices of [`PartsType`](@ref), this function is a no-op.
"""
function gc! end

"""
Get a nullary callable which constructs an (empty) ACSet of the same type
"""
function constructor end

"""
Given a hom, find which parts in its domain are undefined.
"""
function undefined_subparts end

"""
Get the type used to store parts IDs.
"""
parts_type(::ACSet{PT}) where {PT} = PT

# Pretty printing
#################

""" Display an acset using PrettyTables.jl.

This works for any acset that implements [`tables`](@ref).
"""
function pretty_tables(io::IO, acs::ACSet; tables=nothing, kw...)
  options = merge(default_pretty_table_options, (; kw...))
  all_tables = ACSetInterface.tables(acs)
  table_names = isnothing(tables) ? keys(all_tables) : tables
  for name in table_names
    table = all_tables[name]

    # By convention, omit trivial tables with no columns.
    isempty(Tables.columnnames(table)) && continue

    # By necessity, omit tables with no rows. PrettyTables will not print them.
    Tables.rowcount(table) == 0 && continue
    pretty_table(io, table; row_label_column_title=string(name),
                 row_labels=collect(parts(acs,name)), options...)
  end
end

pretty_tables(acs::ACSet; kw...) = pretty_tables(stdout, acs; kw...)

const default_pretty_table_options = (
  show_subheader = false,
  show_row_number = false,
)

function Base.show(io::IO, ::MIME"text/plain", acs::T) where T <: ACSet
  print(io, acset_name(acs))
  print(io, " {")
  join(io, ["$(ob):$(nparts(acs,ob))" for ob in types(acset_schema(acs))], ", ")
  println(io, "}")
  pretty_tables(io, acs)
end

function Base.show(io::IO, ::MIME"text/html", acs::T) where T <: ACSet
  println(io, "<div class=\"c-set\">")
  print(io, "<span class=\"c-set-summary\">")
  print(io, acset_name(acs))
  print(io, " {")
  join(io, ["$(ob):$(nparts(acs,ob))" for ob in types(acset_schema(acs))], ", ")
  println(io, "}</span>")
  pretty_tables(io, acs, backend=Val(:html), standalone=false)
  println(io, "</div>")
end

collect_nonvector(v::AbstractVector) = v
collect_nonvector(v) = collect(v)

# Acset macro
#############

"""
This provides a shorthand for constructing an acset by giving its parts and
subparts

Usage:

@acset WeightedGraph{String} begin
  V = 2
  E = 1
  src = [1]
  tgt = [2]
  weight = ["fig"]
end
"""
macro acset(head, body)
  tuplized_body = @match body begin
    Expr(:block, lines...) => begin
      params = []
      map(lines) do line
        @match line begin
          Expr(:(=), x, y) => push!(params, Expr(:kw, x, y))
          _ => nothing
        end
      end
      Expr(:tuple, Expr(:parameters, params...))
    end
    _ => error("expected block")
  end
  esc(quote
    $(GlobalRef(ACSetInterface, :make_acset))($head, $tuplized_body)
  end)
end

function make_acset end


"""
Remove all AttrType parts that are not in the image of any of the attributes.
"""
function rem_free_vars!(acs::ACSet)
  for k in attrtypes(acset_schema(acs))
    rem_free_vars!(acs, k)
  end
end

rem_free_vars!(X::ACSet, a::Symbol) = rem_parts!(X, a, filter(parts(X,a)) do p
  all(f->isempty(incident(X, AttrVar(p), f)),
      attrs(acset_schema(X); to=a, just_names=true))
end)


end
