module Query

export From, Where, Select, AbstractQueryFormatter, SimpleQueryFormatter, NamedQueryFormatter, DFQueryFormatter

using ..ACSetInterface, ..Schemas
using MLStyle
using DataFrames: DataFrame
using StructEquality

to_name(x) = x
to_name(x::Pair) = Symbol("$(x.second)$(x.first)")
to_name(x::Val{T}) where T = Symbol("Val_$T")

""" This function lifts a singleton into an iterator. This prevents branches in the control flow.
```
a=1; @test [k for k ∈ iterable(a)] == [1]
a=[1,2,3]; @test [k for k ∈ iterable(a)] == [1,2,3]
```
"""
function iterable(x::T) where T
  S = T <: AbstractVector ? eltype(T) : T
  [S[]; x]
end

function Base.get(acset::ACSet, select::Symbol, idx=Colon(); schema=acset_schema(acset))
  val = select ∈ objects(schema) ? parts(acset, select) : subpart(acset, select)
  val[idx]
end

function Base.get(acset, selects::Vector{Symbol}, idx=Colon(); kwargs...)
  zip(get.(Ref(acset), selects, idx; kwargs...)...)
end

abstract type AbstractCondition end

@struct_hash_equal struct WhereCondition <: AbstractCondition
  lhs
  op::Function
  rhs
end

@struct_hash_equal struct AndWhere <: AbstractCondition
  conds::Vector{<:AbstractCondition}
  AndWhere(conds::Vector{<:AbstractCondition}) = new(conds)
  AndWhere(a, b) = new([a; b])
end

AndWhere(a::AndWhere, b) = AndWhere(a.conds, b)
AndWhere(a, b::AndWhere) = AndWhere(a, b.conds)

function Base.:&(a::S, b::T) where {T<:AbstractCondition, S<:AbstractCondition}
  AndWhere(a, b)
end

@struct_hash_equal struct OrWhere <: AbstractCondition
  conds::Vector{<:AbstractCondition}
  OrWhere(conds::Vector{<:AbstractCondition}) = new(conds)
  OrWhere(a, b) = new([a; b])
end

OrWhere(a::OrWhere, b) = OrWhere(a.conds, b)
OrWhere(a, b::OrWhere) = OrWhere(a, b.conds)

function Base.:|(a::S, b::T) where {T<:AbstractCondition, S<:AbstractCondition}
  OrWhere(a, b)
end

""" """
mutable struct ACSetSQLNode
  const from::Symbol
  cond::Union{Vector{<:AbstractCondition}, Nothing}
  select
  ACSetSQLNode(from::Symbol; cond=nothing, select=nothing) = new(from, cond, select)
end

function (w::WhereCondition)(node::ACSetSQLNode)
  push!(node.cond, AndWhere([w]))
  node
end

function (ac::AbstractCondition)(node::ACSetSQLNode)
  push!(node.cond, ac)
  node
end

function Base.:&(n::ACSetSQLNode, a::AbstractCondition)
  n.cond = n.cond & a
  n
end

function Base.:|(n::ACSetSQLNode, a::AbstractCondition)
  n.cond = n.cond | a
  n
end

function From(table::Symbol; select=nothing)
  select = isnothing(select) ? [] : [select]
  ACSetSQLNode(table; cond=AbstractCondition[], select=select)
end

function From(tablecol::Pair{Symbol, Symbol})
  ACSetSQLNode(tablecol.first; cond=AbstractCondition[], select=[tablecol.second])
end

Where(lhs, op::Function, rhs) = WhereCondition(lhs, op, rhs)
Where(lhs::Symbol, rhs::Function) = Where(lhs, |>, rhs)
Where(lhs::Symbol, rhs) = Where(lhs, ∈, rhs)
Where(lhs, rhs::Function) = Where(lhs, |>, rhs)

function Select(sql::ACSetSQLNode, columns::Vector)
  push!(sql.select, columns...)
  sql
end

function Select(columns...)
  sql -> Select(sql, Any[columns...])
end

function process_wheres(conds::Vector{<:AbstractCondition}, acset)
  isempty(conds) && return [Colon()]
  process_where.(conds, Ref(acset))
end

function process_where(cond::WhereCondition, acset::ACSet)
  values = get(acset, cond.lhs)
  map(values) do value
    @match (value, cond.rhs) begin
      # TODO
      (_, ::ACSetSQLNode)            => cond.op(value, cond.rhs(acset)[1].second)
      (::Tuple,          ::Function) => cond.rhs(value...)
      (::AbstractVector, ::Function) => cond.rhs(value...)
      (_, ::Function)                => cond.rhs(value)
      (_, ::Vector)                  => cond.op(iterable(value)..., cond.rhs)
      _                              => cond.op(iterable(value)..., [cond.rhs])
    end
  end
end

function process_where(w::OrWhere, acset::ACSet)
  isempty(w.conds) && return nothing
  reduce((x,y) -> x .| y, process_wheres(w.conds, acset))
end

function process_where(w::AndWhere, acset::ACSet)
  isempty(w.conds) && return nothing
  reduce((x,y) -> x .& y, process_wheres(w.conds, acset))
end

function process_select(q::ACSetSQLNode, acset::ACSet, result::AbstractVector)
  isempty(q.select) && return q.from => result
  map(q.select) do select
    to_name(select) => @match select begin
      ::Val{T} where T           => [T for _ in eachindex(result)]
      ::Symbol                   => get(acset, select, result)
      ::Pair{Symbol, <:Function} => get(acset, select.first, result) .|> select.second
    end
  end
end

"""  AbstractQueryFormatter

Concrete, fieldless structs which are subtypes of AbstractQueryFormatter should have a callable method which ingests a ACSetSQLNode, an ACSet, and the `selected`, the result of the query subject to the given ACSetSQL Select statements.

For example, the standard format for ACSetSQL output is

```
struct SimpleQueryFormatter <: AbstractQueryFormatter end

(qf::SimpleQueryFormatter)(q, a, s) = s

```

See also [`SimpleQueryFormatter`](@ref), [`NamedQueryFormatter`](@ref), [`DFQueryFormatter`](@ref).
"""
abstract type AbstractQueryFormatter end

"""  SimpleQueryFormatter <: AbstractQueryFormatter

The callable method of this fieldless struct consumes an ACSetSQLNode, an ACSet, and the result selection and returns just the result selection.

See also [`AbstractQueryFormatter`](@ref)
"""
struct SimpleQueryFormatter <: AbstractQueryFormatter end

"""  NamedQueryFormatter <: AbstractQueryFormatter

The callable method of this fieldless struct consumes an ACSetSQLNode, an ACSet, and the result selection and returns a named tuple of the selection columns and their values.

See also [`AbstractQueryFormatter`](@ref)
"""
struct NamedQueryFormatter <: AbstractQueryFormatter end

"""  DFQueryFormatter <: AbstractQueryFormatter

The callable method of this fieldless struct consumes an ACSetSQLNode, an ACSet, and the result selection and returns a data frame.

See also [`AbstractQueryFormatter`](@ref)
"""
struct DFQueryFormatter <: AbstractQueryFormatter end

(qf::SimpleQueryFormatter)(q, a, s) = s
(qf::NamedQueryFormatter)(q, a, s) = build_nt(q, s)
(qf::DFQueryFormatter)(q, a, s) = DataFrame(build_nt(q, s))

function build_nt(q::ACSetSQLNode, selected)
  names = isempty(q.select) ? [q.from] : to_name.(q.select)
  NamedTuple{Tuple(names)}(getfield.(iterable(selected), :second))
end

"""
A query with no Select overtly-specified:
```
q = From(:Tri)
```
A query with the select specified. Since the part name is `Tri`, we get the part number, analogous to getting the primary key.
```
q = From(:Tri) |> Select(:Tri)
```
We can also select with Julia's Pair data structure. Here we retrieve the column `∂e0`.
```
q = From(:Tri => :∂e0)
```
A query with a `Where` statement.
```
q = From(:Tri) |> Where(:∂e0, <(10))
```
A query with a function on multiple columns
```
q = From(:Tri) |> Where([:∂e0, :∂e1, :∂e2], (x,y,z) -> StatsBase.var([x,y,z]) < 2)
```
"""
function process_query(q::ACSetSQLNode, acset::ACSet; formatter::AbstractQueryFormatter)
  idx = process_wheres(q.cond, acset)
  result = parts(acset, q.from)[only(idx)]
  isempty(result) && return []
  selected = process_select(q, acset, result)
  formatter(q, acset, selected)
end

(q::ACSetSQLNode)(acset::ACSet; formatter::AbstractQueryFormatter=SimpleQueryFormatter()) =
  process_query(q, acset; formatter)

end
