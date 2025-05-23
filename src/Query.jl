module Query

export From, Where, Select, AbstractQueryFormatter, SimpleQueryFormatter, NamedQueryFormatter, DFQueryFormatter

using ..ACSetInterface, ..Schemas
using MLStyle
using StructEquality

to_name(x) = x
"""
E.g., suppose we want to replace the values in column `name` with Boolean values for whether they equal YourName. Our function `:name => !=(:YourName)` will be represented as `Symbol("!=(YourName)")`
"""
function to_name(p::Pair{Symbol, Base.Fix1{S, T}}) where {S, T}
    Symbol("$(Symbol(S.instance))($(p.second.x))")
end
to_name(x::Pair) = Symbol("$(x.second)$(x.first)")
to_name(x::Val{T}) where T = Symbol("Val_$T")

"""  iterable(x::T)

This function lifts a singleton into an iterator. This prevents branches in the control flow.
```
x=1; @assert [k for k ∈ iterable(x)] == [1]
x=[1,2,3]; @assert [k for k ∈ iterable(x)] == [1,2,3]
x=[[:a, :b]]; @assert [k for k ∈ iterable(x)] == [[:a, :b]]
```
Moreover, given `foo(x) = x`,
```
splat(foo)(:a) # breaks
splat(foo)(iterable(:a)) # works
```
"""
function iterable(x::T) where T
  S = T <: AbstractVector ? eltype(T) : T
  [S[]; x]
end

function get_rows(acset::ACSet, select::Symbol, idx=Colon(); schema=acset_schema(acset))
  val = select ∈ objects(schema) ? parts(acset, select) : subpart(acset, select)
  val[idx]
end

function get_rows(acset, selects::Vector{Symbol}, idx=Colon(); kwargs...)
  zip(get_rows.(Ref(acset), selects, idx; kwargs...)...)
end

"""  AbstractCondition 

A common type for [`WhereCondition`](@ref), [`AndWhere`](@ref), and [`OrWhere`](@ref).
"""
abstract type AbstractCondition end

"""  WhereCondition <: [`AbstractCondition`](@ref)

A struct containing enough information to specifying a WHERE clause in SQL.
"""
@struct_hash_equal struct WhereCondition <: AbstractCondition
  lhs::Union{Symbol, Vector{Symbol}}
  op::Function
  rhs
end

"""  AndWhere <: [`AbstractCondition`](@ref)

A struct containing a list of conditions which are all expected to hold true.
"""
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

"""  OrWhere <: [`AbstractCondition`](@ref)

A struct containing a list of conditions which any are expected to hold true.
"""
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

""" ACSetSQLNode

This data structure stores the necessary information for specifying a query on an ACSet. It is not intended to be instantiated outright; instead, the [`From`](@ref) function will return a ACSetSQLNode with the expectation that composition with [`Where`](@ref) and [`Select`](@ref) functions will mutate this node with more information.
"""
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
  n.cond &= a
end

function Base.:|(n::ACSetSQLNode, a::AbstractCondition)
  n.cond |= a
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

"""  process_where
Iterates over the values specified by the left-hand side of the [`WhereCondition`](@ref) and applies a conditional.

*Note:* If the value is a tuple and the right-hand side is `::Function`, then the value will be splatted into the argument of the function.
"""
function process_where(cond::WhereCondition, acset::ACSet)
  values = get_rows(acset, cond.lhs)
  map(values) do value
    @match (value, cond.rhs) begin
      # TODO Find a more principled away of extracting the ACSetSQLNode
      (_, ::ACSetSQLNode)   => cond.op(value, cond.rhs(acset)[1].second)
      (::Tuple, ::Function) => cond.rhs(value...) # tuples are splatted
      (_, ::Function)       => cond.rhs(value)
      (_, ::Vector)         => cond.op(iterable(value)..., cond.rhs)
      _                     => cond.op(iterable(value)..., [cond.rhs])
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
      ::Symbol                   => get_rows(acset, select, result)
      ::Pair{Symbol, <:Function} => get_rows(acset, select.first, result) .|> select.second
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

The callable method of this fieldless struct consumes an ACSetSQLNode, an ACSet, and the result selection and returns a DataFrame.

This method requires the `DataFrames` package.

See also [`AbstractQueryFormatter`](@ref)
"""
struct DFQueryFormatter <: AbstractQueryFormatter end

(qf::SimpleQueryFormatter)(q, a, s) = s
(qf::NamedQueryFormatter)(q, a, s) = build_nt(q, s)
# DFQueryFormatter is defined in an extension.

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
