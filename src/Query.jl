module Query

export From, Where, Select, SimpleQueryFormatter, NamedQueryFormatter, DFQueryFormatter

using ..ACSetInterface, ..Schemas
using ..DenseACSets: @acset_type
using MLStyle
using DataFrames: DataFrame, nrow

to_name(x) = x
to_name(x::Pair) = Symbol("$(x.second)$(x.first)")
to_name(x::Val{T}) where T = Symbol("Val_$T")

# for iterating over something that might be a singleton
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

struct WhereCondition <: AbstractCondition
    lhs
    op::Function
    rhs
end

struct AndWhere <: AbstractCondition
    conds::Vector{<:AbstractCondition}
    AndWhere(conds::Vector{<:AbstractCondition}) = new(conds)
    AndWhere(a::AndWhere, b) = AndWhere(a.conds, b)
    AndWhere(a, b::AndWhere) = AndWhere(a, b.conds)
    AndWhere(a, b) = new([a; b])
end

function Base.:&(a::S, b::T) where {T<:AbstractCondition, S<:AbstractCondition}
    AndWhere(a, b)
end

struct OrWhere <: AbstractCondition
    conds::Vector{<:AbstractCondition}
    OrWhere(conds::Vector{<:AbstractCondition}) = OrWhere(conds)
    OrWhere(a::OrWhere, b) = OrWhere(a.conds, b)
    OrWhere(a, b::OrWhere) = OrWhere(a, b.conds)
    OrWhere(a, b) = new([a; b])
end

function Base.:|(a::S, b::T) where {T<:AbstractCondition, S<:AbstractCondition}
    OrWhere(a, b)
end

mutable struct SQLACSetNode
    from::Symbol
    cond::Union{Vector{<:AbstractCondition}, Nothing}
    select
    SQLACSetNode(from::Symbol; cond=nothing, select=nothing) = new(from, cond, select)
end

function (w::WhereCondition)(node::SQLACSetNode)
    push!(node.cond, AndWhere([w]))
    node
end

function (ac::AbstractCondition)(node::SQLACSetNode)
    push!(node.cond, ac)
    node
end

function Base.:&(n::SQLACSetNode, a::AbstractCondition)
    n.cond = n.cond & a
    n
end

function Base.:|(n::SQLACSetNode, a::AbstractCondition)
    n.cond = n.cond | a
    n
end

From(table::Symbol) = SQLACSetNode(table; cond=AbstractCondition[], select=[])

function From(tablecol::Pair{Symbol, Symbol})
    SQLACSetNode(tablecol.first; cond=AbstractCondition[], select=[tablecol.second])
end

Where(lhs, op::Function, rhs) = WhereCondition(lhs, op, rhs)
Where(lhs::Symbol, rhs::Function) = Where(lhs, |>, rhs)
Where(lhs::Symbol, rhs) = Where(lhs, ∈, rhs)
Where(lhs, rhs::Function) = Where(lhs, |>, rhs)

function Select(sql::SQLACSetNode, columns::Vector)
    push!(sql.select, columns...)
    sql
end

function Select(columns...)
    sql -> Select(sql, [columns...])
end

function process_wheres(conds::Vector{<:AbstractCondition}, acset)
    isempty(conds) && return [Colon()]
    process_where.(conds, Ref(acset))
end

function process_where(cond::WhereCondition, acset::ACSet)
    values = get(acset, cond.lhs)
    map(values) do value
        @match (value, cond.rhs) begin
            (_, ::SQLACSetNode)            => cond.op(value, cond.rhs(acset)[1].second)
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

function process_select(q::SQLACSetNode, acset::ACSet, result::AbstractVector)
    isempty(q.select) && return q.from => result
    map(q.select) do select
        to_name(select) => @match select begin
            ::Val{T} where T           => [T for _ in eachindex(result)]
            ::Symbol                   => get(acset, select, result)
            ::Pair{Symbol, <:Function} => get(acset, select.first, result) .|> select.second
        end
    end
end

abstract type AbstractQueryFormatter end

struct SimpleQueryFormatter <: AbstractQueryFormatter end
struct NamedQueryFormatter <: AbstractQueryFormatter end
struct DFQueryFormatter <: AbstractQueryFormatter end

(qf::SimpleQueryFormatter)(q, a, s) = s
(qf::NamedQueryFormatter)(q, a, s) = build_nt(q, s)
(qf::DFQueryFormatter)(q, a, s) = DataFrame(build_nt(q, s))

function build_nt(q::SQLACSetNode, selected)
    names = isempty(q.select) ? [q.from] : to_name.(q.select)
    NamedTuple{Tuple(names)}(getfield.(iterable(selected), :second))
end

"""
A query with no select overly-specified:
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
function process_query(q::SQLACSetNode, acset::ACSet; formatter::AbstractQueryFormatter)
    idx = process_wheres(q.cond, acset)
    result = parts(acset, q.from)[only(idx)]
    isempty(result) && return []
    selected = process_select(q, acset, result)
    formatter(q, acset, selected)
end

(q::SQLACSetNode)(acset::ACSet; formatter::AbstractQueryFormatter=SimpleQueryFormatter()) =
    process_query(q, acset; formatter)

end
