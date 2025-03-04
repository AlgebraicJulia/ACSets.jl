module Query

using ..ACSetInterface, ..Schemas
using ..DenseACSets: @acset_type
using MLStyle
using DataFrames: DataFrame, nrow

to_name(x) = @match x begin
    ::Pair => Symbol("$(x.second)$(x.first)")
    ::Val{T} where T => Symbol("Val_$T")
    _ => x
end

# for iterating over something that might be a singleton
function iterable(x::T) where T
    S = T <: AbstractVector ? eltype(T) : T
    [S[]; x]
end

# TODO upstream?
function select_part end
export select_part

function select_part(acset::ACSet, select::Symbol, idx=[]; schema=acset_schema(acset))
    val = select ∈ objects(schema) ? parts(acset, select) : subpart(acset, select)
    !isempty(idx) ? val[idx] : val
end

function select_part(acset, selects::Vector{Symbol}, idx=[]; kwargs...)
    zip(select_part.(Ref(acset), selects; kwargs...)...)
end


abstract type AbstractCondition end
export AbstractCondition

struct WhereCondition <: AbstractCondition
    lhs
    op::Function
    rhs
end
export WhereCondition

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
export SQLACSetNode

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

function From end
export From

From(table::Symbol) = SQLACSetNode(table; cond=AbstractCondition[], select=[])

function From(tablecol::Pair{Symbol, Symbol})
    SQLACSetNode(tablecol.first; cond=AbstractCondition[], select=[tablecol.second])
end

# TODO looks like we don't do this anymore. From is singleton for the time being.
function From(sql::SQLACSetNode; table::Symbol)
    sql.from = [sql.from; table]
    sql
end

function Where end
export Where

Where(lhs, op::Function, rhs) = WhereCondition(lhs, op, rhs)
Where(lhs::Symbol, rhs::Function) = Where(lhs, |>, rhs)
Where(lhs::Symbol, rhs) = Where(lhs, ∈, rhs)
Where(lhs, rhs::Function) = Where(lhs, |>, rhs)

function Select end
export Select

function Select(sql::SQLACSetNode, columns::Vector)
    push!(sql.select, columns...)
    sql
end

function Select(columns...)
    sql -> Select(sql, [columns...])
end

function process_wheres end
export process_wheres

function process_wheres(conds::Vector{<:AbstractCondition}, acset)
	isempty(conds) && return nothing
    process_where.(conds, Ref(acset))
end

function process_where(cond::WhereCondition, acset::ACSet)
    values = select_part(acset, cond.lhs)
    @match cond.rhs begin
        # if SQLACSetNode specifies a Select, then it'll return an array. 
        # XXX This hare-brained shim assumes that there will only be one select in a subquery. 
        ::SQLACSetNode => map(x -> cond.op(x, cond.rhs(acset)[1].second), values)
        ::Vector => map(x -> cond.op(iterable(x)..., cond.rhs), values)
        ::Function => map(x -> x isa Union{Tuple, AbstractVector} ? cond.rhs(x...) : cond.rhs(x), values)
        _ => map(x -> cond.op(iterable(x)..., [cond.rhs]), values)
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
    !isempty(q.select) || return q.from => result
    map(q.select) do select
        to_name(select) => @match select begin
            ::Val{T} where T => [T for _ in 1:length(result)]
            ::Symbol => select_part(acset, select, result)
            ::Pair{Symbol, <:Function} => select_part(acset, select.first, result) .|> select.second
        end
    end
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
q = From(:Tri) |> Where(:∂e0, x -> x < 10)
```
A query with a function on multiple columns
```
q = From(:Tri) |> Where([:∂e0, :∂e1, :∂e2], (x,y,z) -> StatsBase.var([x,y,z]) < 2)
```
"""
function (q::SQLACSetNode)(acset::ACSet; formatter=nothing)
    idx = process_wheres(q.cond, acset)
    result = isnothing(idx) ? parts(acset, q.from) : parts(acset, q.from)[first(idx)]
    isempty(result) && return []
    selected = process_select(q, acset, result)
    output = @match formatter begin
        nothing     => selected
        :df         => DataFrame(build_nt(q, selected))
        :named      => build_nt(q, selected)
        ::Function  => formatter(q, acset, selected)
        _           => nothing
    end
    output
end

function build_nt(q::SQLACSetNode, selected)
    names = isempty(q.select) ? [q.from] : to_name.(q.select)
    NamedTuple{Tuple(names)}(getfield.(iterable(selected), :second))
end

end
