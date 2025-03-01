module Query

using ..ACSetInterface, ..Schemas

using MLStyle

function acset_select(acset::ACSet, select::Symbol; schema::Any=acset_schema(acset))
    if select ∈ objects(schema)
        parts(acset, select)
    else
        subpart(acset, select)
    end
end

function acset_select(acset, selects::Vector{Symbol}; kwargs...)
    zip(acset_select.(Ref(acset), selects; kwargs...)...)
end

abstract type AbstractCondition end
export AbstractCondition

struct WhereCondition <: AbstractCondition
    lhs::Any
    op::Function
    rhs::Any
end
export WhereCondition

abstract type HasConds <: AbstractCondition end

@as_record struct AndWhere <: HasConds
	conds::Vector{<:AbstractCondition}
    AndWhere(conds::Vector{<:AbstractCondition}) = new(conds)
    AndWhere(a::AndWhere, b) = AndWhere(a.conds, b)
    AndWhere(a, b::AndWhere) = AndWhere(a, b.conds)
    AndWhere(a, b) = new([a; b])
end

function Base.:&(a::S, b::T) where {T<:AbstractCondition, S<:AbstractCondition}
	AndWhere(a, b)
end

@as_record struct OrWhere <: HasConds
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
    select::Union{Symbol, Vector{Symbol}, Nothing}
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

# TODO handle nothing case
From(table::Symbol) = SQLACSetNode(table; cond=AbstractCondition[], select=Symbol[])

function From(tablecol::Pair{Symbol, Symbol})
    SQLACSetNode(tablecol.first; cond=AbstractCondition[], select=Symbol[tablecol.second])
end

# TODO looks like we don't do this anymore. From is singleton for the time being.
function From(sql::SQLACSetNode; table::Symbol)
    sql.from = [sql.from; table]
    sql
end
export From

function Where end
export Where

Where(lhs, op::Function, rhs::Any) = WhereCondition(lhs, op, rhs)
Where(lhs::Symbol, rhs::Function) = Where(lhs, |>, rhs)
Where(lhs::Symbol, rhs::Any) = Where(lhs, ∈, rhs)
Where(lhs, rhs::Function) = Where(lhs, |>, rhs)

function Select(sql::SQLACSetNode; columns::Union{Symbol, Vector{Symbol}})
    push!(sql.select, columns...)
    sql
end
export Select

function Select(cols::Union{Symbol, Vector{Symbol}})
    sql -> Select(sql; columns=[Symbol[];cols])
end

function process_wheres end
export process_wheres

function process_wheres(conds::Vector{<:AbstractCondition}, acset)
	isempty(conds) && return nothing
	process_where.(conds, Ref(acset))
end

function process_where(cond::WhereCondition, acset::ACSet)
    values = acset_select(acset, cond.lhs)
    @match cond.rhs begin
        ::SQLACSetNode => map(x -> cond.op(x..., cond.rhs(acset)), values)
        ::Vector => map(x -> cond.op(x..., cond.rhs), values)
        ::Function => map(x -> cond.rhs(x...), values)
        _ => map(x -> cond.op(x..., [cond.rhs]), values)
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

"""
A query with no select overly-specified:
```
q = From(:Summand)
```
A query with the select specified. `_id` is reserved for the part.
```
q = From(:Summand) |>
Select(:_id)
```
A query with two where statements. One Where uses another query
```
q = From(:Op1) |>
Where(:src, :∈, invasion[:res] ∪ invasion[:sum] ∪ infer_states(invasion)) |>
Where(:src, :∈, From(:Op1) |> Where(:op1, :∈, black_list) |> Select(:tgt))
```
"""
function (q::SQLACSetNode)(acset::ACSet)
    idx = process_wheres(q.cond, acset)
    result = isnothing(idx) ? parts(acset, q.from) : parts(acset, q.from)[first(idx)]
    isempty(result) && return []
    selected = @match q.select begin
        ::Nothing || Symbol[] => return result
        ::Symbol => subpart(acset, result, q.select)
        selects => map(selects) do select
            acset_select(acset, select)[result]
        end
    end
    collect(Iterators.flatten(selected))
end

end
