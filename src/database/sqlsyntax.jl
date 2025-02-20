function tostring end
export tostring

tostring(conn, nothing) = ""

# this typing ensures that named tuples have the same keys
struct Values{T}
    table::Union{Symbol, Nothing}
    vals::Vector{<:NamedTuple{T}}
end

Base.length(v::Values{T}) where T = length(v.vals)
Base.iterate(v::Values{T}, args...) where T = iterate(v.vals, args...)
Base.broadcast(f, v::Values{T}) where T = Values{T}(v.table, broadcast(f, v.vals))

columns(v::Values{T}) where T = T
export columns

@as_record struct WhereClause
    operator::Symbol
    clauses::Union{Pair{Symbol, <:Any}, Vector{<:WhereClause}}
end
export WhereClause

@kwdef struct SQLEquation
    lhs::Pair{Symbol, Symbol}
    rhs::Pair{Symbol, Symbol}
    op::Symbol = :(==)
end
export SQLEquation

SQLEquation(lhs, rhs) = SQLEquation(lhs=lhs, rhs=rhs)

# select expr from dual;
# SQLite supports SELECT * FROM A, B, C ON (A.x = B.y AND B.y = C.z)
# select * from t1 inner join

@data SQLSelectQuantity begin
    SelectAll() # default
    SelectDistinct()
    SelectDistinctRow()
    # Pair{Symbol, Symbol} is table.column relation
    SelectColumns(::Vector{Union{Symbol, Pair{Symbol, Symbol}}})
end
export SQLSelectQuantity, SelectAll, SelectDistinct, SelectDistinctRow, SelectColumns

SelectColumns(t::Union{Symbol, Pair{Symbol, Symbol}}) = SelectColumns([t])
SelectColumns(varargs...) = SelectColumns([varargs...])

@data SQLTerms begin
    Insert(table::Symbol, values::Values, wheres::Union{WhereClause, Nothing})
    Select(qty::SQLSelectQuantity, 
            from::Union{Symbol, Vector{Symbol}}, 
            on::Union{Vector{SQLEquation}, Nothing},
            wheres::Union{WhereClause, Nothing}) 
    # Select(table::Symbol, cols::Union{Vector{Symbol}, Nothing},
    #   wheres::Union{WhereClause, Nothing})
    Alter(table::Symbol, refdom::Symbol, refcodom::Symbol)
    Create(schema::BasicSchema{Symbol})
    Delete(table::Symbol, ids::Vector{Int})
end
export SQLTerms, Values, Insert, Select, Alter, Create, Delete

## Constructors

function Select(from::Union{Symbol, Vector{Symbol}}; 
        what::SQLSelectQuantity=SelectAll(),
        on::Union{Vector{SQLEquation}, Nothing}=nothing, 
        wheres::Union{WhereClause, Nothing}=nothing)
    Select(what, from, on, wheres)
end

function Alter(table::Symbol, arrow::Pair{Symbol, Symbol})
    Alter(table, arrow.first, arrow.second)
end

function Create(acset::SimpleACSet)
    Create(acset_schema(acset))
end

function Insert(table::Symbol, vs::Vector{<:NamedTuple{T}}, whereas::Union{WhereClause, Nothing}=nothing) where T
    Insert(table, Values(table, vs), wheres)
end

## SQL Term Operations

function Base.:+(v1::Values, v2::Values)
    Values(v1._1 ∪ v2._1)
end

function Base.:+(i1::Insert, i2::Insert)
    if i1.table == i2.table
        Insert(i1.table, i1.values + i2.values)
    else
        [i1, i2]
    end
end

