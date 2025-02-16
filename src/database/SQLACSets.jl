module SQLACSets

using ..ACSetInterface
using ..DenseACSets: StructACSet, SimpleACSet

using MLStyle
using DataFrames
using DBInterface

flatify(xs::AbstractVector) = Iterators.flatten(xs) |> collect

include("acsets_interface.jl")

function select end
function create end
function insert end
function delete end
function alter end

@data SQLTerms begin
    Values(::AbstractVector)
    Insert(table::Symbol, values::Values)
    Select(cols::Union{Vector{Symbol}, Nothing}, table::Symbol)
    Alter()
    Create()
end
export SQLTerms

function Insert(table, vect::AbstractVector)
    Insert(table, Values(vect))
end

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

# DB specific
tosql(::Type{<:Real}) = "REAL"
tosql(::Type{<:AbstractString}) = "TEXT"
tosql(::Type{<:Symbol}) = "TEXT"
tosql(::Type{<:Integer}) = "INTEGER"
tosql(T::DataType) = error("$T could not be less supported")
#
tosql(::Nothing) = "NULL"
tosql(s::Symbol) = string(s)
tosql(x) = x

end
