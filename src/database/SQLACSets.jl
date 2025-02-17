module SQLACSets

using ..ACSetInterface
using ..DenseACSets: StructACSet, SimpleACSet
using ..Schemas

using MLStyle
using DataFrames
using DBInterface

flatify(xs::AbstractVector) = Iterators.flatten(xs) |> collect

include("sqlmethods.jl")
include("acsets_interface.jl")


end
