module ACSetSerialization
export read_acset, read_acset!

using Reexport

# Interface
###########

""" Read/deserialize an acset from an external source.
"""
function read_acset(source, cons; kw...)
  read_acset!(source, cons(); kw...)
end

""" Mutating variant of [`read_acset`](@ref).
"""
function read_acset! end

# Serializers
#############

include("JSONACSets.jl")
include("ExcelACSets.jl")

@reexport using .JSONACSets
@reexport using .ExcelACSets

end
