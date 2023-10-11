""" Serializing and deserializing acsets to/from different formats.
"""
module ACSetSerialization
export read_acset, read_acset!

using Reexport

# Interface
###########

""" Read/deserialize an acset from an external source.

Supported source types include:

- `AbstractDict`: assumed to be JSON data
- `XLSX.XLSXFile`: Microsoft Excel file (requires XLSX.jl)

# Arguments
- `cons`: constructor for acset, e.g., the type of a struct acset
- `source`: source to read from
"""
function read_acset(cons, source; kw...)
  read_acset!(cons(), source; kw...)
end

""" Mutating variant of [`read_acset`](@ref).

# Arguments
- `acset`: acset to write to
- `source`: source to read from
"""
function read_acset! end

# Serializers
#############

include("JSONACSets.jl")
include("ExcelACSets.jl")

@reexport using .JSONACSets
@reexport using .ExcelACSets

end
