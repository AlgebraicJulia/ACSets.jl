""" Read acsets from Microsoft Excel files.
"""
module ExcelACSets
export read_xlsx_acset

using ...ACSetInterface, ...Schemas, ..ACSetSerialization

# Data types
############

const AbstractMap = Union{AbstractDict,NamedTuple}

@kwdef struct ExcelTableSpec
  sheet::Union{AbstractString,Integer,Missing} = missing
  primary_key::Union{Symbol,Missing} = missing
  row_range::Union{AbstractUnitRange,Integer,Missing} = missing
  column_range::Union{AbstractString,Missing} = missing
  column_labels::AbstractMap = (;)
  convert::AbstractMap = (;)
end

@kwdef struct ExcelSpec
  tables::AbstractDict{Symbol,ExcelTableSpec} = Dict{Symbol,ExcelTableSpec}()
end

function ExcelSpec(schema::Schema; tables::AbstractMap=(;), kw...)
  table_specs = Dict(ob => ExcelTableSpec(; get(tables, ob, (;))...)
                     for ob in objects(schema))
  ExcelSpec(; tables=table_specs, kw...)
end

# Interface
###########

""" Read acset from an Excel (.xlsx) file.

This is a convenience function that loads the Excel file and then calls
[`read_acset`](@ref). To use this function, the package XLSX.jl must be
installed and imported.

# Arguments
- `cons`: constructor for acset, e.g., the acset type for struct acsets
- `source`: filename or IO stream from which to read Excel file
- `tables=(;)`: dictionary or named tuple mapping object names in acset schema
  to Excel table specifications
"""
function read_xlsx_acset(cons, source::Union{AbstractString,IO}; kw...)
  read_acset(cons, read_xlsx(source); kw...)
end

function read_xlsx end

end
