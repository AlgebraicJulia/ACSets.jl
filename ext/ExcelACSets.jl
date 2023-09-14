""" Read acsets from Microsoft Excel files.
"""
module ExcelACSets

import Tables, XLSX
using ACSets

# Excel spec
############

const AbstractMap = Union{AbstractDict,NamedTuple}

@kwdef struct ExcelTableSpec
  sheet::Union{AbstractString,Integer,Missing} = missing
  columns::AbstractMap = (;)
  primary_key::Union{Symbol,Missing} = missing
end

@kwdef struct ExcelSpec
  tables::AbstractDict{Symbol,ExcelTableSpec} = Dict{Symbol,ExcelTableSpec}()
end

function ExcelSpec(schema::Schema; tables::AbstractMap=(;), kw...)
  table_specs = Dict(ob => ExcelTableSpec(; get(tables, ob, (;))...)
                     for ob in objects(schema))
  ExcelSpec(; tables=table_specs, kw...)
end

# Read from spec
################

""" Read acset from an Excel (.xlsx) file.
"""
function ACSets.read_xlsx_acset(source::Union{AbstractString,IO}, cons; kw...)
  read_acset(XLSX.readxlsx(source), cons; kw...)
end

# TODO: Define and export generic functions `read_acset` and `read_acset!`.
function read_acset(xf::XLSX.XLSXFile, cons; kw...)
  read_acset!(xf, cons(); kw...)
end

function read_acset!(xf::XLSX.XLSXFile, acs::ACSet; kw...)
  # Read table for each object.
  schema = acset_schema(acs)
  spec = ExcelSpec(schema; kw...)
  tables = read_tables(xf, schema, spec)

  # Crate parts of each type.
  parts = Dict(ob => add_parts!(acs, ob, Tables.rowcount(table))
               for (ob, table) in pairs(tables))

  # Set attribute data, which will include any primary keys.
  for ob in objects(schema)
    table_spec = spec.tables[ob]
    columns = Tables.columns(tables[ob])
    for attr in attrs(schema, from=ob, just_names=true)
      column_name = get(table_spec.columns, attr, attr)
      column = Tables.getcolumn(columns, column_name)
      set_subpart!(acs, parts[ob], attr, column)
    end
  end

  # Set foreign key data.
  for ob in objects(schema)
    table_spec = spec.tables[ob]
    columns = Tables.columns(tables[ob])
    for (hom, _, tgt) in homs(schema, from=ob)
      pk = spec.tables[tgt].primary_key
      if ismissing(pk)
        @warn "Skipping foreign key $hom since primary key not set for $tgt"
        continue
      end
      column_name = get(table_spec.columns, hom, hom)
      column = Tables.getcolumn(columns, column_name)
      set_subpart!(acs, parts[ob], hom,
                   map(only, incident(acs, column, pk)))
    end
  end

  acs
end

function read_tables(xf::XLSX.XLSXFile, schema::Schema, spec::ExcelSpec)
  Iterators.map(objects(schema)) do ob
    table_spec = spec.tables[ob]
    sheet_name = coalesce(table_spec.sheet, string(ob))
    sheet = XLSX.getsheet(xf, sheet_name)
    table = XLSX.gettable(sheet)
    ob => table
  end |> Dict
end

end
