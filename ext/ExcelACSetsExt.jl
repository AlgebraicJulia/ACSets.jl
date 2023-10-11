module ExcelACSetsExt

import Tables, XLSX

using ACSets
using ACSets.ACSetSerialization.ExcelACSets: ExcelSpec, ExcelTableSpec


function ExcelACSets.read_xlsx(source::Union{AbstractString,IO})
  XLSX.readxlsx(source)
end

function ACSets.read_acset!(acs::ACSet, xf::XLSX.XLSXFile; kw...)
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
      column_name = get(table_spec.column_labels, attr, attr)
      column = Tables.getcolumn(columns, column_name)
      converter = get(table_spec.convert, attr, nothing)
      data = isnothing(converter) ? column : map(converter, column)
      set_subpart!(acs, parts[ob], attr, data)
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
      column_name = get(table_spec.column_labels, hom, hom)
      column = Tables.getcolumn(columns, column_name)
      set_subpart!(acs, parts[ob], hom,
                   map(only, incident(acs, column, pk)))
    end
  end

  acs
end

""" Read table from Excel file for each object in acset schema.
"""
function read_tables(xf::XLSX.XLSXFile, schema::Schema, spec::ExcelSpec)
  Iterators.map(objects(schema)) do ob
    table_spec = spec.tables[ob]
    sheet_name = coalesce(table_spec.sheet, string(ob))
    sheet = XLSX.getsheet(xf, sheet_name)

    row_range, col_range = table_spec.row_range, table_spec.column_range
    row_range isa AbstractRange && error("Specifying end row is not supported")
    first_row = coalesce(row_range, nothing)

    table = XLSX.gettable(
      (ismissing(col_range) ? (sheet,) : (sheet, col_range))...;
      first_row=first_row,
    )
    ob => table
  end |> Dict
end

end
