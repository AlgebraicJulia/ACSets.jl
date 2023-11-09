""" JSON serialization of acsets and acset schemas.
"""
module JSONACSets

import JSON3

export generate_json_acset, parse_json_acset, read_json_acset, write_json_acset,
  generate_json_acset_schema, parse_json_acset_schema,
  read_json_acset_schema, write_json_acset_schema, acset_schema_json_schema, write

using DataStructures: OrderedDict
import Pkg
import Tables

using ...ACSetInterface, ...Schemas, ...DenseACSets
using ...DenseACSets: attr_type
using ...ColumnImplementations: AttrVar # TODO: Move this.
import ..ACSetSerialization: read_acset!

# ACSet serialization
#####################

read_acset!(cons, source::AbstractDict) = parse_json_acset!(cons, source)

""" Generate JSON-able object representing an ACSet.

Inverse to [`parse_json_acset`](@ref).
"""
function generate_json_acset(X::ACSet)
  result = Iterators.map(pairs(tables(X))) do (ob, table)
    ob => map(parts(X, ob), Tables.rowtable(table)) do id, row
      merge((_id=id,), map(attr_to_json, row))
    end
  end |> OrderedDict{Symbol,Any}
  for attrtype in attrtypes(acset_schema(X))
    result[attrtype] = map(id -> (_id=id,), parts(X, attrtype))
  end
  return result
end

attr_to_json(var::AttrVar) = (_var = var.val,)
attr_to_json(val) = val

""" Parse JSON-able object or JSON string representing an ACSet.

Inverse to [`generate_json_acset`](@ref).
"""
parse_json_acset(cons, input::AbstractDict) =
  parse_json_acset!(cons(), input)
parse_json_acset(cons, input::AbstractString) =
  parse_json_acset(cons, JSON.parse(input))
parse_json_acset(acs::ACSet, input::AbstractDict) =
  parse_json_acset(constructor(acs), input)

function parse_json_acset!(out::ACSet, input::AbstractDict)
  schema = acset_schema(out)
  parts = Iterators.map(input) do (type, rows)
    Symbol(type) => add_parts!(out, Symbol(type), length(rows))
  end |> Dict
  for rows ∈ values(input)
    for (rownum, row) ∈ enumerate(rows)
      for (k, v) ∈ pairs(row)
        k = Symbol(k)
        if k == :_id
          # For now, IDs are assumed to coincide with row number.
          @assert rownum == v
          continue
        end
        if k ∈ attrs(schema; just_names=true)
          vtype = attr_type(out, k)
          v = v isa AbstractDict && haskey(v, "_var") ?
            AttrVar(v["_var"]) : vtype(v)
        end
        set_subpart!(out, parts[dom(schema, k)][rownum], k, v)
      end
    end
  end
  out
end

""" Deserialize an ACSet object from a JSON file.

Inverse to [`write_json_acset`](@ref).
"""
function read_json_acset(ty, fname::AbstractString)
  parse_json_acset(ty, JSON.parsefile(fname))
end

""" Serialize an ACSet object to a JSON file.

Inverse to [`read_json_acset`](@ref).
"""
function write_json_acset(x::ACSet, fname::AbstractString)
  open(fname, "w") do f
    write(f, JSON.json(generate_json_acset(x)))
  end
end

# Schema serialization
######################

""" Generate JSON-able object representing an ACSet schema.

Given an ACSet schema (either a `Schema` or a `Presentation`), such as
`SchGraph` or `SchWeightedGraph`, construct a JSON-able dictionary with keys
"Ob", "Hom", "AttrType", and "Attr", conforming to the JSON Schema in
[`acset_schema_json_schema`](@ref).

Inverse to [`parse_json_acset_schema`](@ref).
"""
function generate_json_acset_schema(schema::Schema)
  acsets_pkg_ver = if !isnothing(Pkg.project().version)
    replace(string(Pkg.project().version), "v" => "")
  else
    "0.0.0" # Should be 0.0.0 only for tests.
  end
  OrderedDict(
    "version" => Dict("ACSetSchema" => "0.0.1",
                      "ACSets" => acsets_pkg_ver),
    "Ob" => map(objects(schema)) do x
      Dict("name" => string(x))
    end,
    "Hom" => map(homs(schema)) do (f, x, y)
      Dict("name" => string(f), "dom" => string(x), "codom" => string(y))
    end,
    "AttrType" => map(attrtypes(schema)) do x
      Dict("name" => string(x))
    end,
    "Attr" => map(attrs(schema)) do (f, x, y)
      Dict("name" => string(f), "dom" => string(x), "codom" => string(y))
    end,
  )
end

""" Parse JSON-able object or JSON string representing an ACSet schema.

Given a JSON object specifying a presentation of an ACSet schema, construct a
schema object: either a `Schema` or, by default, a `Presentation`.

Inverse to [`generate_json_acset_schema`](@ref).
"""
function parse_json_acset_schema(::Type{BasicSchema}, data::AbstractDict)
  obs = [Symbol(d["name"]) for d in data["Ob"]]
  homs = map(data["Hom"]) do d
    map(Symbol, (d["name"], d["dom"], d["codom"]))
  end
  attrtypes = [Symbol(d["name"]) for d in data["AttrType"]]
  attrs = map(data["Attr"]) do d
    map(Symbol, (d["name"], d["dom"], d["codom"]))
  end
  BasicSchema(obs, homs, attrtypes, attrs)
end

function parse_json_acset_schema(T, input::AbstractString)
  parse_json_acset_schema(T, JSON.parse(input))
end

""" Deserialize ACSet schema from JSON file.

Similar to [`parse_json_acset_schema`](@ref) except reads from a file.
Inverse to [`write_json_acset_schema`](@ref).
"""
function read_json_acset_schema(T, fname::AbstractString)
  parse_json_acset_schema(T, JSON.parsefile(fname))
end

""" Serialize ACSet schema to JSON file.

Similar to [`generate_json_acset_schema`](@ref) except writes to a file.
Inverse to [`read_json_acset_schema`](@ref).
"""
function write_json_acset_schema(schema, fname::AbstractString)
  open(fname, "w") do f
    write(f, JSON.json(generate_json_acset_schema(schema)))
  end
end

""" Returns the JSON schema for the JSON serialization of ACSet schemas.

The result is a JSON-able object (dictionary) from which a `JSONSchema.Schema`
can be constructed, using the package JSONSchema.jl.
"""
function acset_schema_json_schema(; kw...)
  JSON.parsefile(joinpath(@__DIR__, "data", "acset.schema.json");
                 dicttype=OrderedDict{String,Any}, kw...)
end

""" Dispatch for ACSet

Dispatches write to accept ACSets
"""
JSON3.write(io::IO, acs::ACSet; kwargs...) = JSON3.write(io, generate_json_acset(acs); kwargs...)

end


