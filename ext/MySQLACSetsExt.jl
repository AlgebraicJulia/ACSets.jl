module MySQLACSetsExt

using ACSets
import ACSets: tostring, tosql

using MLStyle
using MySQL

const MYSQL_DELIMITER = "//"

# DB specific, type conversion
tosql(::MySQL.Connection, ::Type{<:Real}) = "REAL"
tosql(::MySQL.Connection, ::Type{<:AbstractString}) = "TEXT"
tosql(::MySQL.Connection, ::Type{<:Symbol}) = "TEXT"
tosql(::MySQL.Connection, ::Type{<:Integer}) = "INTEGER"
tosql(::MySQL.Connection, T::DataType) = error("$T could not be less supported")
# value conversion
tosql(::MySQL.Connection, ::Nothing) = "NULL"
tosql(::MySQL.Connection, s::Symbol) = string(s)
tosql(::MySQL.Connection, s::String) = "\'$s\'"
tosql(::MySQL.Connection, x) = x

# String constructors
function tostring(conn::MySQL.Connection, i::Insert) 
    cols = join(columns(i.values), ", ")
    "INSERT IGNORE INTO $(i.table) ($cols) VALUES " * join(entuple(i.values;f=x -> tosql(conn, x)), ", ") * ";"
end

function tostring(::MySQL.Connection, s::Select)
    columns = !isempty(s.cols) ? join(s.cols, ", ") : "*"
    "SELECT $columns FROM $(s.table)" * ";"
end

function tostring(conn::MySQL.Connection, c::Create)
    create_stmts = map(objects(c.schema)) do ob
        obattrs = attrs(c.schema; from=ob)
        out = "CREATE TABLE IF NOT EXISTS $(ob)(" * 
            join(filter(!isempty, ["_id INTEGER PRIMARY KEY",
                # column_name column_type
                join(map(homs(c.schema; from=ob)) do (col, src, tgt)
                       tgttype = tosql(conn, Int)
                       "$(col) $tgttype"
                end, ", "),
                join(map(obattrs) do (col, _, type)
                   "$(col) $(tosql(conn, subpart_type(c.x, type)))" 
               end, ", ")]), ", ") * ");"
    end
    join(create_stmts, " ")
end

function tostring(::MySQL.Connection, v::Values)
    "VALUES " * join(entuple(v), ", ") * ";"
end

function tostring(::MySQL.Connection, a::Alter)
    "ALTER TABLE $(a.refdom) ADD CONSTRAINT fk_$(ref) FOREIGN KEY ($(a.ref)) REFERENCES $(a.refcodom)(_id); "
end

function ACSets.Insert(conn::MySQL.Connection, acset::ACSet)
    map(objects(acset_schema(acset))) do ob
        Insert(conn, acset, ob)
    end
end

function ACSets.Insert(conn::MySQL.Connection, acset::ACSet, table::Symbol)
    cols = colnames(acset, table)
    vals = getrows(conn, acset, table)
    Insert(table, vals)
end

end
