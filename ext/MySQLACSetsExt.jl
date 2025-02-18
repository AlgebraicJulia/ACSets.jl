module MySQLACSetsExt

using ACSets
import ACSets: tostring, tosql

using MLStyle
using MySQL

const MYSQL_DELIMITER = "//"

function ACSets.reload!(vas::VirtualACSet{Conn}) where Conn
    vas.conn = DBInterface.connect(MySQL.Connection, "localhost", "mysql", 
                                   db="acsets", 
                                   unix_socket="/var/run/mysqld/mysqld.sock")
end

function tosql end

# DB specific, type conversion
tosql(::MySQL.Connection, ::Type{<:Real}) = "REAL"
tosql(::MySQL.Connection, ::Type{<:AbstractString}) = "TEXT"
tosql(::MySQL.Connection, ::Type{<:Symbol}) = "TEXT"
tosql(::MySQL.Connection, ::Type{<:Integer}) = "INTEGER"
tosql(::MySQL.Connection, T::DataType) = error("$T is not supported in this MySQL implementation")
# value conversion
tosql(::MySQL.Connection, ::Nothing) = "NULL"
tosql(::MySQL.Connection, s::Symbol) = string(s)
tosql(::MySQL.Connection, s::String) = "\'$s\'"
tosql(::MySQL.Connection, x) = x

# String constructors
function ACSets.tostring(conn::MySQL.Connection, i::Insert) 
    cols = join(columns(i.values), ", ")
    values = join(entuple(i.values; f=x->tosql(conn, x)), ", ")
    "INSERT IGNORE INTO $(i.table) ($cols) VALUES $values ;"
end

# TODO might have to refactor so we can reuse code for show method
function ACSets.tostring(conn::MySQL.Connection, s::Select)
    from = s.from isa Vector ? join(s.from, ", ") : s.from
    qty = tostring(conn, s.qty)
    wheres = !isnothing(s.wheres) ? tostring(conn, s.wheres) : ""
    "SELECT $qty FROM $from " * wheres * ";"
end

function ACSets.tostring(conn::MySQL.Connection, qty::SQLSelectQuantity)
    @match qty begin
        ::SelectAll => "*"
        ::SelectDistinct => "*" # TODO
        ::SelectDistinctRow => "*" # TODO
        SelectColumns(cols) => join(tostring.(Ref(conn), cols), ", ")
    end
end

function ACSets.tostring(::MySQL.Connection, column::Union{Pair{Symbol, Symbol}, Symbol})
    @match column begin
        ::Pair{Symbol, Symbol} => "$(column.first).$(column.second)"
        _ => column
    end
end

# TODO
function ACSets.tostring(::MySQL.Connection, wheres::WhereClause)
    @match wheres begin
        WhereClause(op, d::Pair) => "WHERE $(d.first) $op ($(join(d.second, ", ")))"
        _ => wheres
    end
end

function ACSets.tostring(conn::MySQL.Connection, c::Create)
    create_stmts = map(objects(c.schema)) do ob
        obattrs = attrs(c.schema; from=ob)
        "CREATE TABLE IF NOT EXISTS $(ob)(" * 
            join(filter(!isempty, ["_id INTEGER PRIMARY KEY",
                # column_name column_type
                join(map(homs(c.schema; from=ob)) do (col, src, tgt)
                       tgttype = tosql(conn, Int)
                       "$(col) $tgttype"
                end, ", "),
                join(map(obattrs) do (col, _, type)
                    # FIXME
                   "$(col) $(tosql(conn, subpart_type(c.schema, type)))" 
               end, ", ")]), ", ") * ");"
    end
    join(create_stmts, " ")
end

function ACSets.tostring(conn::MySQL.Connection, d::Delete)
    "DELETE FROM $(d.table) WHERE _id IN ($(join(d.ids, ",")))"
end

function ACSets.tostring(::MySQL.Connection, v::Values)
    "VALUES " * join(entuple(v), ", ") * ";"
end

function ACSets.tostring(::MySQL.Connection, a::Alter)
    "ALTER TABLE $(a.refdom) ADD CONSTRAINT fk_$(ref) FOREIGN KEY ($(a.ref)) REFERENCES $(a.refcodom)(_id); "
end

# overloading syntactical constructors 
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
