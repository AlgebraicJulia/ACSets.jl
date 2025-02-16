module MySQLACSets

using MySQL

### Read into an ACSet
function ACSets.selectfrom(conn::MySQL.Connection, ::StructACSet{T}, table::Symbol) where T
    tables = @match [T.parameters...] begin
        [symbol, tables, kwargs...] => [tables.parameters...]
        _ => nothing
    end
    # we can use the type information to check 
    # if the table *might* be in the schema
    @assert table ∈ tables
    selectfrom(conn, table)
end

@present SchWeightedLabeledGraph <: SchLabeledGraph begin
    Weight::AttrType
    weight::Attr(E,Weight)
end
@acset_type WeightedLabeledGraph(SchWeightedLabeledGraph, index=[:src, :tgt]) <: AbstractLabeledGraph
g = erdos_renyi(WeightedLabeledGraph{Symbol,Float64}, 10, 0.25)
g[:, :label] = Symbol.(collect('a':'z')[1:nv(g)]);
g[:, :weight] = floor.(rand(ne(g)) .* 100);


"""Builds a SQL statement"""
function build end
# This dispatches on the connection type and SQL sum type.

# let's open the connection
# conn = DBInterface.connect(MySQL.Connection, "localhost", "root", db="acsets")
conn = DBInterface.connect(MySQL.Connection, "localhost", "mysql", db="acsets", unix_socket="/var/run/mysqld/mysqld.sock")

# TODO add where statement
function build(::MySQL.Connection, ::Type{Select}, table::Symbol, 
        cols::Vector{Symbol}=Vector{Symbol}())
    columns = !isempty(cols) ? join(cols, ", ") : "*"
    "SELECT $columns FROM $table"
end

# we will use this to test that a table has been written
function selectfrom(conn::MySQL.Connection, table::Symbol, cols::Vector{Symbol}=Vector{Symbol}())
    stmt = build(conn, Select, table, cols)
    query = DBInterface.execute(conn, stmt)
    DataFrames.DataFrame(query)
end

# here we assume that the primary key is always `_id`.
function build(conn::MySQL.Connection, ::Type{Create}, x::ACSet)::String
    schema = acset_schema(x)
    result = Vector{String}()
    # build tables
    foreach(objects(schema)) do ob
        obattrs = attrs(schema; from=ob)
        stmt = Vector{String}()
        # create table
        push!(stmt, "CREATE TABLE IF NOT EXISTS $(ob)(_id INTEGER PRIMARY KEY")
        # homs are also columns. they say `E.src` must have a value in `V._id`,
        # likewise `E.tgt`
        for (col, src, tgt) in homs(schema; from=ob)
            tgttype = tosql(Int) # assuming tgt is _id
            push!(stmt, "$(col) $tgttype")
        end
        for (col, _, type) in obattrs
            push!(stmt, "$(col) $(tosql(subpart_type(x, type)))")
        end
        push!(stmt, "); ")
        push!(result, join(stmt, ", ", ""))
    end
    # build table relations
    foreach(homs(schema)) do (ref, refdom, refcodom)
        push!(result, build(conn, Alter, ref, refdom, refcodom))
    end
    join(result, "")
end

function build(::MySQL.Connection, ::Type{Alter}, ref, refdom, refcodom)
    "ALTER TABLE $refdom ADD CONSTRAINT fk_$(ref) FOREIGN KEY ($ref) REFERENCES $refcodom(_id); "
end

function create!(conn::MySQL.Connection, x::ACSet)
    stmt = build(conn, Create, x)
    DBInterface.execute(conn, stmt)
end

create!(conn, g)
# @assert typeof(selectfrom(conn, :E)) isa DataFrame

# get attrs
getattrs(g::ACSet, table::Symbol) = first.(filter(attrs(acset_schema(g))) do (attr, tbl, _)
    table == tbl
end)

gethoms(x::ACSet, table::Symbol) = first.(homs(acset_schema(x); from=table))

struct Values
    table::Symbol
    x::Vector{Any}
end

# values should store more data
# FIXME should be smarter. Values(_, [1,2,3]) => "VALUES (1), (2), (3)"
String(vals::Values) = "VALUES " * join(entuple.(vals.x), ", ") * ";"

entuple(xs::Vector)::String = length(xs) == 1 ? "($(only(xs)))" : "$(Tuple(xs))"

# Values should have a method which turns single values into "(1)"
function getrows(x::ACSet, table::Symbol)
    cols = gethoms(x, table) ∪ getattrs(x, table)
    x = map(parts(x, table)) do id
        [id, tosql.(subpart.(Ref(x), Ref(id), cols))...]
    end
    Values(table, x)
end

# DIGRESSION could also support this
t=tables(g)
rows=getproperty(t, :E)
cols=propertynames(rows)
# ##################################

# FIXME Set
function colnames(x::ACSet, table::Symbol)::String
    homnames = first.(homs(acset_schema(x); from=table))
    gattrs = getattrs(x, table)
    # I don't like this as it assumes the order of the columns would agree
    cols = [:_id, (homnames ∪ gattrs)...]
    """($(join(cols, ", ")))"""
end

function build(::MySQL.Connection, ::Type{Insert}, table::Symbol, 
        cols::AbstractVector{Symbol}, values::AbstractVector{<:AbstractVector})
    "INSERT IGNORE INTO $table ($(join(cols, ", "))) VALUES " * join(entuple.(tosql.(values)), ", ") * ";"
end

function build(::MySQL.Connection, ::Type{Insert}, x::ACSet, table::Symbol)
    cols = colnames(x, table)
    values = getrows(x, table)
    "INSERT IGNORE INTO $table $cols " * String(getrows(x, table))
    # upserting could also be an option
end

# we can get a dependency graph based on the ACSet
function build(conn::MySQL.Connection, ::Type{Insert}, x::ACSet)
    schema = acset_schema(x)
    stmts = map(objects(schema)) do ob
        build(conn, Insert, x, ob)
    end
    join(stmts, " ")
end

function Base.insert!(conn::MySQL.Connection, x::ACSet, stmt::Union{String, Nothing}=nothing)
    _stmt = isnothing(stmt) ? build(conn, Insert, x) : stmt
    DBInterface.executemultiple(conn, _stmt)
end

end
