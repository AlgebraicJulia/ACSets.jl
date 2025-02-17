function tostring end
export tostring

function select end
function create! end
function delete end
function alter end
export create!

abstract type AbstractVirtualACSet end

#### Reading large data materialized elsewhere
@kwdef mutable struct VirtualACSet{Conn<:DBInterface.Connection}
    conn::Conn
    acsettype::Type{<:StructACSet}
    view::Union{DataFrames.DataFrame, Nothing} = nothing
end
export VirtualACSet
# TODO we need to convert the `view` into an ACSet

function VirtualACSet(conn::Conn, acs::SimpleACSet) where Conn <: DBInterface.Connection
    VirtualACSet{Conn}(conn=conn, acsettype=typeof(acs))
end

# will this work how I want?
function reload!(vas::VirtualACSet{Conn}) where Conn
    vas.conn = DBInterface.connect(MySQL.Connection, "localhost", "mysql", db="acsets", 
                                   unix_socket="/var/run/mysqld/mysqld.sock")
end
export reload

function execute!(vas::VirtualACSet{Conn}, stmt::String) where Conn
    result = DBInterface.executemultiple(vas.conn, stmt)
    DataFrames.DataFrame(result)
end
export execute!

function create!(conn::DBInterface.Connection, x::SimpleACSet)
    stmt = tostring(conn, Create(x))
    DBInterface.executemultiple(conn, stmt)
end
export create!

function create!(v::VirtualACSet{Conn}) where Conn
    query = tostring(v.conn, v.acsettype)
    DBInterface.execute(v.conn, query)
end

function insert!(v::VirtualACSet{Conn}, acset::SimpleACSet) where Conn
    insert_stmts = tostring.(Ref(v.conn), Insert(v.conn, acset))
    query = DBInterface.executemultiple(conn, insert_stmts)
    DataFrames.DataFrmae(query)
end

# this typing ensures that named tuples have the same keys
struct Values{T}
    table::Union{Symbol, Nothing}
    vals::Vector{<:NamedTuple{T}}
end

Base.length(v::Values{T}) where T = length(v.vals)
Base.iterate(v::Values{T}, args...) where T = iterate(v.vals, args...)
Base.broadcast(f, v::Values{T}) where T = Values{T}(v.table, broadcast(f, v.vals))

columns(v::Values{T}) where T = T
export columns

@data SQLTerms begin
    Insert(table::Symbol, values::Values)
    Select(cols::Union{Vector{Symbol}, Nothing}, table::Symbol)
    Alter(table::Symbol, refdom::Symbol, refcodom::Symbol)
    Create(schema::BasicSchema{Symbol})
    Delete(table::Symbol, ids::Vector{Int})
end
export SQLTerms, Values, Insert, Select, Alter, Create, Delete

## Constructors

function Select(table::Symbol)
    Select(nothing, table)
end

function Alter(table::Symbol, arrow::Pair{Symbol, Symbol})
    Alter(table, arrow.first, arrow.second)
end

function Create(acset::SimpleACSet)
    Create(acset_schema(acset))
end

function Insert(table::Symbol, vs::Vector{<:NamedTuple{T}}) where T
    Insert(table, Values(table, vs))
end

## SQL Term Operations

function Base.:+(v1::Values, v2::Values)
    Values(v1._1 ∪ v2._1)
end

function Base.:+(i1::Insert, i2::Insert)
    if i1.table == i2.table
        Insert(i1.table, i1.values + i2.values)
    else
        [i1, i2]
    end
end

function tosql end
export tosql

"""
"""
function entuple(v::Values;f::Function=identity)
    ["($(join(f.(vals), ",")))" for vals in values.(v.vals)]
end
export entuple

# FIXME
function entuple(nt::NamedTuple)
    @info values(nt)
    ["($(join(vals, ",")))" for vals in values(nt)]
end

# get attrs
getattrs(g::ACSet, table::Symbol) = first.(filter(attrs(acset_schema(g))) do (attr, tbl, _)
    table == tbl
end)
export getattrs

gethoms(x::ACSet, table::Symbol) = first.(homs(acset_schema(x); from=table))
export gethoms

# Values should have a method which turns single values into "(1)"
function getrows(conn::Conn, x::ACSet, table::Symbol) where Conn <: DBInterface.Connection
    cols = gethoms(x, table) ∪ getattrs(x, table)
    x = map(parts(x, table)) do id
        (;zip([:_id, cols...], [id, tosql.(Ref(conn), subpart.(Ref(x), Ref(id), cols))...])...)
    end
    Values(table, x)
end
export getrows

# FIXME Set
function colnames(x::ACSet, table::Symbol)
    homnames = first.(homs(acset_schema(x); from=table))
    gattrs = getattrs(x, table)
    # I don't like this as it assumes the order of the columns would agree
    cols = [:_id, (homnames ∪ gattrs)...]
    """($(join(cols, ", ")))"""
end
export colnames

