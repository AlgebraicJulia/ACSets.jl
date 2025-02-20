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

function VirtualACSet(conn::Conn, acs::SimpleACSet) where Conn
    VirtualACSet{Conn}(conn=conn, acsettype=typeof(acs))
end

function ACSet(vas::VirtualACSet{Conn}) where Conn
    acset = vas.acsettype()
    isnothing(vas.view) && return acset
    # TODO testing
    add_parts!(acset, :V, nrow(vas.view))
    set_subpart!(acset, :label, Symbol.(vas.view.label))
    acset
end

function reload! end
export reload!

# TODO generate multiple statements, then decide to execute single or multiple
function execute!(vas::VirtualACSet{Conn}, stmt::String) where Conn
    result = DBInterface.execute(vas.conn, stmt)
    DataFrames.DataFrame(result)
end
export execute!

function execute!(vas::VirtualACSet{Conn}, query::SQLTerms) where Conn
    execute!(vas, tostring(vas.conn, query))
end

function ACSet!(vas::VirtualACSet{Conn}, query::SQLTerms) where Conn
    vas.view = execute!(vas, query)
    ACSet(vas)
end

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
    DataFrames.DataFrame(query)
end

function tosql end
export tosql

"""
"""
function entuple(v::Values; f::Function=identity)
    ["($(join(f.(vals), ",")))" for vals in values.(v.vals)]
end
export entuple

# FIXME
function entuple(nt::NamedTuple)
    @info values(nt)
    ["($(join(vals, ",")))" for vals in values(nt)]
end

# get attrs
function getattrs(g::ACSet, table::Symbol)
    first.(filter(attrs(acset_schema(g))) do (attr, tbl, _)
        table == tbl
    end)
end
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

