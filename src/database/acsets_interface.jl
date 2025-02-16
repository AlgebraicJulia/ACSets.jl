#####################
### ACSets Interface
#####################
abstract type AbstractVirtualACSet end

#### Reading large data materialized elsewhere
@kwdef mutable struct VirtualACSet
    conn::DBInterface.Connection
    acsettype::Type{<:StructACSet}
    view::Union{DataFrames.DataFrame, Nothing} = nothing
end
export VirtualACSet
# TODO we need to convert the `view` into an ACSet

function VirtualACSet(conn::DBInterface.Connection, acs::SimpleACSet)
    VirtualACSet(conn=conn, acsettype=typeof(acs))
end

# get the number of rows
function ACSetInterface.nparts(acset::VirtualACSet, table::Symbol)
    query = DBInterface.execute(acset.conn, "SELECT COUNT(*) FROM $table;")
    DataFrames.DataFrame(query) # we need to send this to a 
end

function ACSetInterface.maxpart(acset::VirtualACSet, table::Symbol) end

function ACSetInterface.subpart(acset::VirtualACSet, table::Symbol, cols::AbstractVector{Symbol})
    query = DBInterface.execute(acset.conn, build(acset.conn, Select, table, cols))
    DataFrames.DataFrame(query)
end

function ACSetInterface.incident(acset::VirtualACSet, table::Symbol, names::AbstractVector{Symbol}) end

function ACSetInterface.add_part!(acset::VirtualACSet, table::Symbol, 
        cols::AbstractVector{Symbol}, values::AbstractVector{<:AbstractVector})
    stmt = build(acset.conn, Insert, table, cols, values)
    query = DBInterface.execute(acset.conn, stmt)
    DataFrames.DataFrames(query)
end

function ACSetInterface.set_subpart!(acset::VirtualACSet, args...) end

function ACSetInterface.clear_subpart!(acset::VirtualACSet, args...) end

function ACSetInterface.rem_part!(acset::VirtualACSet, args...) end

function ACSetInterface.cascading_rem_part!(acset::VirtualACSet, args...) end
