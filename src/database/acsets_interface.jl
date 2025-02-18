#####################
### ACSets Interface
#####################

# get the number of rows
function ACSetInterface.nparts(acset::VirtualACSet{Conn}, table::Symbol) where Conn
    query = DBInterface.execute(acset.conn, "SELECT COUNT(*) FROM $table;")
    DataFrames.DataFrame(query) 
end

function ACSetInterface.maxpart(acset::VirtualACSet, table::Symbol) end

# get value of 
# subpart(acset, 1, :tgt) =
# subpart(acset, :, col) = get all values in column
function ACSetInterface.subpart(vas::VirtualACSet, table::Symbol, 
        what::SQLSelectQuantity=SelectAll())
    stmt = tostring(vas.conn, Select(table; what=what))
    query = DBInterface.execute(vas.conn, stmt)
    DataFrames.DataFrame(query)
end
# ambiguity

# gets id where
function ACSetInterface.incident(vas::VirtualACSet, table::Symbol, names::AbstractVector{Symbol}) end

function ACSetInterface.add_part!(vas::VirtualACSet, table::Symbol, values::Vector{<:NamedTuple{T}}) where T 
    stmt = tostring(vas.conn, Insert(table, values))
    query = DBInterface.execute(vas.conn, stmt)
    DBInterface.lastrowid(query)
end

function ACSetInterface.add_part!(vas::VirtualACSet, table, value::NamedTuple{T}) where T
    add_part!(vas, table, [value])
end

function ACSetInterface.set_subpart!(acset::VirtualACSet, args...) end

function ACSetInterface.clear_subpart!(acset::VirtualACSet, args...) end

function ACSetInterface.rem_part!(vas::VirtualACSet, table::Symbol, id::Int)
    rem_parts!(vas, table, [id])
end

function ACSetInterface.rem_parts!(vas::VirtualACSet, table::Symbol, ids::Vector{Int}) 
    stmt = tostring(vas.conn, Delete(table, ids))
    query = DBInterface.execute(vas.conn, stmt)
    result = tostring(vas.conn, Select(table))
    query = DBInterface.execute(vas.conn, result)
    DataFrames.DataFrame(query)
end

function ACSetInterface.cascading_rem_part!(acset::VirtualACSet, args...) end
