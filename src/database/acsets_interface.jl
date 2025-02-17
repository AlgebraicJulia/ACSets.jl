#####################
### ACSets Interface
#####################

# get the number of rows
function ACSetInterface.nparts(acset::VirtualACSet{Conn}, table::Symbol) where Conn
    query = DBInterface.execute(acset.conn, "SELECT COUNT(*) FROM $table;")
    DataFrames.DataFrame(query) # we need to send this to a 
end

function ACSetInterface.maxpart(acset::VirtualACSet, table::Symbol) end

function ACSetInterface.subpart(acset::VirtualACSet, table::Symbol, cols::AbstractVector{Symbol})
    stmt = tostring(Select(cols, table))
    query = DBInterface.execute(acset.conn, stmt)
    DataFrames.DataFrame(query)
end

function ACSetInterface.incident(acset::VirtualACSet, table::Symbol, names::AbstractVector{Symbol}) end

function ACSetInterface.add_part!(acset::VirtualACSet, table::Symbol, values::Vector{<:NamedTuple{T}}) where T 
    stmt = tostring(Insert(table, cols, values))
    query = DBInterface.execute(acset.conn, stmt)
    DataFrames.DataFrames(query)
end

function ACSetInterface.set_subpart!(acset::VirtualACSet, args...) end

function ACSetInterface.clear_subpart!(acset::VirtualACSet, args...) end

function ACSetInterface.rem_part!(acset::VirtualACSet, args...) end

function ACSetInterface.cascading_rem_part!(acset::VirtualACSet, args...) end



# DB Interface

# # we will use this to test that a table has been written
# function selectfrom(conn::MySQL.Connection, table::Symbol, cols::Vector{Symbol}=Vector{Symbol}())
#     stmt = build(conn, Select, table, cols)
#     query = DBInterface.execute(conn, stmt)
#     DataFrames.DataFrame(query)
# end

# # we can get a dependency graph based on the ACSet
# function build(conn::MySQL.Connection, ::Type{Insert}, x::SimpleACSet)
#     schema = acset_schema(x)
#     stmts = map(objects(schema)) do ob
#         build(conn, Insert, x, ob)
#     end
#     join(stmts, " ")
# end

# function Base.insert!(conn::MySQL.Connection, x::SimpleACSet, stmt::Union{String, Nothing}=nothing)
#     _stmt = isnothing(stmt) ? build(conn, Insert, x) : stmt
#     DBInterface.executemultiple(conn, _stmt)
# end

