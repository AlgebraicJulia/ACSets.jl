using ACSets
using Catlab
using Catlab.Graphs

@present SchTable(FreeSchema) begin
    Name::AttrType
    Column::Ob
    colname::Attr(Column, Name)
end
@acset_type Table(SchTable)

j = Table{Symbol}()
add_part!(j, :Column, colname=:color)
add_part!(j, :Column, colname=:flavor)

k = Table{Symbol}()
add_part!(k, :Column, colname=:mood)

l = Table{Symbol}()
add_part!(l, :Column, colname=:review)
add_part!(l, :Column, colname=:stars)

@present SchEdgeLabeledGraph <: SchLabeledGraph begin
    TableName::AttrType
    tablename::Attr(V, TableName)
    EdgeLabel::AttrType
    edgelabel::Attr(E, EdgeLabel)
    # edgename
end
@acset_type EdgeLabeledGraph(SchEdgeLabeledGraph)

n = EdgeLabeledGraph{Table{Symbol}, Symbol, Pair{Symbol, Symbol}}()
add_part!(n, :V, label=j, tablename=:Tastes)
add_part!(n, :V, label=k, tablename=:Feelings)
add_part!(n, :V, label=l, tablename=:Review)
add_part!(n, :E, src=1, tgt=2, edgelabel=:Tastes => :Feelings)
add_part!(n, :E, src=2, tgt=3, edgelabel=:Feelings => :Review)

# stitch this together
# 1. break tablename into Obs
# 2. labels becomes columns (attrs)
# 3. edges become homs

""" Sends the graph internal to ACSets into an ACSet schema""" 
function Base.join(x::EdgeLabeledGraph)
    obs = Symbol[]
    attrs = Tuple{Symbol, Symbol, Symbol}[]
    homs = Tuple{Symbol, Symbol, Symbol}[]
    foreach(parts(n, :V)) do id
        table = subpart(n, id, :tablename)
        # push!(obs, id => table)
        push!(obs, table)
        _attrs = subpart(subpart(n, id, :label), :colname)
        push!(attrs, [(a, table, :Name) for a in _attrs]...)
    end
    foreach(parts(n, :E)) do id
        src = subpart(n, id, :src)
        tgt = subpart(n, id, :tgt)
        el = subpart(n, id, :edgelabel) # :flavor => :color
        push!(homs, (Symbol("$(el.first)$(el.second)"), el.first, el.second))
    end
    BasicSchema(obs, homs, Symbol[:Name], attrs)
end

SchAnew = join(n)

@acset_type New(SchNew, index=[:TasteFeelings])
