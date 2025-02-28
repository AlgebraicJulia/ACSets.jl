using ACSets
using Catlab
using Catlab.Graphs
#
using DBInterface
using MLStyle
using DataFrames
using MySQL # loads extension
#
conn = DBInterface.connect(MySQL.Connection, "localhost", "mysql", db="acsets", unix_socket="/var/run/mysqld/mysqld.sock")

@present SchWeightedLabeledGraph <: SchLabeledGraph begin
    Weight::AttrType
    weight::Attr(E,Weight)
end;
@acset_type WeightedLabeledGraph(SchWeightedLabeledGraph, index=[:src, :tgt]) <: AbstractLabeledGraph
g = erdos_renyi(WeightedLabeledGraph{Symbol,Float64}, 5, 0.25);
g[:, :label] = Symbol.(floor.(rand(nv(g)) * nv(g)));
g[:, :weight] = floor.(rand(ne(g)) .* 100);

vas = VirtualACSet(conn, g)

c = Create(g)

execute!(vas, c)

i = join(tostring.(Ref(conn), Insert(conn, g)), " ")

execute!(vas, i)

subpart(vas, :V)

incident(vas, nparts(vas, :V).var"COUNT(*)"[1], :src)

# TODO move "LastRowId" to a type so we can dispatch on it
add_part!(vas, :V, (_id = nparts(vas, :V).var"COUNT(*)"[1] + 1, label = "a")) 

rem_part!(vas, :V, 10) # this will fail because of db constraints

rem_part!(vas, :E, 10)

subpart(vas, :V)

s0 = Select(:V, what=SelectColumns(:V => :label))
execute!(vas, s0)

s1 = Select(:E, what=SelectColumns(:E => :_id, :E => :tgt), wheres=WhereClause(:in, :_id => [1,2,3]))
execute!(vas, s1)

# TODO should be able to pass in Julia expressions for conditions like `where`
set_subpart!(vas, :V, [(label=0,)]; wheres=WhereClause(:in, :_id => [1]))


subpart(vas, 1, :tgt)
subpart(vas, [1,2], :tgt)


execute!(vas, s1)

i=Insert(:Persons, [(PersonID=1, LastName="Last", FirstName="First", Address="a", City="b")])
execute!(vas, i)

u=Update(:Persons, [(LastName="First", FirstName="Last")], WhereClause(:in, :PersonID => [1]))
tostring(conn, u)

# s=Select(qty=SelectAll(), from=[:V, :E],  

j=Join(:LEFT, :V, SQLEquation(:E => :src, :V => :_id))

s=Select(SelectColumns([:E=>[:_id, :src, :tgt, :weight], :V=>:label]), :E, Join(:LEFT, :V, SQLEquation(:E => :src, :V => :_id)), nothing)

s=Select(SelectColumns([:Junct=>:_id, :Student=>:name]), :Junct, 
         Join(:RIGHT, :Student, SQLEquation(:Junct => :student, :Student => :_id)), nothing)

ForeignKeyChecks(conn, tostring(conn, i))

struct GraphSchema
    graph::WeightedLabeledGraph{DataFrame, DataFrame}
    function GraphSchema()
        new(WeightedLabeledGraph{DataFrame, DataFrame}())
    end
end

# want just a diagram of the 
h = GraphSchema()

v = subpart(vas, :V)
e = subpart(vas, :E)

add_part!(h.graph, :V, label = v)
add_part!(h.graph, :E, weight = e)


