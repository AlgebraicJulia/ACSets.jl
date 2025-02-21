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

c = Create(g)

vas = VirtualACSet(conn, g)

execute!(vas, c)

i = join(tostring.(Ref(conn), Insert(conn, g)), " ")

execute!(vas, i)

subpart(vas, :V)

# TODO move "LastRowId" to a type so we can dispatch on it 
add_part!(vas, :V, (_id = nparts(vas, :V).var"COUNT(*)"[1] + 1, label = "a")) 

rem_part!(vas, :V, 10) # this will fail because of db constraints

rem_part!(vas, :E, 10)

subpart(vas, :V)

s0 = Select(:V, what=SelectColumns(:V => :label))
execute!(vas, s0)

s1 = Select(:E, what=SelectColumns(:E => :_id, :E => :tgt), wheres=WhereClause(:in, :_id => [1,2,3]))
execute!(vas, s1)


subpart(vas, 1, :tgt)
subpart(vas, [1,2], :tgt)


execute!(vas, s1)

i=Insert(:Persons, [(PersonID=1, LastName="Last", FirstName="First", Address="a", City="b")])
execute!(vas, i)

u=Update(:Persons, [(LastName="First", FirstName="Last")], WhereClause(:in, :PersonID => [1]))
tostring(conn, u)


ForeignKeyChecks(conn, tostring(conn, i))
