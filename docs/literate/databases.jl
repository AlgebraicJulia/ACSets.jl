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

i = join(tostring.(Ref(conn), Insert(conn, g)))

execute!(vas, i)

subpart(vas, :V)

add_part!(vas, :V, (_id = 10, label = "a")) 

rem_part!(vas, :V, 10)

rem_part!(vas, :E, 10)

subpart(vas, :V)

s0 = Select(:V, what=SelectColumns(:V => :label))
s1 = Select(:E, what=SelectColumns(:E => :_id, :E => :tgt), wheres=WhereClause(:in, :_id => [1,2,3]))

tostring(conn, s1)

execute!(vas, s0)
