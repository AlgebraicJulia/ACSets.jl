using DuckDB

conn = DBInterface.connect(DuckDB.DB, ":memory:")

@present SchWeightedLabeledGraph <: SchLabeledGraph begin
    Weight::AttrType
    weight::Attr(E,Weight)
end
@acset_type WeightedLabeledGraph(SchWeightedLabeledGraph, index=[:src, :tgt]) <: AbstractLabeledGraph

g = erdos_renyi(WeightedLabeledGraph{Symbol,Float64}, 10, 0.25)
g[:, :label] = Symbol.(collect('a':'z')[1:nv(g)])
g[:, :weight] = floor.(rand(ne(g)) .* 100)

# first make tables of each object and its attributes
for o in objects(acset_schema(g))
    o_attrs = attrs(acset_schema(g); from=o)
    stmt = Vector{String}()
    push!(stmt, "CREATE OR REPLACE TABLE $(o)(_id INTEGER PRIMARY KEY")
    for (col, _, type) in o_attrs
        push!(stmt, ", $(col) $(tosql(subpart_type(g, type)))")
    end
    push!(stmt, ");")
    DBInterface.execute(conn, join(stmt))
end

for (h, h_dom, h_codom) in homs(acset_schema(g))
    DBInterface.execute(conn, """
        ALTER TABLE $(h_dom) ADD $(h) INTEGER REFERENCES $(h_codom)(_id);
    """)
end

