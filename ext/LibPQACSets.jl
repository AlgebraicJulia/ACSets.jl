###
## schemata should be able to define their own create table
using LibPQ

function maketables(conn::LibPQ.Connection, x::ACSet)
    schema = acset_schema(x)
    stmts = map(objects(schema)) do ob
        obattrs = attrs(schema; from=ob)
        stmt = Vector{String}()
        push!(stmt, "CREATE TABLE IF NOT EXISTS $(ob)(_id INTEGER PRIMARY KEY")
        for (col, _, type) in obattrs
            push!(stmt, "$(col) $(tosql(subpart_type(x, type)))")
        end
        push!(stmt, ");")
        join(stmt, ", ", "")
    end
    DBInterface.execute(conn, join(stmts))
end

conn = DBInterface.connect(LibPQ.Connection, "user=quffaro dbname=acsets")

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
    push!(stmt, "CREATE TABLE IF NOT EXISTS $(o)(_id INTEGER PRIMARY KEY")
    for (col, _, type) in o_attrs
        push!(stmt, ", $(col) $(tosql(subpart_type(g, type)))")
    end
    push!(stmt, ");")
    DBInterface.execute(conn, join(stmt))
end

# second add all homs (REFERENCE)
for (h, h_dom, h_codom) in homs(acset_schema(g))
    DBInterface.execute(conn, """
        ALTER TABLE $(h_dom) ADD $(h) INTEGER REFERENCES $(h_codom)(_id);
    """)
end

DBInterface.execute(conn, "BEGIN;")
LibPQ.load!(
    (_id = 1:length(tables(g)[1].label), label = tables(g)[1].label .|> String, ),
    conn,
    """
    INSERT INTO v VALUES (\$1, \$2);
    """
)
DBInterface.execute(conn, "COMMIT;")

DBInterface.execute(conn, "BEGIN;")
LibPQ.load!(
    (_id = 1:length(tables(g)[2].weight), weight = tables(g)[2].weight, src = tables(g)[2].src, tgt = tables(g)[2].tgt),
    conn,
    """
    INSERT INTO e VALUES (\$1, \$2, \$3, \$4);
    """
)
DBInterface.execute(conn, "COMMIT;")


