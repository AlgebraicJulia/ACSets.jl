using ACSets
using Catlab
#
using MLStyle
using SQLite
using DataFrames
using DBInterface

tosql(::Type{<:Real}) = "REAL"
tosql(::Type{<:AbstractString}) = "TEXT"
tosql(::Type{<:Symbol}) = "TEXT"
tosql(::Type{<:Integer}) = "INTEGER"
tosql(T::DataType) = error("$T could not be less supported")

tosql(::Nothing) = "NULL"
tosql(s::Symbol) = string(s)
tosql(x) = x

# column order important

function selectfrom(db::SQLite.DB, table)
    query = DBInterface.execute(db, "SELECT * FROM $table")
    DataFrames.DataFrame(query)
end

db = SQLite.DB()

DBInterface.execute(db, """
    CREATE TABLE IF NOT EXISTS artist(
        artistid    INTEGER PRIMARY KEY,
        artistname  TEXT
    );
    CREATE TABLE IF NOT EXISTS track(
        trackid     INTEGER,
        trackname   TEXT,
        trackartist INTEGER,
        FOREIGN KEY(trackartist) REFERENCES artist(artistid)
    );
    """)

macro sql_str(str)
    split(str, ";\n")
end

stmts = sql"""
    CREATE TABLE IF NOT EXISTS artist(
        artistid    INTEGER PRIMARY KEY,
        artistname  TEXT
    );
    CREATE TABLE IF NOT EXISTS track(
        trackid     INTEGER,
        trackname   TEXT,
        trackartist INTEGER,
        FOREIGN KEY(trackartist) REFERENCES artist(artistid)
    );"""


# could have a string macro that parses these
function DBInterface.execute(db::SQLite.DB, xs::Vector{<:AbstractString})
    DBInterface.execute.(Ref(db), xs)
end

DBInterface.execute(db, stmts)

selectfrom(db, "artist")
selectfrom(db, "track")


##
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

#############
# brew services restart mysql
# sudo mysql
using MySQL

@present SchWeightedLabeledGraph <: SchLabeledGraph begin
    Weight::AttrType
    weight::Attr(E,Weight)
end
@acset_type WeightedLabeledGraph(SchWeightedLabeledGraph, index=[:src, :tgt]) <: AbstractLabeledGraph
g = erdos_renyi(WeightedLabeledGraph{Symbol,Float64}, 10, 0.25)
g[:, :label] = Symbol.(collect('a':'z')[1:nv(g)]);
g[:, :weight] = floor.(rand(ne(g)) .* 100);

@data SQL begin
    Insert()
    Select()
    Alter()
end

"""Builds a SQL statement"""
function build end
# This dispatches on the connection type and SQL sum type.

# let's open the connection
# conn = DBInterface.connect(MySQL.Connection, "localhost", "root", db="acsets")
conn = DBInterface.connect(MySQL.Connection, "localhost", "mysql", db="acsets", unix_socket="/var/run/mysqld/mysqld.sock")


# TODO add where statement
function build(::MySQL.Connection, ::Type{Select}, table::Symbol, 
        cols::Vector{Symbol}=Vector{Symbol}())
    columns = !isempty(cols) ? join(cols, ", ") : "*"
    "SELECT $columns FROM $table"
end

# we will use this to test that a table has been written
function selectfrom(conn::MySQL.Connection, table::Symbol, cols::Vector{Symbol}=Vector{Symbol}())
    stmt = build(conn, Select, table, cols)
    query = DBInterface.execute(conn, stmt)
    DataFrames.DataFrame(query)
end

# here we assume that the primary key is always `_id`.
function maketables(conn::MySQL.Connection, x::ACSet)::String
    schema = acset_schema(x)
    result = Vector{String}()
    # build tables
    foreach(objects(schema)) do ob
        obattrs = attrs(schema; from=ob)
        stmt = Vector{String}()
        # create table
        push!(stmt, "CREATE TABLE IF NOT EXISTS $(ob)(_id INTEGER PRIMARY KEY")
        # homs are also columns. they say `E.src` must have a value in `V._id`,
        # likewise `E.tgt`
        for (col, src, tgt) in homs(schema; from=ob)
            tgttype = tosql(Int) # assuming tgt is _id
            push!(stmt, "$(col) $tgttype")
        end
        for (col, _, type) in obattrs
            push!(stmt, "$(col) $(tosql(subpart_type(x, type)))")
        end
        push!(stmt, "); ")
        push!(result, join(stmt, ", ", ""))
    end
    # build table relations
    foreach(homs(schema)) do (ref, refdom, refcodom)
        push!(result, build(conn, Alter, ref, refdom, refcodom))
    end
    join(result, "")
end

function build(::MySQL.Connection, ::Type{Alter}, ref, refdom, refcodom)
    "ALTER TABLE $refdom ADD CONSTRAINT fk_$(ref) FOREIGN KEY ($ref) REFERENCES $refcodom(_id); "
end

function maketables!(conn::MySQL.Connection, x::ACSet)
    stmt = maketables(conn, x)
    DBInterface.execute(conn, stmt)
end

maketables!(conn, g)
# @assert typeof(selectfrom(conn, :E)) isa DataFrame

# get attrs
getattrs(g::ACSet, table::Symbol) = first.(filter(attrs(acset_schema(g))) do (attr, tbl, _)
    table == tbl
end)

gethoms(x::ACSet, table::Symbol) = first.(homs(acset_schema(x); from=table))

struct Values
    table::Symbol
    x::Vector{Any}
end

# values should store more data
# FIXME should be smarter. Values(_, [1,2,3]) => "VALUES (1), (2), (3)"
String(vals::Values) = "VALUES " * join(entuple.(vals.x), ", ") * ";"

entuple(xs::Vector)::String = length(xs) == 1 ? "($(only(xs)))" : "$(Tuple(xs))"

# Values should have a method which turns single values into "(1)"
function getrows(x::ACSet, table::Symbol)
    cols = gethoms(x, table) ∪ getattrs(x, table)
    x = map(parts(x, table)) do id
        [id, tosql.(subpart.(Ref(x), Ref(id), cols))...]
    end
    Values(table, x)
end

# DIGRESSION could also support this
t=tables(g)
rows=getproperty(t, :E)
cols=propertynames(rows)
# ##################################

# FIXME Set
function colnames(x::ACSet, table::Symbol)::String
    homnames = first.(homs(acset_schema(x); from=table))
    gattrs = getattrs(x, table)
    # I don't like this as it assumes the order of the columns would agree
    cols = [:_id, (homnames ∪ gattrs)...]
    """($(join(cols, ", ")))"""
end

function build(::MySQL.Connection, ::Type{Insert}, table::Symbol, 
        cols::AbstractVector{Symbol}, values::AbstractVector{<:AbstractVector})
    "INSERT IGNORE INTO $table ($(join(cols, ", "))) VALUES " * join(entuple.(tosql.(values)), ", ") * ";"
end

function build(::MySQL.Connection, ::Type{Insert}, x::ACSet, table::Symbol)
    cols = colnames(x, table)
    values = getrows(x, table)
    "INSERT IGNORE INTO $table $cols " * String(getrows(x, table))
    # upserting could also be an option
end

# we can get a dependency graph based on the ACSet
function build(conn::MySQL.Connection, ::Type{Insert}, x::ACSet)
    schema = acset_schema(x)
    stmts = map(objects(schema)) do ob
        build(conn, Insert, x, ob)
    end
    join(stmts, " ")
end

function Base.insert!(conn::MySQL.Connection, x::ACSet, stmt::Union{String, Nothing}=nothing)
    _stmt = isnothing(stmt) ? build(conn, Insert, x) : stmt
    DBInterface.executemultiple(conn, _stmt)
end

build(conn, Insert, jd)

# test
stmt = build(conn, Insert, g, :V)
insert!(conn, g, stmt)

stmt = build(conn, Insert, g, :E)
insert!(conn, g, stmt)
#

########
@present SchJunct(FreeSchema) begin
    Name::AttrType
    Color::AttrType
    #
    Student::Ob
    name::Attr(Student, Name)
    favoriteSubject::Attr(Student, Name)
    favoriteColor::Attr(Student, Color)
    #
    Class::Ob
    subject::Attr(Class, Name)
    #
    Junct::Ob
    student::Hom(Junct,Student)
    class::Hom(Junct,Class)
end
@acset_type JunctionData(SchJunct, index=[:name])
jd = JunctionData{Symbol, Symbol}()

maketables!(conn, jd)

df = Dict(:Fiona => [:Math, :Philosophy, :Music],
          :Gregorio => [:Cooking, :Math, :CompSci],
          :Heather => [:Gym, :Art, :Music, :Math])

#=
Now we need to add this data to the junction table. The process for adding table should do the following:
1. For each student `(keys(df`) let's get their classes (`df[student]`) and add the student into the ACSet.
2. For each class, let's see whether it's already present in the ACSet by getting its ID.
3. If the ID value is empty, then the class is not there. We add the class to the table and save the ID. Otherwise if the class is there, we don't need to do anything.
4. Now we add the association between the student and class by adding their respective IDs to the Junction table.
This algorithm is realized in nine lines of code:
=#

foreach(keys(df)) do student
    classes = df[student]
    # let's make this idempotent by adding student only if they aren't in the system
    student_id = incident(jd, student, :name)
    if isempty(student_id); student_id = add_part!(jd, :Student, name=student) end
    # for each of the classes the student has...
    foreach(classes) do class
        # idempotently add their class
        class_id = incident(jd, class, :subject)
        if isempty(class_id); class_id = add_part!(jd, :Class, subject=class) end
        # enforce pair constraint
        id_pair = incident(jd, only(student_id), :student) ∩ incident(jd, only(class_id), :class)
        isempty(id_pair) && add_part!(jd, :Junct, student=student_id, class=only(class_id))
    end
end

#=
Let's check that it worked:
=#

jd



####
stmt = "INSERT IGNORE INTO Student (_id, name, favoriteSubject, favoriteColor) VALUES (1, \"Fiona\", \"NULL\", \"NULL\"), (2, \"Gregorio\", \"NULL\", \"NULL\"), (3, \"Heather\", \"NULL\", \"NULL\"); INSERT IGNORE INTO Class (_id, subject) VALUES (1, \"Math\"), (2, \"Philosophy\"), (3, \"Music\"), (4, \"Cooking\"), (5, \"CompSci\"), (6, \"Gym\"), (7, \"Art\"); INSERT IGNORE INTO Junct (_id, student, class) VALUES (1, 1, 1), (2, 1, 2), (3, 1, 3), (4, 2, 4), (5, 2, 1), (6, 2, 5), (7, 3, 6), (8, 3, 7), (9, 3, 3), (10, 3, 1);"


DBInterface.executemultiple(conn, stmt)


stmt = build(conn, Insert, jd, :Class)
stmt = build(conn, Insert, jd, :Junct)

insert!(conn, jd)

### Read into an ACSet
selectfrom(conn, :Junct)

# XXX piratical
function Base.read(conn::MySQL.Connection, ::StructACSet{T}, table::Symbol) where T
    tables = @match [T.parameters...] begin
        [symbol, tables, kwargs...] => [tables.parameters...]
        _ => nothing
    end
    # we can use the type information to check 
    # if the table *might* be in the schema
    @assert table ∈ tables
    selectfrom(conn, table)
end

#####################
### ACSets Interface
#####################

####
tabular = tables(jd)
t = Tables.rows(tabular)

#### Reading large data materialized elsewhere
mutable struct HugeData
    conn::DBInterface.Connection
    acsettype::Type{<:StructACSet}
    view::DataFrames.DataFrame
end
# TODO we need to convert the `view` into an ACSet

function HugeData(conn::DBInterface.Connection, acs::SimpleACSet)
    HugeData(conn, typeof(acs))
end

j = HugeData(conn, typeof(jd))

# get the number of rows
function ACSets.nparts(acset::HugeData, table::Symbol)
    query = DBInterface.execute(acset.conn, "SELECT COUNT(*) FROM $table;")
    DataFrames.DataFrame(query) # we need to send this to a 
end

function ACSets.maxpart(acset::HugeData, table::Symbol) end

function ACSets.subpart(acset::HugeData, 
        table::Symbol, cols::AbstractVector{Symbol})
    query = DBInterface.execute(acset.conn, build(acset.conn, Select, table, cols))
    DataFrames.DataFrame(query)
end

# incident

build(conn, Insert, :Junct, [:student, :class], [[1,2], [3,4], [5,6]])

function ACSets.add_part!(acset::HugeData, table::Symbol, cols::AbstractVector{Symbol}, values::AbstractVector{<:AbstractVector})
    stmt = build(acset.conn, Insert, table, cols, values)
    query = DBInterface.execute(acset.conn, stmt)
    DataFrames.DataFrames(query)
end

add_part!(j, :Class, [:_id, :subject], [[8, :Spoils]])
