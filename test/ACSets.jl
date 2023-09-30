module TestACSetDataStructures
using Test

using StaticArrays: StaticVector
using Tables

using ACSets
using ACSets.Columns: ColumnView

# Discrete dynamical systems
############################

SchDDS = BasicSchema([:X], [(:Φ,:X,:X)])

@abstract_acset_type AbstractDDS
@acset_type DDS(SchDDS, index=[:Φ]) <: AbstractDDS
@acset_type BitSetDDS(SchDDS, part_type=BitSetParts) <: AbstractDDS
@acset_type UnindexedDDS(SchDDS)
@test DDS <: AbstractDDS
@test DDS <: StructACSet
@test DDS <: StructCSet
@test DDS <: ACSet

dds_makers = [
  DDS,
  BitSetDDS,
  UnindexedDDS,
  () -> DynamicACSet("DDS", SchDDS; index=[:Φ]),
  () -> DynamicACSet("DDS", SchDDS; index=[:Φ], part_type=MarkAsDeleted),
  () -> AnonACSet(SchDDS; index=[:Φ]),
  () -> AnonACSet(SchDDS; index=[:Φ], part_type=MarkAsDeleted)
]

for dds_maker in dds_makers
  dds = dds_maker()
  @test keys(tables(dds)) == (:X,)
  @test nparts(dds, :X) == 0
  @test add_part!(dds, :X) == 1
  @test nparts(dds, :X) == 1
  @test incident(dds, 1, :Φ) == Int[]

  set_subpart!(dds, 1, :Φ, 1)
  @test subpart(dds, 1, :Φ) == 1
  @test incident(dds, 1, :Φ) == [1]

  @test add_part!(dds, :X, Φ=1) == 2
  @test add_part!(dds, :X, Φ=1) == 3
  @test subpart(dds, :Φ) == [1,1,1]
  @test subpart(dds, [2,3], :Φ) == [1,1]
  @test subpart(dds, Bool[0,1,1], :Φ) == [1,1]
  @test incident(dds, 1, :Φ) == [1,2,3]

  @test has_part(dds, :X)
  @test !has_part(dds, :nonpart)
  @test has_part(dds, :X, 3)
  @test !has_part(dds, :X, 4)
  @test has_part(dds, :X, 1:5) == [true, true, true, false, false]

  @test has_subpart(dds, :Φ)
  @test !has_subpart(dds, :nonsubpart)
  @test_throws Exception subpart(dds, 1, :nonsubpart)
  @test_throws Exception incident(dds, 1, :nonsuppart)
  @test_throws Exception set_subpart!(dds, 1, :nonsubpart, 1)

  # Types of subset-ed columns.
  dds = dds_maker()
  add_parts!(dds, :X, 3, Φ=[2,3,3])
  dds[1:2,:Φ] isa Vector{Int}
  dds[:Φ] isa Vector{Int}
  dds[[:Φ,:Φ]] isa Vector{Int}
  dds[1:2, [:Φ,:Φ]] isa Vector{Int}
  view(dds,1:2,:Φ) isa ColumnView
  view(dds,:Φ) isa ColumnView

  # Deletion.
  rem_part!(dds, :X, 2)
  @test nparts(dds, :X) == 2
  @test incident(dds, 1, :Φ) == []
  if dds.parts[:X] isa IntParts
    @test subpart(dds, :Φ) == [0,2]
    @test incident(dds, 2, :Φ) == [2]
  else
    @test subpart(dds, :Φ) == [2,3]
    @test incident(dds, 2, :Φ) == [1]
  end 
  rem_part!(dds, :X, 2)
  if dds.parts[:X] isa IntParts
    @test nparts(dds, :X) == 1
    @test subpart(dds, :Φ) == [0]
  else 
    @test nparts(dds, :X) == 2
  end 
  rem_part!(dds, :X, 1)
  if dds.parts[:X] isa IntParts
    @test nparts(dds, :X) == 0
  else 
    @test nparts(dds, :X) == 1
  end

  dds = dds_maker()
  add_parts!(dds, :X, 4, Φ=[2,3,3,4])
  @test_throws ErrorException rem_parts!(dds, :X, [4,1])
  rem_parts!(dds, :X, [1,4])
  if dds.parts[:X] isa IntParts
    @test subpart(dds, :Φ) == [1,1]
    @test incident(dds, 1, :Φ) == [1,2]
    @test incident(dds, 2, :Φ) == []
  end

  # Recursive deletion.
  dds = dds_maker()
  add_parts!(dds, :X, 3, Φ=[2,3,3])
  cascading_rem_part!(dds, :X, 2)
  @test nparts(dds, :X) == 1

  dds = dds_maker()
  add_parts!(dds, :X, 3, Φ=[2,3,3])
  cascading_rem_parts!(dds, :X, [1,2])
  @test nparts(dds, :X) == 1

  # Pretty printing.
  dds = dds_maker()
  add_parts!(dds, :X, 3, Φ=[2,3,3])
  s = sprint(show, dds)
  if !(dds isa AnonACSet)
    @test contains(s, "DDS:")
  end
  @test contains(s, "X = ")
  @test contains(s, "Φ : X → X = ")
  s = sprint(show, dds, context=:compact=>true)
  if !(dds isa AnonACSet)
    @test contains(s, "DDS")
  end
  @test !contains(s, "\n")
  @test contains(s, "X:3")

  s = sprint(show, MIME"text/plain"(), dds)
  if !(dds isa AnonACSet)
    @test contains(s, "DDS")
  end
  @test contains(s, "X:3")
  @test contains(s, "│ X │")

  s = sprint(show, MIME"text/html"(), dds)
  @test startswith(s, "<div class=\"c-set\">")
  @test contains(s, "<table>")
  @test endswith(rstrip(s), "</div>")

  # Special case of pretty print: empty table.
  empty_dds = dds_maker()
  @test isempty(empty_dds)
  @test !isempty(sprint(show, empty_dds))
  @test !isempty(sprint(show, MIME"text/plain"(), empty_dds))
  @test !isempty(sprint(show, MIME"text/html"(), empty_dds))

  # Error handling when adding parts.
  dds = dds_maker()
  add_parts!(dds, :X, 3, Φ=[1,1,1])
  @test !isempty(dds)
  @test_throws AssertionError add_part!(dds, :X, Φ=5)
  @test nparts(dds, :X) == 3
  @test subpart(dds, :Φ) == [1,1,1]
  @test_throws AssertionError add_parts!(dds, :X, 2, Φ=[3,6])
  @test nparts(dds, :X) == 3
  @test incident(dds, 3, :Φ) == []

  # Hashing
  @test hash(dds_maker()) == hash(dds_maker())

  dds = dds_maker()
  add_parts!(dds, :X, 3, Φ=[2,3,3])
  @test hash(dds) != hash(DDS())

  dds2 = dds_maker()
  add_parts!(dds, :X, 3, Φ=[2,3,2])
  @test hash(dds) != hash(dds2)
end

# Dendrograms
#############

# Strictly speaking, this data structure is not a single dendrogram but a forest
# of dendrograms (whose roots are the elements fixed under the `parent` map) and
# in order to be valid dendrograms, there must be no nontrivial cycles and the
# `height` map must satisfy `compose(parent, height) ≥ height` pointwise.

SchDendrogram = BasicSchema([:X], [(:parent,:X,:X)], [:R], [(:height,:X,:R)])

@abstract_acset_type AbstractDendrogram
@acset_type Dendrogram(SchDendrogram, index=[:parent]) <: AbstractDendrogram
@acset_type BSDendrogram(SchDendrogram, part_type=BitSetParts, index=[:parent]) <: AbstractDendrogram

for DendrogramType in (Dendrogram, BSDendrogram)
  @test DendrogramType <: ACSet
  @test DendrogramType <: AbstractDendrogram
  @test DendrogramType{Real} <: AbstractDendrogram{S,Tuple{Real}} where {S}
end

SchLDendrogram = BasicSchema([:X,:L], [(:parent,:X,:X),(:leafparent,:L,:X)],
                             [:R], [(:height,:X,:R)])

@acset_type LDendrogram(SchLDendrogram, index=[:parent, :leafparent]) <: AbstractDendrogram
@acset_type BSLDendrogram(SchLDendrogram, part_type=BitSetParts, index=[:parent, :leafparent]) <: AbstractDendrogram

dgram_makers = [
  (T -> Dendrogram{T}(), T -> LDendrogram{T}()),
  (T -> BSDendrogram{T}(), T -> BSLDendrogram{T}()),
  (T -> DynamicACSet("Dendrogram", SchDendrogram; type_assignment=Dict(:R=>T), index=[:parent]),
   T -> DynamicACSet("LDendrogram", SchLDendrogram; type_assignment=Dict(:R=>T), index=[:parent, :leafparent])
   )
]
for (dgram_maker, ldgram_maker) in dgram_makers
  d = dgram_maker(Int)
  add_parts!(d, :X, 3, height=0)
  add_parts!(d, :R, 2)
  add_parts!(d, :X, 2, height=[10,AttrVar(add_part!(d,:R))])
  set_subpart!(d, 1:3, :parent, 4)
  set_subpart!(d, [4,5], :parent, 5)

  rem_free_vars!(d) # now the added X is AttrVar(1), if IntParts
  @test nparts(d, :R) == 1
  A = AttrVar(only(parts(d, :R)))
  
  @test nparts(d, :X) == 5
  @test subpart(d, 1:3, :parent) == [4,4,4]
  @test subpart(d, 4, :parent) == 5
  @test subpart(d, :, :parent) == [4,4,4,5,5]
  @test incident(d, 4, :parent) == [1,2,3]
  @test incident(d, 4:5, :parent) == [[1,2,3], [4,5]]
  @test has_subpart(d, :height)
  @test subpart(d, [1,2,3], :height) == [0,0,0]
  @test subpart(d, 4, :height) == 10
  @test subpart(d, :, :height) == [0,0,0,10,A]

  # Chained accessors.
  @test subpart(d, 3, [:parent, :parent]) == 5
  @test subpart(d, 3, [:parent, :height]) == 10
  @test incident(d, 5, [:parent, :parent]) == [1,2,3,4,5]
  @test incident(d, 10, [:parent, :height]) == [1,2,3]

  # Indexing syntax.
  @test d[3, :parent] == 4
  @test d[3, [:parent, :height]] == 10
  @test d[3:5, [:parent, :height]] == [10,A,A]
  @test d[:, :parent] == [4,4,4,5,5]
  d2 = copy(d)
  d2[1, :parent] = 1
  d2[2:3, :parent] = 2:3
  @test d2[1:3, :parent] == 1:3
  d2[1:3, :parent] = 4
  @test d2 == d

  # Copying parts.
  d2 = dgram_maker(Int)
  copy_parts!(d2, d, X=[4,5], R=[A.val])
  @test nparts(d2, :X) == 2
  @test subpart(d2, [1,2], :parent) == [2,2]
  @test subpart(d2, [1,2], :height) == [10, AttrVar(1)]

  du = disjoint_union(d, d2)
  @test nparts(du, :X) == 7
  @test subpart(du, :parent) == [4,4,4,5,5,7,7]
  @test subpart(du, :height) == [0,0,0,10,A,10,AttrVar(A.val+1)]

  # Pretty printing of data attributes.
  s = sprint(show, d)
  @test contains(s, "Dendrogram{$Int}:")
  @test contains(s, "height : X → R = ")

  s = sprint(show, MIME"text/plain"(), d)
  @test contains(s, "Dendrogram{$Int}")

  # Allow type inheritance for data attributes.
  d_abs = dgram_maker(Number)
  add_parts!(d_abs, :X, 2, height=[10.0, 4])
  @test subpart(d_abs, :height) == [10.0, 4]

  # Tables interface
  td = tables(d)
  @test keys(td) == (:X,)
  @test Tables.istable(td.X) == true
  cols = Tables.columns(td.X)
  @test Tables.columnnames(cols) == (:parent, :height)
  @test Tables.getcolumn(cols, :parent) == [4,4,4,5,5]
  @test Tables.getcolumn(cols, 1) == [4,4,4,5,5]
  rows = [Tables.rows(td.X)...]
  @test length(rows) == 5
  @test Tables.columnnames(rows[1]) == (:parent, :height)
  @test Tables.getcolumn(rows[1], :parent) == 4
  @test Tables.getcolumn(rows[1], :height) == 0
  @test Tables.getcolumn(rows[1], 1) == 4
  @test parent(td.X) == d
  @test parent(Tables.rows(td.X)[1]) == d
  @test idx(rows[1]) == 1

  # Copying between C-sets and C′-sets with C != C′.
  ld = ldgram_maker(Int)
  copy_parts!(ld, d)
  @test nparts(ld, :L) == 0
  @test subpart(ld, :parent) == subpart(d, :parent)

  add_parts!(ld, :L, 3, leafparent=Number[2,3,4])
  @test subpart(ld, :leafparent) == [2,3,4]
  d′ = dgram_maker(Int)
  copy_parts!(d′, ld)

  @test d′[:parent] == d[:parent]
  @test d[:height] == [0,0,0,10,A]
  @test d′[:height] == [0,0,0,10,AttrVar(1)]
end

# Subsets
#########

# A set together with a subset, with unique indexing for fast membership checks.

SchSubset = BasicSchema([:Set,:Sub], [(:ι,:Sub,:Set)])

@acset_type Subset(SchSubset, unique_index=[:ι])

Base.in(x::Integer, X::Subset) = !isempty(incident(X, x, :ι))

A = Subset()
add_parts!(A, :Set, 2)
add_part!(A, :Sub, ι=1)
B = Subset()
add_parts!(B, :Set, 2)
add_part!(B, :Sub, ι=2)
@test 1 ∈ A && 2 ∉ A
@test 1 ∉ B && 2 ∈ B
@test incident(A, :, :ι) == [[1], []]
@test incident(B, :, :ι) == [[], [1]]

rem_part!(A, :Set, 2)
@test 1 ∈ A
rem_part!(B, :Set, 1)
@test 1 ∈ B

# Labeled sets
##############

# The simplest example of a C-set with a data attribute, to test data indexing.
SchLabeledSet = BasicSchema([:X], [], [:Label], [(:label,:X,:Label)])

# Labeled sets with index
#------------------------

@acset_type IndexedLabeledSet(SchLabeledSet, index=[:label])

lset_makers = [
  T -> IndexedLabeledSet{T}(),
  T -> DynamicACSet("IndexedLabeledSet", SchLabeledSet; type_assignment=Dict(:Label=>T), index=[:label])
]

for lset_maker in lset_makers
  lset = lset_maker(Symbol)
  add_parts!(lset, :X, 2, label=[:foo, :bar])
  @test subpart(lset, :, :label) == [:foo, :bar]
  @test incident(lset, :foo, :label) == [1]
  @test isempty(incident(lset, :nonkey, :label))

  add_part!(lset, :X, label=:foo)
  @test incident(lset, :foo, :label) == [1,3]
  set_subpart!(lset, 1, :label, :baz)
  @test subpart(lset, 1, :label) == :baz
  @test incident(lset, [:foo,:baz], :label) == [[3],[1]]
  set_subpart!(lset, 3, :label, :biz)
  @test incident(lset, :foo, :label) == []

  # Labeled set with compound label (tuple).
  lset = lset_maker(Tuple{Int,Int})
  add_parts!(lset, :X, 2, label=[(1,1), (1,2)])
  @test incident(lset, (1,2), :label) == [2]

  # Deletion with indexed data attribute.
  lset = lset_maker(Symbol)
  add_parts!(lset, :X, 3, label=[:foo, :foo, :bar])
  rem_part!(lset, :X, 1)
  @test subpart(lset, :label) == [:bar, :foo]
  @test incident(lset, [:foo, :bar], :label) == [[2], [1]]

  # Deletion with unitialized data attribute.
  lset = lset_maker(Tuple{Int,Int})
  add_part!(lset, :X)
  rem_part!(lset, :X, 1)
  @test nparts(lset, :X) == 0

  # Shallow copying is shallow.
  lset = lset_maker(Vector{Symbol})
  label = [:foo]
  add_part!(lset, :X, label=label)
  @test subpart(copy(lset), 1, :label) === label

  # Pretty-printing with unitialized data attribute.
  lset = lset_maker(Symbol)
  add_part!(lset, :X)
  @test contains(sprint(show, lset), "nothing")
  @test contains(sprint(show, MIME"text/plain"(), lset), "nothing")
  @test contains(sprint(show, MIME"text/html"(), lset), "nothing")
end

# Labeled sets with unique index
#-------------------------------

@acset_type UniqueIndexedLabeledSet(SchLabeledSet, unique_index=[:label])

lset_makers = [
  T -> UniqueIndexedLabeledSet{T}(),
  T -> DynamicACSet("UniqueIndexedLabeledSet", SchLabeledSet;
                    type_assignment=Dict(:Label=>T), unique_index=[:label])
]

for lset_maker in lset_makers
  lset = lset_maker(Symbol)
  add_parts!(lset, :X, 2, label=[:foo, :bar])
  @test subpart(lset, :, :label) == [:foo, :bar]
  @test incident(lset, :foo, :label) isa StaticVector
  @test incident(lset, :foo, :label) == [1]
  @test incident(lset, [:foo,:bar], :label) == [[1],[2]]
  @test isempty(incident(lset, :nonkey, :label))

  set_subpart!(lset, 1, :label, :baz)
  @test subpart(lset, 1, :label) == :baz
  @test incident(lset, :baz, :label) == [1]
  @test isempty(incident(lset, :foo, :label))

  @test_throws Exception set_subpart!(lset, 1, :label, :bar)
  set_subpart!(lset, :, :label, [:bar, :foo])
  @test subpart(lset, :, :label) == [:bar, :foo]
end

SchDecGraph = BasicSchema([:E,:V], [(:src,:E,:V),(:tgt,:E,:V)],
                          [:X], [(:dec,:E,:X)])

@acset_type DecGraph(SchDecGraph, index=[:src,:tgt])

g = @acset DecGraph{String} begin
  V = 4
  E = 4
  src = [1,2,3,4]
  tgt = [2,3,4,1]
  dec = ["a","b","c","d"]
end

@test nparts(g, :V) == 4
@test subpart(g, :, :src) == [1,2,3,4]
@test incident(g, 1, :src) == [1]

function path_graph(n::Int)
  @acset DecGraph{Float64} begin
    V = n
    E = n-1
    src = 1:n-1
    tgt = 2:n
    dec = zeros(n-1)
  end
end

pg = path_graph(30)
@test (nparts(pg, :V), nparts(pg, :E)) == (30, 29)
@test incident(pg, 1, :src) == [1]

n = 4
pg = @acset DecGraph{Tuple{Int,Int}} begin
  V = n
  E = n-1
  src = 1:n-1
  tgt = 2:n
  dec = zip(1:n-1, 2:n)
end
@test pg[:dec] == [(1,2), (2,3), (3,4)]

# Acset macro
#------------

h = @acset DecGraph{String} begin
  V = 4
  E = 4
  src = [1,2,3,4]
  tgt = [2,3,4,1]
  dec = ["b","c","d","a"]
end

k = @acset DecGraph{String} begin
  V = 2
  E = 2
  src = [1,1]
  tgt = [2,2]
  dec = ["a","a"]
end
l = @acset DecGraph{String} begin
  V = 2
  E = 2
  src = [1,2]
  tgt = [1,2]
  dec = ["a","a"]
end

# Test mapping
#-------------

f(s::String) = Int(s[1])

h1 = map(g, dec = f)
h2 = map(g, X = f)

@test h1 == h2

@test subpart(h1,:src) == subpart(g,:src)
@test typeof(h1).super.parameters[2] == Tuple{Int}
@test subpart(h1,:dec) == f.(["a","b","c","d"])

SchLabeledDecGraph = BasicSchema([:E,:V], [(:src,:E,:V),(:tgt,:E,:V)],
                                 [:X], [(:dec,:E,:X),(:label,:V,:X)])

@acset_type LabeledDecGraph(SchLabeledDecGraph, index=[:src,:tgt])

g = @acset LabeledDecGraph{String} begin
  V = 4
  E = 4

  src = [1,2,3,4]
  tgt = [2,3,4,1]

  dec = ["a","b","c","d"]
  label = ["w", "x", "y", "z"]
end

h1 = map(g, X = f)
@test subpart(h1,:label) == f.(["w","x","y","z"])

h2 = map(g, dec = f, label = i -> 3)
@test subpart(h2,:dec) == f.(["a","b","c","d"])
@test subpart(h2,:label) == [3,3,3,3]

@test_throws Exception map(g, dec = f)

# Garbage collection
####################

@acset_type MadDecGraph(SchDecGraph, part_type=MarkAsDeleted, index=[:src,:tgt])

g = @acset MadDecGraph{String} begin
  V = 4; E = 4; X=3
  src = [1,2,3,4]
  tgt = [2,3,4,1]
  dec = ["a","b",AttrVar(3),AttrVar(1)]
end

rem_parts!(g, :E, [1,4])
rem_part!(g, :V, 1)
rem_part!(g, :X, 1)
@test g[:src] == [2,3]
@test g[:tgt] == [3,4]
@test g[:dec] == ["b",AttrVar(3)]
@test gc!(g) == Dict(:V=>[2,3,4], :E=>[2,3], :X=>[2,3])
@test g[:src] == [1,2]
@test g[:tgt] == [2,3]
@test g[:dec] == ["b",AttrVar(2)]

# Densify and sparsify 
g = @acset MadDecGraph{String} begin
  V = 4
  E = 4
  src = [1,2,3,4]
  tgt = [2,3,4,1]
  dec = ["a","b","c","d"]
end

rem_parts!(g, :E, [1,4])
rem_part!(g, :V, 1)

g′, _ = densify(g)
g′′ = sparsify(g′)
@test g′ isa ACSet{<:DenseParts}
@test g′′ isa ACSet{<:MarkAsDeleted}
@test g′[:src] == [1,2]
@test g′′[:src] == [1,2]

# Recursive deletion
#-------------------

RecSch = BasicSchema(
  [:Thing,:Node,:Edge], [(:src,:Edge,:Node),(:tgt,:Edge,:Node),(:thing,:Thing,:Node)],
  [],[]
)

@acset_type RecDataInj(RecSch, index=[:src,:tgt], unique_index=[:thing])
@acset_type RecDataIdx(RecSch, index=[:src,:tgt,:thing])
@acset_type RecDataNoIdx(RecSch)

datainj = @acset RecDataInj begin
    Thing=3
    Node=3
    Edge=3
    thing=[1,2,3]
    src=[1,1,2]
    tgt=[1,2,3]
end

dataidx = @acset RecDataIdx begin
  Thing=3
  Node=3
  Edge=3
  thing=[1,2,3]
  src=[1,1,2]
  tgt=[1,2,3]
end

datanoidx = @acset RecDataNoIdx begin
  Thing=3
  Node=3
  Edge=3
  thing=[1,2,3]
  src=[1,1,2]
  tgt=[1,2,3]
end

map_inj = cascading_rem_parts!(datainj, :Node, 1)
map_idx = cascading_rem_parts!(dataidx, :Node, 1)
map_noidx = cascading_rem_parts!(datanoidx, :Node, 1)

@test map_inj == map_idx
@test map_idx == map_noidx

@test nparts(datainj,:Thing) == 2
@test nparts(datainj,:Node) == 2
@test nparts(datainj,:Edge) == 1
@test incident(datainj, 3, :thing) == []
@test incident(datainj, 3, :src) == []
@test incident(datainj, 3, :tgt) == []

end
