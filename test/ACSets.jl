module TestACSetDataStructures
using Test

using StaticArrays: StaticVector, SA
using Tables
using Random

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

# Test cascading rem part results in a natural transformation
#------------------------------------------------------------
DDS(i::Int) = DDS(rand(1:i, i))

function DDS(v::Vector{Int})
  x = DDS()
  add_parts!(x, :X, length(v))
  set_subpart!(x, :Φ, v)
  x
end

function cascading_delete_is_natural(i::Int)
  X = DDS(i)
  X′ = copy(X)
  d = cascading_rem_part!(X, :X, 1)[:X]
  all(enumerate(X[:Φ])) do (i, ϕᵢ)
    X′[d[i], :Φ] == d[ϕᵢ]
  end
end

Random.seed!(100)

for _ in 1:100
  @test cascading_delete_is_natural(10)
end

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
  @test dds[1:2,:Φ] isa Vector{Int}
  @test dds[:Φ] isa Vector{Int}
  @test dds[[:Φ,:Φ]] isa Vector{Int}
  v = dds[:Φ]
  v[1] = 1
  @test v != dds[:Φ]
  @test dds[1:2, [:Φ,:Φ]] isa Vector{Int}
  @test view(dds,1:2,:Φ) isa ColumnView
  @test view(dds,:Φ) isa ColumnView

  # Deletion.
  @test_throws ArgumentError undefined_subparts(dds, :X)
  @test undefined_subparts(dds, :Φ) == []
  rem_part!(dds, :X, 2)
  @test undefined_subparts(dds, :Φ) == [1]
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

  # delete all parts of an Ob
  dds = dds_maker()
  add_parts!(dds, :X, 4, Φ=[2,3,3,4])
  rem_parts!(dds, :X, :)
  @test nparts(dds, :X) == 0

  # Recursive deletion.
  dds = dds_maker()
  add_parts!(dds, :X, 3, Φ=[2,3,3])
  cascading_rem_part!(dds, :X, 2)
  @test nparts(dds, :X) == 1

  dds = dds_maker()
  add_parts!(dds, :X, 3, Φ=[2,3,3])
  cascading_rem_parts!(dds, :X, [1,2])
  @test nparts(dds, :X) == 1

  dds = dds_maker()
  add_parts!(dds, :X, 3, Φ=[2,3,3])
  cascading_rem_parts!(dds, :X, :)
  @test nparts(dds, :X) == 0

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
  @test subpart_type(d,:R) == Int

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
  @test subpart_type(lset, :label) == Symbol
  @test subpart_type(lset, :Label) == Symbol
  @test_throws Exception subpart_type(lset, :abel)

  # Labeled set with compound label (tuple).
  lset = lset_maker(Tuple{Int,Int})
  add_parts!(lset, :X, 2, label=[(1,1), (1,2)])
  @test incident(lset, (1,2), :label) == [2]
  @test subpart_type(lset, :label) == Tuple{Int,Int}
  @test subpart_type(lset, :Label) == Tuple{Int,Int}

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

# errors when user inputs nonexistent names
@test_throws Exception @acset DDS begin
  W=3
  Φ=[1,2,3]
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

# Recursive deletion with indexing
#---------------------------------

RecSch = BasicSchema(
  [:Thing,:Node,:Edge], [(:src,:Edge,:Node),(:tgt,:Edge,:Node),(:thing,:Thing,:Node)],
  [],[]
)

# DenseParts
@acset_type RecDataInj(RecSch, index=[:src,:tgt], unique_index=[:thing])
@acset_type RecDataIdx(RecSch, index=[:src,:tgt,:thing])
@acset_type RecDataNoIdx(RecSch)

recdata_makers = [
  RecDataInj,
  RecDataIdx,
  RecDataNoIdx,
  () -> DynamicACSet("RecData", RecSch; index=[:src,:tgt]),
  () -> AnonACSet(RecSch; index=[:src,:tgt])
]

for recdata in recdata_makers
  mydata = recdata()
  add_parts!(mydata, :Node, 3)
  add_parts!(mydata, :Thing, 3, thing=[1,2,3])
  add_parts!(mydata, :Edge, 3, src=[1,1,2], tgt=[1,2,3])

  @test parts_type(mydata) <: DenseParts

  new2old = cascading_rem_parts!(mydata, :Node, 1)

  @test length(new2old[:Thing]) == nparts(mydata,:Thing)
  @test nparts(mydata,:Thing) == 2
  @test length(new2old[:Node]) == nparts(mydata,:Node)
  @test nparts(mydata,:Node) == 2
  @test length(new2old[:Edge]) == nparts(mydata,:Edge)
  @test nparts(mydata,:Edge) == 1
  @test incident(mydata, 3, :thing) == []
  @test incident(mydata, 3, :src) == []
  @test incident(mydata, 3, :tgt) == []
end

# MarkAsDeleted parts
@acset_type RecDataInjMarkDel(RecSch, index=[:src,:tgt], unique_index=[:thing], part_type=BitSetParts)
@acset_type RecDataIdxMarkDel(RecSch, index=[:src,:tgt,:thing], part_type=BitSetParts)
@acset_type RecDataNoIdxMarkDel(RecSch, part_type=BitSetParts)

recdata_makers = [
  RecDataInjMarkDel,
  RecDataIdxMarkDel,
  RecDataNoIdxMarkDel,
  () -> DynamicACSet("RecData", RecSch; index=[:src,:tgt], part_type=MarkAsDeleted),
  () -> AnonACSet(RecSch; index=[:src,:tgt], part_type=MarkAsDeleted)
]

for recdata in recdata_makers
  mydata = recdata()
  add_parts!(mydata, :Node, 3)
  add_parts!(mydata, :Thing, 3, thing=[1,2,3])
  add_parts!(mydata, :Edge, 3, src=[1,1,2], tgt=[1,2,3])

  @test parts_type(mydata) <: MarkAsDeleted

  new2old = cascading_rem_parts!(mydata, :Node, 1)

  @test length(new2old[:Thing]) == nparts(mydata,:Thing)
  @test nparts(mydata,:Thing) == 2
  @test length(new2old[:Node]) == nparts(mydata,:Node)
  @test nparts(mydata,:Node) == 2
  @test length(new2old[:Edge]) == nparts(mydata,:Edge)
  @test nparts(mydata,:Edge) == 1
  # IDs are not updated in MarkAsDeleted, so "1" is the deleted element
  @test incident(mydata, 1, :thing) == []
  @test incident(mydata, 1, :src) == []
  @test incident(mydata, 1, :tgt) == []
end

# attributes and an injective index
RecAttrSch = BasicSchema(
  [:Thing,:Node,:Edge], [(:src,:Edge,:Node),(:tgt,:Edge,:Node),(:thing,:Thing,:Node)],
  [:Attr1,:Attr2,:Attr3],[(:attr1,:Node,:Attr1),(:attr2,:Edge,:Attr2),(:attr3,:Thing,:Attr3)]
)

@acset_type RecAttrData(RecAttrSch, index=[:src,:tgt], unique_index=[:thing])

dataattr = @acset RecAttrData{String,Symbol,Float64} begin
  Thing=3
  Node=3
  Edge=3
  thing=[1,2,3]
  src=[1,1,2]
  tgt=[1,2,3]
  attr1=["1","2","3"]
  attr2=[:a,:b,:c]
  attr3=[10.0,11.0,12.0]
end

cascading_rem_parts!(dataattr, :Node, 1)

@test all(map(x -> x ∈ subpart(dataattr,:attr1), ["2","3"]))
@test only(subpart(dataattr,:attr2)) == :c
@test all(map(x -> x ∈ subpart(dataattr,:attr3), [12.0,11.0]))

# Composites of subparts for subpart
#-----------------------------------

# StructACSet
CompositesSch = BasicSchema([:X,:Y,:Z,:W], [(:f,:X,:Y), (:g,:Y,:Z), (:h,:X,:W)], [:Zattr], [(:zattr,:Z,:Zattr)])
@acset_type CompositesData(CompositesSch, index=[:f,:g,:h])
datcomp = @acset CompositesData{Symbol} begin
  X=5
  Y=4
  Z=3
  W=3
  f=[1,2,3,1,2]
  g=[3,2,1,3]
  h=[1,2,3,1,2]
  zattr=[:a,:b,:c]
end

@test subpart(datcomp, :, (:f,:g)) == [3,2,1,3,2]
@test subpart(datcomp, :, (:f,:g,:zattr)) == [:c,:b,:a,:c,:b]
@test subpart(datcomp, (:f,:g)) == [3,2,1,3,2]
@test subpart(datcomp, 1:5, (:f,:g)) == [3,2,1,3,2]
@test subpart(datcomp, 1, (:f,:g)) == 3

@test_throws Exception subpart(datcomp, :, (:f,:h))
@test_throws Exception subpart(datcomp, (:f,:h))
@test_throws Exception subpart(datcomp, 1, (:f,:h))

@test datcomp[:, (:f,:g)] == [3,2,1,3,2]
@test datcomp[1:5, (:f,:g)] == [3,2,1,3,2]
@test datcomp[1, (:f,:g)] == 3

@test_throws Exception datcomp[:, (:f,:h)]
@test_throws Exception datcomp[1, (:f,:h)]

@test subpart(datcomp, 1, (:f,)) == 1
@test subpart(datcomp, :, (:f,)) == [1,2,3,1,2]
@test subpart(datcomp, (:f,)) == [1,2,3,1,2]

# DynamicACSet
datcompdyn = DynamicACSet(datcomp)

@test subpart(datcompdyn, :, (:f,:g)) == [3,2,1,3,2]
@test subpart(datcompdyn, :, (:f,:g, :zattr)) == [:c,:b,:a,:c,:b]
@test subpart(datcompdyn, (:f,:g)) == [3,2,1,3,2]
@test subpart(datcompdyn, 1:5, (:f,:g)) == [3,2,1,3,2]
@test subpart(datcompdyn, 1, (:f,:g)) == 3

@test_throws Exception subpart(datcompdyn, :, (:f,:h))
@test_throws Exception subpart(datcompdyn, (:f,:h))
@test_throws Exception subpart(datcompdyn, 1, (:f,:h))

@test datcompdyn[:, (:f,:g)] == [3,2,1,3,2]
@test datcompdyn[1:5, (:f,:g)] == [3,2,1,3,2]
@test datcompdyn[1, (:f,:g)] == 3

@test_throws Exception datcompdyn[:, (:f,:h)]
@test_throws Exception datcompdyn[1, (:f,:h)]

@test subpart(datcompdyn, 1, (:f,)) == 1
@test subpart(datcompdyn, :, (:f,)) == [1,2,3,1,2]
@test subpart(datcompdyn, (:f,)) == [1,2,3,1,2]

# Composites of subparts for incident
#------------------------------------

# StructACSet
@test incident(datcomp, 1, (:g,)) == [3]
@test incident(datcomp, 3, (:g,)) == [1,4]
@test incident(datcomp, 1:3, (:g,)) == [3,2,1,4]
@test incident(datcomp, 1:3, (:f,:g)) == [3,2,5,1,4]
@test incident(datcomp, 1:3, (:f,:g))  == incident(datcomp, 1:3, [:f,:g])

@test incident(datcomp, [:a,:b,:c], (:f,:g,:zattr)) == incident(datcomp, [:a,:b,:c], [:f,:g,:zattr])

@test_throws Exception incident(datcomp, 1, (:h,:g))

# DynamicACSet
@test incident(datcompdyn, 1, (:g,)) == [3]
@test incident(datcompdyn, 3, (:g,)) == [1,4]
@test incident(datcompdyn, 1:3, (:g,)) == [3,2,1,4]
@test incident(datcompdyn, 1:3, (:f,:g)) == [3,2,5,1,4]
@test incident(datcompdyn, [:a,:b,:c], (:f,:g,:zattr)) == [3,2,5,1,4]

@test incident(datcompdyn, [:a,:b,:c], (:f,:g,:zattr)) == incident(datcompdyn, [:a,:b,:c], [:f,:g,:zattr])

@test_throws Exception incident(datcompdyn, 1, (:h,:g))

# Test @acset_type with type parameters
#--------------------------------------
# Single
@acset_type IntLabeledSet(SchLabeledSet, index=[:label]){Int}
@test isempty(IntLabeledSet())

# Multiple
SchLabeledDecGraph′ = BasicSchema([:E,:V], [(:src,:E,:V),(:tgt,:E,:V)],
                                 [:X, :Y], [(:dec,:E,:X),(:label,:V,:Y)])
"""Example Docstring"""
@acset_type SymSymDecGraph(SchLabeledDecGraph′){Symbol,Symbol}
@test strip(string(@doc SymSymDecGraph)) == """Example Docstring"""

@acset SymSymDecGraph begin V=1; E=1; src=1; tgt=1; dec=[:a]; label=[:b] end

# Cast Dynamic <-> StructACSet
#------------------------------
g = @acset MadDecGraph{String} begin
  V = 4; E = 4; X=3
  src = [1,2,3,4]
  tgt = [2,3,4,1]
  dec = ["a","b",AttrVar(3),AttrVar(1)]
end

@test g isa StructACSet

dyn_g = DynamicACSet(g)

@test dyn_g isa DynamicACSet
@test nparts(dyn_g, :X) == 3

g′ = StructACSet(dyn_g)

@test g′ isa AnonACSet
@test nparts(g′, :X) == 3

# Test edge cases where attrtype is a vector
SchLDDS = BasicSchema([:X], [(:Φ,:X,:X)],[:Y],[(:l,:X,:Y)])
@acset_type AbsLDDS(SchLDDS)
const LDDS = AbsLDDS{StaticVector}  # DDS w/ labeled states

X = @acset LDDS begin
  X = 2; Φ = [2,2]; l = [SA[:a,:b], SA[:a]]
end

@test X[(:Φ,:l)] isa Vector{<:StaticVector}
@test X[1,(:Φ,:l)] isa StaticVector

SchInference = BasicSchema([:V,:E], [(:v0,:E,:V)])
@acset_type InferenceTest(SchInference, index=[:v0])
let s = InferenceTest()
  @test Base.return_types((typeof(s),)) do s
    s[1, :v0]
  end |> only === Int
  add_part!(s, :V)
  add_part!(s, :E)
  call_setindex!(s) = s[1, :v0] = 1
  @test call_setindex!(s) == 1
  @test iszero(@allocated call_setindex!(s))
  call_getindex(s) = s[1, :v0]
  @test call_getindex(s) == 1
  @test iszero(@allocated call_getindex(s))
end

end # module
