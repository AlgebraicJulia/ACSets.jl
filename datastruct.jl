using Revise, ACSets
using CompTime
using Test

# need a thing that stores indexed spans

abstract type TypeLevelIndexedSpans{Spans} end

struct IndexedSpans
    spans::Vector{Tuple{Vararg{Symbol}}}
end

function typelevel_indexedspans(s::IndexedSpans)
    TypeLevelIndexedSpans{
        Tuple{s.spans...}
    }
end

# my_indexed_spans = IndexedSpans([(:f1,:f2),(:g1,:g2,:g3)])
# typelevel_indexedspans(my_indexed_spans)

# to store the cached indices, we'll just use a vector of dicts

# we need a type to store the wrapped ACSet
struct IndexedSpanACSet{S<:TypeLevelIndexedSpans, I}
    acs::StructACSet
    idx::I
end

function IndexedSpanACSet(acs::StructACSet{S}, to_index::I) where {S,I<:Vector{<:Tuple{Vararg{Symbol}}}}
    idxs = [make_spanidx(acs, span) for span in to_index]
    IndexedSpanACSet{typelevel_indexedspans(IndexedSpans(to_index)), typeof(idxs)}(
        acs,
        idxs
    )
end

get_typelevel_indexed_spans(::IndexedSpanACSet{S}) where {S} = S

function get_indexed_spans(::Type{TypeLevelIndexedSpans{Spans}}) where {Spans}
    get_indexed_spans(
        IndexedSpans(
            [Spans.parameters...]
        )
    )
end
get_indexed_spans(s::IndexedSpans) = s.spans

# to get the IndexedSpans from the type level indexed spans
# get_indexed_spans(get_typelevel_indexed_spans(mydataidx))

# --------------------------------------------------------------------------------
# build the index for an indexed span

function make_spanidx(acs::StructACSet{S}, span::Tuple{Vararg{Symbol}}) where {S}
    s = Schema(S)
    length(span) > 1 || error("the span $(span) only has one hom")
    allequal([dom(s, f) for f in span]) || error("some homs in $(span) have inconsistent domain")

    cache_keys = reduce(vcat, Iterators.product([codom_parts(acs, f) for f in span]...))
    span_legs = collect(zip([subpart(acs, f) for f in span]...))

    index = Dict{eltype(cache_keys),Set{Int}}()
    for key in cache_keys
        index[key] = Set(findall(isequal(key), span_legs))
    end
    return index
end


# --------------------------------------------------------------------------------
# incident when querying multiple columns
# note: if the cols are part of an indexed span, use the dict lookup
#       if not, default to the normal one using intersection

# incident when `f` is not indexed
function _incident_noidx(acs::IndexedSpanACSet, parts, f::I) where {I<:Tuple{Vararg{Symbol}}}
    return intersect([incident(acs.acs, parts[i], f[i]) for i in eachindex(f)]...)
end

# incident when `f` is an indexed span
# ix: the numeric index of the span (acs.idx[ix])
function _incident_idx(acs::IndexedSpanACSet, parts, ix)
    return collect(acs.idx[ix][parts])
end

function ACSetInterface.incident(acs::IndexedSpanACSet{S,I}, parts::T, f::F) where {S,I,T<:Tuple,F<:Tuple{Vararg{Symbol}}}
    indexed_spans = get_indexed_spans(S)
    f_pos = findfirst(isequal(f), indexed_spans)
    isnothing(f_pos) ? _incident_noidx(acs, parts, f) : _incident_idx(acs, parts, f_pos)
end


# --------------------------------------------------------------------------------
# add/rem part
# these methods add and remove keys from indexed spans, if elements are added/removed to ob
# sets of the legs of any indexed spans

# acs = deepcopy(mydataidx)
# sch = acset_schema(acs.acs)
# spans = get_indexed_spans(get_typelevel_indexed_spans(acs))

# add part
function ACSetInterface.add_part!(acs::IndexedSpanACSet{S,I}, type) where {S,I}
    sch = acset_schema(acs.acs)
    spans = get_indexed_spans(S)

    # get objects in legs of each indexed span
    spans_legs = [[codom(sch, f) for f in s] for s in spans]

    # if `type` in legs of an indexed span, update keys of index
    for s in eachindex(spans_legs)
        if type ∉ spans_legs[s]
            continue
        end
        # get index of `type` and not `type` in legs of indexed span `s`
        type_ix = findfirst(isequal(type), spans_legs[s])
        nottype_ix = setdiff(eachindex(spans_legs[s]), type_ix)
        # NOTE: not maybe generalizable? we assume the newly added part
        # will have an ID that is just one more than the current maximum part
        # ACSetInterface.add_parts!(m::IntParts/BitSetParts, n::Int) could be seperated
        # into a fn that returns in the new part ID and one that actually updates m

        # add keys for new part of ob `type` with product of other obs in the legs
        new_part = nparts(acs.acs, type) + 1
        nottype_keys = Iterators.product([parts(acs.acs, spans_legs[s][f′]) for f′ in nottype_ix]...)
        for i in nottype_keys
            new_key = zeros(Int, length(spans_legs[s]))
            new_key[type_ix] = new_part
            new_key[nottype_ix] .= i
            acs.idx[s][new_key...] = Set{Int}()
        end
    end

    # call existing add_part!
    add_part!(acs.acs, type)
end

# rem part
function ACSetInterface.rem_part!(acs::IndexedSpanACSet{S,I}, type, part) where {S,I}
    # call existing rem_part!
    rem_part!(acs.acs, type, part)

    sch = acset_schema(acs.acs)
    spans = get_indexed_spans(S)

    # get objects in legs of each indexed span
    spans_legs = [[codom(sch, f) for f in s] for s in spans]

    # if `type` in legs of an indexed span, update keys of index
    for s in eachindex(spans_legs)
        if type ∉ spans_legs[s]
            continue
        end
        # position of `type` in legs of indxd span `s`
        type_ix = findfirst(isequal(type), spans_legs[s])
        # delete all keys with the deleted element of `type`
        for key in keys(acs.idx[s])
            if key[type_ix] == part
                delete!(acs.idx[s], key)
            end
        end
    end
end


# --------------------------------------------------------------------------------
# set/clear subpart
# these methods add or remove values associated to keys, if subparts (homs) from
# the apex of an indexed span are set or removed

# acs = deepcopy(mydataidx)
# sch = acset_schema(acs.acs)
# spans = get_indexed_spans(get_typelevel_indexed_spans(acs))

# set_subpart!
function ACSetInterface.set_subpart!(acs::IndexedSpanACSet{S,I}, part, f, subpart) where {S,I}
    # basic set subpart
    set_subpart!(acs.acs, part, f, subpart)

    sch = acset_schema(acs.acs)
    spans = get_indexed_spans(S)

    # needed in case `f` is involved in multiple spans
    for s in eachindex(spans)
        # only need to update idx if `f` was in idxd span `s`
        if f ∉ spans[s]
            continue
        end

        # is using 0 for uninitialized homs an implementation detail i shouldn't rely on?
        span_subparts = [acs.acs[part, f′] for f′ in spans[s]]
        if all(span_subparts .!= 0)
            push!(acs.idx[s][Tuple(span_subparts)], part) 
        end
    end
end


# clear subpart!
function ACSetInterface.clear_subpart!(acs::IndexedSpanACSet{S,I}, part, f) where {S,I}

    sch = acset_schema(acs.acs)
    spans = get_indexed_spans(S)

    # 
    for s in eachindex(spans)
        if f ∉ spans[s]
            continue
        end
        span_subparts = [acs.acs[part,f′] for f′ in spans[s]]
        if all(span_subparts .!= 0)
            delete!(acs.idx[s][Tuple(span_subparts)], part)
        end        
    end

    # basic clear subpart
    clear_subpart!(acs.acs, part, f)
end


# --------------------------------------------------------------------------------
# code for testing

# a schema with 2 spans, and an object involved in both
# Rel1 is product, Rel2 is general relation with both sparsity and redundancy
MySch = BasicSchema(
    [:X,:Y,:Rel1,:W,:Z,:Rel2],
    [
        (:proj_x1,:Rel1,:X),(:proj_y,:Rel1,:Y), # relation 1
        (:proj_x2,:Rel2,:X),(:proj_w,:Rel2,:W),(:proj_z,:Rel2,:Z)
    ],
    [],
    []
)

@acset_type MyDataType(MySch)

X_parts = 3
Y_parts = 4
rel1_proj = collect(Iterators.product(1:X_parts,1:Y_parts))
rel1_proj = vcat(rel1_proj...)

W_parts = 3
Z_parts = 2
rel2_proj = collect(Iterators.product(1:X_parts,1:W_parts,1:Z_parts))
rel2_proj = vcat(rel2_proj...)
# to sparsify the relation, delete some elements
to_del = [1,5,6,7,10]
deleteat!(rel2_proj, to_del)

# to densify the relation, duplicate some elements
to_dup = [2,10,12]
rel2_proj = [rel2_proj; rel2_proj[to_dup]]

mydata = @acset MyDataType begin
    X = X_parts
    Y = Y_parts
    W = W_parts
    Z = Z_parts
    Rel1 = length(rel1_proj)
    Rel2 = length(rel2_proj)
    proj_x1 = first.(rel1_proj)
    proj_y = last.(rel1_proj)
    proj_x2 = first.(rel2_proj)
    proj_w = getindex.(rel2_proj, 2)
    proj_z = last.(rel2_proj)
end

# generate an ACSet with indexed spans
mydataidx = IndexedSpanACSet(mydata, [(:proj_x1,:proj_y),(:proj_x2,:proj_w,:proj_z)])

# tests
for i in parts(mydataidx.acs, :Rel1)
    @test i ∈ incident(mydataidx, (mydataidx.acs[i,:proj_x1], mydataidx.acs[i,:proj_y]), (:proj_x1,:proj_y))
end
@test_throws KeyError incident(mydataidx, (100,0), (:proj_x1,:proj_y))


for i in parts(mydataidx.acs, :Rel2)
    @test i ∈ incident(
        mydataidx, 
        (mydataidx.acs[i,:proj_x2], mydataidx.acs[i,:proj_w], mydataidx.acs[i,:proj_z]), 
        (:proj_x2,:proj_w,:proj_z)
    )
end

@test [11] == incident(mydataidx, (1,3), (:proj_x2,:proj_w))

@test Int[] == incident(
    mydataidx, 
    (1,1,1), 
    (:proj_x2,:proj_w,:proj_z)
)

idx_spans = get_indexed_spans(get_typelevel_indexed_spans(mydataidx))
@test length(idx_spans) == 2

# adding a part (will add keys to the span(s) which include that part in their legs)
keys1 = deepcopy(keys(mydataidx.idx[1]))
keys2 = deepcopy(keys(mydataidx.idx[2]))

@test nparts(mydataidx.acs, :X) == maximum(first.(keys1))
@test nparts(mydataidx.acs, :X) == maximum(first.(keys2))

add_part!(mydataidx, :X)

@test nparts(mydataidx.acs, :X) == maximum(first.(keys(mydataidx.idx[1])))
@test nparts(mydataidx.acs, :X) == maximum(first.(keys(mydataidx.idx[2])))
@test length(keys(mydataidx.idx[1])) > length(keys1)
@test length(keys(mydataidx.idx[2])) > length(keys2)

newkeys1 = setdiff(keys(mydataidx.idx[1]), keys1)
newkeys2 = setdiff(keys(mydataidx.idx[2]), keys2)

@test all(first.(newkeys1) .== nparts(mydataidx.acs, :X))
@test all(first.(newkeys2) .== nparts(mydataidx.acs, :X))

@test all([length(mydataidx.idx[1][k]) for k in newkeys1] .== 0)
@test all([length(mydataidx.idx[2][k]) for k in newkeys2] .== 0)

# remove that newly added part
rem_part!(mydataidx, :X, nparts(mydataidx.acs, :X))

@test keys(mydataidx.idx[1]) == keys1
@test keys(mydataidx.idx[2]) == keys2
@test nparts(mydataidx.acs, :X) == 3

# test setting subparts
new_part = add_part!(mydataidx, :Rel1) # check that indexing was unaffected

preimage_11 = incident(mydataidx, (1,1), (:proj_x1, :proj_y))

set_subpart!(mydataidx, new_part, :proj_x1, 1)

@test preimage_11 == incident(mydataidx, (1,1), (:proj_x1, :proj_y))

set_subpart!(mydataidx, new_part, :proj_y, 1)

@test sort(incident(mydataidx, (1,1), (:proj_x1, :proj_y))) == [1,new_part]

# test removal of subparts
clear_subpart!(mydataidx, new_part, :proj_x1)

@test incident(mydataidx, (1,1), (:proj_x1, :proj_y)) == [1]

# --------------------------------------------------------------------------------
# things to look into later

# need to make sure we test:
# 1. a hom which is part of multiple indexed spans
# 2. an ob which is in the leg of multiple indexed spans

# find colons in a tuple for sliced indexing
# findcolons(parts::T) where {T <: Tuple} = begin
#     findall(isequal(Colon), T.parameters)
# end
# findcolons((:,5))
# findcolons((5,:))
# findcolons((:,:))
# findcolons((5,5))


# # test fn that will check if the span the user inputs is indexed
# function testfn(::IndexedSpanACSet{S,I}, f::F) where {S,I,F<:Tuple{Vararg{Symbol}}}
#     spans = get_indexed_spans(S)
#     # return f ∈ spans
#     findfirst(isequal(f), spans)
# end

# testfn(mydataidx, (:proj_x1,:proj_y))
# testfn(mydataidx, (:proj_x2,:proj_w,:proj_z,:blah))