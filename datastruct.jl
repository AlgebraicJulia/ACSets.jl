using Revise, ACSets

# need a thing that stores indexed spans

abstract type TypeLevelIndexedSpans{Spans} end

struct IndexedSpans
    spans::Vector{Tuple{Vararg{Symbol}}}
end

my_indexed_spans = IndexedSpans([(:f1,:f2),(:g1,:g2,:g3)])

function typelevel_indexedspans(s::IndexedSpans)
    TypeLevelIndexedSpans{
        Tuple{s.spans...}
    }
end

typelevel_indexedspans(my_indexed_spans)

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

get_indexed_spans(::IndexedSpanACSet{S}) where {S} = S

function get_spans(::Type{TypeLevelIndexedSpans{Spans}}) where {Spans}
    get_spans(
        IndexedSpans(
            [Spans.parameters...]
        )
    )
end
get_spans(s::IndexedSpans) = s.spans

# to get the IndexedSpans from the type level indexed spans
# get_spans(get_indexed_spans(mydataidx))

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
function _incident_noidx(acs, parts, f::I) where {I<:Tuple{Vararg{Symbol}}}
    return intersect([incident(acs, parts[i], f[i]) for i in eachindex(f)]...)
end

# incident when `f` is an indexed span
# we want to use CompTime.jl on this eventually to do the searching
function _incident_idx(acs, parts, f::I) where {I<:Tuple{Vararg{Symbol}}}
    # do stuff
    error("implement me!")
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