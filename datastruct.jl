# our initial data structure for indexing general relations on multiple columns
# should be based on a Dict

# should be able to index with a colon or range into part of the overall index
# e.g. cache[(1,:,2)] or cache[(1,1:3,2)]

using Catlab, ACSets, StatsBase

# abstract type MultiMapping{S,T,N} <: AbstractDict{S,T} end

# struct DictMultiMap{S,T,N<:Tuple{Vararg{Symbol}},D<:AbstractDict{S,T}} <: MultiMapping{S,T,N}
#     d::D
# end

# struct WrappedDict{S,T}
#     d::Dict{S,T}
# end

# WrappedDict{S,T}() where {S,T} = WrappedDict{S,T}(Dict{S,T}())

# tmp = [WrappedDict{Int,String}(), WrappedDict{Symbol,Float64}()]
# eltype(tmp)
# typeof(tmp[1])

# wrapped acsets
struct WrappedACSet{S,I<:AbstractVector{<:AbstractDict}}
    spans::S
    indices::I
    acs::StructACSet
end

function WrappedACSet(acs::StructACSet, idxs::I, spans::S) where {S,I}
    return WrappedACSet{S,I}(spans, idxs, acs)
end

# testing WrappedACSet
MySch = BasicSchema(
    [:X,:Y,:Rel],
    [(:proj_x,:Rel,:X),(:proj_y,:Rel,:Y)],
    [:NameType,:NumberType],
    [(:xname,:X,:NameType),(:yname,:Y,:NameType),(:cost,:Rel,:NumberType)]
)

@acset_type MyDataType(MySch, index=[:proj_x,:proj_y], unique_index=[:xname,:yname])

# generate a relation
rel_sparse = @acset MyDataType{Symbol,Float64} begin
    X = 4
    Y = 3
    xname = [:widget1,:widget2,:widget3,:widget4]
    yname = [:place1,:place2,:place3]
end

X_set = FinSet(nparts(rel_sparse,:X))
Y_set = FinSet(nparts(rel_sparse,:Y))
Z_prod = Catlab.product(X_set, Y_set)

Z_ob = apex(Z_prod)
Z_f = legs(Z_prod)[1]
Z_g = legs(Z_prod)[2]

to_remove = 3
to_keep = sample(collect(Z_ob), length(Z_ob)-to_remove, replace=false)

add_parts!(
    rel_sparse, 
    :Rel, 
    length(to_keep),
    proj_x = collect(Z_f)[to_keep],
    proj_y = collect(Z_g)[to_keep]
)


"""
    Make a cache for a general relation (span) that may not be a product, the cache is
        a Dict mapping n-tuples (pairs for binary relations) which index the objects
        in the legs of the span, to integer vectors giving the part(s) in the apex
        related to that key.
"""
function index_spans(acs::StructACSet{S}, spans) where {S}
    s = Schema(S)
    # need to build each cache seperately    
    idxs = map(spans) do span
        length(unique([dom(s, f) for f in span])) == 1 || error("some morphisms in $(span) have different domains")

        cache_keys = reduce(vcat, Iterators.product([codom_parts(acs, f) for f in span]...))
        span_legs = collect(zip([subpart(acs, f) for f in span]...))
    
        cache_index = Dict{eltype(cache_keys),Set{Int}}()
        for key in cache_keys
            cache_index[key] = Set(findall(isequal(key), span_legs))
        end
        return cache_index
    end

    return WrappedACSet(deepcopy(acs), idxs, spans)
end

rel_wrapped = index_spans(rel_sparse, [(:proj_x,:proj_y)])
