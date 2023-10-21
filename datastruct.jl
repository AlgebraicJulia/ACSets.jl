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
struct IndexedSpanACSet{S<:TypeLevelIndexedSpans, K, I<:AbstractVector{<:AbstractDict{<:Tuple{Vararg{K}},K}}}
    acs::StructACSet
    idx::I
end




# --------------------------------------------------------------------------------
# code for testing

# a schema with 2 spans, and an object involved in both
# Rel1 is product, Rel2 is general relation with both sparsity and redundancy
MySch = BasicSchema(
    [:X,:Y,:Rel1,:W,:Z,:Rel2],
    [
        (:proj_x1,:Rel1,:X),(:proj_y,:Rel1,:Y), # relation 1
        (:proj_x2,:Rel2,:X),(:proj_w,:Rel2,:W),(:proj_Z,:Rel,:Z)
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