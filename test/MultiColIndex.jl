using Test
using ACSets

# a schema with 2 spans that we'll index, and an object (X) involved in both
# Rel1 is product
# Rel2 is general relation with both sparsity (keys w/no vals) and redundancy (keys w/many vals)
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

# test that indexing into a product works
for i in parts(mydataidx.acs, :Rel1)
    @test i ∈ incident(mydataidx, (mydataidx.acs[i,:proj_x1], mydataidx.acs[i,:proj_y]), (:proj_x1,:proj_y))
end
@test_throws KeyError incident(mydataidx, (100,0), (:proj_x1,:proj_y))

# test that indexing into a general relation works
for i in parts(mydataidx.acs, :Rel2)
    @test i ∈ incident(
        mydataidx, 
        (mydataidx.acs[i,:proj_x2], mydataidx.acs[i,:proj_w], mydataidx.acs[i,:proj_z]), 
        (:proj_x2,:proj_w,:proj_z)
    )
end

# if we do not specify the entire indexed span, fallback on unindexed search
@test [11] == incident(mydataidx, (1,3), (:proj_x2,:proj_w))

@test Int[] == incident(
    mydataidx, 
    (1,1,1), 
    (:proj_x2,:proj_w,:proj_z)
)

# check we can recover the indexed spans from the type
idx_spans = get_indexed_spans(get_typelevel_indexed_spans(mydataidx))
@test idx_spans == [(:proj_x1,:proj_y),(:proj_x2,:proj_w,:proj_z)]

# test adding a part will add keys to the span(s) which include that part in the legs
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