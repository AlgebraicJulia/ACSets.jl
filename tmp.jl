# using Revise, ACSets

# # ------------------------------------------------------------
# # test for Homs

# MySch = BasicSchema(
#     [:A,:B], [(:f,:A,:B)], [], []
# )

# @acset_type MyDataNoIdx(MySch)
# @acset_type MyDataIdx(MySch, index=[:f])
# @acset_type MyDataInjIdx(MySch, unique_index=[:f])

# data_noidx = @acset MyDataNoIdx begin
#   A = 5
#   B = 5
#   f = [1,2,3,4,5]
# end

# data_idx = @acset MyDataIdx begin
#   A = 5
#   B = 5
#   f = [1,2,3,4,5]
# end

# data_inj = @acset MyDataInjIdx begin
#     A = 5
#     B = 5
#     f = [1,2,3,4,5]
#   end

# rem_parts!(data_noidx, :B, 1:3)
# rem_parts!(data_idx, :B, 1:3)
# rem_parts!(data_inj, :B, 1:3)

# incident(data_noidx, 0, :f)
# incident(data_idx, 0, :f)
# incident(data_inj, 0, :f)

# data_noidx.subparts[:f].m
# data_noidx.subparts[:f].m.v
# data_noidx.subparts[:f].m.defined
# data_noidx.subparts[:f].pc

# data_idx.subparts[:f].m
# data_idx.subparts[:f].m.v
# data_idx.subparts[:f].m.defined
# data_idx.subparts[:f].pc
# data_idx.subparts[:f].pc.preimages

# data_inj.subparts[:f].m
# data_inj.subparts[:f].m.v
# data_inj.subparts[:f].m.defined
# data_inj.subparts[:f].pc
# data_inj.subparts[:f].pc.inverse

# # get how many parts in domain
# dom_parts(data_inj, :f)

# # get col view
# colview = view(data_noidx,1:5,:f)

# colview.column.m.defined

# function test_find_undef(acs, part)
#     # need to check that its really a hom
#     findall([!haskey(acs.subparts[part],i) for i in dom_parts(acs,part)])
# end

# # would be nice if incident could do
# findall(data_noidx[:,:f] .== 0)
# test_find_undef(data_noidx,:f)
# undefined_subparts(data_noidx,:f)

# findall(data_idx[:,:f] .== 0)
# test_find_undef(data_idx,:f)

# findall(data_inj[:,:f] .== 0)
# test_find_undef(data_inj,:f)