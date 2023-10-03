using Revise, ACSets

MySch = BasicSchema([:A,:B], [(:f,:A,:B)], [], [])

@acset_type MyDataNoIdx(MySch)
@acset_type MyDataIdx(MySch, index=[:f])
@acset_type MyDataInj(MySch, unique_index=[:f])

data_noidx = @acset MyDataNoIdx begin
  A = 5
  B = 5
  f = [1,2,3,4,5]
end

data_idx = @acset MyDataIdx begin
  A = 5
  B = 5
  f = [1,2,3,4,5]
end

data_inj = @acset MyDataInj begin
    A = 5
    B = 5
    f = [1,2,3,4,5]
end

undefined_subparts(data_noidx, :f)
undefined_subparts(data_idx, :f)
undefined_subparts(data_inj, :f)

rem_parts!(data_noidx, :B, 1:3)
rem_parts!(data_idx, :B, 1:3)
rem_parts!(data_inj, :B, 1:3)

undefined_subparts(data_noidx, :f)
undefined_subparts(data_idx, :f)
undefined_subparts(data_inj, :f)

# with non-DenseParts
@acset_type MyDataNoIdxBit(MySch, part_type=BitSetParts)
@acset_type MyDataIdxBit(MySch, index=[:f], part_type=BitSetParts)
@acset_type MyDataInjBit(MySch, unique_index=[:f], part_type=BitSetParts)

data_noidx_bit = @acset MyDataNoIdxBit begin
    A = 5
    B = 5
    f = [1,2,3,4,5]
end

data_idx_bit = @acset MyDataIdxBit begin
    A = 5
    B = 5
    f = [1,2,3,4,5]
end

data_inj_bit = @acset MyDataInjBit begin
    A = 5
    B = 5
    f = [1,2,3,4,5]
end

undefined_subparts(data_noidx_bit, :f)
undefined_subparts(data_idx_bit, :f)
undefined_subparts(data_inj_bit, :f)

rem_parts!(data_noidx_bit, :B, 1:3)
rem_parts!(data_idx_bit, :B, 1:3)
rem_parts!(data_inj_bit, :B, 1:3)

undefined_subparts(data_noidx_bit, :f)
undefined_subparts(data_idx_bit, :f)
undefined_subparts(data_inj_bit, :f)

# with the oddballs
dyn_acset = () -> DynamicACSet("MyData", MySch; index=[:f])
dyn_acset_idx_bit = () -> DynamicACSet("MyData", MySch; index=[:f], part_type=MarkAsDeleted)
anon_acset = () -> AnonACSet(MySch; index=[:f])
anon_acset_bit = () -> AnonACSet(MySch; index=[:f], part_type=MarkAsDeleted)

data = anon_acset_bit()
add_parts!(data, :B, 5)
add_parts!(data, :A, 5, f = [1,2,3,4,5])

undefined_subparts(data, :f)

rem_parts!(data, :B, 1:3)

undefined_subparts(data, :f)

# f=:f
# acs=data_noidx_bit

# dom_parts(acs,f)

# acs.subparts[f]
# acs.subparts[f]

# codom_ids = codom_parts(acs,f)

# findall([acs.subparts[f][i] ∉ codom_ids for i in dom_parts(acs,f)])