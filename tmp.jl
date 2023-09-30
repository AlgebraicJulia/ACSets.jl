using Revise, ACSets

# ------------------------------------------------------------
# test for Homs

MySch = BasicSchema(
    [:A,:B], [(:f,:A,:B)], [], []
)

@acset_type MyDataNoIdx(MySch)
@acset_type MyDataIdx(MySch, index=[:f])
@acset_type MyDataInjIdx(MySch, unique_index=[:f])

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

data_inj = @acset MyDataInjIdx begin
    A = 5
    B = 5
    f = [1,2,3,4,5]
  end

rem_parts!(data_noidx, :B, 1:3)
rem_parts!(data_idx, :B, 1:3)
rem_parts!(data_inj, :B, 1:3)

incident(data_noidx, 0, :f)
incident(data_idx, 0, :f)
incident(data_inj, 0, :f)

# would be nice if incident could do
findall(data_noidx[:,:f] .== 0)
findall(data_idx[:,:f] .== 0)