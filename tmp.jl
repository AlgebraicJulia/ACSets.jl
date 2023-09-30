using Revise, ACSets
MySch = BasicSchema([:Thing,:Node,:Edge], [(:src,:Edge,:Node),(:tgt,:Edge,:Node),(:thing,:Thing,:Node)],[],[])

# INJECTIVE HOM --------------------------------------------------

@acset_type MyData(MySch, index=[:src,:tgt], unique_index=[:thing])

mydata = @acset MyData begin
    Thing=3
    Node=3
    Edge=3
    thing=[1,2,3]
    src=[1,1,2]
    tgt=[1,2,3]
end

# errors
cascading_rem_parts!(mydata, :Node, 1)

# by hand --------------------------------------------------
delparts = Dict(:Node=>1)
acs = mydata
X = deepcopy(acs)
dels = DenseACSets.delete_subobj(X, delparts)

# go into delete_subobj!
for o in ob(acset_schema(X))
    # o = :Thing

    ps = collect(parts(X,o))
    for r in dels[o]
        # r = 1
        idx = findfirst(==(r), ps)
        idx2 = pop!(ps)
        if idx <= length(ps) 
            ps[idx] = idx2 # map from new to old
        end
        # rem_parts!(X, o, dels[o])
        rem_part!(X, o, dels[o][1])
    end

end

# "going into" rem_part!
s = acset_schema(X)
part = dels[o][1]
in_homs = homs(s; to=o, just_names=true)
in_attrs = attrs(s; to=o, just_names=true)
out_homs = homs(s; from=o, just_names=true)
out_attrs = attrs(s; from=o, just_names=true)

last_part = X.parts[o].val

f = only(out_homs)

if haskey(X.subparts[f], last_part)
    # subpart(X, last_part, f) is the last_part's value of the subpart f
    # we then set part's subpart to that value
    set_subpart!(X, part, f, subpart(X, last_part, f))
end
clear_subpart!(X, last_part, f)

# REGULAR HOM --------------------------------------------------

@acset_type MyDataNoInjective(MySch, index=[:src,:tgt,:thing])

mydatanoinj = @acset MyDataNoInjective begin
    Thing=3
    Node=3
    Edge=3
    thing=[1,2,3]
    src=[1,1,2]
    tgt=[1,2,3]
end

# no error
cascading_rem_parts!(mydatanoinj, :Node, 1)


# FAILING TESTS --------------------------------------------------
using Test
SchDDS = BasicSchema([:X], [(:Φ,:X,:X)])

@abstract_acset_type AbstractDDS
@acset_type DDS(SchDDS, index=[:Φ]) <: AbstractDDS

dds = DDS()
add_parts!(dds, :X, 3, Φ=[1,1,1])
@test !isempty(dds)
@test_throws AssertionError add_part!(dds, :X, Φ=5)
@test nparts(dds, :X) == 3
@test subpart(dds, :Φ) == [1,1,1]
@test_throws AssertionError add_parts!(dds, :X, 2, Φ=[3,6])
@test nparts(dds, :X) == 3
@test incident(dds, 3, :Φ) == []