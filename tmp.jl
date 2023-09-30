using Revise, ACSets
MySch = BasicSchema([:Thing,:Node,:Edge], [(:src,:Edge,:Node),(:tgt,:Edge,:Node),(:thing,:Thing,:Node)],[],[])

@acset_type MyData(MySch, index=[:src,:tgt], unique_index=[:thing])

mydata = @acset MyData begin
    Thing=3
    Node=3
    Edge=3
    thing=[1,2,3]
    src=[1,1,2]
    tgt=[1,2,3]
end

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



# errors
cascading_rem_parts!(mydata, :Node, 1)

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