using Revise, ACSets
# rm these three after done testing
using Catlab
using StatsBase
using Test

MySch = BasicSchema(
    [:X,:Y,:Rel],
    [(:proj_x,:Rel,:X),(:proj_y,:Rel,:Y)],
    [:NameType,:NumberType],
    [(:xname,:X,:NameType),(:yname,:Y,:NameType),(:cost,:Rel,:NumberType)]
)

@acset_type MyDataType(MySch, index=[:proj_x,:proj_y], unique_index=[:xname,:yname])

# --------------------------------------------------------------------------------
# need to make a couple of types of relations (now binary, but should test for n-ary)
# these are used to test the software

# 1. a relation with more stuff in it than a product (redundant rel?)

rel_redundant = @acset MyDataType{Symbol,Float64} begin
    X = 4
    Y = 3
    xname = [:widget1,:widget2,:widget3,:widget4]
    yname = [:place1,:place2,:place3]
end

X_set = FinSet(nparts(rel_redundant,:X))
Y_set = FinSet(nparts(rel_redundant,:Y))
Z_prod = Catlab.product(X_set, Y_set)

Z_ob = apex(Z_prod)
Z_f = legs(Z_prod)[1]
Z_g = legs(Z_prod)[2]

add_parts!(
    rel_redundant, 
    :Rel, 
    length(Z_ob) + 5,
    proj_x = [collect(Z_f); sample(collect(codom(Z_f)), 5, replace=true)],
    proj_y = [collect(Z_g); sample(collect(codom(Z_g)), 5, replace=true)]
)

# 2. a relation that is the product

rel_product = @acset MyDataType{Symbol,Float64} begin
    X = 4
    Y = 3
    xname = [:widget1,:widget2,:widget3,:widget4]
    yname = [:place1,:place2,:place3]
end

add_parts!(
    rel_product, 
    :Rel, 
    length(Z_ob),
    proj_x = collect(Z_f),
    proj_y = collect(Z_g)
)

# 3. a relation with less stuff in it than a product (sparse rel?)

rel_sparse = @acset MyDataType{Symbol,Float64} begin
    X = 4
    Y = 3
    xname = [:widget1,:widget2,:widget3,:widget4]
    yname = [:place1,:place2,:place3]
end

to_remove = 3
to_keep = sample(collect(Z_ob), length(Z_ob)-to_remove, replace=false)

add_parts!(
    rel_sparse, 
    :Rel, 
    length(to_keep),
    proj_x = collect(Z_f)[to_keep],
    proj_y = collect(Z_g)[to_keep]
)


# --------------------------------------------------------------------------------
# multicolumn index being computed on the fly

function multicol_incident_nocache(acs, parts, f)
    return intersect([incident(acs, parts[i], f[i]) for i in eachindex(f)]...)
end

# --------------------------------------------------------------------------------
# multicolumn index with general caching for relations

# # input
# span = (:proj_x, :proj_y)

# # work on the fn
# acs = rel_redundant
# s = acset_schema(acs)

# # we cannot do this, because in sparse relations the legs will have more stuff than the apex
# # cache_T = collect(zip([subpart(acs, f) for f in span]...))

# # it has to be the Cartesian product of the stuff in the legs, since that's what the user
# # can in principle enter as a lookup
# cache_legkeys = reduce(vcat, Iterators.product([codom_parts(acs, f) for f in span]...))

# span_parts = collect(zip([subpart(acs, f) for f in span]...))

# cache_index = Dict{eltype(cache_legkeys),Vector{Int}}()
# for key in cache_legkeys
#     cache_index[key] = findall(isequal(key), span_parts)
# end

"""
    Make a cache for a general relation (span) that may not be a product, the cache is
        a Dict mapping n-tuples (pairs for binary relations) which index the objects
        in the legs of the span, to integer vectors giving the part(s) in the apex
        related to that key.
"""
function make_cache_rel(acs::StructACSet{S}, span::Tuple{Vararg{Symbol}}) where {S}
    s = Schema(S)
    length(unique([dom(s, f) for f in span])) == 1 || error("not all morphisms in span have the same domain")

    cache_keys = reduce(vcat, Iterators.product([codom_parts(acs, f) for f in span]...))
    span_legs = collect(zip([subpart(acs, f) for f in span]...))

    cache_index = Dict{eltype(cache_keys),Set{Int}}()
    for key in cache_keys
        cache_index[key] = Set(findall(isequal(key), span_legs))
    end
    return cache_index
end

# for acs in [rel_redundant,rel_product,rel_sparse]
#     cache = make_cache_rel(acs, (:proj_x,:proj_y))
#     for (i,j) in Iterators.product(parts(rel_redundant,:X), parts(rel_redundant,:Y))
#         rel_parts_cache = cache[(i,j)]
#         rel_parts_inc = intersect(incident(acs, i, :proj_x), incident(acs, j, :proj_y))
#         @test rel_parts_cache == rel_parts_inc
#     end
# end


# --------------------------------------------------------------------------------
# note that set_subpart! and clear_subpart! do not change keys, because obs were not 
# changed. Only values associated to the preimages are added/subtracted.

acs = deepcopy(rel_redundant)
span_homs = (:proj_x,:proj_y)
cache = make_cache_rel(acs, span_homs)

# how to make set_subpart! work
new_part = add_part!(acs, :Rel)

# in the final version, acs would have a field that stores
# all the indexed spans, and we should check if f was a part
# of any indexed spans. For now we input manually.
function test_set_subpart!(acs, part, f, val, span_homs, cache)
    set_subpart!(acs, part, f, val)
    # is using 0 for uninitialized homs an implementation detail i shouldn't rely on?
    span_homs_vals = [acs[part, span_f] for span_f in span_homs]
    if all(span_homs_vals .!= 0)
        # not sure what SortedDict uses, check if push! is inappropriate
        push!(cache[Tuple(span_homs_vals)], part) 
    end
end

old_idx = length(cache[(4,1)])
test_set_subpart!(acs, new_part, span_homs[1], 4, span_homs, cache)
test_set_subpart!(acs, new_part, span_homs[2], 1, span_homs, cache)
@test length(cache[(4,1)]) == old_idx + 1
@test new_part ∈ cache[(4,1)]

# how to make clear_subpart! work
subpart_ix = 9

# the clear_subpart! method is pretty simple.
function test_clear_subpart!(acs, part, f, span_homs, cache)    
    legs_parts = [acs[part,f′] for f′ in span_homs]
    clear_subpart!(acs, part, f)    
    delete!(cache[tuple(legs_parts...)], part)
end

key = tuple([acs[subpart_ix,f] for f in span_homs]...)
@test subpart_ix ∈ cache[key]
test_clear_subpart!(acs, old_part, span_homs[1], span_homs, cache)
@test subpart_ix ∉ cache[key]


# --------------------------------------------------------------------------------
# add_part!
# 1. if the part is added to the apex, do not need to do anything
# 2. if the part is added to the legs, we need to add keys _before_ calling existing methods
# NOTE: this only works right now if the leg is involved in only one cached span

function test_add_part!(acs, type, cache, span_homs)
    s = acset_schema(acs)
    # if the part is being added to the legs of the homs, update the keys
    span_legs = [codom(s, f) for f in span_homs]
    if type ∈ span_legs
        # find which position in the legs `type` is
        y_pos = findfirst(isequal(type), span_legs)
        not_y = setdiff(eachindex(span_homs), y_pos)
        # NOTE: not generalizable yet, but now we assume the newly added part
        # will have an ID that is just one more than the maximum already present
        new_part = nparts(acs, type) + 1
        not_y_idx = Iterators.product([parts(acs, span_legs[f′]) for f′ in not_y]...)
        for ix in not_y_idx
            new_key = zeros(Int, length(span_homs))
            new_key[y_pos] = new_part
            new_key[not_y] .= ix
            cache[new_key...] = Set{Int}()
        end
        
    end
    add_part!(acs, type)
end

acs = deepcopy(rel_sparse)
span_homs = (:proj_x,:proj_y)
cache = make_cache_rel(acs, span_homs)

@test sum(length.(values(cache))) == nparts(acs,:Rel)
old_cache_length = nparts(acs,:X) * nparts(acs,:Y)
@test length(keys(cache)) == old_cache_length
test_add_part!(acs, :X, cache, span_homs)
@test length(keys(cache)) == old_cache_length + nparts(acs,:Y)

# --------------------------------------------------------------------------------
# rem_part!
# 1. if the part is removed from the apex, do not need to do anything (set/clear_subpart! will handle it)
# 2. if the part is removed from the legs, we need to delete keys _after_ calling existing methods
#    because we get the id associated to the deleted object and then delete all keys including that id
# NOTE: this only works right now if the leg is involved in only one cached span

# function test_rem_part!(acs, type, part, cache, span_homs)    
#     rem_part!(acs, type, part)

#     s = acset_schema(acs)
#     homs_to_type = homs(s, to=type_to_del)
#     hom_to_type_in_span = 0
#     for i in eachindex(homs_to_type)
#         if homs_to_type[i][3] == type
#             hom_to_type_in_span = i
#         end
#     end

#     check_pos = findfirst(span_homs .== hom_to_type_in_span) # this will break on lacsets
#     for key in keys(cache)
#         if key[check_pos] == part
#             delete!(cache, key)
#         end
#     end
# end

acs = deepcopy(rel_sparse)
span_homs = (:proj_x,:proj_y)
cache = make_cache_rel(acs, span_homs)

type_to_del = :X
part_to_del = 4

num_keys_with_part = sum(first.(keys(cache)) .== part_to_del)
@test num_keys_with_part > 0


# test_rem_part!(acs, type_to_del, part_to_del, cache, span_homs)

rem_part!(acs, type_to_del, part_to_del)

# we need to find out which arrow in span_homs was going into the ob `type`, because
# that one will be the index of keys that need to be erased
s = acset_schema(acs)
homs_in = homs(s, to=type_to_del, just_names=true)

span_leg = [f ∈ span_homs ? f : nothing for f in homs_in]
filter!(x->!isnothing(x), span_leg)

span_leg_ix = findfirst(span_homs .== span_leg)

for key in keys(cache)
    if key[span_leg_ix] == part_to_del
        delete!(cache, key)
    end
end

# we will need to repeat the above for _each_ indexed span

# --------------------------------------------------------------------------------
# what i planned to do here was to make another version of cache that is
# matrix backed rather than dict backed. it may or may not be very useful.

# cache_dims = [nparts(mydata, codom(s, f)) for f in span]
# product_cache = Array{Int}(undef, cache_dims...)
# for (i, ind) in enumerate(relation_cache)
#     product_cache[ind...] = i
# end

# # find it
# product_cache[parts...]

# """
#     Make a cache for a relation (span) that is a product
# """
# function make_cache_prod()
# end


# --------------------------------------------------------------------------------
# obvious to-do's ... updating the cache after adding/rem parts

# 1. adding parts involved in a cached span
# 1a. part is to the apex: if the subparts were set, find which key the new part points to and add its index to 
#     the existing values (indices) at that key
#     NOTE: actually maybe all this logic should only be done at set_subparts! time, because if
#     the user didnt set all the subparts for this part, the preimage won't exist
# 1b. part is in a leg. Nothing to do, because the relevant subparts are only associated with the apex (domain)
# 
# 2. removing parts involved in a cached span
# 2a. part is in the apex, subparts will update after deletion...probably need to recache the values (indices)
#     but at least the keys are the same
# 2b. part is in legs, uh oh, the keys changed. Everything has to be redone.

# Things to do:
# 1. figure out how to get our wrapped StructACSet to know when set_subpart! or clear_subpart! is called;
#    those are the points when it will be necessary to recompute some of the cache
# 1a. NOTE: it seems it really would be enough to just write a method for the wrapper for these 2 functions,
#     check at compile time if the passed symbol f is a hom that is part of a cached span, if not, we just do as
#     usual, if it is, then we need to recompute things
# 2. figure out how the existing setup recomputes the dict backed preimages

# interesting q, when one adds a part to the legs, should the keys be updated? YES
