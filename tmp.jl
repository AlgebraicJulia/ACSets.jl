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

mydata = @acset MyDataType{Symbol,Float64} begin
    X = 4
    Y = 3
    xname = [:widget1,:widget2,:widget3,:widget4]
    yname = [:place1,:place2,:place3]
end

costs = rand(4,3)

add_parts!(
    mydata, :Rel, prod(size(costs)),
    proj_x = repeat(1:4,3),
    proj_y = vcat(map(i->fill(i,4),1:3)...),
    cost = vcat(costs...)
)

# a pattern that comes up very, very often
mydata[
    intersect(
        incident(mydata, :widget3, [:proj_x,:xname]),
        incident(mydata, :place2, [:proj_y,:yname])
    ),
    :cost
]

# Kris' example
SchTest = BasicSchema(
    [:A,:B,:C,:R],
    [(:f,:R,:A),(:g,:R,:B),(:h,:R,:C)],
    [],
    []
)

# @acset_type Test(SchTest, index=[:h, (:f,:g)])
@acset_type DataTest(SchTest)

X = @acset DataTest begin
    A=1
    B=1
    C=1
    R=1
    f=[1]
    g=[1]
    h=[1]
end

# incident(X, (1,1), (:f,:g)) # == [1]
incident(X, 1, :f)
incident(X, 1, :g)

# acs = X
# parts = (1,1)
# f = (:f,:g)

function ACSetInterface.incident(
        acs::SimpleACSet, parts::T1, f::T2) where {T1<:Tuple{Vararg{<:Integer}},T2<:Tuple{Vararg{Symbol}}}
    s = acset_schema(acs)
    length(unique([dom(s,f[i]) for i in eachindex(f)])) == 1 || error("some homs in $(f) do not have the same domain")
    length(parts) == length(f) || error("parts should be the same length as f")
    return intersect([incident(acs, parts[i], f[i]) for i in eachindex(f)]...)
end


incident(X, (1,1), (:f,:g))

function ACSetInterface.incident(
        acs::SimpleACSet, parts::T1, f::T2) where {T1<:Tuple,T2<:Tuple{Vararg{AbstractVector{Symbol}}}}
    s = acset_schema(acs)
    length(unique([dom(s,f[i][1]) for i in eachindex(f)])) == 1 || error("some homs in $(f) do not have the same domain")
    length(parts) == length(f) || error("parts should be the same length as f")
    return intersect(map(i->incident(acs, parts[i], f[i]), eachindex(f))...)
end

incident(mydata, (:widget3,:place2), ([:proj_x,:xname],[:proj_y,:yname]))

# does the new incident with composed arrows work with the composed arrows are of diff lengths?
# e.g. z -> x -> y
#      z -> a -> b -> c

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

Z_ob = apex(rel_prod)
Z_f = legs(rel_prod)[1]
Z_g = legs(rel_prod)[2]

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
    s = acset_schema(acs)
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

    cache_index = Dict{eltype(cache_keys),Vector{Int}}()
    for key in cache_keys
        cache_index[key] = findall(isequal(key), span_legs)
    end
    return cache_index
end

for acs in [rel_redundant,rel_product,rel_sparse]
    cache = make_cache_rel(acs, (:proj_x,:proj_y))

end


# --------------------------------------------------------------------------------
# now for when the apex is a product
cache_dims = [nparts(mydata, codom(s, f)) for f in span]
product_cache = Array{Int}(undef, cache_dims...)
for (i, ind) in enumerate(relation_cache)
    product_cache[ind...] = i
end

# find it
product_cache[parts...]

"""
    Make a cache for a relation (span) that is a product
"""
function make_cache_prod()
end


# --------------------------------------------------------------------------------
# obvious to-do's ... updating the cache after adding/rem parts.