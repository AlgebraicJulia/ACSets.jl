using ACSets
using DataStructures

SchDDS = BasicSchema([:X], [(:Φ,:X,:X)])
@abstract_acset_type AbstractDDS
@acset_type DDS(SchDDS, index=[:Φ]) <: AbstractDDS
DDS(i::Int) = DDS(rand(1:i, i))
function DDS(v::Vector{Int})
  x = DDS()
  add_parts!(x, :X, length(v))
  set_subpart!(x, :Φ, v)
  x
end

x = DDS(2)
y = copy(x)
y[1, :Φ] = 2

z = DDS(3)

# the LCS algorithm was borrowed from the RosettaCode page on LCS, under the
# Julia entry, on Mar 1, 2025
function longest(a::AbstractVector, b::AbstractVector)
    length(a) ≥ length(b) ? a : b
end
# Dynamic
function lcs(a::T, b::T) where T<:AbstractVector
    lengths = zeros(Int, length(a) + 1, length(b) + 1)
    # row 0 and column 0 are initialized to 0 already
    for (i, x) in enumerate(a), (j, y) in enumerate(b)
        if x == y
            lengths[i+1, j+1] = lengths[i, j] + 1
        else
            lengths[i+1, j+1] = max(lengths[i+1, j], lengths[i, j+1])
        end
    end
    # read the substring out from the matrix
    matches = Pair{eltype(T), Tuple{Int, Int}}[]
    result = T[]
    x, y = length(a) + 1, length(b) + 1
    while x > 1 && y > 1
        if lengths[x, y] == lengths[x-1, y]
            x -= 1
        elseif lengths[x, y] == lengths[x, y-1]
            y -= 1
        else
            @assert a[x-1] == b[y-1]
            result = [a[x-1]; result]
            push!(matches, a[x-1] => (x-1, y-1))
            x -= 1
            y -= 1
        end
    end
    return OrderedDict([k => v for (k, v) in reverse(matches)])
end
function lcs(a::T, b::T) where T<:ACSet
    schema = acset_schema(T())
    diff_homs = map(homs(schema)) do (f, src, tgt)
        (f, src, tgt) => lcs(subpart(a, f), subpart(b, f))
    end
    diff_attrs = map(attrs(schema)) do (attr, src, tgt)
        (attr, src, tgt) => lcs(subpart(a, attr), subpart(b, attr))
    end
    OrderedDict([diff_homs; diff_attrs])
end

p = DDS(8)
q = DDS(7)


RecAttrSch = BasicSchema(
  [:Thing,:Node,:Edge], [(:src,:Edge,:Node),(:tgt,:Edge,:Node),(:thing,:Thing,:Node)],
  [:Attr1,:Attr2,:Attr3],[(:attr1,:Node,:Attr1),(:attr2,:Edge,:Attr2),(:attr3,:Thing,:Attr3)])
@acset_type RecAttrData(RecAttrSch, index=[:src,:tgt], unique_index=[:thing])
sch = acset_schema(RecAttrData{String, Symbol, Float64}())

d1 = @acset RecAttrData{String,Symbol,Float64} begin
    Thing=3
    Node=3
    Edge=3
    thing=[1,2,3]
    src=[1,1,2]
    tgt=[1,2,3]
    attr1=["1","2","3"]
    attr2=[:a,:b,:c]
    attr3=[10.0,11.0,12.0]
end
d2 = @acset RecAttrData{String,Symbol,Float64} begin
    Thing=3
    Node=3
    Edge=3
    thing=[1,2,3]
    src=[1,3,2]
    tgt=[1,2,3]
    attr1=["a!","b!","c!"]
    attr2=[:b,:d,:c]
    attr3=[10.0,11.0,12.0]
end

struct Diff
    dict::Dict{Symbol, SQLACSetNode}
end

function (d::Diff)(x::ACSet, key::Symbol)
    haskey(d.dict, key) || return nothing
    d.dict[key](x; formatter=:df)
end

function (d::Diff)(x::ACSet)
    sch = acset_schema(x)
    @info arrows(sch), objects(sch)
    ks = first(attrs(acset_schema(x))) ∪ first(homs(acset_schema(x))) ∪
    objects(acset_schema(x))
    filter(!isnothing, map(ks) do k
               @info k
        d(x, k)
    end)
end

function Base.diff(a::T, b::T) where T<:ACSet
    v = lcs(a,b)
    queries = Dict(Iterators.map(keys(v)) do k
        ids = collect(Iterators.map(values(v[k])) do (a1, _)
            a1
        end |> collect)
        # TODO ACSet schema builds its own query constructor
        k[1] => From(k[2] => k[2]) |> 
        Where(k[2], ∉, ids) |> Select(k[1])
    end)
    Diff(queries)
end

d = diff(d1, d2)

d(d1)

d(d1, :attr2)

d′ = diff(d2, d1)

d′(d2, :attr2)


DiffSch = BasicSchema(
    [:Diff], [], [:Part, :Id, :Col, :Val], 
    [(:part,:Diff,:Part), (:id,:Diff,:Id), (:col, :Diff, :Col), (:val, :Diff, :Val)])
@acset_type Differ(DiffSch)
out = Differ{Symbol, Int, Symbol, Any}()

s = d(d1)
s′ = [first(first(k)) => 
      (first(k)[2], filter(∉(objects(acset_schema(d1))) ∘ first, k)) for k ∈ s]

foreach(s′) do (k, v)
    foreach(v[2]) do (col, val)
        foreach(enumerate(val)) do (i, x)
            t = NamedTuple{(:part, :id, :col, :val)}((k, v[1][i], col, x))
            add_part!(out, :Diff, t)
        end
    end
end


v = map(s′) do _s
    filter(!isnothing, getfield.(_s, :second))
end




# transpose
_ts(v) = map(enumerate(reduce(hcat, v))) do (i, vals)
    NamedTuple{(:part, :id, :val)}((first(s[2]), keys(v)[i], vals))
end

foreach(ts) do t
    add_part!(out, :Diff, t)
end

function build_acset(q::SQLACSetNode, acset::ACSet, selected)
    out_acset = DiffData{Symbol, Symbol, Any}()
    # TODO color
    _part = filter(∈(objects(acset_schema(acset))) ∘ first, selected)
    not_parts = filter(∉(objects(acset_schema(acset))) ∘ first, selected)
    selected
    # add_parts!(out_acset, q.from, nrow(nt), nt)
    # return out_acset
end


