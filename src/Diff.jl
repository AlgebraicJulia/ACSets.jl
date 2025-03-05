module Diff

using DataStructures

# the LCS algorithm was borrowed from the RosettaCode page on LCS, under the
# Julia entry, on Mar 1, 2025

function longest(a::AbstractVector, b::AbstractVector)
    length(a) ≥ length(b) ? a : b
end

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
    diff_arrows = map(arrows(schema)) do (f, src, tgt)
        (f, src, tgt) => lcs(subpart(a, f), subpart(b, f))
    end
    OrderedDict(diff_arrows)
end

struct Diff
    dict::Dict{Symbol, SQLACSetNode}
end

function (d::Diff)(x::ACSet, key::Symbol)
    haskey(d.dict, key) || return nothing
    d.dict[key](x; formatter=DFQueryFormatter())
end

function (d::Diff)(x::ACSet)
    sch = acset_schema(x)
    ks = arrows(acset_schema(x))
    filter(!isempty, map(ks) do k
        d(x, k[1])
    end)
end

function Base.diff(a::T, b::T) where T<:ACSet
    v = lcs(a,b)
    queries = Dict(Iterators.map(keys(v)) do k
        ids = collect(Iterators.map(values(v[k])) do (a1, _)
            a1
        end)
        # TODO ACSet schema builds its own query constructor
        k[1] => From(k[2] => k[2]) |> 
        Where(k[2], ∉(ids)) |> Select(k[1])
    end)
    Diff(queries)
end
export diff

DiffSch = BasicSchema(
    [:Diff], [], [:Part, :Id, :Col, :Val], 
    [(:part,:Diff,:Part), (:id,:Diff,:Id), (:col, :Diff, :Col), (:val, :Diff, :Val)])
@acset_type Differ(DiffSch)

function Differ(Δ::Diff)
    out = Differ{Symbol, Int, Symbol, Any}()
    dfs = Δ(d)
    # assumes dataframes
    foreach(dfs) do df
        map(eachrow(df)) do row
            cols = propertynames(row)
            add_part!(out, :Diff, part=cols[1], id=getproperty(row, cols[1]),
                  col=cols[2], val=getproperty(row, cols[2]))
        end
    end
    out
end
export Differ

end
