"""Compute automorphism group via nauty.c"""
module NautyInterface
export NautyRes, CSetNautyRes, call_nauty, all_autos, canon, orbits, canonmap, 
       strhsh, generators, ngroup

using ..Schemas
using ..DenseACSets, ..ACSetInterface
using DataStructures: OrderedSet, DefaultDict
using Permutations 
using StructEquality

# Permutations
###############
const PermDict = Dict{Symbol, Permutation}
# Pointwise multiplication of permutations
Base.:(*)(x::PermDict, y::PermDict) = Dict([k => x[k]*y[k] for k in keys(x)])

idperm(n::Int) = Permutation(1:n)

"""
Convert an all-parts permutation into a symbol-indexed set of 
permutations, given a partition of 1:n_total into ranges for each symbol.
"""
function to_perm(p::Permutation, oinds::Dict{Symbol,<:UnitRange})::PermDict
  canon = collect(p)
  # ignore extraneous parts, e.g. _src
  ks = filter(k->oinds[k].stop <= length(canon), keys(oinds)) |> collect
  Dict(map(ks) do k
    range = oinds[k]
    k => Permutation(canon[range].-(range.start-1))
  end)  
end

compose(f::Vector{Int}, g::Vector{Int}) = g[f]
compose(xs::Vector{Int}...) = foldl(compose, xs)

# Nauty
#######
abstract type NautyRes end
"""
NautyResults must satisfy the following interface

- strhsh: string value representing the isomorphism class
- orbits: partitions of Cset parts into orbits
  e.g. if E#2 = E#5, then these two elements are symmetric
- generators: generating automorphisms of the automorphism group
- ngroup: number of elements in the automorphism group
- canonmap: isomorphism from the input into the canonoical isomorph
- canon: canonical isomorph (codom of `canonmap`)
"""
@struct_hash_equal struct CSetNautyRes <: NautyRes 
  strhsh::String
  orbits::Dict{Symbol, Vector{Int}}
  generators::Vector{Pair{Int, Vector{Permutation}}}
  ngroup::Int
  canonmap::Dict{Symbol,Vector{Int}}
  canon::ACSet
end

strhsh(n::CSetNautyRes) = n.strhsh
orbits(n::CSetNautyRes) = n.orbits
generators(n::CSetNautyRes) = n.generators
ngroup(n::CSetNautyRes) = n.ngroup
canonmap(n::CSetNautyRes) = n.canonmap
canon(n::CSetNautyRes) = n.canon


"""CSetNautyRes for an empty ACSet"""
function CSetNautyRes(g::T) where T<:ACSet
  isempty(g) || error("Cannot make CSetNautyRes for nonempty ACSet")
  emp= Dict([o=>Int[] for o in ob(acset_schema(g))])
  CSetNautyRes("", emp, Pair{Int, Vector{Permutation}}[], 1, emp, g)
end


"""Compute CSetNautyRes from an ACSet."""
function call_nauty end 

"""Parse nauty stdout text"""
function parse_res(res::String, g::ACSet)::CSetNautyRes
  m, oinds = to_mat(to_udg(g)), get_oinds(g) # convert g to matrix
  S = acset_schema(g)

  # regexes
  reg_cycle = r"\(([^)]+)\)"
  reg_gens = r"((?:\((?:\d+\s+)*\d+\)\s*)+)level \d+:  (?:\d cells\; )?\d+ orbits?; \d+ fixed; index (\d+)(:?\/\d+)?\n"
  reg_perm = r"(?: \d+)+\n(?:   (?: \d+)+\n)*"
  reg_canon = r"\d+ :[ ]((\s+\d+)*);"
  reg_hash = r"\[(\w+ \w+ \w+)\]"

  # get generators
  sec = findfirst("seconds",res).stop
  max_n = maximum(oinds[o].stop for o in ob(S))
  gens = map(eachmatch(reg_gens, res[1 : sec])) do mtch
    idx = parse(Int,mtch[2])
    return idx => map(split(mtch[1][2:end],"\n(")) do cyc_str
      cycs = eachmatch(reg_cycle, "("*cyc_str)
      perm = filter(x->maximum(x) <= max_n,
                    [[parse(Int,x)+1 for x in split(only(mch.captures),r"\s+")] 
                     for mch in cycs])
      is = Set(vcat(perm...))
      Permutation([perm;[[i] for i in 1:max_n if i ∉ is]])
    end
  end
  # parse permutation for the canonical graph
  rng = match(reg_perm, res[sec : end])
  # cp = CPerm(oinds, [parse(Int, x) for x in split(strip(rng.match), r"\s+")], S)
  # canonoffset = Dict([k=>canon[v].-(v.start-2) for (k,v) in oinds if k ∈ ob(S)])
  cp = Dict(map(ob(S)) do o 
    canon = [parse(Int, x) for x in split(strip(rng.match), r"\s+")]
    o=>Permutation(canon[oinds[o]].-(oinds[o].start-2))
  end)

  # parse canonical graph
  canonm = zeros(Bool, size(m))
  for (i,r) in enumerate([strip(first(y.captures))
                          for y in eachmatch(reg_canon, res)])
    if !isempty(r)
      canonm[i,[parse(Int,x)+1 for x in split(r,r"\s+")]] .= true
    end
  end

  canong = from_adj(g, oinds, canonm)

  # parse other things
  hshstr = match(reg_hash, res)[1] * "$(hash(attr_dict(g)))"
  orb = parse_orb(g, oinds, res)
  grpsize = parse(Int, match(r"grpsize=(\d+)", res)[1])

  # sanity check
  codh, h = apply(g, cp)
  string(codh) == string(canong) || error("$(codh)\n\n$(string(canong))")

  CSetNautyRes(hshstr, orb, gens, grpsize, h, canong)
end

"""Parse / postprocess orbits from the end of dreadnaut input"""
function parse_orb(g::ACSet, oinds, res::String)
  S = acset_schema(g)
  orbd = DefaultDict{Symbol, Vector{Vector{Int}}}(()->Vector{Int}[])
  reg = r"(\d+)(:(\d+)\s\(\d+\))?\;" # match orbits
  orb = Dict([o => zeros(Int, nparts(g, o)) for o in ob(S)])
  for m in eachmatch(reg, res[findlast(']',res):end])
    rg =[parse(Int, x)+1 for x in [m[1], isnothing(m[3]) ? m[1] : m[3]]]
    rng = collect(rg[1]:rg[2])
    symb = only([k for (k,v) in collect(oinds) if rng ⊆ v])
    push!(orbd[symb], rng)
  end
  for o in ob(S)
    for (i,v) in enumerate(orbd[o])
      orb[o][v.-(oinds[o].start-1)] .= i
    end
  end
  orb
end

"""Get all attribute values that are touched upon by the ACSet. Order them."""
function attr_dict(X::ACSet)
  d = DefaultDict(()->Any[])
  for (a,_,dt) in attrs(acset_schema(X))
    append!(d[dt], X[a])
  end
  Dict([k=>sort(collect(unique(v))) for (k,v) in collect(d)])
end

"""Data structure for undirected graph."""
struct UnDiGraph
  V::Int
  src::Vector{Int}
  tgt::Vector{Int}
  edge::Dict{Tuple{Int,Int},Int}
  UnDiGraph(n) = new(n,Int[],Int[],Dict{Tuple{Int,Int},Int}())
end

function add_edge!(u::UnDiGraph, s::Int, t::Int)::Int
  !(haskey(u.edge, (s,t)) || haskey(u.edge, (t,s))) || error("Duplicate ($s,$t)")
  push!(u.src, s)
  push!(u.tgt, t)
  u.edge[(s, t)] = u.edge[(t, s)] = length(u.src)
end

function add_edges!(u::UnDiGraph, sts::Vector{Tuple{Int,Int}})::Vector{Int}
  map(sts) do (s, t)
    add_edge!(u, s, t) 
  end 
end

add_edges!(u::UnDiGraph, ss::Vector{Int}, ts::Vector{Int})::Vector{Int} =
  add_edges!(u, collect(zip(ss, ts)))

function to_mat(u::UnDiGraph)::Matrix{Bool}
  mat = zeros(Bool, (u.V, u.V))
  for (s, t) in zip(u.src, u.tgt)
    mat[s, t] = mat[t, s] = true
  end
  mat
end

prime(hom_name::Symbol) = Symbol("$(hom_name)_$(hash(hom_name))")

function to_unitrange(v::Vector{Int})
  ur = minimum(v) : maximum(v)
  collect(ur) == v ? ur : error("Vector is not a UnitRange $v")
end

"""
Convert C-Set to an adjacency matrix of an *undirected* simple graph.

the matrix has rows for all parts (e.g. |E| and |V|), all homs (e.g. |E|
quantity of src & tgt nodes), and another copy of all homs (called, e.g., _src
and _tgt). For a given edge in the category of elements eₙ -- srcₙ --> vₘ,
we set edges in the simple diagraph:

        ↗ _srcₙ
      ↙    ↕
    eₙ <-> srcₙ <-> vₘ

For attributes, there is no possibility of Attr(X,X), so we simply have, e.g.:
    eₘ <-> weightₘ <-> Numberₙ
"""
function to_udg(X::ACSet)
  S        = acset_schema(X)
  attrdict = attr_dict(X)
  oinds    = get_oinds(X)
  udg      = UnDiGraph(maximum(maximum.(values(oinds))))

  for (hom_name, d, cd) in homs(S)
    hom_name_ = prime(hom_name)
    for (i, f_i) in enumerate(X[hom_name])
      es = [(hom_name, i) => (d, i),         (hom_name, i)  => (cd, f_i),
            (hom_name, i) => (hom_name_, i), (hom_name_, i) => (d, i)]
      add_edges!(udg, [(oinds[x][a],oinds[y][b]) for ((x,a),(y,b)) in es])
    end
  end

  for (attr_name, d, cd) in attrs(S)
    for (i, f_i) in enumerate(X[attr_name])
      es = [(attr_name, i) => (d, i), 
            (attr_name, i) => (cd, findfirst(==(f_i), attrdict[cd]))]
      add_edges!(udg, [(oinds[x][a], oinds[y][b]) for ((x,a),(y,b)) in es])
    end
  end
  udg 
end

"""
Specify for nauty input what the initial partition is
"""
function get_colorsarray(S, oinds::Dict{Symbol,UnitRange})
  colorsarray = Vector{Int}[]
  for c in colornames(S)
    if c ∉ attrtypes(S)
      push!(colorsarray,collect(oinds[c])) # [1..n] we don't know order
    else # for attrs we know the canonical order, so it starts out partitioned
      # TODO: When upgrading to VarACSets, this will have to be changed as some of the
      # attribute parts will be indistinct.
      append!(colorsarray, [[elem] for elem in oinds[c]])
    end
  end
  colorsarray
end

"""
Create partition of flattened indices (all parts altogether) to dict with 
indices for each distinct object, e.g.:

  {V => 1:8, E => 9:13, src => 14:18, tgt => 19:23, 
    src_... => 24:28, tgt_... => 29:33, weight => 34:38}
"""
function get_oinds(X)::Dict{Symbol,UnitRange}
    S        = acset_schema(X)
    colors   = Symbol[]
    attrdict = attr_dict(X)
  
    for o in ob(S)
      append!(colors, fill(o, nparts(X, o)))
    end
  
    for (k,v) in collect(attrdict)
      append!(colors, fill(k, length(v)))
    end
  
    for o in ob(S)
      for h in homs(S; from=o, just_names=true)
        append!(colors, fill(h, nparts(X, o)))
        append!(colors, fill(prime(h), nparts(X, o)))
      end
      for h in attrs(S; from=o, just_names=true)
        append!(colors, fill(h, nparts(X, o)))
      end
    end
  
    Dict(k => to_unitrange(findall(==(k), colors)) for k in colornames(S))
end

"""List of distinct node types in the undigraph representation of an ACSet""" 
colornames(S) = [
  types(S)..., 
  Iterators.flatten([[h,prime(h)] for h in homs(S;just_names=true)])..., 
  first.(attrs(S))...
]

"""
Convert symmetric adjacency matrix to an ACSet which is isomorphic to `X`.
"""
function from_adj(X::ACSet, oinds::Dict{Symbol, UnitRange},
                  m::AbstractMatrix{Bool})
  S = acset_schema(X)
  Y = deepcopy(X) # DB with the right # of rows. We completely overwrite it.
  attrdict = attr_dict(X)

  inv_dict = Dict(vcat(map(collect(oinds)) do (k,vs)
    [v=>i for (i,v) in enumerate(vs)]
  end...))

  # Recover the homs
  for h in homs(S; just_names=true) 
    h_ = prime(h)
    for (_, h_i) in enumerate(oinds[h_])
      src_ind, hom_ind = findall(m[h_i,:])
      src_tgt = findall(m[hom_ind,:])
      tgt_ind_ = setdiff(src_tgt, vcat([h_i,src_ind...]))
      tgt_ind = isempty(tgt_ind_) ? src_ind : only(tgt_ind_)
      set_subpart!(Y, inv_dict[src_ind], h, inv_dict[tgt_ind])
    end
  end
  # Recover the attributes
  for (h, _, t) in attrs(S) 
    for h_i in oinds[h]
      src_ind, tgt_ind = findall(m[h_i, :])
      set_subpart!(Y, inv_dict[src_ind], h, attrdict[t][inv_dict[tgt_ind]])
    end
  end
  Y
end

"""
Construct input for dreadnaut to compute automorphism group generators,
canonical permutation/hash, and orbits.
"""
function dreadnaut(g::ACSet)
  m = to_mat(to_udg(g))
  colorsarray = get_colorsarray(acset_schema(g), get_oinds(g))
  join(["n=$(size(m)[1]) g",
        join(map(1:size(m)[1]) do r
          join(string.((x->x-1).(findall(==(1),m[r,:]))) ," ")
        end, ";"),
        ". f = [$(join([join(c.-1, ",") for c in colorsarray],"|"))]",
        "c x b z o"], " ")
end

"""
Action of a permutation on a ACSet, X. Results in a new ACSet, Y, and a map X->Y
"""
function apply(X::ACSet, p::PermDict)
  S = acset_schema(X)
  cd = deepcopy(X)
  for h in homs(S; just_names=true)
    σs, σti = Vector{Int}.(collect.([p[dom(S, h)], inv(p[codom(S, h)])]))
    set_subpart!(cd, h, compose(σs, X[h], σti))
  end

  for h in attrs(S; just_names=true)
    σs = Vector{Int}(collect(p[dom(S,h)]))
    set_subpart!(cd, h, X[h][σs])
  end

  (cd, Dict([k=>invperm(Vector{Int}(collect(v))) for (k,v) in p]))
end


"""
Take advantage of the very special structure of automorphism generators given
by nauty to efficiently enumerate the automorphism group.
We iteratively expand our automorphism group with each generator. A `while` loop 
explores the possible words that we can built (starting with gₙ₊₁)
"""
function all_autos(X::CSetNautyRes)
  res = _all_autos(canon(X), generators(X))
  length(res) == X.ngroup || error("# of autos doesn't match nauty's calculation")
  res
end

# TODO: nauty outputs "index" and "level" information along the generators 
# of the automorphism group. Somehow taking advantage of these could allow us 
# to avoid a relatively brute force enumeration of the whole group.
function _all_autos(X::ACSet, gens)
  oinds = get_oinds(X)
  gens = vcat(last.(gens)...) # discard level info, don't yet know how to use it
  if isempty(gens)
    return [Dict(o => Permutation(nparts(X, o)) for o in ob(acset_schema(X)))]
  end
  all_gens = Set([idperm(length(gens[1]))])
  for (i, g) in enumerate(gens)
    old_gens, queue = deepcopy(all_gens), [[g]]
    while !isempty(queue)
      q = pop!(queue)
      qgen = prod(q)
      if first(old_gens)*qgen ∉ all_gens
        for og in old_gens push!(all_gens, og * qgen) end
        append!(queue, [[q...,prev_g] for prev_g in gens[1:i]])
      end
    end
  end
  [to_perm(p,oinds) for p in all_gens]
end

end # module
