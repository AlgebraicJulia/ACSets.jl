module NautyInterface
export NautyRes, CanonicalCSet, call_nauty, all_autos, canon, orbits, canonmap, 
       strhsh, generators, ngroup

using ..Schemas
using ..DenseACSets, ..ACSetInterface
using DataStructures: OrderedSet, DefaultDict
using Permutations 
using StructEquality
using nauty_jll

bashit(str) = run(`bash -c "$str"`)

# Permutations
###############
const Perm = Dict{Symbol, Permutation}
Base.:(*)(x::Perm, y::Perm) = Dict([k => x[k]*y[k] for k in keys(x)])

idperm(n::Int) = Permutation(1:n)
function to_perm(p::Permutation, oinds::Dict{Symbol, UnitRange})::Perm
  canon = collect(p)
  canon2 = Dict([k=>canon[v].-(v.start-1) 
                 for (k,v) in oinds if v.stop <= length(canon)])
  Dict([k=>Permutation(v) for (k,v) in collect(canon2)])
end

compose(f::Vector{Int}, g::Vector{Int}) = g[f]
compose(xs::Vector{Int}...) = foldl(compose, xs)

"""
Because the hom/attr perms really are just given by the perms on their
domain/codomain, we don't actually need to track anything other than 'obs'. TODO
"""
struct CPerm
  obs::Perm
  homs::Dict{Symbol, Pair{Vector{Int}, Vector{Int}}}
  attrs::Perm
end

"""
Construct a CPerm given some parsed nauty output
E.g. oinds  - (:E => 1:7, :V => 8:12,...) 
     canon  - [6, 0, 5, 4, 1, 2, 3, 11, 10, 7, 8, 9, ...]
     canon2 - (:E => [7, 1, 6, 5, 2, 3, 4], :V => [5, 4, 1, 2, 3])
     omap   - (:E => (1,7,4,5,2)(3,6), :V => (1,5,3)(2,4))
"""
function CPerm(oinds::Dict{Symbol, UnitRange}, canon::Vector{Int}, S)
  canon2 = Dict([k=>canon[v].-(v.start-2) for (k,v) in oinds if k ∈ ob(S)])
  omap = Dict([k=>Permutation(v) for (k,v) in collect(canon2)])
  hmap = Dict(h => (canon2[s] => canon2[t]) for (h,s,t) in homs(S))
  amap = Dict(h => Permutation(canon2[s]) for (h,s,_) in attrs(S))
  CPerm(omap, hmap, amap)
end

# Nauty
#######
abstract type NautyRes end
"""
- hsh: string value representing the isomorphism class
- orb: partitions of Cset parts into orbits
  e.g. if E#2 = E#5, then these two elements are symmetric
- gens: generating automorphisms of the automorphism group
- ngroup: number of elements in the automorphism group
- iso from nauty input into canonical isomorph
- cset: canonical isomorph, codom of cmap
"""
@struct_hash_equal struct CanonicalCSet <: NautyRes 
  strhsh::String
  orbits::Dict{Symbol, Vector{Int}}
  generators
  ngroup::Int
  canonmap::Dict{Symbol,Vector{Int}}
  canon::ACSet
end

strhsh(n::CanonicalCSet) = n.strhsh
orbits(n::CanonicalCSet) = n.orbits
generators(n::CanonicalCSet) = n.generators
ngroup(n::CanonicalCSet) = n.ngroup
canonmap(n::CanonicalCSet) = n.canonmap
canon(n::CanonicalCSet) = n.canon


"""CanonicalCSet for an empty C-set"""
function CanonicalCSet(g::T) where T<:ACSet
  isempty(g) || error("Cannot make CanonicalCSet for nonempty ACSet")
  emp= Dict([o=>Int[] for o in ob(acset_schema(g))])
  CanonicalCSet("", emp, CPerm[], 1, emp, g)
end

"""
Make shell command to dreadnaut and collect output.
"""
function call_nauty(g::ACSet)
  S = acset_schema(g)
  if isempty(g) return NautyRes(g) end
  all(o -> nparts(g, o) == 0, attrtypes(S)) || error("VarACSets not yet supported")

  m, oinds, _ = to_adj(g) # convert g to matrix

  # Run nauty and collect output as a string
  inp = dreadnaut(g)
  tmp = tempname()
  cmd = "echo \"$inp\" | $(nauty_jll.dreadnaut_path) > $tmp"
  bashit(cmd)
  res = open(f->read(f, String), tmp)

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
                    [[parse(Int,x)+1 for x in split(only(m.captures)," ")] for m in cycs])
      is = Set(vcat(perm...))
      Permutation([perm;[[i] for i in 1:max_n if i ∉ is]])
    end
  end
  # parse permutation for the canonical graph
  rng = match(reg_perm, res[sec : end])
  cp = CPerm(oinds, [parse(Int, x) for x in split(strip(rng.match), r"\s+")], S)

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

  return CanonicalCSet(hshstr, orb, gens, grpsize, h, canong)
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
  return orb
end

"""Get all attribute values that are touched upon by the ACSet. Order them."""
function attr_dict(X::ACSet)
  d = DefaultDict(()->Any[])
  for (a,_,dt) in attrs(acset_schema(X))
    append!(d[dt], X[a])
  end
  return Dict([k=>sort(collect(unique(v))) for (k,v) in collect(d)])
end

"""
Convert C-Set to an adjacency matrix of an *undirected* simple graph.

the matrix has rows for all parts (e.g. |E| and |V|), all homs (e.g. |E|
quantity of src & tgt nodes), and another copy of all homs (called, e.g., _src
and _tgt). For a given edge in the category of elements eₙ -- src --> vₘ,
we set edges in the simple diagraph:

        ↗ _src
      ↙    ↕
    eₙ <-> srcₙ <-> vₘ


This "duplicate" hom vertex encodes the directionality of the hom, which is
ambiguous in the case of endomorphisms. (potential idea: only have the extra hom
when it's an endomorphism. counterpoint: color saturation should pretty easily
deal with it as it is, though).

For attributes, there is no possibility of Attr(X,X), so we simply have, e.g.:
    vₘ <-> vlabelₘ <-> valₓ
"""
function to_adj(X::ACSet)
  S = acset_schema(X)
  colors, curr, oinds = Any[], 1, Dict{Symbol}{UnitRange}()
  for o in ob(S)
    append!(colors, fill(o, nparts(X, o)))
    oinds[o] = curr:length(colors)
    curr = length(colors) + 1
  end

  attrdict = attr_dict(X)
  for (k,v) in collect(attrdict)
    append!(colors, fill(k, length(v)))
    oinds[k] = curr:length(colors)
    curr = length(colors) + 1
  end

  n_ob = length(colors)
  mat = zeros(Bool, (n_ob,n_ob))

  for (hom_name, d, cd) in homs(S)
    hom_name_ = Symbol("_$hom_name")
    orig_rows = size(mat)[1]
    nd = nparts(X, d)
    mat = [mat zeros(Bool, (orig_rows, 2*nd))]
    hom_mat = zeros(Bool, (2*nd,size(mat)[2]))
    for (i,v) in enumerate(X[hom_name])
      hom_mat[i,[oinds[cd][v],oinds[d][i]]] .= true
      hom_mat[nd+i,[oinds[d][i],orig_rows+i]] .= true
    end
    append!(colors, vcat(fill(hom_name, nparts(X, d))))
    oinds[hom_name] = curr:length(colors)
    curr = length(colors) + 1
    append!(colors, vcat(fill(hom_name_, nparts(X, d))))
    oinds[hom_name_] = curr:length(colors)
    curr = length(colors) + 1
    mat = [mat;hom_mat]
  end

  for (hom_name, d, cd) in attrs(S)
    nd = nparts(X, d)
    orig_rows = size(mat)[1]

    mat = [mat zeros(Bool, (orig_rows, nd))]
    hom_mat = zeros(Bool, (nd,size(mat)[2]))
    for (i,val) in enumerate(X[hom_name])
      v = findfirst(==(val), attrdict[cd])
      hom_mat[i,[oinds[cd][v],oinds[d][i]]] .= true
    end
    append!(colors, vcat(fill(hom_name, nparts(X, d))))
    oinds[hom_name] = curr:length(colors)
    curr = length(colors) + 1
    mat = [mat;hom_mat]
  end

  mr, mc = size(mat)
  mat = [mat zeros(Bool, mr, mr-mc)]
  colorsarray = Vector{Int}[]
  for c in OrderedSet(colors)
    if c ∉ keys(attrdict)
      push!(colorsarray,findall(==(c), colors)) # [1..n] we don't know order
    else
      # for attributes we do know the canonical order, so it's fully partitioned
      append!(colorsarray, [[oinds[c][i]] for i in 1:length(attrdict[c])])
    end
  end
  (mat .| mat', oinds, colorsarray)  # symmetrize matrix
end

"""
Convert from flattened indices (all parts altogether) to dict with indices 
for each distinct object, e.g. {V => 1:8, E => 9:13}
  """
get_oinds(X) = to_adj(X)[2]

"""
Symmetric adjacency matrix to ACSet.
"""
function from_adj(X::ACSet, oinds::Dict{Symbol, UnitRange},
                  m::AbstractMatrix{Bool})
  S = acset_schema(X)
  Y = deepcopy(X) # db with the right # of rows. We completely overwrite it.
  attrdict = attr_dict(X)

  inv_dict = Dict(vcat(map(collect(oinds)) do (k,vs)
    [v=>i for (i,v) in enumerate(vs)]
  end...))

  # Recover the homs
  for h in homs(S; just_names=true) 
    h_ = Symbol("_$h")
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
    for (_, h_i) in enumerate(oinds[h])
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
  m,_, colorsarray = to_adj(g)
  join(["n=$(size(m)[1]) g",
        join(map(1:size(m)[1]) do r
          join(string.((x->x-1).(findall(==(1),m[r,:]))) ," ")
        end, ";"),
        ". f = [$(join([join(c.-1, ",") for c in colorsarray],"|"))]",
        "c d x b z o"], " ")
end

"""
Apply a permutation on a ACSet, X. Results in a new ACSet, Y, and a map X->Y
"""
function apply(X::ACSet, p::CPerm)
  S = acset_schema(X)
  cd = deepcopy(X)
  for h in homs(S; just_names=true)
    σs, σt = [collect(x) for x in p.homs[h]]
    σti = invperm(collect(σt))
    f = collect(X[h])
    m =  compose(σs, f, σti)
    set_subpart!(cd, h, m)
  end

  for h in attrs(S; just_names=true)
    σs = Vector{Int}(collect(p.attrs[h]))
    f = collect(X[h])
    m =  f[σs]
    set_subpart!(cd, h, m)
  end

  h = Dict([k=>invperm(Vector{Int}(collect(v))) for (k,v) in p.obs])
  return cd, h
end


"""
Take advantage of the very special structure of automorphism generators given
by nauty to efficiently enumerate the automorphism group.
We expand our automorphism group with each generator.

To do: nauty provides an "index" along with each level of generators saying by 
what factor it increases the group. So if after gₙ we
have an automorphism group of size N, then if gₙ₊₁ has an index of 3,
then we will have fully incorporated the new generator when our group is of size
3*N. Moreover, there are just three new elements which start with gₙ₊₁ that we
need to find, which we compose with our earlier group to get the new group. Our
while loop explores the possible words that we can build (starting with gₙ₊₁)
"""
function all_autos(X::CanonicalCSet)
  res = all_autos(canon(X), generators(X))
  length(res) == X.ngroup || error("# of autos doesn't match nauty's calculation")
  return res
end

function all_autos(X::ACSet, gens)
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
      qgen, qlast = prod(q), q[end]
      if first(old_gens)*qgen ∉ all_gens
        for og in old_gens push!(all_gens, og * qgen) end
        append!(queue, [[q...,prev_g] for prev_g in
                       filter(!=(qlast), gens[1:i])])
      end
    end
  end
  return [to_perm(p,oinds) for p in all_gens]
end

end # module
