"""
This is code which is required because nauty_jll is not supported on Windows: 
thus we need a makeshift implementation of Nauty within Julia. There is 
potential to do this in a much cleaner and more efficient way with a virtual 
machine, but for now performance is not a priority. 
"""
module CSetAutomorphisms

using ...ACSetInterface, ...DenseACSets, ...Schemas
using ..NautyInterface
using Permutations 


# Color assigned to each elem of each component
const CDict = Dict{Symbol, Vector{Int}}

"""Construct permutation σ⁻¹ such that σσ⁻¹=id"""
invert_perms(x::CDict) = Dict([k=>Base.invperm(v) for (k, v) in collect(x)])

check_auto(x::CDict)::Bool = all(Base.isperm, values(x))

# Sequence of keys, to index a Tree
const VPSI = Vector{Pair{Symbol, Int}}

max0(x::Vector{Int})::Int = isempty(x) ? 0 : maximum(x)

include("ColorRefine.jl")

# CSets with attributes replaced w/ combinatorial representatives
#################################################################

"""
To compute automorphisms of Attributed CSets, we create a pseudo CSet which has
additional components for each data type.

This is inefficient for attributes which have a total order on them
(e.g. integers/strings) since we solve for a canonical permutation of the
attributes. Future work could address this by initializing the coloring with
the 'correct' canonical order.
"""
function pseudo_cset(g::ACSet)::Tuple{ACSet, Dict{Symbol,Vector{Any}}} 
  # Create copy of schema (+ an extra component for each datatype)
  S = acset_schema(g)
  pres = deepcopy(S)
  append!(pres.obs, pres.attrtypes)
  append!(pres.homs, pres.attrs)
  empty!.([pres.attrtypes, pres.attrs])

  # Use Julia ordering to give each value an index
  attrvals = Dict(map(attrtypes(S)) do at
    vals = Set{Any}()
    [union!(vals, g[a]) for a in attrs(S; just_names=true, to=at)] 
    at => vcat(filter(x->!(x isa AttrVar), vals) |> collect |> sort, 
               AttrVar.(parts(g, at)))
  end)

  # Create and populate pseudo-cset
  res = AnonACSet(pres, index=arrows(S; just_names=true))

  copy_parts!(res, g)

  # Replace data value with an index for each attribute
  for t in attrtypes(S)
    add_parts!(res, t, length(attrvals[t]) - nparts(g, t))
    for (a,d,_) in attrs(S; to=t)
      for p in parts(g, d)
        res[p, a] = findfirst(==(g[p, a]), attrvals[t])
      end
    end
  end

  (res, attrvals)
end

"""
Inverse of pseudo_cset. Requires mapping (generated by `pseudo_cset`) of indices
for each Data to the actual data values.
"""
function pseudo_cset_inv(g::ACSet, orig::ACSet, attrvals::AbstractDict) 
  S = acset_schema(orig)
  orig = deepcopy(orig)
  for arr in hom(S)
    orig[arr] = g[arr]
  end
  for (darr,_,tgt) in attrs(S)
    orig[darr] = attrvals[tgt][g[darr]]
  end
  orig
end

# Results
#########

"""Apply a coloring to a C-set to get an isomorphic cset"""
function apply_automorphism(c::ACSet, d::CDict)
  check_auto(d) || error("received coloring that is not an automorphism: $d")
  new = deepcopy(c)
  for (arr, src, tgt) in homs(acset_schema(c))
    new[d[src], arr] = d[tgt][c[arr]]
  end
  for (arr, src, _) in attrs(acset_schema(c))
    new[d[src], arr] = c[arr]
  end
  new
end

function to_nauty_res(g::ACSet)
  p, avals = pseudo_cset(g)
  c, m = [pseudo_cset_inv(apply_automorphism(p, Dict(a)), g, avals) => a 
          for a in autos(p)[1]] |> sort |> first
  strhsh = string(c)
  orbits = Dict{Symbol, Vector{Int}}() # todo
  generators = Pair{Int, Vector{Permutation}}[] # todo
  CSetNautyRes(strhsh, orbits, generators, 1, m, c)
end

# Trees
#######

"""
Search tree explored by Nauty. Each node has an input coloring, a refined 
coloring, and a set of children indexed by which element (in the smallest 
nontrivial orbit) has its symmetry artificially broken.
"""
struct Tree
  coloring::CDict
  saturated::CDict
  children::Dict{Pair{Symbol, Int}, Tree}
  Tree() = new(CDict(), CDict(), Dict{Pair{Symbol, Int}, Tree}())
end

"""Get a node via a sequence of edges from the root"""
function Base.getindex(t::Tree, pth::VPSI)::Tree
  ptr = t
  for p in pth
    ptr = ptr.children[p]
  end
  ptr
end

"""
Get vector listing nontrivial colors (which component and which color index) as
well as how many elements have that color. E.g. for (V=[1,1,2], E=[1,2,2,2,3,3])
we would get `[2=>(:V,1), 3=>(:E,2), 2=>(:E, 3)]`
"""
function get_colors_by_size(coloring::CDict)::Vector{Pair{Int,Tuple{Symbol, Int}}}
  res = []
  for (k, v) in coloring
    for color in 1:max0(v)
      n_c = count(==(color), v)
      n_c > 1 && push!(res, n_c => (k, color)) # Store which table and which color
    end
  end
  res
end


"""To reduce branching factor, split on the SMALLEST nontrivial partition"""
function split_data(coloring::CDict)::Tuple{Symbol, Int, Vector{Int}}
  colors_by_size = sort(get_colors_by_size(coloring), rev=false)
  isempty(colors_by_size) && return :_nothing, 0, []
  split_tab, split_color = colors_by_size[1][2]
  colors = coloring[split_tab]
  split_inds = findall(==(split_color), colors)
  (split_tab, split_color, split_inds)
end

"""
DFS tree of colorings, with edges being choices in how to break symmetry
Goal is to acquire all leaf nodes.

Algorithm from "McKay’s Canonical Graph Labeling Algorithm" by Hartke and
Radcliffe (2009).

McKay's "Practical Graph Isomorphism" (Section 2.29: "storage of identity
nodes") warns that it's not a good idea to check for every possible automorphism
pruning (for memory and time concerns). To do: look into doing this in a more
balanced way. Profiling code will probably reveal that checking for automorphism
pruning is a bottleneck.

Inputs:
 - g: our structure that we are computing automorphisms for
 - res: all automorphisms found so far
 - split_seq: sequence of edges (our current location in the tree)
 - tree: all information known so far - this gets modified
 - leafnodes: coordinates of all automorphisms found so far
"""
function search_tree!(g::ACSet, init_coloring::CDict, split_seq::VPSI,
                      tree::Tree, leafnodes::Set{VPSI})
  curr_tree = tree[split_seq]
  # Perform color saturation
  coloring = color_saturate(g; init_color=init_coloring)
  for (k, v) in pairs(coloring)
    curr_tree.coloring[k] = init_coloring[k]
    curr_tree.saturated[k] = v
  end

  split_tab, _, split_inds = split_data(coloring)

  # Check if we are now at a leaf node
  if isempty(split_inds)
    # Add result to list of results
    push!(leafnodes, split_seq)
    check_auto(coloring) # fail if not a perm
  else 
    # Branch on this leaf
    for split_ind in split_inds
      if split_ind == split_inds[1]
        # Construct arguments for recursive call to child
        new_coloring = deepcopy(coloring)
        new_seq = vcat(split_seq, [split_tab => split_ind])
        new_coloring[split_tab][split_ind] = maximum(coloring[split_tab]) + 1
        curr_tree.children[split_tab => split_ind] = Tree()
        search_tree!(g, new_coloring, new_seq, tree, leafnodes)
      end
    end
  end
end

"""Compute the automorphisms of a CSet"""
function autos(g::ACSet)::Tuple{Set{CDict}, Tree}
  tree, leafnodes = Tree(), Set{VPSI}()
  search_tree!(g, nocolor(g), VPSI(), tree, leafnodes)
  Set([tree[ln].saturated for ln in leafnodes]), tree
end

end # module