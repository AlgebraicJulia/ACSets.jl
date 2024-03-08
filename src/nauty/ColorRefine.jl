
using StructEquality 
using .....ColumnImplementations: AttrVar

"""
Data for an individual component (each vector corresponds to its elements)
1.) how many of each color (for each in-arrow) targets this point
2.) what color this point targets (for each out arrow)

This could be extended to add extra automorphism-invariant properties.
E.g. detecting if src+tgt both point to the same element
"""
@struct_hash_equal struct CDataPoint
  indata::Vector{Vector{Int}}
  outdata::Vector{Int}
end

"""Data required to color a CSet (each element of each component)"""
const CData = Dict{Symbol, Vector{CDataPoint}}

"""
Computes colors for a CSet, distinguishing nodes by their immediate
connectivity. It is not sufficient to compute the automorphism group, but it is
a good starting point.

This does not generalize to ACSets. We cannot naively throw the attributes as
raw data into the color data. It will make indistinguishable elements (e.g. two
elements that map to different data but otherwise can be permuted) as
distinguishable.
"""
function compute_color_data(g::ACSet, color::CDict)::CData
  S = acset_schema(g)
  res = CData()
  for tab in ob(S) # compute colordata for each tab
    subres = map(homs(S; to=tab)) do (arr, src, _) # vector for each in-arrow
      color_src = color[src]
      subsubres = zeros(Int, nparts(g, tab), max0(color_src))
      for (colorsrc, arrtgt) in zip(color_src, g[arr])
        subsubres[arrtgt, colorsrc] += 1
      end
      subsubres
    end

    # Also compute per-element data for table `tgt` (now, regard as a src)
    out_subres = map(homs(S; from=tab)) do (oga, _, tgt)
      color[tgt][g[oga]]
    end 
    
    # Combine the two pieces of data for each elmeent in tgt, store in res
    res[tab] = map(parts(g,tab)) do i 
      CDataPoint([ssr[i,:] for ssr in subres], [osr[i] for osr in out_subres])
    end
  end
  res
end

"""Initial state for tree search: every element is symmetric"""
nocolor(g::ACSet) = 
  CDict([k => ones(Int, nparts(g, k)) for k in ob(acset_schema(g))])

"""
Iterative color refinement based on the number (and identity) of incoming and
outgoing arrows.
Inputs:
  - g: CSet we are color saturating
  - init_color: initial coloring, if any (default: uniform)
Returns:
  - trajectory of colorings
"""
function color_saturate(g::ACSet; init_color::Union{Nothing,CDict}=nothing)
  # Default: uniform coloring
  new_color = isnothing(init_color) ? nocolor(g) : init_color

  prev_n, curr_n, iter = 0, 1, 0
  hashes = Dict{Symbol, Vector{UInt}}()
  while prev_n != curr_n
    iter += 1
    prev_color = new_color
    # All that matters about newdata's type is that it is hashable
    newdata = compute_color_data(g, prev_color)
    # Distinguish by both color AND newly computed color data
    new_datahash = Dict{Symbol, Vector{UInt}}(
      [k=>map(hash, zip(prev_color[k],v)) for (k, v) in collect(newdata)])
    # Identify set of new colors for each component
    hashes = Dict{Symbol, Vector{UInt}}(
      [k=>sort(collect(Set(v))) for (k, v) in new_datahash])
    # Assign new colors by hash value of color+newdata
    new_color = CDict([
      k=>[findfirst(==(new_datahash[k][i]), hashes[k])
          for i in 1:nparts(g, k)]
      for (k, v) in new_datahash])
    prev_n = sum(map(max0, values(prev_color)))
    curr_n = sum(map(max0, values(new_color)))
  end
  new_color
end
