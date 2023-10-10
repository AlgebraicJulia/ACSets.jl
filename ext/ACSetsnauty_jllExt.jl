module ACSetsnauty_jllExt

using ACSets
using nauty_jll

bashit(str) = run(`bash -c "$str"`)

"""Compute CSetNautyRes from an ACSet."""
ACSets.call_nauty(g::ACSet)::CSetNautyRes = 
  ACSets.NautyInterface.parse_res(nauty_res(g), g)

"""Make shell command to dreadnaut (nauty) and collect stdout text."""
function nauty_res(g::T)::String where T<:ACSet
  if isempty(g) return NautyRes(g) end
  all(o -> nparts(g, o) == 0, attrtypes(acset_schema(g))
     ) || error("VarACSets not yet supported")
  tmp = tempname()
  bashit("echo \"$(ACSets.NautyInterface.dreadnaut(g))\" | $(nauty_jll.dreadnaut_path) > $tmp")
  return open(f->read(f, String), tmp)
end

end # module
