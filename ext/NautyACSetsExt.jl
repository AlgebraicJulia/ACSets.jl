module NautyACSetsExt

using ACSets
using nauty_jll

"""Compute CSetNautyRes from an ACSet."""
function ACSets.call_nauty(g::ACSet)::CSetNautyRes
  ACSets.NautyInterface.parse_res(nauty_res(g), g)
end

"""Make shell command to dreadnaut (nauty) and collect stdout text."""
function nauty_res(g::ACSet)::AbstractString
  isempty(g) && return NautyRes(g)
  all(o -> nparts(g, o) == 0, attrtypes(acset_schema(g))) ||
    error("Nauty integration with VarACSets not yet supported")
  process = open(`$(nauty_jll.dreadnaut_path)`, write=true, read=true)
  print(process, ACSets.NautyInterface.dreadnaut(g))
  close(process.in)
  read(process, String)
end

end # module
