module DataFramesACSetsExt

using ACSets
using ACSets.Query: build_nt
using DataFrames

(qf::DFQueryFormatter)(q, a, s) = DataFrame(build_nt(q, s))

end # module
