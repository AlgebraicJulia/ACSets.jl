module Nauty

using Reexport 

include("NautyInterface.jl")
include("CSetAutomorphisms.jl")

@reexport using .CSetAutomorphisms
@reexport using .NautyInterface

end # module
