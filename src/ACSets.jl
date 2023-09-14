module ACSets

using Reexport 

include("IndexUtils.jl")
include("Defaults.jl")
include("Mappings.jl")
include("PreimageCaches.jl")
include("Columns.jl")
include("ColumnImplementations.jl")
include("Schemas.jl")
include("ACSetInterface.jl")
include("DenseACSets.jl")
include("ADTs.jl")

@reexport using .ColumnImplementations: AttrVar
@reexport using .Schemas
@reexport using .ACSetInterface
@reexport using .DenseACSets
using .ADTs

include("serialization/JSONACSets.jl")

@reexport using .JSONACSets

end
