using Test

@testset "Utilities" begin
  include("IndexUtils.jl")
end

@testset "Mappings" begin
  include("Mappings.jl")
end

@testset "Columns" begin
  include("Columns.jl")
end

@testset "Schemas" begin
  include("Schemas.jl")
end

@testset "ACSets" begin
  include("ACSets.jl")
end

@testset "Parsers" begin
  include("Parsers.jl")
end

@testset "Serialization" begin
  include("serialization/Serialization.jl")
end

@testset "InterTypes" begin
  include("intertypes/InterTypes.jl")
end

@testset "ADTs" begin
  include("ADTs.jl")
end

if !Sys.iswindows()
  @testset "Nauty" begin
    include("NautyInterface.jl")
  end
end
