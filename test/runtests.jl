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

@testset "Serialization" begin
  include("serialization/Serialization.jl")
end

@testset "ADTs" begin
  include("ADTs.jl")
end
