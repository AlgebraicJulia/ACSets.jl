using Test

@testset "JSON" begin
  include("JSONACSets.jl")
end

@testset "Excel" begin
  include("ExcelACSets.jl")
end

@testset "CCL" begin
  include("CCLACSets.jl")
end
