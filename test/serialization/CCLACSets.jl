module TestCCLACSets

using Test
import JSON3
using ACSets

@testset "Parsing CCL JSON" begin

    modeljson = JSON3.read("serialization/data/json/diffusivity_constant/model.json")
    parsed_model = CCLModel(modeljson)
    testmodel = UUIDLabeledGraph(parsed_model)

    @test testmodel[1, :elabel] == "d̃₁"
    @test testmodel[3, :elabel] == "-"

    diagramjson = JSON3.read("serialization/data/json/diffusivity_constant/diagram.json")
    parsed_diagram = CCLDiagram(diagramjson, testmodel)
    testdiagram = UUIDLabeledGraph(parsed_diagram)

    @test testdiagram[1, :elabel] == "d₀"

end

end
