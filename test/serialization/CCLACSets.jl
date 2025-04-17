module TestCCLACSets

using Test

import JSON3
using ACSets

@testset "" begin

    modeljson = JSON3.read("data/json/diffusivity_constant/model.json")
    parsed_model = parse_json(modeljson)
    testmodel = UUIDLabeledGraph(parsed_model)

    @test testmodel[1, :elabel] == "d̃₁"
    @test testmodel[3, :elabel] == "-"

    diagramjson = JSON3.read("data/test/json/diffusivity_constant/diagram.json")
    parsed_diagram = parse_json(diagramjson, predecessor=testmodel)
    testdiagram = UUIDLabeledGraph(parsed_diagram)

    @test testdiagram[1, :elabel] == "d₀"

end

end
