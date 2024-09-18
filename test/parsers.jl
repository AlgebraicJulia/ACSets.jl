# ## Testing our parser
module ParserTests
using Test
using ACSets
using ACSets.ADTs
using ACSets.Parsers

#Testing arg rule before applied transformation
@testset "arg" begin
    @test arg("1")[1] == "1"
end

end