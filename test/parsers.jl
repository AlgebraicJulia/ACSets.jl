# ## Testing our parser
module ParserTests
using Test
using ACSets
using ACSets.ADTs
using ACSets.Parsers

@testset "args" begin
    @test arg("1")[1] == Value(1) #Fails but appears to be correct
    @test arg("test=2")[1] == Kwarg(:test, Value(2)) #Fails but appears to be correct
    #@test arg("(1, 2, 3)") #Collection has not been implemented yet
end

end