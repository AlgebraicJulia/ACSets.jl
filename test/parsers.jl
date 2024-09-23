# ## Testing our parser
module ParserTests
using Test
using ACSets
using ACSets.ADTs
using ACSets.Parsers

#Overaloads "==" to properly comapre two statement structs
function Base.:(==)(s1::ACSets.ADTs.Statement, s2::ACSets.ADTs.Statement)
    return s1.table == s2.table && s1.element == s2.element
end

#Taken from "PEG.jl/blob/master/test/misc.jl" to test parsing failure
function parse_fails_at(rule, input)
    try
      parse_whole(rule, input)
      "parse succeeded!"
    catch err
      isa(err, Meta.ParseError) || rethrow()
      m = match(r"^On line \d+, at column \d+ \(byte (\d+)\):", err.msg)
      m == nothing && rethrow()
      parse(Int, m.captures[1])
    end
  end


@testset "arg_test" begin
    @test arg("1")[1] == Value(1)
    @test arg("test=2")[1] == Kwarg(:test, Value(2))
    @test arg("(1,2,3)")[1] == ([Value(1), Value(2), Value(3)])
end

@testset "args_test" begin
    @test args("a, b, c")[1] == ([Value(:a), Value(:b), Value(:c)])
    @test args("1,2,3")[1] == ([Value(1), Value(2), Value(3)])
    @test args("a=1, b=2, c=3")[1] == ([Kwarg(:a, Value(1)), Kwarg(:b, Value(2)), Kwarg(:c, Value(3))])
end

@testset "statement_test" begin
    @test statement("test(a)")[1] == Statement(:test, [Value(:a)])
    @test statement("test(a, b)")[1] == Statement(:test, [Value(:a), Value(:b)])
    @test statement("E(src=1,tgt=3)")[1] == Statement(:E, [Kwarg(:src, Value(1)), Kwarg(:tgt, Value(3))])

end


@testset "line_test" begin
    @test line("test(a)\n")[1] == Statement(:test, [Value(:a)])
    @test line("test(a);")[1] == Statement(:test, [Value(:a)])
    @test line("     test(a)     \n")[1] == Statement(:test, [Value(:a)])
end

@testset "block_test" begin
    @test block("test(a)\n test(b)\n test(c)\n")[1] == [Statement(:test, [Value(:a)]), Statement(:test, [Value(:b)]), Statement(:test, [Value(:c)])]
end

@testset "body_test" begin
    @test body("quote\n test(a)\n test(b)\n test(c)\n end")[1] == [Statement(:test, [Value(:a)]), Statement(:test, [Value(:b)]), Statement(:test, [Value(:c)])]
end

@testset "Error_Handling" begin
    @test parse_fails_at(statement, "test(a, b") == 10
    #Needs a lot more testing
end

end