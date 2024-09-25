# ## Testing our parser
module ParserTests

using Test
using ACSets, ACSets.ADTs
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

# -------------- Test Groups per rule: -------------- #
@testset "arg_test" begin
    @test arg("1")[1] == Value(1)
    @test arg("test=2")[1] == Kwarg(:test, Value(2))
    @test arg("(1,2,3)")[1] == ([Value(1), Value(2), Value(3)])
end

@testset "args_test" begin
    @test args("a, b, c")[1] == ([Value(:a), Value(:b), Value(:c)])
    @test args("1,2,3")[1] == ([Value(1), Value(2), Value(3)])
    @test args("1 , 1")[1] == ([Value(1), Value(1)])
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


# -------------- Test Error Handling -------------- #

@testset "arg_handling" begin
   @test parse_fails_at(arg, "(1") == 3 #Missing "(" at index 3
   @test parse_fails_at(arg, "a=") == 3 #Missing value after "=" at index 3
   @test parse_fails_at(arg,"invalid→ident") == 8 #Invalid character at index 1
end

@testset "args_handling" begin
    @test parse_fails_at(args, "1,") == 3 #Missing value after "," at index 3
    @test parse_fails_at(args, ", 3") == 1 #Missing initial value before "," at index 1
    @test parse_fails_at(args, "1  1") == 4 #Missing "," at index 4
end

@testset "statement_handling" begin
    @test parse_fails_at(statement, "test") == 5 #Missing "(" at index 5
    @test parse_fails_at(statement, "test()") == 6 #Missing argument at index 6
    @test parse_fails_at(statement, "test(1") == 7 #Missing ")" at index 7
end

@testset "line_handling" begin
    @test parse_fails_at(line, "test(a)") == 8 #Missing EOL at index 8
    @test parse_fails_at(line, "test(a) test(b)") == 9 #Missing EOL at index 9
    @test parse_fails_at(line, ";") == 1 #Missing statement at index 1
end

@testset "block_handling" begin
    @test parse_fails_at(block, "test(a) test(b)") == 9 #Missing EOL at index 9
end

@testset "acset_spec_handling" begin
    @test parse_fails_at(acset_spec, "begin\ntest(a)\ntest(b)\nend") == 7
    # Missing head at index 1, however, parser will try and create a head until
    # it reaches a white space at index 6. This can be modified for better parsing errors.
    # However, for the simplicity of the grammar, I'm keeping it as is for now.
    # Right now, it registers begin as a head and fails due to the true "begin" missing.
end

# ------------ Full Scale Tests ----------- #

PEG.setdebug!(false)

SchLabeledGraph = BasicSchema([:E,:V], [(:src,:E,:V),(:tgt,:E,:V)],
                          [:L], [(:label,:V,:L)])

@acset_type LabeledGraph(SchLabeledGraph, index=[:src,:tgt])

@testset "Constructing ACSets from Specs" begin
    gspec = ACSetSpec(
      :(LabeledGraph{Symbol}),
      [
          Statement(:V, [Kwarg(:label, Value(:a))])
          Statement(:V, [Kwarg(:label, Value(:b))])
          Statement(:V, [Kwarg(:label, Value(:c))])
          Statement(:E, [Kwarg(:src, Value(1)), Kwarg(:tgt, Value(3))])
          Statement(:E, [Kwarg(:src, Value(2)), Kwarg(:tgt, Value(3))])
      ]
    )
    g = construct(LabeledGraph{Symbol}, gspec)
    @test nparts(g, :V) == 3
    @test nparts(g, :E) == 2

    hspec = acsetspec"""
    LabeledGraph{Symbol}
    begin
      V(label=a)
      V(label=b)
      V(label=c)
      E(src=1,tgt=3)
      E(src=2,tgt=3)
    end"""

    @test construct(LabeledGraph{Symbol}, hspec) == construct(LabeledGraph{Symbol}, gspec)
end

end