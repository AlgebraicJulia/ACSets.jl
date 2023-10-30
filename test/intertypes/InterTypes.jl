module TestInterTypes

using SyntacticModels.InterTypes
using Test

include(as_intertypes(), "ast.intertypes")

@test intertype(Term) isa InterType

t = Plus([Constant(ConstInt(1)), Constant(ConstInt(2))])

@test jsonwrite(t) isa String
@test jsonread(jsonwrite(t), Term) == t

end
