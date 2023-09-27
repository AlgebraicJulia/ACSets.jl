# MySch = BasicSchema([:X], [],[:XAttr],[(:attr,:X,:XAttr)])
# @acset_type MyData(MySch)

# x = MyData{Union{String,Symbol}}()
# add_part!(x, :X, attr = "hi")
# add_part!(x, :X, attr = :hi)

# x = MyData{Union{String,Symbol,Nothing}}()
# add_part!(x, :X, attr = "hi")
# add_part!(x, :X, attr = :hi)
# add_part!(x, :X, attr = nothing)
# add_part!(x, :X, attr = "hi")

# # this is curious, why do the preimage() tests fail but this works
# incident(x, nothing, :attr)

# preimage(dom_parts(x, :attr), x.subparts[:attr], nothing)
# @which preimage(dom_parts(x, :attr), x.subparts[:attr], nothing)
# preimage(dom_parts(x, :attr), x.subparts[:attr].m, x.subparts[:attr].pc, nothing)

# col =column_type(AttrChoice(Union{Symbol,Nothing}), NoIndex, Dense)(1=>:hi,2=>:hi,3=>nothing)
# preimage(OneTo(3), col, nothing)

# x = MyData{Union{String,Symbol,Missing}}()
# add_part!(x, :X, attr = "hi")
# add_part!(x, :X, attr = :hi)
# add_part!(x, :X, attr = missing)

# incident(x, missing, :attr)
# preimage(dom_parts(x, :attr), x.subparts[:attr].m, x.subparts[:attr].pc, missing)