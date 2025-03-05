using ACSets

SchDDS = BasicSchema([:X], [(:Φ,:X,:X)])
@abstract_acset_type AbstractDDS
@acset_type DDS(SchDDS, index=[:Φ]) <: AbstractDDS
DDS(i::Int) = DDS(rand(1:i, i))
function DDS(v::Vector{Int})
  x = DDS()
  add_parts!(x, :X, length(v))
  set_subpart!(x, :Φ, v)
  x
end

x = DDS(2)
y = copy(x)
y[1, :Φ] = 2

z = DDS(3)


p = DDS(8)
q = DDS(7)

RecAttrSch = BasicSchema(
  [:Thing,:Node,:Edge], [(:src,:Edge,:Node),(:tgt,:Edge,:Node),(:thing,:Thing,:Node)],
  [:Attr1,:Attr2,:Attr3],[(:attr1,:Node,:Attr1),(:attr2,:Edge,:Attr2),(:attr3,:Thing,:Attr3)])
@acset_type RecAttrData(RecAttrSch, index=[:src,:tgt], unique_index=[:thing])
sch = acset_schema(RecAttrData{String, Symbol, Float64}())

d1 = @acset RecAttrData{String,Symbol,Float64} begin
    Thing=3
    Node=3
    Edge=3
    thing=[1,2,3]
    src=[1,1,2]
    tgt=[1,2,3]
    attr1=["1","2","3"]
    attr2=[:a,:b,:c]
    attr3=[10.0,11.0,12.0]
end
d2 = @acset RecAttrData{String,Symbol,Float64} begin
    Thing=3
    Node=3
    Edge=3
    thing=[1,2,3]
    src=[1,3,2]
    tgt=[1,2,3]
    attr1=["a!","b!","c!"]
    attr2=[:b,:d,:c]
    attr3=[10.0,11.0,12.0]
end

Δ = diff(d1, d2)

Δ(d1)

d(d1, :attr2)

d′ = diff(d2, d1)

d′(d2, :attr2)

Differ(Δ(d1))
