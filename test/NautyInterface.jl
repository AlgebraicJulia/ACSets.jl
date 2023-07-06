module TestNautyInterface

using Test
using ACSets
using Permutations

# Helper function
#################
"""Check if a map between acsets is an isomorphism"""
iso(X::T, Y::T, comps::Dict{Symbol, Permutation}) where {T <: ACSet} = 
  iso(X,Y,Dict(k=>Vector{Int}(collect(v)) for (k,v) in comps))
function iso(X::T, Y::T, comps::Dict{Symbol, Vector{Int}}) where {T <: ACSet}
  all(isperm, values(comps)) || return false
  all(((h, c, cd),) -> comps[cd][X[h]] == Y[h][comps[c]], homs(acset_schema(X)))
end
iso(X,R::CanonicalCSet) = iso(X, canon(R), canonmap(R))

# Graphs
########

SchGraph = BasicSchema([:E,:V], [(:src,:E,:V),(:tgt,:E,:V)])

@acset_type Graph(SchGraph, index=[:src,:tgt])

G = @acset Graph begin V=5; E=7; src=[1,2,3,2,3,1,4]; tgt=[2,3,1,1,2,3,5] end
H = @acset Graph begin V=5; E=7; src=[2,3,1,3,1,2,5]; tgt=[3,1,2,2,3,1,4] end

cG, cH = call_nauty.([G,H])
@test canon(cG) == canon(cH)
@test iso(G,cG) && iso(H,cH)
# calling `all_autos` confirms the # of automorphisms = # that nauty computes
@test all(h->iso(G, G, h), all_autos(G, generators(cG)))

# 8 symmetries of D₄
sqr = @acset Graph begin V=4;E=8;src=[1,2,3,4,1,2,3,4];
                                 tgt=[2,3,4,1,4,3,2,1] end

# A square has D₄ symmetry id, s, r, r², r³, sr, sr², sr³
@test all(h->iso(sqr, sqr, h), all_autos(call_nauty(sqr)))


# ACSets
########
SchDecGraph = BasicSchema([:E,:V], [(:src,:E,:V),(:tgt,:E,:V)],
                          [:X], [(:dec,:E,:X)])

@acset_type Labeled(SchDecGraph)

G = @acset Labeled{String} begin
  V = 4; E = 4; src = [1,2,3,4]; tgt = [2,3,4,1]; dec = ["a","b","c","d"]
end;
H = @acset Labeled{String} begin
  V = 4; E = 4; src = [1,3,2,4]; tgt = [3,2,4,1]; dec = ["a","b","c","d"]
end;

cG, cH = call_nauty.([G,H])
@test strhsh(cG) == strhsh(cH)
@test canon(cG) == canon(cH)
@test iso(G,cG) && iso(H,cH)
@test all(h->iso(G, G, h), all_autos(G, generators(cG)))
@test length(all_autos(G, generators(cG))) == 1

G1 = @acset Labeled{String} begin
  V = 1; E = 1; src = [1]; tgt = [1]; dec = ["a"]
end;
H1 = @acset Labeled{String} begin
  V = 1; E = 1; src = [1]; tgt = [1]; dec = ["b"]
end;
cG1, cH1 = call_nauty.([G1,H1])

@test strhsh(cG1) != strhsh(cH1)

end # module
