module TestCSetAutomorphisms

using Test
using ACSets
using Permutations

using ACSets.CSetAutomorphisms: to_nauty_res

SchGraph = BasicSchema([:E,:V], [(:src,:E,:V),(:tgt,:E,:V)])

@acset_type Graph(SchGraph, index=[:src,:tgt])

@test CSetNautyRes(Graph()).canon == Graph()

"""
Add a graph with 5 vertices and 7 edges to an existing graph. This is one 
of two isomorphic graphs, controlled by the boolean argument.
"""
function addV5E7!(G::Graph=Graph(), flag::Bool=true)
  vs = add_parts!(G,:V, 5)
  s, t = (flag ? ([1,2,3,2,3,1,4],[2,3,1,1,2,3,5])
               : ([2,3,1,3,1,2,5],[3,1,2,2,3,1,4]))
  add_parts!(G, :E, 7; src=vs[s], tgt=vs[t])
  G
end

G = addV5E7!()
H = addV5E7!(Graph(), false)

@test allequal(canon.(to_nauty_res.([G, H])))

# Attributes

SchDecGraph = BasicSchema([:E,:V], [(:src,:E,:V),(:tgt,:E,:V)],
                          [:X], [(:dec,:E,:X)])

@acset_type Labeled(SchDecGraph)

G = @acset Labeled{String} begin
  V = 4; E = 4; src = [1,2,3,4]; tgt = [2,3,4,1]; dec = ["a","b","c","d"]
end;
K = @acset Labeled{String} begin
  V = 4; E = 4; src = [2,3,4,1]; tgt = [3,4,1,2]; dec = ["b","c","d","a"]
end;
H = @acset Labeled{String} begin
  V = 4; E = 4; src = [3,2,4,1]; tgt = [2,4,1,3]; dec = ["b","c","d","a"]
end;

@test allequal(canon.(to_nauty_res.([G, K, H])))

# AttrVars 

G = @acset Labeled{String} begin
  V = 4; E = 4; X=2; src = [1,2,3,4]; tgt = [2,3,4,1]; 
  dec = ["a", AttrVar.(1:2)...,"d"]
end;
K = @acset Labeled{String} begin
  V = 4; E = 4; X=2;src = [2,3,4,1]; tgt = [3,4,1,2]; 
  dec = [reverse(AttrVar.(1:2))...,"d","a"]
end;
H = @acset Labeled{String} begin
  V = 4; E = 4; X=2; src = [3,2,4,1]; tgt = [2,4,1,3]; 
  dec = [reverse(AttrVar.(1:2))...,"d","a"]
end;

@test allequal(canon.(to_nauty_res.([G, K, H])))

end # module
