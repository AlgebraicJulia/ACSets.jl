module CCLACSets
export read_ccl_acset

using ...ACSetInterface, ...Schemas, ..ACSetSerialization, ...DenseACSets

using ACSets

using JSON3
using MLStyle

@data AnElement begin
    ObjectElem(name::String, type::String)
    HomElem(name::String, dom::String, cod::String)
end

struct CCLModel
    name::Symbol
    elem::Dict{String, AnElement}
end

struct CCLDiagram
    name::Symbol
    elem::Dict{String, AnElement}
end

# Interface
###########

function parse_json(json::AbstractDict; predecessor=nothing)
    @match json.type begin
        "model" => parse_model(json, predecessor)
        "diagram" => parse_diagram(json, predecessor)
        _ => nothing
    end
end

# TODO convert function name to constructor
function parse_model(json::AbstractDict, predecessor=nothing)
    dict = Dict{String,AnElement}()
    foreach(json.notebook.cells) do cell
        @match cell.content.tag begin
            "object" => 
            push!(dict, cell.content[:id] => ObjectElem(cell.content[:name], ""))
            "morphism" => begin
                name = 
                if isempty(cell.content[:name])
                    if isnothing(cell.content[:over])
                        cell.content[:name]
                    else
                        cell.content[:over][:content]
                    end
                else
                    cell.content[:name]
                end
                push!(dict, cell.content[:id] => 
                HomElem(name, 
                     cell.content[:dom][:content], cell.content[:cod][:content]))
            end
        end
    end
    CCLModel(Symbol(json.theory), dict)
end

function parse_diagram(json::AbstractDict, predecessor=nothing)
    dict = Dict{String,AnElement}()
    foreach(json.notebook.cells) do cell
        @match cell.content.tag begin
            "object" => begin
                type = subpart(predecessor, incident(predecessor, cell.content[:over][:content], :vUuid), :vlabel)
                push!(dict, cell.content[:id] => ObjectElem(cell.content[:name], only(type)))
            end
            "morphism" => begin
                name = if isempty(cell.content[:name])
                    if isnothing(cell.content[:over])
                        cell.content[:name]
                    else
                        cell.content[:over][:content]
                    end
                else
                    cell.content[:name]
                end
                # TODO elabel is empty because it is a scalar
                label = @something emptyMaybe(subpart(predecessor, incident(predecessor, name, :eUuid), :elabel)) emptyMaybe(name) ["EMPTY"]
                if label == ["EMPTY"]
                    @warn cell
                end
                push!(dict, cell.content[:id] => 
                      HomElem(only(label), cell.content[:dom][:content], cell.content[:cod][:content]))
            end
        end
    end
    CCLDiagram(gensym(), dict)
end

SchLabeledGraph = BasicSchema([:E, :V], 
                              [(:src, :E, :V), (:tgt, :E, :V)], [:Label, :UUID], 
                              [(:vlabel, :V, :Label), (:elabel, :E, :Label), (:vtype, :V, :Label)
             ,(:vUuid, :V, :UUID), (:eUuid, :E, :UUID)])
@acset_type UUIDLabeledGraph(SchLabeledGraph, index=[:src,:tgt])

emptyMaybe(x) = isempty(x) ? nothing : Some(x)

function UUIDLabeledGraph(m::CCLModel)
    g = UUIDLabeledGraph{String, String}()
    # TODO a little redundant
    newobs = Dict()
    newhoms = Dict()
    foreach(pairs(m.elem)) do (k,v)
        @match v begin
            ::ObjectElem => push!(newobs, k => v)
            ::HomElem => push!(newhoms, k => v)
        end
    end
    nc = 0
    foreach(pairs(newobs)) do (k,v)
        add_part!(g, :V, vlabel=v.name, vUuid=k)
    end
    foreach(pairs(newhoms)) do (k,v)
        src = @something emptyMaybe(incident(g, v.dom, :vUuid)) begin
                             nc += 1
                             add_part!(g, :V, vlabel="•$nc", vUuid=v.dom)
                        end
        tgt = @something emptyMaybe(incident(g, v.cod, :vUuid)) begin
                             nc += 1
                             add_part!(g, :V, vlabel="•$nc", vUuid=v.cod)
                        end
        add_part!(g, :E, src=only(src), tgt=only(tgt), eUuid=k, elabel=v.name)
    end
    g
end

function UUIDLabeledGraph(m::CCLDiagram)
    g = UUIDLabeledGraph{String, String}()
    # TODO a little redundant
    newobs = Dict()
    newhoms = Dict()
    foreach(pairs(m.elem)) do (k,v)
        @match v begin
            ::ObjectElem => push!(newobs, k => v)
            ::HomElem => push!(newhoms, k => v)
        end
    end
    nc = 0
    foreach(filter(x -> x[2] isa ObjectElem, pairs(m.elem))) do (k, v)
        add_part!(g, :V, vlabel=v.name, vUuid=k, vtype=v.type)
    end
    foreach(pairs(newhoms)) do (k,v)
        src = @something emptyMaybe(incident(g, v.dom, :vUuid)) begin
                             nc += 1
                             add_part!(g, :V, vlabel="•$nc", vUuid=v.dom)
                        end
        tgt = @something emptyMaybe(incident(g, v.cod, :vUuid)) begin
                             nc += 1
                             add_part!(g, :V, vlabel="•$nc", vUuid=v.cod)
                        end
        add_part!(g, :E, src=only(src), tgt=only(tgt), eUuid=k, elabel=v.name)
    end
    g
end

export parse_json, UUIDLabeledGraph


end
