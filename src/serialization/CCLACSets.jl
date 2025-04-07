module CCLACSets
export read_ccl_acset

using ...ACSetInterface, ...Schemas, ..ACSetSerialization

@kwdef struct CCLSpec end

@data _CCL begin
    TheModel(name::Symbol, elem::Dict{String, ModelElement})
    ThDiagram
end


struct CCLModel
    name::Symbol
    elem::Dict{String, AnElement}
end

struct CCLDiagram
    name::Symbol
    elem::Dict{String, AnElement}
end

@data AnElement begin
    ObjectElem(name::String)
    HomElem(name::String, dom::String, cod::String)
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
    dict = Dict{String, Any}()
    foreach(json.notebook.cells) do cell
        @match cell.content.tag begin
            "object" => 
            push!(dict, cell.content[:id] => ObjectElem(cell.content[:name]))
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
                push!(dict, cell.content[:id] => 
                HomElem(name, 
                     cell.content[:dom][:content], cell.content[:cod][:content]))
            end
        end
    end
    CCLModel(Symbol(json.theory), dict)
end

function parse_diagram(json::AbstractDict, predecessor=nothing)
    dict = Dict{String, Any}()
    foreach(json.notebook.cells) do cell
        @match cell.content.tag begin
            "object" => 
            push!(dict, cell.content[:id] => ObjectElem(cell.content[:name]))
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
                push!(dict, cell.content[:id] => 
                HomElem(name, cell.content[:dom][:content], cell.content[:cod][:content]))
            end
        end
    end
    CCLDiagram(gensym(), dict)
end

SchLabeledGraph = BasicSchema([:E, :V], 
                              [(:src, :E, :V), (:tgt, :E, :V)], [:Label, :UUID], 
            [(:vlabel, :V, :Label), (:elabel, :E, :Label)
             ,(:vUuid, :V, :UUID), (:eUuid, :E, :UUID)])
@acset_type UUIDLabeledGraph(SchLabeledGraph, index=[:src,:tgt])


modeljson = JSON3.read("test/json/model_dec.json")
parsed_model = parse_json(modeljson)
model = UUIDLabeledGraph(parsed_model)

diagramjson = JSON3.read("test/json/diagram.json")
parsed_diagram = parse_json(diagramjson, predecessor=model)
diagram = UUIDLabeledGraph(parsed_diagram)

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
        # abstraction
        src = let src = incident(g, v.dom, :vUuid)
            if isempty(src)
                nc += 1
                add_part!(g, :V, vlabel="•$nc", vUuid=v.dom)
            else
                src
            end
        end
        tgt = let tgt = incident(g, v.cod, :vUuid)
            if isempty(tgt)
                nc += 1
                add_part!(g, :V, vlabel="•$nc", vUuid=v.cod)
            else
                tgt
            end
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
    foreach(pairs(newobs)) do (k,v)
        add_part!(g, :V, vlabel=v.name, vUuid=k)
    end
    foreach(pairs(newhoms)) do (k,v)
        # abstraction
        src = let src = incident(g, v.dom, :vUuid)
            if isempty(src)
                nc += 1
                add_part!(g, :V, vlabel="•$nc", vUuid=v.dom)
            else
                src
            end
        end
        tgt = let tgt = incident(g, v.cod, :vUuid)
            if isempty(tgt)
                nc += 1
                add_part!(g, :V, vlabel="•$nc", vUuid=v.cod)
            else
                tgt
            end
        end
        add_part!(g, :E, src=only(src), tgt=only(tgt), eUuid=k, elabel=v.name)
    end
    g
end
