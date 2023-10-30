module InterSchemas
export InterSchema

using ..Schemas
using OrderedCollections
using MLStyle

struct Meta
  title::Union{String, Nothing}
  description::Union{String, Nothing}
end

function Meta()
  Meta(nothing, nothing)
end

struct Ob
  meta::Meta
end

function Ob()
  Ob(Meta())
end

struct Hom
  dom::Symbol
  codom::Symbol
  meta::Meta
end

function Hom(dom::Symbol, codom::Symbol)
  Hom(dom, codom, Meta())
end

struct Attr{T}
  dom::Symbol
  codom::T
  meta::Meta
end

function Attr(dom::Symbol, codom::T) where {T}
  Attr{T}(dom, codom, Meta())
end

function Attr{T}(dom::Symbol, codom::T) where {T}
  Attr{T}(dom, codom, Meta())
end

struct InterSchema{T} <: Schema{Symbol}
  objects::OrderedDict{Symbol, Ob}
  homs::OrderedDict{Symbol, Hom}
  attrs::OrderedDict{Symbol, Attr{T}}
end

end
