module ADTs
export Value, Kwarg, Args, Statement, ACSetSpec,
  construct, acsetspec, generate_expr,
  to_dict, label2index

using MLStyle
using ..ACSetInterface

import Base: show
# using Catlab
# using Catlab.CategoricalAlgebra

abstract type AbstractACSetSpec end
@data Args{T} <: AbstractACSetSpec begin
  Value(T)
  Kwarg(Symbol, Value{T})
end

@as_record struct Statement <: AbstractACSetSpec
  table::Symbol
  element::Vector{Args}
end

@as_record struct ACSetSpec <: AbstractACSetSpec
  acstype::Union{Symbol,Expr}
  body::Vector{Statement}
end

"""    show(io::IO, s::AbstractACSetSpec)

generates a human readable string of the `ACSetSpec` (or any sub-term).
"""
function show(io::IO, s::AbstractACSetSpec)
  let ! = show
    @match s begin
      Value(v) => print(io, v)
      Kwarg(k, v) => begin print(io, "$k="); !(io,v) end
      Statement(t, e) => print(io, "  $t($(join(sprint.(show, e), ",")))")
      ACSetSpec(s, body) => print(io, "$s begin \n$(join([sprint(show, b) for b in body], "\n"))\n end")
    end
  end
end

"""    to_dict(s::ACSetSpec)

generates a dictionary representation the `ACSetSpec` (or any sub-term).
This dict should be serializable with `JSON.json`.
"""
function to_dict(s)
  let ! = to_dict
    @match s begin
      Value(v) => v
      Kwarg(k, v) => Pair(k, v._1)
      Statement(t, e) => Dict(:table => t, :fields => Dict(map(!, e)...))
      ACSetSpec(s, body) => Dict(:type => string(s), :data => map(!, body))
      x => error("got input $x")
    end
  end
end

escape_if_symbol(v) = v
escape_if_symbol(v::Symbol) = QuoteNode(v)


"""    generate_expr(s::ACSetSpec)

creates a julia Expr that will generate_expr the specified ACSet. 
"""
function generate_expr(e::Args)
  @match e begin
    Value(v) => escape_if_symbol(v)
    Kwarg(k, v) => (k => generate_expr(v))
  end
end

function generate_expr(b::Statement)
  args = generate_expr.(b.element)
  if eltype(args) <: Pair
    ex = :(add_part!(X, $(QuoteNode(b.table))))
    append!(ex.args, map(args) do (k, v)
      Expr(:kw, k, v)
    end)
    return ex
  else
    return :(add_part!(X, $(QuoteNode(b.table)), $(args...)))
  end
end

function generate_expr(sp::ACSetSpec)
  body = generate_expr.(sp.body)
  bexp = :(
    begin end
  )
  push!(bexp.args, body)
  quote
    X = $(sp.acstype)()
    $(body...)
  end
end



function construct!(X, b::Statement)
  args = map(b.element) do e
    @match e begin
      Value(v) => v
      Kwarg(k, v) => (k => v._1)
    end
  end
  ACSetInterface.add_part!(X, b.table; args...)
end

"""    construct(T::Type, sp::ACSetSpec)

invoke the constructor and build the acset by adding parts.
"""
function construct(T::Type, sp::ACSetSpec)
  X = T()
  # X = eval(sp.acstype)()
  map(sp.body) do b
    construct!(X, b)
  end
  return X
end

find_kwargs(exp) = begin
  @match exp begin
    Expr(:call, args...) => map(find_kwargs, args)
    Expr(:kw, args...) => Kwarg(args[1], Value(args[2]))
    x::Symbol => x
    a => error("could not find kwargs or values in $a")
  end
end

expand_statement(args) = begin
  let ! = expand_statement
    @match args begin
      as::AbstractVector => map(!, as)
      Expr(:kw, args...) => Kwarg(args[1], !(args[2]))
      x => Value(x)
      x => error("hit fallthrough on $(typeof(x)), $x")
    end
  end
end

"""    acsetspec(head::Symbol, body::Expr)

processes a Julia Expr specifying the ACSet construction into a the ADT representation. Approximate inverse to `show`
"""
function acsetspec(head, body)
  processed_body = @match body begin
    Expr(:block, lines...) => begin
      map(lines) do line
        @match line begin
          ::LineNumberNode => nothing
          Expr(:call, args...) => begin
            pargs = expand_statement(args)
            Statement(pargs[1]._1, pargs[2:end])
          end
          _ => error("all lines must be calls $line")
        end
      end
    end
    _ => error("expected block")
  end
  spec = ACSetSpec(head, filter(!isnothing, processed_body))
end

function label2index(lt, s)
  @match s begin
    vs::Vector => [label2index(lt, v) for v in vs]
    Statement(t, elts) => Statement(t, map(e -> label2index(lt, e), elts))
    Kwarg(k, v::Value{Symbol}) => Kwarg(k, Value(k != :label ? lt[v._1] : v._1))
    Kwarg(k, v) => Kwarg(k, v)
    x => x
  end
end

"""    label2index(s::ACSetSpec)

replace symbolic identifiers in an ACSet spec with the indices that have that label.
This function assumes that all labels are globally unique across tables. 
So prefix them with the table name if you want scopes.
It also assumes that you don't have any other attributes of type symbol, so use strings instead.
"""
function label2index(s::ACSetSpec)
  lookuptable = Dict{Symbol,Int}()
  map(enumerate(s.body)) do (i, b)
    @match b begin
      Statement(t, elts) => map(elts) do e
        @match e begin
          Kwarg(k, v) => if k == :label
            v._1 in keys(lookuptable) && error("Labels are not globally unique")
            lookuptable[v._1] = i
          end
          _ => nothing
        end
      end
      _ => nothing
    end
  end
  newbody = label2index(lookuptable, s.body)
  ACSetSpec(s.acstype, newbody)
end

symb2string(d) = begin
  let ! = symb2string
    @match d begin
      s::String => s
      s::Symbol => string(s)
      d::Dict => Dict(!k => !(d[k]) for k in keys(d))
      d::Vector => map(!, d)
      v => v
    end
  end
end

end