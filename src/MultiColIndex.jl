module MultiColIndex

export TypeLevelIndexedSpans, IndexedSpans, typelevel_indexedspans,
    IndexedSpanACSet, get_typelevel_indexed_spans, get_indexed_spans,
    make_spanidx

using ..ACSetInterface, ..DenseACSets, ..Schemas

# we want to store indexed spans at type level, so eventually we will be able
# to compute some of the searching at compile time
abstract type TypeLevelIndexedSpans{Spans} end

struct IndexedSpans
    spans::Vector{Tuple{Vararg{Symbol}}}
end

function typelevel_indexedspans(s::IndexedSpans)
    TypeLevelIndexedSpans{
        Tuple{s.spans...}
    }
end

# a type that wraps an ACSet: the spans that are being indexed are stored in the type,
# and idx will store the machinery that gives faster lookup for each one
# right now we only allow StructACSets to be wrapped since I don't know how the others work
struct IndexedSpanACSet{S<:TypeLevelIndexedSpans, I}
    acs::StructACSet
    idx::I
end

function IndexedSpanACSet(acs::StructACSet{S}, to_index::I) where {S,I<:Vector{<:Tuple{Vararg{Symbol}}}}
    idxs = [make_spanidx(acs, span) for span in to_index]
    IndexedSpanACSet{typelevel_indexedspans(IndexedSpans(to_index)), typeof(idxs)}(
        acs,
        idxs
    )
end

# get the type level indexed spans from a wrapped ACSet
get_typelevel_indexed_spans(::IndexedSpanACSet{S}) where {S} = S

# get the concrete indexed spans from the type level storage
function get_indexed_spans(::Type{TypeLevelIndexedSpans{Spans}}) where {Spans}
    get_indexed_spans(
        IndexedSpans(
            [Spans.parameters...]
        )
    )
end

get_indexed_spans(s::IndexedSpans) = s.spans


# given an ACSet and a span we want to index, make a Dict based inverse mapping
# (going from parts in the legs to part(s) in the apex of the span/relation)
function make_spanidx(acs::StructACSet{S}, span::Tuple{Vararg{Symbol}}) where {S}
    s = Schema(S)
    length(span) > 1 || error("the span $(span) only has one hom")
    allequal([dom(s, f) for f in span]) || error("some homs in $(span) have inconsistent domain")

    cache_keys = reduce(vcat, Iterators.product([codom_parts(acs, f) for f in span]...))
    span_legs = collect(zip([subpart(acs, f) for f in span]...))

    index = Dict{eltype(cache_keys),Set{Int}}()
    for key in cache_keys
        index[key] = Set(findall(isequal(key), span_legs))
    end
    return index
end

# incident: if homs in `f` are indexed, use fast lookup, otherwise
# use the slower search
function ACSetInterface.incident(acs::IndexedSpanACSet{S,I}, parts::T, f::F) where {S,I,T<:Tuple,F<:Tuple{Vararg{Symbol}}}
    indexed_spans = get_indexed_spans(S)
    f_pos = findfirst(isequal(f), indexed_spans)
    isnothing(f_pos) ? _incident_noidx(acs, parts, f) : _incident_idx(acs, parts, f_pos)
end

# incident when `f` is not indexed
function _incident_noidx(acs::IndexedSpanACSet, parts, f::I) where {I<:Tuple{Vararg{Symbol}}}
    return intersect([incident(acs.acs, parts[i], f[i]) for i in eachindex(f)]...)
end

# incident when `f` is an indexed span
# ix: the numeric index of the span (acs.idx[ix])
function _incident_idx(acs::IndexedSpanACSet, parts, ix)
    return collect(acs.idx[ix][parts])
end


# add_part!: if elements are added to the legs of any indexed span, we must add keys to
# the inverse mapping
function ACSetInterface.add_part!(acs::IndexedSpanACSet{S,I}, type) where {S,I}
    sch = acset_schema(acs.acs)
    spans = get_indexed_spans(S)

    # get objects in legs of each indexed span
    spans_legs = [[codom(sch, f) for f in s] for s in spans]

    # if `type` in legs of an indexed span, update keys of index
    # (note the same object might be involved in >1 indexed span)
    for s in eachindex(spans_legs)
        if type ∉ spans_legs[s]
            continue
        end
        # get index of `type` and not `type` in legs of indexed span `s`
        type_ix = findfirst(isequal(type), spans_legs[s])
        nottype_ix = setdiff(eachindex(spans_legs[s]), type_ix)

        # NOTE: below is maybe not generalizable? we assume the newly added part
        # will have an ID that is just one more than the current maximum part
        # ACSetInterface.add_parts!(m::IntParts/BitSetParts, n::Int) could be seperated
        # into a fn that returns in the new part ID and one that actually updates m

        # add keys for new part of ob `type` with product of other obs in the legs
        new_part = nparts(acs.acs, type) + 1
        nottype_keys = Iterators.product([parts(acs.acs, spans_legs[s][f′]) for f′ in nottype_ix]...)
        for i in nottype_keys
            new_key = zeros(Int, length(spans_legs[s]))
            new_key[type_ix] = new_part
            new_key[nottype_ix] .= i
            acs.idx[s][new_key...] = Set{Int}()
        end
    end

    # call existing add_part!
    add_part!(acs.acs, type)
end


# rem_part!: if elements are deleted from the legs of any indexed span, we must remove keys from
# the inverse mapping
function ACSetInterface.rem_part!(acs::IndexedSpanACSet{S,I}, type, part) where {S,I}
    # call existing rem_part!
    rem_part!(acs.acs, type, part)

    sch = acset_schema(acs.acs)
    spans = get_indexed_spans(S)

    # get objects in legs of each indexed span
    spans_legs = [[codom(sch, f) for f in s] for s in spans]

    # if `type` in legs of an indexed span, update keys of index
    for s in eachindex(spans_legs)
        if type ∉ spans_legs[s]
            continue
        end
        # position of `type` in legs of indxd span `s`
        type_ix = findfirst(isequal(type), spans_legs[s])
        # delete all keys with the deleted element of `type`
        for key in keys(acs.idx[s])
            if key[type_ix] == part
                delete!(acs.idx[s], key)
            end
        end
    end
end


# set_subpart!: for all indexed spans, if all homs in the span `f` is involved in
# have valid mappings at `part`, then add `part` to the key associated with those mappings.
function ACSetInterface.set_subpart!(acs::IndexedSpanACSet{S,I}, part, f, subpart) where {S,I}
    # basic set subpart
    set_subpart!(acs.acs, part, f, subpart)

    spans = get_indexed_spans(S)

    # needed in case `f` is involved in multiple indexed spans
    for s in eachindex(spans)
        # only need to update idx if `f` was in idxd span `s`
        if f ∉ spans[s]
            continue
        end

        # NOTE: is using 0 for uninitialized homs an implementation detail i shouldn't rely on?
        span_subparts = [acs.acs[part, f′] for f′ in spans[s]]
        if all(span_subparts .!= 0)
            push!(acs.idx[s][Tuple(span_subparts)], part) 
        end
    end
end


# clear subpart!: before clearing the `part` element of the hom `f`, remove
# the key from the index corresponding to the "row" of that indexed span
function ACSetInterface.clear_subpart!(acs::IndexedSpanACSet{S,I}, part, f) where {S,I}

    spans = get_indexed_spans(S)

    # needed in case `f` is involved in multiple indexed spans
    for s in eachindex(spans)
        if f ∉ spans[s]
            continue
        end
        span_subparts = [acs.acs[part,f′] for f′ in spans[s]]
        if all(span_subparts .!= 0)
            delete!(acs.idx[s][Tuple(span_subparts)], part)
        end        
    end

    # basic clear subpart
    clear_subpart!(acs.acs, part, f)
end

end