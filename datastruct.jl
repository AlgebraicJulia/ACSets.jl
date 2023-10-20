# need a thing that stores indexed spans

abstract type TypeLevelIndexedSpans{spans} end

struct IndexedSpans
    spans::Vector{Tuple{Vararg{Symbol}}}
end

my_indexed_spans = IndexedSpans([(:f1,:f2),(:g1,:g2,:g3)])

function typelevel_indexedspans(s::IndexedSpans)
    TypeLevelIndexedSpans{
        Tuple{s.spans...}
    }
end

typelevel_indexedspans(my_indexed_spans)