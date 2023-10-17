# notes on code design

## types

There are a lot of parameterized types in ACSets.jl that make everything work. It's confusing to figure out what they all do. Let's map them out. We use a directed arrow to denote that the type at the source of the arrow is a subtype of the type at the target of the arrow.


```mermaid
classDiagram

namespace ACSetInterface {
    class PartsType ["abstract PartsType{S,T}"]
    class DenseParts ["abstract DenseParts{S,T}"]
    class MarkAsDeleted ["abstract MarkAsDeleted{S,T}"]
    class UnionFind ["abstract UnionFind{S,T}"]
    class MarkAsDeletedUnionFind ["MarkAsDeletedUnionFind UnionFind{S,T}"]

    class ACSet ["abstract ACSet{PT}"] {
        PT <: PartsType
    }
}
PartsType <|-- DenseParts : PartsType{S,T}
PartsType <|-- MarkAsDeleted : PartsType{S,T}
PartsType <|-- UnionFind : PartsType{S,T}
PartsType <|-- MarkAsDeletedUnionFind : PartsType{S,T}

namespace DenseACSets {
    class SimpleACSet ["abstract SimpleACSet{PT}"]
    class StructACSet ["abstract StructACSet{S,Ts,PT}"] {
        S <: TypeLevelSchema&lcub;Symbol&rcub; : schema
        Ts <: Tuple : types of attributes
    }
    class StructCSet ["StructCSet{S,PT}"]
}
ACSet <|-- SimpleACSet : ACSet{PT}
SimpleACSet <|-- StructACSet : SimpleACSet{PT}
StructACSet <|-- StructCSet : StructACSet{S,Tuple{},PT}

namespace Columns {
    class Column ["abstract Column{S,T}"]
}

namespace PreimageCaches {
    class PreimageCache ["abstract PreimageCache{S,T}"] {

    }
    class TrivialCache ["TrivialCache{S,T}"]

    class StoredPreimageCache ["StoredPreimageCache{S,T,Preimage,Storage}"] {
        Preimage <: AbstractSet&lcub;S&rcub;
        Storage <: Mapping&lcub;T,Preimage&rcub;: preimage for non injective maps
    }

    class InjectiveCache ["InjectiveCache{S,T,Storage}"] {
        Storage <: Mapping&lcub;T,S&rcub;: inverse
    }
}
PreimageCache <|-- TrivialCache : PreimageCache{S,T}
PreimageCache <|-- StoredPreimageCache : PreimageCache{S,T}
PreimageCache <|-- InjectiveCache : PreimageCache{S,T}

namespace Mappings {
    class AbstractDict ["abstract AbstractDict{S,T}"]
    class Mapping ["abstract Mapping{S,T}"] {   
        S: type of parts in source
        T: type of parts in target
    }
    class VecMap ["VecMap{T,V}"] {
        V <: AbstractVector&lcub;T&rcub;: container type
    }
    class DictMap ["DictMap{K,V,D}"] {
        D <: AbstractDict&lcub;K,V&rcub;: container type
    }
    %% views are not included here
}
AbstractDict <|-- Mapping
Mapping <|-- VecMap : Mapping{Int,T}
Mapping <|-- DictMap : Mapping{K,V}
```