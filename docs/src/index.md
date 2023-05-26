# ACSets.jl

```@meta
CurrentModule = ACSets
```

Acsets ("attributed C-sets") are a family of data structures generalizing both
graphs and data frames. They are an efficient in-memory implementation of a
category-theoretic formalism for relational databases.

ACSets.jl is a lightweight package that provides

- data structures for acset schemas, acsets, and tabular columns
- serialization of acset schemas and acsets

[Catlab.jl](https://github.com/AlgebraicJulia/Catlab.jl) extends this package to
offer many more features, beginning with homomorphisms between acsets and
including limits and colimits of acsets, functorial data migration, and
automated homomorphism finding.
[AlgebraicRewriting.jl](https://github.com/AlgebraicJulia/AlgebraicRewriting.jl)
goes further still to provide declarative rewriting for acsets.

## Citation

The ideas behind this package are described in the paper:

> Patterson, Lynch, Fairbanks. Categorical data structures for technical
> computing. *Compositionality* 4, 5 (2022).
> [arXiv:2106.04703](https://arxiv.org/abs/2106.04703).
> [DOI:10.32408/compositionality-4-5](https://doi.org/10.32408/compositionality-4-5).
