# ACSets.jl

[![Stable Documentation](https://img.shields.io/badge/docs-stable-blue.svg)](https://AlgebraicJulia.github.io/ACSets.jl/stable)
[![Development Documentation](https://img.shields.io/badge/docs-dev-blue.svg)](https://AlgebraicJulia.github.io/ACSets.jl/dev)
[![Code Coverage](https://codecov.io/gh/AlgebraicJulia/ACSets.jl/branch/main/graph/badge.svg)](https://codecov.io/gh/AlgebraicJulia/ACSetse.jl)
[![CI/CD](https://github.com/AlgebraicJulia/ACSets.jl/actions/workflows/julia_ci.yml/badge.svg)](https://github.com/AlgebraicJulia/ACSets.jl/actions/workflows/julia_ci.yml)

ACSets ("attributed C-sets") are a family of data structures generalizing both
graphs and data frames. They are an in-memory implementation of a
category-theoretic formalism for relational databases.

This lightweight package provides

- data structures for acset schemas, acsets, and tabular columns
- serialization of acset schemas and acsets

[Catlab.jl](https://github.com/AlgebraicJulia/Catlab.jl) extends this package to
offer many more features, beginning with homomorphisms between acsets and
including limits and colimits of acsets, functorial data migration, and
automated homomorphism finding.
[AlgebraicRewriting.jl](https://github.com/AlgebraicJulia/AlgebraicRewriting.jl)
goes further still to provide declarative rewriting for acsets.

## Learning

Graphs, and their generalization as C-sets, are introduced with minimal
prerequisites in a series of blog posts on the [AlgebraicJulia
blog](https://blog.algebraicjulia.org/):

1. [Graphs and C-sets I](https://blog.algebraicjulia.org/post/2020/09/cset-graphs-1/):
   What is a graph?
2. [Graphs and C-sets II](https://blog.algebraicjulia.org/post/2020/09/cset-graphs-2/):
   Half-edges and rotation systems
3. [Graphs and C-sets III](https://blog.algebraicjulia.org/post/2021/04/cset-graphs-3/):
   Reflexive graphs and C-set homomorphisms
4. [Graphs and C-sets IV](https://blog.algebraicjulia.org/post/2021/09/cset-graphs-4/):
   The propositional logic of subgraphs and sub-C-sets

These blog posts use Catlab.jl in addition to ACSets.jl.

## Citation

The ideas behind this package are described in the paper:

> Patterson, Lynch, Fairbanks. Categorical data structures for technical
> computing. *Compositionality* 4, 5 (2022).
> [arXiv:2106.04703](https://arxiv.org/abs/2106.04703).
> [DOI:10.32408/compositionality-4-5](https://doi.org/10.32408/compositionality-4-5).
