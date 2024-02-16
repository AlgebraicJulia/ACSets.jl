module TestExcelACSets
using Test

import XLSX
using ACSets

# Test data
###########

# Mutagenesis
#------------

# Original source: https://relational.fit.cvut.cz/dataset/Mutagenesis
mutagenesis_path = joinpath(@__DIR__, "data", "mutagenesis.xlsx")

SchMutagenesis = BasicSchema(
  [:Molecule, :Atom, :Bond],
  [(:molecule, :Atom, :Molecule),
   (:atom1, :Bond, :Atom),
   (:atom2, :Bond, :Atom)],

  [:Indicator, :Categorical, :Name, :Numerical],
  [(:molecule_id, :Molecule, :Name),
   (:ind1, :Molecule, :Indicator),
   (:inda, :Molecule, :Indicator),
   (:logp, :Molecule, :Numerical),
   (:lumo, :Molecule, :Numerical),
   (:mutagenic, :Molecule, :Indicator),

   (:atom_id, :Atom, :Name),
   (:atom_type, :Atom, :Categorical),
   (:element, :Atom, :Name),
   (:charge, :Atom, :Numerical),

   (:bond_type, :Bond, :Categorical)]
)

@acset_type MutagenesisData(SchMutagenesis,
  unique_index=[:atom_id, :molecule_id])

function yesno_to_bool(s::AbstractString)
  s == "yes" && return true
  s == "no" ? false : error("Value must be \"yes\" or \"no\"")
end

# Labeled graph
#--------------

# Vertex and edge data are stored in single sheet.
labeled_graph_path = joinpath(@__DIR__, "data", "graph_one_sheet.xlsx")

SchLabeledGraph = BasicSchema(
  [:E,:V], [(:src,:E,:V), (:tgt,:E,:V)],
  [:Label], [(:label,:V,:Label)]
)

@acset_type LabeledGraph(SchLabeledGraph, unique_index=[:label])

# Read XLSX
###########

# Mutagenesis
#------------

tables = (
  Molecule = (primary_key = :molecule_id,
              convert = (mutagenic = yesno_to_bool,)),
  Atom = (primary_key = :atom_id,
          column_labels = (atom_type=:type, molecule=:molecule_id)),
  Bond = (column_labels = (bond_type=:type, atom1=:atom1_id, atom2=:atom2_id),)
)

T = MutagenesisData{Bool,Int,String,Float64}
result = read_xlsx_acset(T, mutagenesis_path, tables=tables)
@test nparts(result, :Molecule) == 188
@test nparts(result, :Atom) == 4893
@test nparts(result, :Bond) == 5243

@test result[1:5, :mutagenic] == [true, true, false, true, true]
@test result[1,:molecule] == 3
@test (result[1,:atom1], result[1,:atom2]) == (1, 12)

# Labeled graph
#--------------

g = @acset LabeledGraph{String} begin
  V = 5
  E = 3
  label = ["a","b","c","d","e"]
  src = [1,2,2]
  tgt = [2,3,4]
end

result = read_xlsx_acset(LabeledGraph{String}, labeled_graph_path, tables=(
  V = (primary_key = :label,
       sheet = 1,
       row_range = 4,
       column_range = "A:A",
       column_labels = (label = :Name,)),
  E = (sheet = 1,
       row_range = 4,
       column_range = "C:D",
       column_labels = (src = :Source, tgt = :Target))
))
@test result == g

# General functionality
# ---------------------

@test_throws Exception read_xlsx_acset(LabeledGraph{String}, "badpath.json")

end
