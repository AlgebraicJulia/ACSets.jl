module TestExcelACSets
using Test

import XLSX
using ACSets

# Test data
###########

mutagenesis_path = joinpath(@__DIR__, "data", "mutagenesis.xlsx")

SchMutagenesis = BasicSchema(
  [:Molecule, :Atom, :Bond],
  [(:molecule, :Atom, :Molecule),
   (:atom1, :Bond, :Atom),
   (:atom2, :Bond, :Atom)],

  [:Indicator, :Categorical, :Name, :Numerical],
  [(:molecule_id, :Molecule, :Name),
   #(:ind1, :Molecule, :Indicator),
   #(:inda, :Molecule, :Indicator),
   (:logp, :Molecule, :Numerical),
   (:lumo, :Molecule, :Numerical),
   #(:mutagenic, :Molecule, :Indicator),

   (:atom_id, :Atom, :Name),
   (:atom_type, :Atom, :Categorical),
   (:element, :Atom, :Name),
   (:charge, :Atom, :Numerical),

   (:bond_type, :Bond, :Categorical)]
)

@acset_type MutagenesisData(SchMutagenesis,
  unique_index=[:atom_id, :molecule_id])

# Read XLSX
###########

T = MutagenesisData{Bool,Int,String,Float64}
tables = (
  Molecule = (primary_key = :molecule_id,),
  Atom = (primary_key = :atom_id,
          columns = (atom_type=:type, molecule=:molecule_id)),
  Bond = (columns = (bond_type=:type, atom1=:atom1_id, atom2=:atom2_id),)
)
result = read_xlsx_acset(mutagenesis_path, T, tables=tables)
@test nparts(result, :Molecule) == 188
@test nparts(result, :Atom) == 4893
@test nparts(result, :Bond) == 5243

@test result[1,:molecule] == 3
@test (result[1,:atom1], result[1,:atom2]) == (1, 12)

show(IOContext(stdout, :limit => true), "text/plain", result)

end
