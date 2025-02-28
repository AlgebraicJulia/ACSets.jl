#!/bin/bash
#SBATCH --job-name=diagrammatic_equations_CI    # Job name
#SBATCH --mail-type=END,FAIL          # Mail events (NONE, BEGIN, END, FAIL, ALL)
#SBATCH --mail-user=cuffaro.m@ufl.edu # Where to send mail	
#SBATCH --ntasks=1                    # Run on a single CPU
#SBATCH --mem=8gb                     # Job memory request
#SBATCH --time=00:15:00               # Time limit hrs:min:sec
pwd; hostname; date

module load julia

echo "Running Tests..."
julia --project -e 'using Pkg; Pkg.status(); Pkg.test()'

echo "Building Documentation..."
julia --project=docs -e 'using Pkg; Pkg.develop(PackageSpec(path=pwd())); Pkg.status(); Pkg.instantiate(); include("docs/make.jl")'
