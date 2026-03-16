#!/bin/bash
#SBATCH --job-name=osu_bench
#SBATCH --partition=batch
#SBATCH --nodes=2
#SBATCH --ntasks-per-node=40
#SBATCH --time=00:30:00
#SBATCH --output=logs/%x_%j.out
#SBATCH --error=logs/%x_%j.err

# Clean environment and load specific MPI version
module purge
module load compiler/gnu/11.2 openmpi/4.1.5

# Verify host allocation
echo "Allocated nodes: $SLURM_JOB_NODELIST"

# Run MPI benchmark without specifying -np (Slurm handles it)
mpirun ./osu_bibw