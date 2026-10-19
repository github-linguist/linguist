#!/bin/bash
#SBATCH --job-name=sim_sweep
#SBATCH --array=1-100%20
#SBATCH --cpus-per-task=1
#SBATCH --mem=4G
#SBATCH --time=01:00:00
#SBATCH --output=results/sim_%A_%a.log

# Map array index to specific config file
CONFIG_FILE=$(find ./configs -name "*.json" | sed -n "${SLURM_ARRAY_TASK_ID}p")

if [ -z "$CONFIG_FILE" ]; then
    echo "Error: No config file found for task ID $SLURM_ARRAY_TASK_ID"
    exit 1
fi

# Execute simulation
./bin/simulator --config "$CONFIG_FILE"