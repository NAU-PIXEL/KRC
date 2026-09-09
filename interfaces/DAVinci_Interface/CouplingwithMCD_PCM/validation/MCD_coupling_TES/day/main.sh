#!/bin/bash
#SBATCH --job-name=KRCvsTES
#SBATCH --time=00:30:00
#SBATCH --mem=2G
#SBATCH --cpus-per-task=1
#SBATCH --array=1-47700
#SBATCH --output=logs/%x_%A_%a.out
#SBATCH --error=logs/%x_%A_%a.err

module load davinci
INDEX=$SLURM_ARRAY_TASK_ID

davinci -f /scratch/ll2456/KRC_mcd_final_validationTESday/maincheck.dv "$INDEX"

