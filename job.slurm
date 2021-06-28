#!/usr/bin/env bash
#SBATCH --job-name=ESRP
#SBATCH --partition=saude
#SBATCH --nodes=1
#SBATCH --time=
#SBATCH --ntasks=
#SBATCH --output=%x_%j.out
#SBATCH --error=%x_%j.err

set -eu

source experiments.sh
