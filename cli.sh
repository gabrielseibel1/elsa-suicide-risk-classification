#!/usr/bin/env bash

set -eu

print_usage() {
  printf "Usage: ..."
  printf "  -c CPU : number of cpus"
  printf "  -d DURATION : duration in hh:mm:ss for slurm"
  printf "  -s : use slurm (sbatch)"
  printf "  -t : quick verification"
}

serious=true
slurm=false
cpus=1

while getopts 'c:d:st' flag; do
  case "${flag}" in
    c) cpus="${OPTARG}" ;;
    d) duration="${OPTARG}" ;;
    s) slurm='true' ;;
    t) serious='false' ;;
    *) print_usage
       exit 1 ;;
  esac
done

if $slurm
then
    trap 'cleanup' 1 2 3 6
    cleanup()
    {
      mv job.slurm.bak job.slurm
      exit 1
    }

    # replace the cpu count in ntasks
    sed -i'.bak' "s/--ntasks=/--ntasks=$cpus/;s/--time=/--time=$duration/" job.slurm

    # schedule job
    sbatch --export=ESRP_SERIOUS_EXEC="$serious",ESRP_SLURM_EXEC=true,ESRP_MAX_CPUS="$cpus" job.slurm

    # restore backed-up job.slurm
    cleanup
else
    (
      export ESRP_SERIOUS_EXEC=$serious
      export ESRP_SLURM_EXEC=false
      export ESRP_MAX_CPUS=$cpus
      source experiments.sh
    )
fi
