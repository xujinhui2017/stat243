#!/bin/bash
# Job name:
#SBATCH --job-name=q4
#
# Account:
#SBATCH --account=ic_stat243
#
# Partition:
#SBATCH --partition=savio
#
# Wall clock limit (30 seconds here):
#SBATCH --time=02:00:00
#
## Command(s) to run:
module load r/3.2.5 readr doParallel foreach stringr
R CMD BATCH --no-save q4.R q4.out
