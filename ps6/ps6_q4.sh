#!/bin/bash
# Job name:
#SBATCH --job-name=ps6_q4
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
R CMD BATCH --no-save ps6_q4.R ps6_q4.out
