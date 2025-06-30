#!/bin/bash
#SBATCH -p fasse
#SBATCH -c 24
#SBATCH --mem 120G
#SBATCH -t 0-15:00
#SBATCH -o myRjob_%j.out
#SBATCH -e myRjob_%j.err
#SBATCH --mail-type=ALL

module load R/4.3.1-fasrc01
export R_LIBS_USER=$HOME/apps/R_4.3.1:$R_LIBS_USER
R CMD BATCH --quiet --no-restore --no-save ReAd_single.R
rm *.err *.out