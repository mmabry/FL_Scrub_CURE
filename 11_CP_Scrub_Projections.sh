#!/bin/bash

#SBATCH --job-name=currentCP    # Job name
#SBATCH --output=%j.out         # Standard output and error log
#SBATCH --nodes=1 			        # number of nodes to use
#SBATCH --ntasks=1              # number of tasks
#SBATCH --cpus-per-task=1       # number of cpu cores
#SBATCH --mem=60gb              # Job memory request
#SBATCH --time=01-00:00:00      # Time limit days-hrs:min:sec

date;hostname;pwd

#load modules

module load R/4.2

#do some coding

Rscript --vanilla /blue/soltis/share/FL_Scrub/share/07_scripts/11_CP_Scrub_Projections.R


