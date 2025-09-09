#!/bin/bash

#SBATCH --job-name=ENM        # Job name
#SBATCH --output=%j.out       # Standard output and error log
#SBATCH --nodes=1 			      # number of nodes to use
#SBATCH --ntasks=1            # number of tasks
#SBATCH --cpus-per-task=1     # number of cpu cores
#SBATCH --mem=80gb            # Job memory request
#SBATCH --time=03-00:00:00    # Time limit days-hrs:min:sec

date;hostname;pwd

#load modules

module load R/4.2

#do some coding

Rscript --vanilla /blue/soltis/share/FL_Scrub/share/07_scripts/06_FL_Scrub_Ecological_Niche_Modeling.R

##example for running: sbatch /home/charissesproha/FL_Scrub/share/scripts/06_FL_Scrub_Ecological_Niche_Modeling.sh

