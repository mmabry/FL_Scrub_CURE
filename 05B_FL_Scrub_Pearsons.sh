#!/bin/bash

#SBATCH --job-name=Pearsons               # Job name
#SBATCH --output=Pearsons_%A_%a.out       # Standard output and error log, with array job ID and task ID
#SBATCH --array=1-36                      # Set the array range to the number of species
#SBATCH --nodes=1                         # Number of nodes
#SBATCH --ntasks=1                        # Number of tasks
#SBATCH --cpus-per-task=1                 # Number of CPU cores
#SBATCH --mem=600gb                       # Job memory request
#SBATCH --time=02-00:00:00                # Time limit days-hrs:min:sec
#SBATCH --qos=soltis-b                    # Specific queue called burst

date;hostname;pwd

# Load modules
module load R/4.2

# Get the species name from the species list file
species=$(sed -n "${SLURM_ARRAY_TASK_ID}p" /home/kenziemabry/FL_Scrub/share/species_list.txt)

echo "Processing species: $species"

# Run the R script with the species name as argument
Rscript --vanilla /blue/soltis/share/FL_Scrub/share/07_scripts/05B_FL_Scrub_Pearsons.R $species

