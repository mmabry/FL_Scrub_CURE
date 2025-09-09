### Resolving taxonomic names
## 09/27/21
##note, make sure you have made the proper file and folder structure as needed below.

### Load Packages
library(dplyr)
library(tidyr)

##set working directory
setwd("/blue/soltis/share/FL_Scrub/share")

## Run from directory where crop directories are located.
#args = commandArgs(trailingOnly=TRUE)
#dir <- args[1] #example : $(pwd)
#crop <- args[2] #example: Brassica_oleracea

#dir <- "/home/kenziemabry/CWR_Proj/01_data"
#crop <- "Phaseolus_vulgaris"

############################################################################
########################### 1. Read in CSV files ##########################
############################################################################
FL_Scrub_Dir <- list.files(path="FL_Scrub_Raw_Data", pattern = "*.csv", full.names = TRUE)
print(FL_Scrub_Dir)

# Create logfile folder for files to print to
#dir.create(paste0(dir, "/", crop, "/logfiles"))

############################################################################
########################### 2. Make species lists ##########################
############################################################################

# make csv file of all unique names. Use this files to match to known synonyms and remove those which should not be included
for(i in FL_Scrub_Dir){
  print(i)
  species_df <- read.csv(i)
  speciesUnique <- data.frame(unique(species_df$scientificName))
  names(speciesUnique) <- NULL
  names(speciesUnique)[1] <- "scientificName"
  species_name <- strsplit(i, '[/]')[[1]][2]
  species_name2 <- strsplit(species_name, '[.]')[[1]][1]
  write.csv(speciesUnique, file = paste0( "FL_Scrub_Clean/",species_name2, "_uniquenames.csv"),row.names = FALSE)
}
