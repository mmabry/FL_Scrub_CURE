## Load libraries
library(raster)
library(gtools)
library(dplyr)
#library(rgdal) #not working needs to update
library(sp)
library(rangeBuilder) 
library(sf)
library(caret)
library(usdm)  
library(dismo)
library(stringr)
library(rJava)
library(gatoRs)
library(ggpubr)
library(rgdal)

### setwd 
#setwd("blue/soltis/share/FL_Scrub") #try this if error in above 

## Make new directory for keeping rasters/environmental data
dir <- "06_rasters/SpeciesLayers/"

############################################################################
####################### 5. Variable selection ##############################
############################################################################
# Select layers for MaxEnt
## We only want to include layers that are not highly correlated.
## To assess which layers we will include, we will use Variable inflation factors (VIFs) 
## VIF can detect for multicollinearity in a set of multiple regression variables. 
### tutorial https://rstudio-pubs-static.s3.amazonaws.com/300995_e6e0edf09915431480ed089c3a3e0ff3.html

alldf <- read.csv("02_FL_Scrub_Clean/FL_Scrub_maxentready_v4.csv")

### tutorial which was followed https://rstudio-pubs-static.s3.amazonaws.com/300995_e6e0edf09915431480ed089c3a3e0ff3.html

for(i in  1:length(unique(alldf$accepted_name))){ #can run this with all species as normal for loop
  species <- unique(alldf$accepted_name)[i]
  print(species)
  
  ## species name with underscore
  spec <- gsub(" ", "_", species)
  
  ##creat dir
  dir.create(paste0(dir, spec, "/VIF"))
  
  # ### Stack layers for each species
  
  clippedlist <- list.files(paste0(dir, spec ), pattern = "\\.asc$", full.names = TRUE)
  
  clippedstack <- raster::stack(clippedlist)
  #clippedstack2 <- terra::rast(clippedstack)
  print("rasters stacked")
  
  #calculate VIFs using a threshold of 10
  stepstack <- usdm::vifstep(clippedstack, th=10)
  print("VIF calculated")
  
  # exclude the collinear variables that were identified in the previous step
  v2 <- exclude(clippedstack,stepstack)
  
  print("Variables Excluded")
  ## finally copy the layers we want to a new folder!
  print("Starting to copy the layers")
  
  ## transfer files into directory per species per method
  for(i in 1:length(names(v2))){
    name <- names(v2)[i]
    print(name)
    
    from <- paste0(dir, spec, "/", name, ".asc")
    to <- paste0(dir, spec, "/VIF/", name, ".asc")
    
    file.copy(from, to,
              overwrite = TRUE, recursive = FALSE, 
              copy.mode = TRUE)
  }
}

