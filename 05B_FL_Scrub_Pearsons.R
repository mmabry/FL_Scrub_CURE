## Species Environmental Variables/Predictors
## Modified from and cite: https://github.com/mbelitz/Odo_SDM_Rproj and https://github.com/soltislab/BotanyENMWorkshops
## 03-16-2022

library(raster)
library(gtools)
library(dplyr)
library(rgdal)
library(sp)
library(rangeBuilder)
library(sf)
library(caret)
library(usdm)
library(dismo)
library(stringr)
library(rJava)
library(viridis)

### setwd 
#setwd("/blue/soltis/share/FL_Scrub/share/") 

args <- commandArgs(trailingOnly = TRUE)
species <- args[1]

## Make new directory for keeping rasters/environmental data
dir <- "06_rasters/SpeciesLayers/"


#Read in layers per species
clippedlist <- list.files(paste0(dir, species), pattern = "*.asc", full.names = TRUE)
clippedstack <- raster::stack(clippedlist, quick = TRUE)
print("rasters read")

## Start pearsons' - This can then be used after running ENMs to refer to other correlated variables
corr <- layerStats(clippedstack, 'pearson', na.rm=TRUE)
print("correlation has completed")

### Isolate only the pearson correlation coefficient and take absolute value
c <- abs(corr$`pearson correlation coefficient`)
print("absolute value has completed")

## write file
write.csv(c, paste0(dir, species, "/", species, "_Pearson_correlations.csv"), row.names = FALSE)


