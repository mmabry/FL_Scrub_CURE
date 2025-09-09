# Projecting models to The Coastal Plain


# Load Packages
library(tidyverse)
library(raster)
library(gtools)
library(dplyr)
library(dismo)
library(devtools)
library(kuenm)
library(ggplot2)
library(ggspatial)
library(viridis)
library(ggpubr)


##set working directory
#setwd("/blue/soltis/share/FL_Scrub/share/")


############################################################################
################## 1. load  maxent species file ###########################
############################################################################
# Load data file just to get species names
alldf <- read.csv("02_FL_Scrub_Clean/FL_Scrub_maxentready_v4.csv")

############################################################################
################## 2. load  FL scrub Rasters ############################
############################################################################
# Load Coastal Plain layers
climlist <- list.files("06_rasters/CoastalPlain/Current", pattern = "*.asc", full.names = TRUE)

### Load rasters and stack them
climstack <- raster::stack(climlist) 


############################################################################
####################### 3. Project ENMs to CP ##########################
############################################################################
species_list <- unique(alldf$accepted_name)

for(i in species_list){
  species <- i

  spec <- gsub(" ", "_", species)
  print(spec)
  spec_subset <- dplyr::filter(alldf, accepted_name == species)
  
  # Get names of rasters that were used for species models (using vif)
  specstack <- stack(mixedsort(sort(list.files(path=paste0("06_rasters/SpeciesLayers/", spec, "/VIF/"), full.names = TRUE))))
  
  layerNames <- names(specstack)
  
  # Get FL_scrub rasters which match the layers used in your model by the names
  CP_Rasters <- subset(climstack, layerNames)
  
  
  # Load Rdata file of the models
  load(paste0("04_ENM_output/", spec, "_optimalSeq_ENM.RData"))
  
  
  # Project model to FL scrub rasters
  p <- dismo::predict(mod.seq, CP_Rasters, filename = paste0("04_ENM_output/", spec, "_CP_Prediction.asc"))
  
  save(p, file = paste0("04_ENM_output/", spec, "_CP_Prediction.RData"))

  p_df <- as.data.frame(rasterToPoints(p))

  # Change the name of the third column
  colnames(p_df)[3] <- "habitat_suitablity"

  # Set basemap
  states <- map_data("state")
  world <- map_data("world")

  # ggplot to make sure it is correct
  ggplot(data = world) + 
    geom_polygon(aes(x = long, y = lat, group = group), fill = "grey90", color = "white")+
    geom_raster(data = p_df, aes(x = x, y = y, fill = habitat_suitablity)) +
    geom_polygon(data = states, aes(x = long, y = lat, group = group), fill = "transparent", color = "white") +
    coord_sf(xlim = c(-100, -68), ylim = c(24, 42), expand = FALSE) +  
    xlab("Longitude") +
    ylab("Latitude") +
    scale_fill_gradientn(colours = viridis::mako(99, direction = -1),
                         na.value = "white", breaks=c(0, 0.5, 1),labels=c("Low", 0.5, "High"),
                         limits=c(0,1)) +
    ggtitle(paste0(species," North American Coastal Plain Habitat Suitability")) +
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
          panel.background = element_blank(), axis.line = element_line(colour = "black")) +
    theme(axis.text = element_text(size = 12), 
          axis.title = element_text(size = 12)) + #originally 5
    #theme(plot.title = element_text(face = "italic")) + 
    theme(plot.title = element_text(size = 10))

  ggsave(file = paste0("04_ENM_output/", spec, "_CP_Projection.pdf"), width = 18, height = 10)

}



