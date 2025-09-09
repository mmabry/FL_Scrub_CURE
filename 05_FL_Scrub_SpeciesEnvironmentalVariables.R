## FL Scrub Species Environmental Variables
## Modified from ML Gaynor's scripts, modified from Jonathan Barz's scripts
## 10-27-2023 by Charisse Sproha 

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
####### 1. Make your species specific environmental variables ##############
############################################################################
# Load bioclim layers
biolist <- list.files("/blue/soltis/share/CWR_Proj/02_rasters/BioClim/", pattern = "*.tif", full.names = TRUE)
soillist <- list.files("/blue/soltis/share/CWR_Proj/02_rasters/SoilGrids/", pattern = "*_v2.tif", full.names = TRUE)

# add these together
climlist <- c(biolist, soillist)

## Order list using gtools
climlist <- mixedsort(sort(climlist))

### Load rasters and stack them
climstack <- raster::stack(climlist) 

## Read in the cleaned csv
alldf <- read.csv("02_FL_Scrub_Clean/FL_Scrub_maxentready_v4.csv") #which one are you using?

## crop and mask global enviromental layers
for(i in 1:length(unique(alldf$accepted_name))){
  species <- unique(alldf$accepted_name)[i]
  #species <- unique(alldf$accepted_name)[25]
  print(species)
  
  #subset species from dataframe
  spp_df <- alldf %>%
    dplyr::filter(accepted_name == species)
  
  ## Make into a spatial point data frame
  sppdfsp <- st_as_sf(spp_df, coords = c("long", "lat"), crs = 4326)
  
  #spatial dataframe
  #coordinates(spp_df) <- ~ long+lat
  #proj4string(spp_df) <- CRS("+proj=longlat +datum=WGS84")
  
  ## Create alpha hull
  sphull <- rangeBuilder::getDynamicAlphaHull(x = spp_df, 
                                              coordHeaders = c("long", "lat"),
                                              fraction = 1, # min. fraction of records we want included
                                              partCount = 1, # number of polygons allowed
                                              initialAlpha = 20, # initial alpha size, 20m
                                              clipToCoast = "terrestrial", 
                                              verbose = TRUE)
  #maybe need to fix plots for - adjust partCount or fraction 
  #ceratiola ericoides
  
  
  ### Visualize
  plot(sphull[[1]], col=transparentColor('gray50', 0.5), border = NA)
  points(x = spp_df$long, y = spp_df$lat, cex = 0.5, pch = 3)
  
  ### Transform into CRS related to meters
  #sphullTrans <- spTransform(sphull[[1]], "+proj=cea +lat_ts=0 +lon_0=0")
  sphullTrans <- st_transform(sphull[[1]], crs="+proj=cea +lat_ts=0 +lon_0") 
  spp_dfTrans <- st_transform(sppdfsp, crs="+proj=cea +lat_ts=0 +lon_0")
  
  ### Calculate buffer size
  #### Here we take the 80th quantile of the max distance between points
  spbuffDist <- quantile(x = (apply(sf::st_distance(spp_dfTrans), 2, FUN = function(x) sort(x)[2])), 
                         probs = 0.80, na.rm = TRUE) ## 7397 m
  
  ### Buffer the hull
  spbuffer_m <- sf::st_buffer(sphullTrans, spbuffDist, dissolve = TRUE)
  spbuffer <- sf::st_transform(spbuffer_m, "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")
  
  spbuffer <- st_transform(spbuffer, "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")
  
  ### Visualize
  plot(spbuffer, col=transparentColor('gray50', 0.5), border = NA)
  points(x = spp_df$long, y = spp_df$lat, cex = 0.5, pch = 3)
  
  spbuffer_df <- st_sf(var=1, spbuffer)
  ggplot(spbuffer_df)+
    geom_sf()
  
  #Crop and Mask
  spec <- gsub(" ", "_", species)
  dir.create(paste0(dir, spec))
  path <- paste0(dir, spec, "/")
  end <- ".asc"
  
  for(j in 1:length(names(climstack))){
    
    # Subset raster layer
    rast <- climstack[[j]]
    
    # Setup file names
    name <- names(rast)
    out <- paste0(path, name)
    outfile <- paste0(out, end) 
    
      
    # Crop and mask
    c <- crop(rast, sf::as_Spatial(spbuffer))
    c <- mask(c, sf::as_Spatial(spbuffer))
      
    # Write raster
    writeRaster(c, outfile, format = "ascii", overwrite = TRUE)
      
    
  }
}  
  
