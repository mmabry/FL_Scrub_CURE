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
setwd("FL_Scrub/share") #try this if get error in setwd() and wd is /home/charissesproha

#setwd("blue/soltis/share/FL_Scrub") #try this if error in above 

#############################################################################
############################# 1. Check Records #############################
############################################################################
############################# Can Skip This Step ###########################

## Read in all cleaned files
#alldf <- list.files("data", full.names = TRUE, 
                    #recursive = FALSE, include.dirs = FALSE, pattern = "*09082023_cleaned.csv")

## Read all files as .csv file
#alldf <- lapply(alldf, read.csv)

## Combine the three species files by rows
#alldf <- do.call(rbind, alldf)

## Select needed columns
#alldf <- alldf %>%
  #dplyr::select(accepted_name, latitude, longitude, basisOfRecord)

## visually check records and remove points interactively 
#colnames(alldf)[1] ="scientificName"

#for(i in 1:length(unique(alldf$scientificName))){
#species <- unique(alldf$scientificName)[8]
#spec <- gsub(" ", "_", species)
#spec_subset <- dplyr::filter(alldf, scientificName == species)

#spec_subset2 <- process_flagged(spec_subset, interactive = TRUE)

#write.csv(spec_subset,file = paste0("data/", spec, "_91523_cleaned2.csv"),row.names = FALSE)


############################################################################
######################### 2. Make Maxent Ready file ########################
############################################################################
########################## Can Skip This Step (Already Have Maxent File) ###########

## Read in all cleaned files
#alldf <- list.files("data", full.names = TRUE, 
                    #recursive = FALSE, include.dirs = FALSE, pattern = "*_91523_cleaned2.csv")

## Read all files as .csv file
#alldf <- lapply(alldf, read.csv)

## Combine the three species files by rows
#alldf <- do.call(rbind, alldf)

## Select needed columns
#alldf <- alldf %>%
  #dplyr::select(scientificName, latitude, longitude, basisOfRecord)

## Save Maxent.csv
#write.csv(alldf,file = "data/maxentready_v5.csv",row.names = FALSE)

# ############################################################################
# #################### 3. Remove any necessary points ########################
# ############################################################################
# 
# ## Example ways to remove those points if needed
#  
# 
# alldf2 <- alldf[!(alldf$name == "Rhexia virginica" & alldf$long == -47.79), ] # Removed point in middle of ocean
# alldf3 <- alldf2[!(alldf2$name == "Rhexia nashii" & alldf2$long == -66.12), ] # Removed another point in middle of ocean
# alldf4 <- alldf3[!(alldf3$name == "Rhexia nashii" & alldf3$lat == 32.33 & alldf3$long == -79.42), ] # removed last point in middle of ocean
# 
#alldf2 <- alldf[!(alldf$accepted_name == "Dicerandra christmanii" & alldf$long == -80.40), ] #removed point not in range of species

#alldf3 <- alldf2[!(alldf2$accepted_name == "Sabal etonia" & alldf2$long == -81.73 & alldf2$lat == 32.42), ] # species is endemic to FL removed point in GA 

#alldf4 <- alldf3[!(alldf3$accepted_name == "Warea carteri" & alldf3$long == -80.32 & alldf3$lat == 24.68), ] #removed point in ocean 

#alldf5 <- alldf4[!(alldf4$accepted_name == "Polygala lewtonii" & alldf4$long == -81.92 & alldf4$lat == 26.58), ] #removed point that was disjunct from rest of population

#alldf6 <- alldf5[!(alldf5$accepted_name == "Conradina brevifolia" & alldf5$long == -86.78 & alldf5$lat == 28.89), ] #removed point in ocean

#alldf7 <- alldf6[!(alldf6$accepted_name == "Pinus clausa" & alldf6$long == -84.80 & alldf6$lat == 34.13), ] #removing points in GA and SC and one point in TN

#alldf8 <- alldf7[!(alldf7$accepted_name == "Pinus clausa" & alldf7$long == -77.91 & alldf7$lat == 34.05), ]

#alldf9 <- alldf8[!(alldf8$accepted_name == "Pinus clausa" & alldf8$long == -78.85 & alldf8$lat == 33.79), ]

#alldf10 <- alldf9[!(alldf9$accepted_name == "Pinus clausa" & alldf9$long == -84.39 & alldf9$lat == 32.59), ]

#alldf11 <- alldf10[!(alldf10$accepted_name == "Pinus clausa" & alldf10$long == -81.60 & alldf10$lat == 31.47), ]

#alldf12 <- alldf11[!(alldf11$accepted_name == "Pinus clausa" & alldf11$long == -83.33 & alldf11$lat == 30.59), ]

#alldf13 <- alldf12[!(alldf12$accepted_name == "Pinus clausa" & alldf12$long == -81.57 & alldf12$lat == 31.13), ]

#alldf14 <- alldf13[!(alldf13$accepted_name == "Pinus clausa" & alldf13$long == -80.59 & alldf13$lat == 32.48), ]

#alldf15 <- alldf14[!(alldf14$accepted_name == "Paronychia chartacea" & alldf14$long < -100), ] ## remove records in the west

# ## write out cleaned maxent file
write.csv(alldf15, file= "02_FL_Scrub_Clean/FL_Scrub_maxentready_v4.csv", row.names = FALSE)

# ## remap if you had to remove any points
# ## Change name to character
# alldf4$name <- as.character(alldf4$name)
# 
#alldf14$accepted_name <- as.character(alldf14$accepted_name)

# ## set up spatial dataframe
# alldfsp <- alldf4
# 
#alldfsp <- alldf14

# ## Set basemap
#states <- map_data("state")
#world <- map_data("world")
# 
# ### ggplot
#simple_map2 <- ggplot(data = world) + 
#  geom_polygon(aes(x = long, y = lat, group = group), fill = "grey90", color = "gray20")+
#  geom_point(alldfsp, mapping = aes(x = long, y = lat, color = accepted_name), alpha = 0.5, size= 0.5)+
#  coord_sf(xlim = c(-100, -60), ylim = c(20, 50)) + #only use this line if you want to zoom where your taxa occur
#  xlab("Longitude") +
#  ylab("Latitude") +
#  guides(colour = guide_legend(override.aes = list(size=2))) +
#  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
#        panel.background = element_blank(), axis.line = element_line(colour = "black"))
# 
# ## save your plot for your poster and project
# ggsave("CleanData_map2.png", plot = simple_map2 , path = "data/", height = 7, width = 9)

############################################################################
##################### 2. Make simple map of Taxa ###########################
############################################################################
#################### Start Here Read In alldf ##################################

alldf <- read.csv("02_FL_Scrub_Clean/FL_Scrub_maxentready_v4.csv") #read in alldf with removed data points 


##Clean Basis of Records ##Already did this 
#unique(alldf$basisOfRecord)


# remove fossils and living records
#alldf2 <- alldf[!(alldf$basisOfRecord %in% c("FOSSIL_SPECIMEN", "LIVING_SPECIMEN")), ]

#reformat states
#alldf$basisOfRecord[alldf$basisOfRecord == "HumanObservation"] <- "HUMAN_OBSERVATION"

#alldf$basisOfRecord[alldf$basisOfRecord == "MATERIAL_SAMPLE"] <- "PRESERVED_SPECIMEN"

#alldf$basisOfRecord[alldf$basisOfRecord == "Preserved specimen"] <- "PRESERVED_SPECIMEN"
#alldf$basisOfRecord[alldf$basisOfRecord == "Preserved Specimen"] <- "PRESERVED_SPECIMEN"
#alldf$basisOfRecord[alldf$basisOfRecord == "preservedspecimen"] <- "PRESERVED_SPECIMEN"
#alldf$basisOfRecord[alldf$basisOfRecord == "Physical specimen"] <- "PRESERVED_SPECIMEN"
#alldf$basisOfRecord[alldf$basisOfRecord == "PreservedSpecimen"] <- "PRESERVED_SPECIMEN"

# Make sure you have just the basis of records you want
#unique(alldf$basisOfRecord)
# "HUMAN_OBSERVATION"  "PRESERVED_SPECIMEN"

#write.csv(alldf,file = paste0("FL_Scrub_Clean/", "FL_Scrub_maxentready_v2.csv"),row.names = FALSE)



scrub.shape <- st_read("08_shapefile/vcom67_SandPineScrub.shp")
st_geometry_type(scrub.shape)
raster_img <- raster(scrub.shape)
proj4string(raster_img) <- CRS("+proj=longlat +datum=WGS84")

# Create an empty list to store the plots
#plot_list <- list()

#species <- "Conradina brevifolia"
for(i in 1:length(unique(alldf$accepted_name))){
  species <- unique(alldf$accepted_name)[25]
  spec <- gsub(" ", "_", species)
  spec_subset <- dplyr::filter(alldf, accepted_name == species)
  
  ## Change name to character
  spec_subset$accepted_name <- as.character(spec_subset$accepted_name)
  
  ## Set basemap
  states <- map_data("state")
  world <- map_data("world")
  
  # Calculate the latitude and longitude bounds from spec_subset
  lat_range <- range(spec_subset$lat, na.rm = TRUE) + c(-1, 1)
  lon_range <- range(spec_subset$long, na.rm = TRUE) + c(-3, 1)
  
  # Use these bounds in coord_sf
  plot <- ggplot(data = world) + 
    geom_polygon(aes(x = long, y = lat, group = group), fill = "grey90", color = "grey50") +
    geom_polygon(data = states, aes(x = long, y = lat, group = group), fill = "grey90", color = "grey70") +
    geom_sf(data = scrub.shape, fill = "grey30", color = "grey30") +
    geom_point(data = spec_subset, aes(x = long, y = lat, color = basisOfRecord), 
               alpha = 0.6, size = 0.5) +
    scale_color_manual(values = c("HUMAN_OBSERVATION" = "blue", 
                                  "PRESERVED_SPECIMEN" = "red", 
                                  "MATERIAL_SAMPLE" = "green4")) +
    coord_sf(xlim = lon_range, ylim = lat_range) + # dynamically set limits based on spec_subset data
    xlab("Longitude") +
    ylab("Latitude") +
    labs(title = species) +
    theme(axis.text = element_text(size = 10), 
          axis.title = element_text(size = 10),
          plot.title = element_text(face = "italic", size = 12),
          panel.grid.major = element_blank(), 
          panel.grid.minor = element_blank(),
          panel.background = element_blank(), 
          axis.line = element_line(colour = "black"))

plot 
ggsave(paste0("03_Florida_Scrub_Combined_maps/", spec, "_CleanDataMap.png"),plot = plot, width = 10, height = 8, dpi = 300)
  
#plot_list <- plot9
#par(mar = c(0, 0, 0, 0))
  
  #Add the plot to the list
  plot_list[[i]] <- plot2
  #}

## Arrange the plots in a grid and plot them together
ggarrange(plotlist=plot_list[c(1, 2)], hjust = -6, ncol=3, nrow = 3, legend="bottom", common.legend = TRUE) #,labels = c("s", "t", "u", "v", "w", "x", "y", "z", "zz"))
#1:6, 7:11, 12:15, 17:19, 21:24, 26, 28:32, 36
########### 

ggarrange(plot1, plot2, plot3, plot4, plot5, plot6, plot7, plot8, plot9, hjust = -6, ncol=3, nrow=3, legend="bottom", common.legend = TRUE)


## save your plot 
ggsave("Florida_Scrub_map_5_v2.pdf", path = "Florida_Scrub_maps/", units= "in", height = 7.5, width = 11)


