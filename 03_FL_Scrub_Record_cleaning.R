## Record Cleaning and spatial thinning 
## Modified from Botany ENM workshop Cite: https://github.com/soltislab/BotanyENMWorkshops

### Load Packages
library(dplyr)
library(tidyr)
library(raster)
library(sp)
library(spatstat)
library(spThin)
library(fields)
library(lubridate)
#library(CoordinateCleaner) old version 
library(gatoRs)

#installing gethub version of CoordinateCleaner
#devtools::install_github("ropensci/CoordinateCleaner")
library(CoordinateCleaner)

##set working directory
setwd("/blue/soltis/share/FL_Scrub/share")

#clean environment for each new species 

############
#Reducing the data frame 
speciesfile <- "/blue/soltis/share/FL_Scrub/share/FL_Scrub_Raw_Data/Warea_carteri_Sept_20_23.csv" #reading in each new species file ,can tab for each new species
species_df <- read.csv(speciesfile) #reading in the csv file

# 598 initial observations in Asclepias
# 242 initial observations in Asimina
# 612 initial observations in Bonamia 
# 1028 initial observations in Calamintha
# 1224 initial observations in Carya
# 5175 initial observations in Ceratiola
# 399 initial observations in Chamaesyce 
# 272 initial observations in Chionanthus 
# 417 initial observations in Cladonia 
# 463 initial observations in Clitoria 
# 3135 initial observations in Conradina brev
# 975 initial observations in Conradina grand 
# 190 initial observations in Dicerandra ch
# 790 initial observations in Dicerandra fru
# 254 initial observations in Eryngium 
# 1078 initial observations in Euphorbia 
# 1452 initial observations in Garberia 
# 1055 initial observations in Helianthemum 
# 503 initial observations in Hypericum 
# 6607 initial observations in Ilex 
# 867 initial observations in Lechea
# 515 initial observations in Liatris 
# 609 initial observations in Nolina
# 257 initial observations in Osmanthus 
# 1850 initial observations in Paronychia 
# 1446 initial observations in Persea
# 3100 initial observations in Pinus
# 293 initial observations in Polygala 
# 611 initial observations in Polygonella bas
# 933 initial observations in Polygonella myr
# 605 initial observations in Prunus 
# 676 initial observations in Quercus 
# 970 initial observations in Sabal
# 5400 initial observations in Schrankia
# 976 initial observations in Sisyrinchium 
# 359 initial observations in Warea


species_name <- strsplit(speciesfile, '[/]')[[1]][8] #splitting up the file path of species name to take out everything until species name 
species_name2 <- strsplit(species_name, '[.]')[[1]][1] #taking out the .csv from end 
species_name3 <- strsplit(species_name2, '[_]')[[1]][c(1,2)] #splitting up genus and specific epithet to take out the date
accepted_name <- paste0(species_name3[1], "_", species_name3[2]) #pasting the species name back together with underscore in middle
species2_df <- species_df %>% #using pipe to create species2 data frame to reduce columns into only the ten important columns
  dplyr::select(ID = ID, 
                scientificName = scientificName, 
                basisOfRecord = basisOfRecord, 
                occurrenceID = occurrenceID,
                institutionCode = institutionCode,
                latitude = latitude, 
                longitude = longitude, 
                month = month, 
                day = day, 
                year = year)
write.table(species2_df, file = paste0("FL_Scrub_Clean/", accepted_name, "_observations.csv"),col.names = FALSE, append = FALSE, sep = ",")
#writing the file name for the observations csv for the species

##########################
##########################
 
### filtering raw data based on accepted synonyms

unique_df <- read.csv(file = paste0("FL_Scrub_Clean/", species_name2, "_uniquenames.csv"), header = TRUE) # data frame of the unique names 
species_search <- as.list(unique_df[,1]) #list of unique names in the species 
species3_df <- gatoRs::taxa_clean(species2_df, synonyms.list = species_search, taxa.filter = "exact", accepted.name = accepted_name) #adding column for accepted name in data frame 
write.csv(species3_df, file = paste0("FL_Scrub_Clean/",accepted_name, "_filtered1.csv" ),row.names = FALSE) #writing csv file for species 3


##########
#Modify column names

species3_df <- species3_df %>% #creating pipe to shorted species3 data frame to only 8 columns
  dplyr::select(ID = ID, 
                accepted_name = accepted_name, 
                basisOfRecord = basisOfRecord, 
                lat = latitude, 
                long = longitude, 
                year = year, 
                month = month, 
                day = day)

## filter NA's, Precision, Remove 00s,Remove Cultivated and Outlier coordinates.
# https://www.rdocumentation.org/packages/CoordinateCleaner/versions/2.0-20
species4_df <- species3_df %>% #filtering out NAs
  filter(!is.na(long)) %>%
  filter(!is.na(lat))

#no observations removed for Asclepias
# 192 observations removed for Asimina
# 421 observations removed for Bonamia
# 836 observations removed for Calamintha
# 728 observations removed for Carya
# 3187 observations removed for Ceratiola
# 279 observations removed for Chamaesyce 
# 289 observations removed for Chionanthus 
# 325 observation removed for Cladonia 
# 366 observations removed for Clitoria 
# 1737 observations removed for Conradina brev
# 625 observations removed for Conradina grand 
# 152 observations removed for Dicerandra ch
# 619 observations removed for Dicerandra fru
# 195 observations removed for Eryngium 
# 566 observations removed for Euphorbia 
# 757 observations removed for Garberia 
# 599 observations removed for Helianthumum 
# 393 observations removed for Hypericum 
# 5308 observations removed for Ilex 
# 634 observations removed for Lechea
# 372 observations removed for Liatris
# 465 observations removed for Nolina
# 195 observations removed for Osmanthus 
# 1244 observations removed for Paronychia 
# 951 observations removed for Persea
# 1367 observations removed for Pinus 
# 210 observations removed for Polygala
# 549 observations removed for Polygonella bas
# 751 observations removed for Polygonella myr
# 531 observations removed for Prunus 
# 418 observations removed for Quercus
# 570 observations removed for Sabal 
# 4442 observations removed for Schrankia
# 620 observations removed for Sisyrinchium 
# 252 observations removed for Warea

species4_df$lat <- round(species4_df$lat, digits = 2) #rounding lat and long to only 2 digits
species4_df$long <- round(species4_df$long, digits = 2)

species5_df <- species4_df %>% #filtering out lat and longs that are 0 
    filter(long != 0.00) %>%
    filter(lat != 0.00)
#no observations removed for Asclepias
# 1 observation removed for Asimina
# no observations removed for Bonamia
# 8 observations removed for Calamintha 
# no observations removed for Carya
# no observations removed for Ceratiola
# no observations removed for Chamaesyce 
# 1 observation removed for Chionanthus 
# 2 observations removed for Cladonia
# no observations removed for Clitoria 
# 3 observations removed for Conradina brev
# no observations removed for Conradina grand 
# no observations removed for Dicerandra ch
# no observations removed for Dicerandra fru
# no observations removed for Eryngium 
# no observations removed for Euphorbia 
# 1 observations removed for Garberia
# no observations removed for Helianthemum 
# no observations removed for Hypericum 
# 11 observations removed for Ilex
# no observations removed for Lechea
# no observations removed for Liatris 
# no observations removed for Nolina
# no observations removed for Osmanthus 
# no observations removed for Paronychia
# 1 observation removed for Persea
# no observations removed for Pinus
# no observations removed for Polygala
# no observations removed for Polygonella bas
# no observations removed for Polygonella myr 
# 1 observations removed for Prunus 
# no observations removed for Quercus 
# 3 observations removed for Sabal
# no observations removed for Schrankia
# no observations removed for Sisyrinchium 
# no observations removed for Warea

species5_df <- cc_inst(species5_df, 
                       lon = "long", 
                       lat = "lat", 
                       species = "accepted_name", 
                       #value = "clean", 
                       #buffer = 1,
                       geod = FALSE) ## testing observations against institutions 




# no observations removed for Asclepias 
# no observations removed for Asimina
# no observations removed for Bonamia
# no observations removed for Calamintha 
# no observations removed for Carya
#no observations removed for Ceratiola
# no observations removed for Chamaesyce 
# no observations removed for Chionanthus
# no observations removed for Cladonia
# no observations removed for Clitoria 
# no observations removed for Conradina brev
# no observations removed for Conradina grand
# no observations removed for Dicerandra ch
# no observations removed for Dicerandra fru
# no observations removed for Eryngium 
# no observations removed for Euphorbia 
# no observations removed for Garberia 
# no observations removed for Helianthumum 
# no observations removed for Hypericum 
# no observations removed for Ilex 
# no observations removed for Lechea
# no observations removed for Liatris 
# no observations removed for Nolina
# no observations removed for Osmanthus 
# no observations removed for Paronychia 
# no observations removed for Persea
# no observations removed for Pinus 
# no observations removed for Polygala
# no observations removed for Polygonella bas
# no observations removed for Polygonella myr 
# no observations removed for Prunus
# no observations removed for Quercus 
# no observations removed for Sabal
# no observations removed for Schrankia
# no observations removed for Sisyrinchium 
# no observations removed for Warea
 


species5_df <- cc_outl(species5_df, 
                        lon = "long", 
                        lat = "lat", 
                        species = "accepted_name") ## removing outliers 


#no observations removed for Asclepias 
# 5 observations removed for Asimina
# no observations removed for Bonamia
# 30 observations removed for Calamintha
# 18 observations removed for Carya
# 4 observations removed for Ceratiola
# no observations removed for Chamaesyce 
# 2 observations removed for Chionanthus
# 15 observations removed for Cladonia 
# 9 observations removed for Clitoria 
# 140 observations removed for Conradina brev
# no observations removed for Conradina grand 
# no observations removed for Dicerandra ch
# 1 observation removed for Dicerandra fru
# 11 observations removed for Eryngium 
# 73 observations removed for Euphorbia 
# 3 observations removed for Garberia
# 8 observations removed for Helianthumum 
# no observations removed for Hypericum 
# 29 observations removed for Ilex 
# 2 observations removed for Lechea
# 1 observation removed for Liatris  
# 1 observation removed for Nolina
# 1 observation removed for Osmanthus 
# no observations removed for Paronychai 
# 14 observations removed for Persea
# 34 observations removed for Pinus 
# no observations removed for Polygala
# no observations removed for Polygonella bas
# no observations removed for Polygonella myr
# 1 observation removed for Prunus
# 18 observations removed for Quercus  
# 7 observations removed for Sabal
# 11 observations removed for Schrankia
# 2 observations removed for Sisyrinchium 
# 8 observations removed for Warea


write.csv(species5_df, file = paste0("FL_Scrub_Clean/",accepted_name, "_filtered2.csv" ),row.names = FALSE)
#writing csv file for filtered 2


### Removing Duplicates, Fix dates, separate into Year/Month/Day,Remove Identical Rows

species6_df <- distinct(species5_df, lat, long, year, month, day, .keep_all = TRUE) 
write.csv(species6_df, file = paste0("FL_Scrub_Clean/",accepted_name, "_filtered3.csv" ),row.names = FALSE) #writing csv file for filtered 3
# not sure how many removed for Asclepias 
# 16 removed for Asimina
# 31  removed for Bonamia
# 35 removed for Calamintha
# 168 removed for Carya
# 431 removed for Ceratiola
# 33 removed for Chamaesyce 
# 13 removed for Chionanthus 
# 33 removed for Cladonia 
# 8 removed for Clitoria 
# 246 removed for Conradina brev
# 60 removed for Conradina grand 
# 18 removed for Dicerandra ch
# 42 removed for Dicerandra fru
# 2 removed for Eryngium 
# 157 removed for Euphorbia 
# 122 removed for Garberia
#  299 removed for Helianthemum
# 7 removed for Hypericum 
# 494 removed for Ilex 
# 36 removed for Lechea
# 10 removed for Liatris 
# 36 removed for Nolina
# 12 removed for Osmanthus 
# 238 removed for Paronychia 
# 179 removed for Persea
# 391 removed for Pinus 
# 7 removed for Polygala
# 12 removed for Polygonella bas
# 38 removed for Polygonella myr
# 6 removed for Prunus 
# 90 removed for Quercus 
# 178 removed for Sabal
# 337 removed for Schrankia
# 100 removed for Sisyrinchium 
# 21 removed for Warea


### spatial thinning based on nearest neighbor

nnDm_species <- rdist.earth(as.matrix(data.frame(lon = species6_df$long, lat = species6_df$lat)), miles = FALSE, R = NULL) #using matrix with curve of earth to remove nearest neighbor
diag(nnDm_species) <- NA #making the diagonal of the matrix into NAs
thinpar <- min(nnDm_species[nnDm_species >0], na.rm = TRUE) #thinning to take out nearest neighbor
print(thinpar) #smallest distance between two individuals 
thin_data <- spThin::thin(loc.data =  species6_df,
                               verbose = FALSE,
                               long.col = "long",
                               lat.col = "lat",
                               spec.col = "accepted_name",
                               thin.par = thinpar,
                               reps = 100,
                               locs.thinned.list.return = TRUE,
                               write.files = FALSE)
  
thin_data <- thin_data[[100]] #list of one of the 100 with lat and long
species7_df <- species6_df[species6_df$lat %in% thin_data$Latitude & species6_df$long %in% thin_data$Longitude, ]
write.csv(species7_df, file = paste0("FL_Scrub_Clean/",accepted_name, "_filtered4.csv" ),row.names = FALSE)


### function for Spatial Correction, we will retain only one pt per pixel. 

bio1 <- raster("rasters/BioClim/wc2.1_30s_bio_1.tif") ## Read in raster file, using bio1 raster file
rasterResolution <- max(res(bio1)) # Set resolution

  
#Remove a point which nearest neighbor distance is smaller than the resolution size
while(min(spatstat.geom::nndist(species7_df[,4:5])) < rasterResolution){
  nnD_species <- spatstat.geom::nndist(species7_df[,4:5])
  species7_df <- species7_df[-(which(min(nnD_species) == nnD_species) [1]), ] ##weird never ending thing unless you overwrite species7
}
#while loop to run until it has one point per pixel and when the distance is less than the resolution

write.csv(species7_df, file = paste0("FL_Scrub_Clean/",accepted_name, "_filtered5.csv" ),row.names = FALSE)
#creating filtered5 csv file

#########*STOP HERE*##########

##############
#Filtered 5 is last filtered file 
##############


#Wait to do this until have all files 

############################################################################
######## 4. Make a single maxent ready file from cleaned data ##############
############################################################################

### Make maxent file
## Read in all cleaned files
alldf <- list.files(paste0("FL_Scrub_Clean/"), full.names = TRUE, 
                    recursive = FALSE, include.dirs = FALSE, pattern = "*filtered5.csv")
alldf <- lapply(alldf, read.csv)
alldf <- do.call(rbind, alldf)

## Select needed columns
alldf <- alldf %>%
  dplyr::select(accepted_name, basisOfRecord, lat, long)

##check basis column 
unique(alldf$basisOfRecord)

alldf2 <- alldf[!(alldf$basisOfRecord == "MATERIAL_CITATION"), ] # To remove records
#alldf3 <- alldf2[!(alldf2$basisOfRecord == "HumanObservation"), ]
#alldf4 <- alldf3[!(alldf3$basisOfRecord == "HUMAN_OBSERVATION"),]


#length(alldf4$accepted_name[alldf4$accepted_name=="Sabal etonia"])

# 12 Asclepias 
# 8 Asimina
# 20 Bonamia
# 19 Calamintha 
# 85 Carya
# 223 Ceratiola
# 25 Chamaesyce 
# 15 Chionanthus 
# 12 Cladonia perforata
# 9 Clitoria
# 38 Conradina
# 8 Dicerandra chr
# 23 Dicerandra frut
# 4 Eryngium 
# 100 Euphorbia
# 50 Garberia
# 61 Helianthemum 
# 10 Hypericum 
# 413 Ilex 
# 25 Lechea
# 8 Liatris
# 17 Nolina
# 41 Osmanthus
# 152 Paronychia
# 81 Persea
# 232 Pinus 
# 14 Polygala
# 15 Polygonella
# 13 Prunus
# 41 Quercus 
# 56 Sabal
# 

alldf2$basisOfRecord[alldf2$basisOfRecord == "PreservedSpecimen"] <- "PRESERVED_SPECIMEN" # To modify basis names for mapping

## Save Maxent.csv
write.csv(alldf2,file = paste0("FL_Scrub_Clean/", "FL_Scrub_maxentready.csv"),row.names = FALSE)

