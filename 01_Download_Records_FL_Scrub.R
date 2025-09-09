## Download Record Data for FL_Scrub
## Modified from Botany ENM workshop Cite: https://github.com/soltislab/BotanyENMWorkshops
### 9/8/2023
#Charisse Sproha
#Modifying from another project script
#For Asclepias curtissii



#devtools::install_github("nataliepatten/gatoRs")
### Load packages 
library(dplyr) 
library(tidyr) 
library(plyr) 
library(spocc) 
library(ridigbio) 
library(tibble) 
#library(rbison)
#install.packages("devtools")
library(devtools)
#devtools::install_github("nataliepatten/gatoRs")
library(gatoRs)


### setwd 
setwd("/blue/soltis/share/FL_Scrub/share/FL_Scrub_Raw_Data")



##for testing DDM package
#source("scripts/copy_DDM.R") ##need to make sure the list_of_wants.csv file can be found. 
#it is hard coded into the function


############################################################################
########################### 1. Make species lists ##########################
############################################################################

Asclepiascurtissii <- c("Asclepias curtissii", "Asclepias aceratoides", 
                        "Asclepias arenicola", "Oxypteryx arenicola", 
                        "Oxypteryx curtissii")

Ilexambigua <- c("Ilex ambigua", "Cassine caroliniana", "Ilex ambigua var. coriacea", 
                  "Ilex buswellii", "Ilex caroliniana", "Ilex caroliniana var. jejuna",
                  "Ilex ambigua var. ambigua", "Ilex ambigua var. monticola", "Ilex ambigua var. cariacea", 
                  "Ilex ambigua var. montana", "Ilex beadlei", "Ilex beadlei var. laevis", 
                  "Ilex dubia", "Ilex dubia var. beadlei", "Ilex dubia var. condensata", 
                  "Ilex dubia var. monticola", "Ilex mollis", "Ilex montana var. beadlei", 
                  "Ilex montana var. mollis", "Prinos dubius", "Synstima acuminata", 
                  "Synstima ambigua", "Synstima caroliniana", "Cassine caroliniana")

Nolinabrittoniana <- c("Nolina brittoniana")

Asiminatetramera <- c("Asimina tetramera", "Pityothamnus tetramerus")

Paronychiachartacea <- c("Paronychia chartacea", "Nyachia pulvinata", 
                         "Paronychia chartacea var. minima", "Paronychia pulvinata", 
                         "Paronychia chartacea var. chartacea", "Paronychia minima")

Prunusgeniculata <- c("Prunus geniculata")

Bonamiagrandiflora <- c("Bonamia grandiflora", "Breweria grandiflora")

Liatrisohlingerae <- c("Liatris ohlingerae", "Ammopursus ohlingerae", "Lacinaria ohlingerae")

Schrankiamicrophylla <- c("Schrankia microphylla", "Mimosa quadrivalis var. angustana", 
                          "Leptoglottis angustisiliqua", "Leptoglottis chapmanii", 
                          "Leptoglottis halliana", "Leptoglottis microphylla", "Leptoglottis uncinata", 
                          "Mimosa horridula", "Mimosa microphylla", "Morongia angustata", 
                          "Morongia horridula", "Morongia horridula var. angularis", 
                          "Morongia microphylla", "Morongia uncinata", "Schrankia floridana", 
                          "Schrankia angustata", "Schrankia angustata var. branchycarpa", 
                          "Schrankia angustisiliqua", "Schrankia chapmanii", "Schrankia halliana", 
                          "Schrankia horridula", "Schrankia horridula var. angularis", 
                          "Schrankia microphylla", "Schrankia microphylla var. microphylla", 
                          "Schrankia uncinata", "Mimosa quadrivalvis var. floridana", "Leptoglottis floridana", 
                          "Mimosa floridana", "Morongia floridana", "Schrankia floridana", 
                          "Schrankia microphylla var. floridana")

Calaminthaashei <- c("Calamintha ashei", "Clinopodium ashei", "Satureja ashei", 
                     "Clinopodium ashei")

Cladoniaperforata <- c("Cladonia perforata")

Clitoriafragrans <- c("Clitoria fragrans", "Martiusia fragrans")

Caryafloridana <- c("Carya floridana", "Hicoria floridana", "Hicorius floridana")

Helianthemumnashii <- c("Helianthemum nashii", "Crocanthemum nashii", 
                        "Crocanthemum thyroideum", "Helianthemum nashii", 
                        "Halimium nashii", "Helianthemum thyrsoideum", "Crocanthemum nashii")

Pinusclausa <- c("Pinus clausa", "Pinus clausa subsp. immuginata", 
                 "Pinus clausa var. immuginata", "Pinus inops var. clausa", 
                 "Pinus inops subsp. clausa", "Pinus virginiana subsp. clausa")

Ceratiolaericoides <- c("Ceratiola ericoides", "Ceratiola falcatula", "Empetrum aciculare")

Dicerandrachristmanii <- c("Dicerandra christmanii", "Dicerandra frutescens var. christmanii")

Osmanthusmegacarpus <- c("Osmanthus megacarpus", "Cartrema floridanum", 
                         "Amarolea floridana", "Amarolea megacarpa", 
                         "Cartrema megacarpum", "Cartrema megacarpa", 
                         "Osmanthus americanus subsp. megacarpus", "Osmanthus americanus var. megacarpus", 
                         "Osmanthus floridanus", "Osmanthus megacarpus", 
                         "Amarolea megacarpa", "Cartrema megacarpa")

Chamaesycecumulicola <- c("Chamaesyce cumulicola", "Euphorbia cumulicola", 
                          "Chamaesyce cumulicola")

Conradinabrevifolia <- c("Conradina brevifolia", "Conradina canescens", 
                         "Calamintha canescens", "Conradina puberula")

Conradinagrandiflora <- c("Conradina grandiflora", "Conradina etonia", 
                          "Conradina grandiflora var. etonia")

Chionanthuspygmaneus <- c("Chionanthus pygmaeus")

Dicerandrafrutescens <- c("Dicerandra frutescens", "Dicerandra christmanii", 
                          "Dicerandra cornutissima", "Dicerandra frutescens var. christmanii", 
                          "Dicerandra frutescens var. cornutissima", "Dicerandra frutescens var. immaculata",
                          "Dicerandra frutescens var. modesta", "Dicerandra frutescens var. savannarum", 
                          "Dicerandra frutescens var. thinicola", "Dicerandra immaculata var. immaculata", 
                          "Dicerandra immaculata var. savannarum", "Dicerandra modesta", 
                          "Dicerandra thinicola")

Hypericumcumulicola <- c("Hypericum cumulicola", "Sanidophyllum cumulicola")

Eryngiumcuneifolium <- c("Eryngium cuneifolium")

Lecheacernua <- c("Lechea cernua")

Polygonellamyriophylla <- c("Polygonella myriophylla", "Polygonum dentoceras", 
                            "Dentoceras myriophyllum", "Polygonella myriophylla", 
                            "Polygonum dentaceras", "Polygonella dentoceras")

Garberiaheterophylla <- c("Garberia heterophylla", "Cacalia heterophylla", 
                          "Garberia fruticosa", "Leptoclinium fruticosum", 
                          "Liatris fruticosa")

Perseahumilis <- c("Persea humilis", "Persea borbonia", "Persea borbonia var. humilis", 
                   "Tamala humilis", "Barbonia humilis", 
                   "Persea borbonia subsp. humilis", "Persea humilis", 
                   "Tamala humilis")

Sabaletonia <- c("Sabal etonia", "Sabal adansonii var. megacarpa", 
                 "Sabal megacarpa", "Sabal miamiensis")

Polygalalewtonii <- c("Polygala lewtonii")

Polygonellabasiramia <- c("Polygonella basiramia", "Polygonum basiramia", 
                          "Polygonella ciliata var. basiramia", "Delopyrum basiramia", 
                          "Polygonella basiramia", "Polygonella ciliata var. basiramia")

Quercusinopina <- c("Quercus inopina", "Quercus inopinai")

Sisyrinchiumxerophyllum <- c("Sisyrinchium xerophyllum", "Sysrinchium solstitiale")

Wareacarteri <- c("Warea carteri")

Euphorbiafloridana <- c("Euphorbia floridana", "Galarhoeus floridanus", 
                        "Euphorbia sphaerosperma", "Tithymalus sphaerospermus")






############################################################################
################## 2. Make species variable ######################
############################################################################
species <- "Euphorbia_floridana"

#dir <- "/blue/soltis/share/FL_Scrub/share/FL_Scrub_Raw_Data/"

#dir.create(paste0(dir, "/", species))

############################################################################
###### 3. use gators_download to pull data from iDigBio, and GBIF #####
############################################################################

gators_download(synonyms.list = Perseahumilis,
                write.file = TRUE, 
                filename = paste0(species, "_1April2025.csv"),
                gbif.match = "fuzzy",
                idigbio.filter = TRUE)


