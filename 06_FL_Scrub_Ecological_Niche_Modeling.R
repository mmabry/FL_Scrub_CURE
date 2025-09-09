# Ecological Niche Modeling with ENMevaluate

# This script is for generating and testing ENMs using ENMEval. Please see 
# https://besjournals.onlinelibrary.wiley.com/doi/full/10.1111/2041-210X.13628
# for the paper describing ENMEval and 
# https://jamiemkass.github.io/ENMeval/articles/ENMeval-2.0.0-vignette.html
# for the vignette.

# Set up java memory 
options(java.parameters = "-Xmx32g") # increase memory that can be used

# Load Packages
library(ENMeval)
library(raster)
library(gtools)
library(dplyr)
library(dismo)
library(ggplot2)
library(viridis)
library(devtools)
#install_github("marlonecobos/kuenm")
library(kuenm)
library(rJava)

#setwd
#setwd("FL_Scrub/share")

# Load data file
alldf <- read.csv("02_FL_Scrub_Clean/FL_Scrub_maxentready_v4.csv") ## make sure you used your latest version with all cleaned points

species_list <- unique(alldf$accepted_name)

# Description for Maxent args: https://groups.google.com/g/maxent/c/yRBlvZ1_9rQ

for(i in species_list){
  species <- i
  print(species)
  
  spec <- gsub(" ", "_", species)
  spec_subset <- dplyr::filter(alldf, accepted_name == species)
  
  
  ## read in VIF selected rasters
  specstack <- stack(mixedsort(sort(list.files(path=paste0("06_rasters/SpeciesLayers/", spec, "/VIF/"), full.names = TRUE))))
  projection(specstack) <- "+proj=longlat +ellps=WGS84 +towgs84=0,0,0,0,0,0,0 +no_defs"
  
  eval1 <- ENMeval::ENMevaluate(occs = spec_subset[, c("long", "lat")], 
                                envs = specstack,
                                tune.args = list(fc = c("L","LQ","LQH","H"), rm = 0.5:5),# test the feature classes L = linear and Q = quadratic
                                partitions = "block",
                                n.bg = 10000,
                                algorithm = 'maxent.jar')
  
  ## Save enm as rdata object
  save(eval1, file = paste0("04_ENM_output/", spec, "_ENMeval.RData"))
  
  ### ENMevaluate results
  ## Overall results
  results <- eval.results(eval1)
  write.table(results, file = paste0("04_ENM_output/", spec, "_results.txt"), sep = "\t")
  
  ## Visualize all models
  maps <- eval1@predictions
  pdf(file = paste0("04_ENM_output/", spec, "_ENMresults.pdf"), width = 24, height = 24)
  raster::plot(maps, nc=4, maxnl=20)
  dev.off()
  
  ## Calculate niche overlap between models to determine similarity between models
  mod_overlap <- calc.niche.overlap(eval1@predictions, overlapStat = "D")
  write.table(mod_overlap, file = paste0("04_ENM_output/", spec, "_niche_overlap.txt"), sep = "\t")
  
  ## write results for each partition 
  partition_results <- eval.results.partitions(eval1)
  write.table(partition_results, file = paste0("04_ENM_output/", spec, "_partition_results.txt"), sep = "\t")
  
  ## write file that holds environmental values for each occurrence data point
  occurance_env_values <- eval.occs(eval1)
  write.table(occurance_env_values, file = paste0("04_ENM_output/", spec, "_occurance_env_values.txt"), sep = "\t")
  
  ## write file that holds environmental values for each background data point
  background_env_values <- eval.bg(eval1)
  write.table(background_env_values, file = paste0("04_ENM_output/", spec, "_background_env_values.txt"), sep = "\t")
  
  ### Visualizing tuning results
  evalplot.stats(e = eval1, stats = c("or.mtp", "auc.val"), color = "fc", x.var = "rm", 
                 dodge = 0.5)
  ggsave(paste0("04_ENM_output/", spec, "_model_comparison.pdf"))
  
  ### Model Selection
  ## Select the model with delta AICc equal to 0, or the one with the lowest AICc score.
  ## In practice, models with delta AICc scores less than 2 are usually considered statistically equivalent.
  ## select models without considering cross-validation results using AICc (Warren & Seifert 2011; but see Velasco & González-Salazar 2019).
  ## loop through the results, look for zeroes first then min values
  opt.seq <- results %>% 
    dplyr::filter(!is.na(AICc))%>% #any model with an NA for AICc means there are more parameters than observations
    dplyr::filter(AICc == min(AICc))%>%
    dplyr::filter(or.10p.avg !=0)  %>%#exclude any resulting zeroes
    dplyr::filter(or.10p.avg == min(or.10p.avg))%>% #pick model(s) with minimum values
    dplyr::filter(auc.val.avg == max(auc.val.avg)) #pick model with max auc, if more than one min or10p score
  
  write.table(opt.seq, file = paste0("04_ENM_output/", spec, "_opt_seq.txt"), sep = "\t") 
  
  ## We can select a single model from the ENMevaluation object using the tune.args of our optimal model.
  mod.seq <- eval.models(eval1)[[opt.seq$tune.args]]
  
  ## Write a table of variable importance
  variable_importance <- mod.seq@results
  write.table(variable_importance, file = paste0("04_ENM_output/", spec, "_variable_importance.txt"), sep = "\t")
  
  ## Look at variable contribution
  pdf(file = paste0("04_ENM_output/", spec, "_variable_contribution.pdf"), width = 12, height = 24)
  plot(mod.seq)
  dev.off()
  
  ## Look at the response curves
  pdf(file = paste0("04_ENM_output/", spec, "_response_curves.pdf"), width = 24, height = 24)
  dismo::response(mod.seq)
  dev.off()
  
  ## Save mod.seq as rdata object
  save(mod.seq, file = paste0("04_ENM_output/", spec, "_optimalSeq_ENM.RData"))
  
  ## We can select the model predictions for our optimal model the same way we did for the model object above.
  pred.seq <- eval.predictions(eval1)[[opt.seq$tune.args]]

  pdf(file = paste0("04_ENM_output/", spec, "_optimal_model_prediction.pdf"))
  plot(pred.seq)
  dev.off()
  
}

