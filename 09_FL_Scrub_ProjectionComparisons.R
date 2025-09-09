### Projection Comparions
## script by ME Mabry

# Load necessary libraries
library(tidyverse)
library(terra)
library(biomod2)
library(raster)
library(dplyr)
library(ggplot2)
library(maps)
library(viridis)

#setwd
#setwd("/blue/soltis/share/FL_Scrub/share/")


# Load data file to get species names dynamically
alldf <- read.csv("02_FL_Scrub_Clean/FL_Scrub_maxentready_v4.csv")

# Extract unique species from the column `accepted_name`
species_list <- unique(alldf$accepted_name)

# Define scenarios and time periods
time_periods <- c("2041-2060", "2081-2100")
ssps <- c("ssp245","ssp370", "ssp585")
models <- c("ACCESS-CM2", "GISS-E2-1-G")

# Define function to perform comparison and plot for a given species, scenario, and time period
compare_scenarios <- function(species, ssp, year, model) {
  
  print(paste0("processing:", species, " ", model, " ", year, " ", ssp))
  
  spec <- gsub(" ", "_", species)
  spec_subset <- dplyr::filter(alldf, accepted_name == species)
  
  # Read in current and future raster files
  current <- raster(paste0("04_ENM_output/", spec, "_FL_Scrub_Prediction.asc"))
  future <- raster(paste0("05_FutureProjections/", spec, "_", model, "_", year, "_", ssp, "_ENM_Projection.asc"))
  
  
  # Create a binary raster using a cutoff value of 0.7
  current_binary <- calc(current, fun = function(x) ifelse(x >= 0.6, 1, 0))
  future_binary <- calc(future, fun = function(x) ifelse(x >= 0.6, 1, 0))
  
  #Define output file names for binary rasters
  current_output_file <- paste0("09_NicheComparisons/", spec, "_current_binary.asc")
  future_output_file <- paste0("09_NicheComparisons/", spec, "_", model, "_", year, "_", ssp, "_binary.asc")
  
  # Save the current binary raster as an ASCII file
  writeRaster(current_binary, filename = current_output_file, format = "ascii", overwrite = TRUE)
  
  # Save the future binary raster as an ASCII file
  writeRaster(future_binary, filename = future_output_file, format = "ascii", overwrite = TRUE)
  
  # Calculate range size differences
  RangeSizeDiff <- BIOMOD_RangeSize(current_binary, future_binary)
  results <- RangeSizeDiff$Compt.By.Models
  
  # Save results to CSV
  write.csv(results, file = paste0("09_NicheComparisons/", spec, "_current_v_", model, "_", year, "_", ssp, "_Scrub_Comparison.csv"))
  
  # Create a dataframe for plotting
  df <- as.data.frame(RangeSizeDiff$Diff.By.Pixel, xy = TRUE)
  fill <- factor(df[, 3])
  
  # Set basemap
  states <- map_data("state")
  
  # Extract counts dynamically from the results variable
  loss_count <- results[1]
  stable0_count <- results[2]
  stable1_count <- results[3]
  gain_count <- results[4]
  
  # Generate plot
  plot <- ggplot() +
    geom_polygon(data = states, aes(x = long, y = lat, group = group), fill = "grey90", color = "white") +
    geom_raster(data = df, aes(x = x, y = y, fill = fill)) +
    coord_sf(xlim = c(-88, -79), ylim = c(25, 31)) + 
    theme_bw() +
    xlab("Longitude") +
    ylab("Latitude") +
    ggtitle(paste0(species, " Current versus ", model, " ", year, "(", ssp, ") FL Scrub Habitat Suitability")) +
    guides(colour = guide_legend(override.aes = list(size=2))) +
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
          panel.background = element_blank(), axis.line = element_line(colour = "black")) +
    scale_fill_viridis_d(na.value = "white", 
                         name="Pixel Change",
                         breaks=c(-2, -1, 0, 1),
                         labels=c(
                           paste0("lost (", loss_count, " pixels)"),
                           paste0("unchanged (", stable1_count, " pixels)"),
                           paste0("not occupied (", stable0_count, " pixels)"),
                           paste0("occupied in the future (", gain_count, " pixels)")
                         )) +
    theme(legend.title = element_blank(), 
          plot.title = element_text(size = 10))
  
  # Save plot as PNG
  ggsave(paste0("09_NicheComparisons/", spec, "_current_v_", model, "_", year, "_", ssp, "_Scrub_Comparison.png"))
}


# Loop through all combinations of unique species, models, scenarios, and years
for (species in species_list) {
  #print(species)
  for (model in models) {
    #print(model)
    for (ssp in ssps) {
      #print(ssp)
      for (time_period in time_periods) {
        #print(time_period)
        # Run the comparison function for each combination
        compare_scenarios(species, ssp, time_period, model)
      }
    }
  }
}
