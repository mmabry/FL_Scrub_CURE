### Species Richness 
## script by ME Mabry

# Load required libraries
library(raster)  
library(terra)   
library(ggplot2)
library(dplyr)

setwd("FL_Scrub/share/")

# Load your raster stack (change here for each model)
# Specify the directory containing your files ## CHANGE HERE FOR FUTURE COMPARISON
directory_path <- "09_NicheComparisons"

# Define the file patterns for all the models and scenarios
file_patterns <- c(
  "current_binary.asc", # for the current projection, run by itself
  "ACCESS-CM2_2041-2060_ssp245_binary.asc",
  "ACCESS-CM2_2041-2060_ssp370_binary.asc",
  "ACCESS-CM2_2041-2060_ssp585_binary.asc",
  "ACCESS-CM2_2081-2100_ssp245_binary.asc",
  "ACCESS-CM2_2081-2100_ssp370_binary.asc",
  "ACCESS-CM2_2081-2100_ssp585_binary.asc",
  "GISS-E2-1-G_2041-2060_ssp245_binary.asc",
  "GISS-E2-1-G_2041-2060_ssp370_binary.asc",
  "GISS-E2-1-G_2041-2060_ssp585_binary.asc",
  "GISS-E2-1-G_2081-2100_ssp245_binary.asc",
  "GISS-E2-1-G_2081-2100_ssp370_binary.asc",
  "GISS-E2-1-G_2081-2100_ssp585_binary.asc"
)

# Set basemap data for plotting
states <- map_data("state")


# Loop through each file pattern
for (pattern in file_patterns) {
  # Get list of files that match the current pattern
  file_list <- list.files(path = directory_path, pattern = pattern, full.names = TRUE)
  
  # Print progress message
  cat("Processing files for pattern:", pattern, "\n")
  
  # Load the raster stack
  habitat_stack <- stack(file_list)
  
  # Sum the raster stack layers to calculate the number of species with suitable habitat per pixel
  species_richness_raster <- calc(habitat_stack, sum)
  
  # Create an output filename for the raster
  raster_output_file <- paste0("10_SpeciesRichness/", sub("\\.asc$", "_SpeciesRichness.asc", pattern))
  
  # Save the raster as an ASCII file
  writeRaster(species_richness_raster, filename = raster_output_file, format = "ascii", overwrite = TRUE)
  
  # Convert the raster to a data frame for ggplot
  species_df <- as.data.frame(species_richness_raster, xy = TRUE) %>%
    rename(fill = layer)  # Rename 'layer' to 'fill' for consistency
  
  # Create a plot filename
  plot_filename <- paste0("10_SpeciesRichness/", sub("\\.asc$", "_SpeciesRichness.pdf", pattern))
  
  # Open a PDF device to save the plot
  pdf(file = plot_filename, width = 18, height = 10)
  
  # Generate the plot
  plot <- ggplot() +
    geom_polygon(data = states, aes(x = long, y = lat, group = group),
                 fill = "grey90", color = "white") +
    geom_raster(data = species_df, aes(x = x, y = y, fill = fill)) +
    coord_sf(xlim = c(-88, -79), ylim = c(25, 31)) +
    scale_fill_viridis_c(option = "viridis", name = "Species Count", na.value = "transparent") +
    theme_minimal() +
    labs(title = paste0("Species Richness (", sub("\\.asc$", "", pattern), ")"),
         x = "Longitude", y = "Latitude") +
    theme(panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.background = element_blank(),
          axis.line = element_line(colour = "black")) +
    theme(legend.title = element_text(size = 12),
          plot.title = element_text(size = 10))
  
  # Print the plot to the PDF
  print(plot)
  
  # Close the PDF device
  dev.off()
  
  # Print message indicating the plot was saved
  cat("Saved plot and raster for pattern:", pattern, "\n")
}




### Part 2, calculate species changes
library(raster)
library(ggplot2)
library(dplyr)
library(tidyr)
library(maps)
library(viridis)

# List of model files
model_files <- c(
  "ACCESS-CM2_2041-2060_ssp245_binary_SpeciesRichness.asc",
  "ACCESS-CM2_2041-2060_ssp370_binary_SpeciesRichness.asc",
  "ACCESS-CM2_2041-2060_ssp585_binary_SpeciesRichness.asc",
  "ACCESS-CM2_2081-2100_ssp245_binary_SpeciesRichness.asc",
  "ACCESS-CM2_2081-2100_ssp370_binary_SpeciesRichness.asc",
  "ACCESS-CM2_2081-2100_ssp585_binary_SpeciesRichness.asc",
  "GISS-E2-1-G_2041-2060_ssp245_binary_SpeciesRichness.asc",
  "GISS-E2-1-G_2041-2060_ssp370_binary_SpeciesRichness.asc",
  "GISS-E2-1-G_2041-2060_ssp585_binary_SpeciesRichness.asc",
  "GISS-E2-1-G_2081-2100_ssp245_binary_SpeciesRichness.asc",
  "GISS-E2-1-G_2081-2100_ssp370_binary_SpeciesRichness.asc",
  "GISS-E2-1-G_2081-2100_ssp585_binary_SpeciesRichness.asc"
)

# Load current species richness raster
current_richness <- raster("10_SpeciesRichness/current_binary_SpeciesRichness.asc")

# Load basemaps
states <- map_data("state")
fl_counties <- map_data("county", region = "florida")

# Loop through each future model
for (model_file in model_files) {
  
  future_path <- file.path("10_SpeciesRichness", model_file)
  future_richness <- raster(future_path)
  
  # Calculate change
  richness_change <- future_richness - current_richness
  
  # Classify change
  richness_change_classified <- calc(richness_change, function(x) {
    ifelse(is.na(x), NA, ifelse(x < 0, -1, ifelse(x == 0, 0, 1)))
  })
  
  # Count changes
  species_loss_count <- cellStats(richness_change_classified == -1, sum)
  species_no_change_count <- cellStats(richness_change_classified == 0, sum)
  species_gain_count <- cellStats(richness_change_classified == 1, sum)
  
  # Data frame for plotting
  change_df <- as.data.frame(richness_change_classified, xy = TRUE) %>%
    rename(change = layer) %>%
    filter(!is.na(change))
  
  # Output filename
  plot_filename <- paste0("10_SpeciesRichness/Current_v_", gsub(".asc", ".pdf", model_file))
  
  # Create plot
  pdf(file = plot_filename, width = 18, height = 10)
  
  ggplot() +
    geom_polygon(data = states, aes(x = long, y = lat, group = group),
                 fill = "grey90", color = "white") +
    geom_raster(data = change_df, aes(x = x, y = y, fill = factor(change))) +
    scale_fill_manual(values = c("-1" = "#FDE725FF", "0" = "#31688EFF", "1" = "#440154FF"),
                      labels = c(
                        paste0("Species Loss (", species_loss_count, " pixels)"),
                        paste0("No Change (", species_no_change_count, " pixels)"),
                        paste0("Species Gain (", species_gain_count, " pixels)")
                      ),
                      name = "Change in Species Richness") +
    geom_polygon(data = fl_counties, aes(x = long, y = lat, group = group),
                 fill = NA, color = "grey70", size = 0.3) +
    coord_sf(xlim = c(-88, -79), ylim = c(25, 31)) +
    theme_minimal() +
    labs(title = paste("Change in Species Richness:", model_file),
         x = "Longitude", y = "Latitude") +
    theme(panel.grid = element_blank(),
          axis.line = element_line(color = "black"),
          legend.title = element_text(size = 12),
          plot.title = element_text(size = 10)) -> p
  
  print(p)
  dev.off()
}


# Part 3: Combine species richness change maps into a single supplementary figure

library(ggpubr)
library(ggplot2)
library(dplyr)
library(tidyr)
library(raster)
library(maps)
library(viridis)
library(patchwork)
library(purrr)

setwd("FL_Scrub/share/")

model_files <- c(
  "ACCESS-CM2_2041-2060_ssp245_binary_SpeciesRichness.asc",
  "ACCESS-CM2_2041-2060_ssp370_binary_SpeciesRichness.asc",
  "ACCESS-CM2_2041-2060_ssp585_binary_SpeciesRichness.asc",
  "ACCESS-CM2_2081-2100_ssp245_binary_SpeciesRichness.asc",
  "ACCESS-CM2_2081-2100_ssp370_binary_SpeciesRichness.asc",
  "ACCESS-CM2_2081-2100_ssp585_binary_SpeciesRichness.asc",
  "GISS-E2-1-G_2041-2060_ssp245_binary_SpeciesRichness.asc",
  "GISS-E2-1-G_2041-2060_ssp370_binary_SpeciesRichness.asc",
  "GISS-E2-1-G_2041-2060_ssp585_binary_SpeciesRichness.asc",
  "GISS-E2-1-G_2081-2100_ssp245_binary_SpeciesRichness.asc",
  "GISS-E2-1-G_2081-2100_ssp370_binary_SpeciesRichness.asc",
  "GISS-E2-1-G_2081-2100_ssp585_binary_SpeciesRichness.asc"
)

# Base maps
states <- map_data("state")
fl_counties <- map_data("county", region = "florida")
current_richness <- raster("10_SpeciesRichness/current_binary_SpeciesRichness.asc")

# Create panel labels A), A'), B), B'), ...
letters_base <- LETTERS[1:length(model_files)]
plot_labels <- as.vector(rbind(paste0(letters_base), paste0(letters_base, "'")))

# Store plots
all_plots <- list()

for (i in seq_along(model_files)) {
  model_file <- model_files[i]
  future_path <- file.path("10_SpeciesRichness", model_file)
  future_richness <- raster(future_path)
  # Extract model scenario ID from path
  model_id <- gsub("_binary_SpeciesRichness.asc", "", basename(future_path))
  
  
  # Richness map
  richness_df <- as.data.frame(future_richness, xy = TRUE)
  colnames(richness_df)[3] <- "richness"  # Rename the third column to 'richness'
  richness_df <- richness_df %>% filter(!is.na(richness))
  
  
  richness_plot <- ggplot() +
    geom_polygon(data = states, aes(x = long, y = lat, group = group),
                 fill = "grey90", color = "white") +
    geom_raster(data = richness_df, aes(x = x, y = y, fill = richness)) +
    labs(
      title = model_id, 
      x = "Longitude", y = "Latitude") +
    scale_fill_viridis(
      name = "Species Count",
      #limits = c(0, 1),
      option = "turbo",
      #na.value = "white"
    ) +
    geom_polygon(data = fl_counties, aes(x = long, y = lat, group = group),
                 fill = NA, color = "grey70", size = 0.3) +
    coord_sf(xlim = c(-88, -79), ylim = c(25, 31)) +
    theme_minimal(base_size = 10) +
    theme(
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      panel.background = element_blank(),
      plot.background = element_blank(),
      panel.border = element_rect(color = "black", fill = NA, size = 0.5),
      axis.line = element_line(color = "black", size = 0.3),
      axis.ticks = element_line(color = "black", size = 0.3),
      axis.text = element_text(size = 12),       # <-- larger tick labels
      axis.title = element_text(size = 12),
      legend.title = element_text(size = 8),
      legend.text = element_text(size = 6),
      legend.key.height = unit(0.3, "cm"),
      legend.key.width = unit(0.6, "cm"),
      legend.position = "bottom",
      legend.direction = "horizontal")
  
  # Richness change
  richness_change <- future_richness - current_richness
  richness_change_classified <- calc(richness_change, function(x) {
    ifelse(is.na(x), NA, ifelse(x < 0, -1, ifelse(x == 0, 0, 1)))
  })
  
  # Count pixel changes
  species_loss_count <- cellStats(richness_change_classified == -1, sum)
  species_no_change_count <- cellStats(richness_change_classified == 0, sum)
  species_gain_count <- cellStats(richness_change_classified == 1, sum)
  
  # Data for plotting
  change_df <- as.data.frame(richness_change_classified, xy = TRUE) %>%
    rename(change = layer) %>%
    filter(!is.na(change))
  
  # Labels with pixel counts
  legend_labels <- c(
    paste0("Species Loss (", species_loss_count, " pixels)"),
    paste0("No Change (", species_no_change_count, " pixels)"),
    paste0("Species Gain (", species_gain_count, " pixels)")
  )
  
  change_plot <- ggplot() +
    geom_polygon(data = states, aes(x = long, y = lat, group = group),
                 fill = "grey90", color = "white") +
    geom_raster(data = change_df, aes(x = x, y = y, fill = factor(change))) +
    labs(
      title = paste0("Current vs ", model_id), 
      x = "Longitude", y = "Latitude") +
    scale_fill_manual(
      values = c("-1" = "#4777EFFF", "0" = "#62FC6BFF", "1" = "#DB3A07FF"),
      labels = legend_labels,
      name = "Change in Species Richness"
    ) +
    geom_polygon(data = fl_counties, aes(x = long, y = lat, group = group),
                 fill = NA, color = "grey70", size = 0.3) +
    coord_sf(xlim = c(-88, -79), ylim = c(25, 31)) +
    theme_minimal(base_size = 10) +
    theme(
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      panel.background = element_blank(),
      plot.background = element_blank(),
      panel.border = element_rect(color = "black", fill = NA, size = 0.5),
      axis.line = element_line(color = "black", size = 0.3),
      axis.ticks = element_line(color = "black", size = 0.3),
      axis.text = element_text(size = 12),       # <-- larger tick labels
      axis.title = element_text(size = 12),
      legend.title = element_text(size = 8),
      legend.text = element_text(size = 6),
      legend.key.height = unit(0.3, "cm"),
      legend.key.width = unit(0.6, "cm"),
      legend.position = "bottom",
      legend.direction = "horizontal")
  
  # Add to plot list
  all_plots[[2 * i - 1]] <- richness_plot
  all_plots[[2 * i]]     <- change_plot
}

# --- After your for-loop: re-order the plots ---

# ACCESS plots: richness and change
access_indices <- as.vector(rbind(2 * (1:6) - 1, 2 * (1:6)))
access_plots <- all_plots[access_indices]

# GISS plots: richness and change
giss_indices <- as.vector(rbind(2 * (7:12) - 1, 2 * (7:12)))
giss_plots <- all_plots[giss_indices]

# Correct the loop
reordered_plots <- list()
for (i in 1:(length(access_plots) / 2)) {
  reordered_plots <- append(reordered_plots, access_plots[(2*i-1):(2*i)])
  reordered_plots <- append(reordered_plots, giss_plots[(2*i-1):(2*i)])
}


# Then recreate the combined figure
combined_fig <- ggarrange(
  plotlist = reordered_plots,
  ncol = 4, 
  nrow = 6, 
  labels = plot_labels, 
  label.x = 0.01, 
  label.y = 0.99, 
  font.label = list(size = 12, face = "bold"))

ggsave(
  "10_SpeciesRichness/Supplementary_Richness_and_Change_V4.pdf",
  plot = combined_fig,
  width = 24,
  height = 24
)

