### Species Richness 
## script by ME Mabry
library(terra)
library(sf)
library(ggplot2)
library(viridis)
library(rnaturalearth)
library(rnaturalearthdata)
library(ggpubr)

# Set working directory
setwd("FL_Scrub/share/")

# 1. Load the species richness raster
species_current <- rast("10_SpeciesRichness/current_binary_SpeciesRichness.asc")

# 2. Load Sea level rise polygon (simplified, aggregated)
slr_poly <- st_read("08_shapefile/NOAA_SLR_2FT_INUNDATED_AGG100.shp") |> st_make_valid()
slr_poly <- vect(slr_poly)  # convert to SpatVector for terra functions

# Reproject polygon to match raster CRS (if needed)
slr_poly <- project(slr_poly, crs(species_current))

# Mask species raster with SLR polygon (to get overlap)
species_in_slr <- mask(species_current, slr_poly)

# 5. Calculate overlap statistics
total_slr_pixels <- global(species_current, fun = "sum", na.rm = TRUE)[1,1]
overlap_slr_pixels <- global(species_in_slr, fun = "sum", na.rm = TRUE)[1,1]

# 6. Calculate proportion
overlap_slr_percent <- (overlap_slr_pixels / total_slr_pixels) * 100

# 7. Print result
cat("Total suitable pixels:", total_slr_pixels, "\n")
cat("Pixels overlapping with SLR:", overlap_slr_pixels, "\n")
cat("Proportion overlapping (%):", round(overlap_slr_percent, 2), "\n")

# 3. load Management zones
management_zones <- st_layers("../../FL_HardwoodForests/02_rasters/SeaLevel_ManagementZones/doc.kml")
selected_layers <- c("Federal Managing Agency", 
                     "State Managing Agency", 
                     "Local Managing Agency", 
                     "Private Managing Agency",
                     "Conservation Easements")
filtered_layer_names <- management_zones$name %in% selected_layers

# Read the filtered layers and keep both geometry and layer_name columns
filtered_layers <- lapply(management_zones$name[filtered_layer_names], function(layer_name) {
  layer_data <- st_read("../../FL_HardwoodForests/02_rasters/SeaLevel_ManagementZones/doc.kml", layer = layer_name)
  layer_data$layer_name <- layer_name  # Add the layer_name column to the data
  return(layer_data)})

# Combine the filtered layers into a single sf object
combined_filtered_layers <- do.call(rbind, filtered_layers)

# Convert sf to SpatVector
combined_filtered_layers_vect <- vect(combined_filtered_layers)

# Reproject polygon to match raster CRS (if needed)
man_poly <- project(combined_filtered_layers_vect, crs(species_current))

# Mask species raster with SLR polygon (to get overlap)
species_in_man <- mask(species_current, man_poly)

# 5. Calculate overlap statistics
total_man_pixels <- global(species_current, fun = "sum", na.rm = TRUE)[1,1]
overlap_man_pixels <- global(species_in_man, fun = "sum", na.rm = TRUE)[1,1]

# 6. Calculate proportion
overlap_man_percent <- (overlap_man_pixels / total_man_pixels) * 100

# 7. Print result
cat("Total suitable pixels:", total_man_pixels, "\n")
cat("Pixels overlapping with SLR:", overlap_man_pixels, "\n")
cat("Proportion overlapping (%):", round(overlap_man_percent, 2), "\n")



# 8. plot it
species_df <- as.data.frame(species_current, xy = TRUE, na.rm = TRUE)
colnames(species_df) <- c("x", "y", "richness")

# Load additional rasters
species_giss_2041 <- rast("10_SpeciesRichness/GISS-E2-1-G_2041-2060_ssp370_binary_SpeciesRichness.asc")
species_giss_2081 <- rast("10_SpeciesRichness/GISS-E2-1-G_2081-2100_ssp585_binary_SpeciesRichness.asc")

# Convert rasters to data frames
df_current <- as.data.frame(species_current, xy = TRUE, na.rm = TRUE)
df_2041 <- as.data.frame(species_giss_2041, xy = TRUE, na.rm = TRUE)
df_2081 <- as.data.frame(species_giss_2081, xy = TRUE, na.rm = TRUE)
names(df_current)[3] <- names(df_2041)[3] <- names(df_2081)[3] <- "richness"


# Load Florida basemap
states <- ne_states(country = "United States of America", returnclass = "sf")
florida <- states %>% dplyr::filter(name == "Florida")

# load world
world <- map_data("world")

# Panel A: Current species richness
pA <- ggplot(world) +
  geom_polygon(aes(x = long, y = lat, group = group), fill = "grey90", color = "white") +
  geom_sf(data = florida, fill = "grey90", color = "white") +
  geom_tile(data = df_current, aes(x = x, y = y, fill = richness)) +
  coord_sf(xlim = c(-88, -79), ylim = c(25, 31)) +
  labs(title = "Current Species Richness",
       x = "Longitude", y = "Latitude") +
  scale_fill_viridis(name = "Richness",
                     option = "turbo",
                     na.value = "white") +
  theme_minimal(base_size = 10) +
  theme(panel.grid = element_blank(),
    panel.background = element_blank(),
    plot.background = element_blank(),
    panel.border = element_rect(color = "black", fill = NA, size = 0.5),
    axis.line = element_line(color = "black", size = 0.3),
    axis.ticks = element_line(color = "black", size = 0.3),
    axis.text = element_text(size = 10),
    axis.title = element_text(size = 10),
    legend.position = "bottom")

# Panel B: 2041-2060 GISS with Management Zones
pB <- ggplot(world) +
  geom_polygon(aes(x = long, y = lat, group = group), fill = "grey90", color = "white") +
  geom_sf(data = florida, fill = "grey90", color = "white") +
  geom_raster(data = df_2041, aes(x = x, y = y, fill = richness)) +
  geom_sf(data = st_as_sf(man_poly), aes(), fill = "grey40", alpha = 0.3, color = NA) +
  coord_sf(xlim = c(-88, -79), ylim = c(25, 31)) +
  scale_fill_viridis(name = "Richness",
                     option = "turbo",
                     na.value = "white") +
  theme_minimal(base_size = 10) +
  theme(panel.grid = element_blank(),
        panel.background = element_blank(),
        plot.background = element_blank(),
        panel.border = element_rect(color = "black", fill = NA, size = 0.5),
        axis.line = element_line(color = "black", size = 0.3),
        axis.ticks = element_line(color = "black", size = 0.3),
        axis.text = element_text(size = 10),
        axis.title = element_text(size = 10),
        legend.position = "bottom") +
  labs(title = "2041-2060 (GISS, SSP3-7.0) + Management Zones",
       subtitle = paste0("Overlap: ", round(overlap_man_percent, 2), "% of FL Scrub Habitat"), 
       x = "Longitude", y = "Latitude")

# Panel C: 2081-2100 GISS with 2 ft Sea Level Rise
pC <- ggplot(world) +
  geom_polygon(aes(x = long, y = lat, group = group), fill = "grey90", color = "white") +
  geom_sf(data = florida, fill = "grey90", color = "white") +
  geom_raster(data = df_2081, aes(x = x, y = y, fill = richness)) +
  geom_sf(data = st_as_sf(slr_poly), aes(), fill = "blue", alpha = 0.3, color = NA) +
  coord_sf(xlim = c(-88, -79), ylim = c(25, 31)) +
  scale_fill_viridis(name = "Richness",
                     option = "turbo",
                     na.value = "white") +
  theme_minimal(base_size = 10) +
  theme(panel.grid = element_blank(),
        panel.background = element_blank(),
        plot.background = element_blank(),
        panel.border = element_rect(color = "black", fill = NA, size = 0.5),
        axis.line = element_line(color = "black", size = 0.3),
        axis.ticks = element_line(color = "black", size = 0.3),
        axis.text = element_text(size = 10),
        axis.title = element_text(size = 10),
        legend.position = "bottom") +
  labs(title = "2081-2100 (GISS, SSP5-8.5) + 2ft Sea Level Rise",
       subtitle = paste0("Overlap: ", round(overlap_slr_percent, 2), "% of FL Scrub Habitat"),
       x = "Longitude", y = "Latitude")

# Combine and export
combined_plot <- ggarrange(pA, pB, pC, 
                           ncol = 3, nrow = 1,
                           labels = c("A", "B", "C"),            # <-- adds ABC labels
                           label.x = 0.01, label.y = 0.99,       # <-- position (optional tweak)
                           font.label = list(size = 12, face = "bold"))


ggsave("12_SeaLevel_Mang/Figure4_RichnessPanels.png", 
       plot = combined_plot, width = 18, height = 6, dpi = 300)
