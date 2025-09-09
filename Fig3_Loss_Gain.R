## Fig3
library(ggplot2)
library(sf)
library(raster)
library(dplyr)
library(tidyr)
library(viridis)
library(ggpubr)
library(grid)
library(biomod2)

setwd("FL_Scrub/share")

time_period <- "2081-2100"
model <- "ACCESS-CM2"
ssp <- "ssp370"

# Load scrub shapefile and state outlines
scrub.shape <- st_read("08_shapefile/vcom67_SandPineScrub.shp", quiet = TRUE)
states <- map_data("state")
world <- map_data("world")

all_plots <- list()

spec <- "Hypericum_cumulicola"
species <- gsub("_", " ", spec)

# Load current projection
load(paste0("04_ENM_output/", spec, "_FL_Scrub_Prediction.RData"))
p_df <- as.data.frame(rasterToPoints(p))
colnames(p_df)[3] <- "habitat_suitability"

current_proj <- ggplot(data = world) + 
  geom_polygon(aes(x = long, y = lat, group = group), fill = "grey90", color = "white") +
  geom_polygon(data = states, aes(x = long, y = lat, group = group), fill = "grey90", color = "white") +
  geom_tile(data = p_df, aes(x = x, y = y, fill = habitat_suitability)) +
  coord_sf(xlim = c(-88, -79), ylim = c(25, 31)) +
  labs(
    title = bquote(italic(.(species)) ~ "Florida Scrub"),
    subtitle = "Current Habitat Suitability (21.8%)", 
    x = "Longitude", y = "Latitude"
  ) +
  scale_fill_viridis(
    name = "Suitability",
    limits = c(0, 1),
    option = "turbo",
    na.value = "white"
  ) +
  theme_minimal(base_size = 10) +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.background = element_blank(),
    plot.background = element_blank(),
    panel.border = element_rect(color = "black", fill = NA, size = 0.5),
    axis.line = element_line(color = "black", size = 0.3),
    axis.ticks = element_line(color = "black", size = 0.3),
    axis.text = element_text(size = 10),       # <-- larger tick labels
    axis.title = element_text(size = 10),      # <-- larger axis titles
    legend.position = "bottom")

all_plots[[length(all_plots) + 1]] <- current_proj

# Load future projection
load(paste0("05_FutureProjections/", spec, "_", model, "_", time_period, "_", ssp, "_ENM_Projection.RData"))
p_df_future <- as.data.frame(rasterToPoints(p))
colnames(p_df_future)[3] <- "habitat_suitability"

future_proj <- ggplot(data = world) + 
  geom_polygon(aes(x = long, y = lat, group = group), fill = "grey90", color = "white") +
  geom_polygon(data = states, aes(x = long, y = lat, group = group), fill = "grey90", color = "white") +
  geom_tile(data = p_df_future, aes(x = x, y = y, fill = habitat_suitability)) +
  coord_sf(xlim = c(-88, -79), ylim = c(25, 31)) +
  labs(
    title = bquote(italic(.(species)) ~ "Florida Scrub"),
    subtitle = "Future Habitat Suitability (-89.4%)", 
    x = "Longitude", y = "Latitude"
  ) +
  scale_fill_viridis(
    name = "Suitability",
    limits = c(0, 1),
    option = "turbo",
    na.value = "white"
  ) +
  theme_minimal(base_size = 10) +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.background = element_blank(),
    plot.background = element_blank(),
    panel.border = element_rect(color = "black", fill = NA, size = 0.5),
    axis.line = element_line(color = "black", size = 0.3),
    axis.ticks = element_line(color = "black", size = 0.3),
    axis.text = element_text(size = 10),
    axis.title = element_text(size = 10),
    legend.position = "bottom")


all_plots[[length(all_plots) + 1]] <- future_proj

# Load binary change raster and plot
current_bin <- raster(paste0("09_NicheComparisons/", spec, "_current_binary.asc"))
future_bin <- raster(paste0("09_NicheComparisons/", spec, "_", model, "_", time_period, "_", ssp, "_binary.asc"))
change <- BIOMOD_RangeSize(current_bin, future_bin)
results <- change$Compt.By.Models
df_change <- as.data.frame(change$Diff.By.Pixel, xy = TRUE)

# Identify the name of the raster column (it's the third column)
raster_col <- colnames(df_change)[3]

# Rename it to "layer" for clarity
colnames(df_change)[3] <- "layer"

# Convert to factor with meaningful labels
df_change$layer <- factor(df_change$layer,
                          levels = c(-2, -1, 0, 1),
                          labels = c("lost", "unchanged", "not occupied", "occupied in the future"))

# Extract pixel change counts
loss_count     <- results[1]
stable0_count  <- results[2]
stable1_count  <- results[3]
gain_count     <- results[4]

color_palette <- viridis::turbo(8)
category_colors <- c("lost" = color_palette[2],
                     "unchanged" = color_palette[6],
                     "not occupied" = color_palette[4],
                     "occupied in the future" = color_palette[7])

# Ensure all factor levels are represented in the data
all_levels <- levels(df_change$layer)
missing_levels <- setdiff(all_levels, unique(df_change$layer))

# Add dummy rows with NA x/y for any missing level
if (length(missing_levels) > 0) {
  dummy_df <- data.frame(
    x = NA,
    y = NA,
    layer = factor(missing_levels, levels = all_levels)
  )
  df_change <- rbind(df_change, dummy_df)
}


comp_plot <- ggplot() +
  geom_polygon(data = states, aes(x = long, y = lat, group = group),
               fill = "grey90", color = "white") +
  geom_tile(data = df_change, aes(x = x, y = y, fill = layer)) +
  coord_sf(xlim = c(-88, -79), ylim = c(25, 31)) +
  labs(
    title = bquote(italic(.(species)) ~ "Florida Scrub"),
    subtitle = expression("Change in Habitat Suitability (1 pixel " %~% "1 km"^2*")"),
    x = "Longitude", y = "Latitude"
  ) +
  scale_fill_manual(
    values = category_colors,
    na.value = "white",
    drop = FALSE,
    name = "Pixel Change",
    guide = guide_legend(nrow = 2, byrow = TRUE),
    labels = c(
      paste0("lost (", loss_count, " pixels)"),
      paste0("remain occupied (", stable1_count, " pixels)"),
      paste0("never occupied (", stable0_count, " pixels)"),
      paste0("gained (", gain_count, " pixels)"))) + 
  theme_minimal(base_size = 10) +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.background = element_blank(),
    plot.background = element_blank(),
    panel.border = element_rect(color = "black", fill = NA, size = 0.5),
    axis.line = element_line(color = "black", size = 0.3),
    axis.ticks = element_line(color = "black", size = 0.3),
    axis.text = element_text(size = 10),
    axis.title = element_text(size = 10),
    legend.position = "bottom")



all_plots[[length(all_plots) + 1]] <- comp_plot

# Load NACP projection
load(paste0("05_FutureProjections/", spec, "_", model, "_", time_period, "_", ssp, "_ENM_CP_Projection.RData"))
p_df_cp <- as.data.frame(rasterToPoints(p))
colnames(p_df_cp)[3] <- "habitat_suitability"

cp_proj <- ggplot(data = world) +
  geom_polygon(aes(x = long, y = lat, group = group), fill = "grey90", color = "white") +
  geom_tile(data = p_df_cp, aes(x = x, y = y, fill = habitat_suitability)) +
  geom_polygon(data = states, aes(x = long, y = lat, group = group), fill = "transparent", color = "white") +
  coord_sf(xlim = c(-100, -73), ylim = c(24, 42)) +
  labs(
    title = bquote(italic(.(species)) ~ "Coastal Plain"),
    subtitle = "Future Habitat Suitability", 
    x = "Longitude", y = "Latitude"
  ) +
  scale_fill_viridis(
    name = "Suitability",
    limits = c(0, 1),
    option = "turbo",
    na.value = "white"
  ) +
  theme_minimal(base_size = 10) +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.background = element_blank(),
    plot.background = element_blank(),
    panel.border = element_rect(color = "black", fill = NA, size = 0.5),
    axis.line = element_line(color = "black", size = 0.3),
    axis.ticks = element_line(color = "black", size = 0.3),
    axis.text = element_text(size = 10),
    axis.title = element_text(size = 10),
    legend.position = "bottom")


all_plots[[length(all_plots) + 1]] <- cp_proj


## second species

spec <- "Eryngium_cuneifolium"
species <- gsub("_", " ", spec)

# Load current projection
load(paste0("04_ENM_output/", spec, "_FL_Scrub_Prediction.RData"))
p_df <- as.data.frame(rasterToPoints(p))
colnames(p_df)[3] <- "habitat_suitability"

current_proj <- ggplot(data = world) + 
  geom_polygon(aes(x = long, y = lat, group = group), fill = "grey90", color = "white") +
  geom_polygon(data = states, aes(x = long, y = lat, group = group), fill = "grey90", color = "white") +
  geom_tile(data = p_df, aes(x = x, y = y, fill = habitat_suitability)) +
  coord_sf(xlim = c(-88, -79), ylim = c(25, 31)) +
  labs(
    title = bquote(italic(.(species)) ~ "Florida Scrub"),
    subtitle = "Current Habitat Suitability (10.84%)", 
    x = "Longitude", y = "Latitude"
  ) +
  scale_fill_viridis(
    name = "Suitability",
    limits = c(0, 1),
    option = "turbo",
    na.value = "white"
  ) +
  theme_minimal(base_size = 10) +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.background = element_blank(),
    plot.background = element_blank(),
    panel.border = element_rect(color = "black", fill = NA, size = 0.5),
    axis.line = element_line(color = "black", size = 0.3),
    axis.ticks = element_line(color = "black", size = 0.3),
    axis.text = element_text(size = 10),       # <-- larger tick labels
    axis.title = element_text(size = 10),      # <-- larger axis titles
    legend.position = "bottom")

all_plots[[length(all_plots) + 1]] <- current_proj

# Load future projection
load(paste0("05_FutureProjections/", spec, "_", model, "_", time_period, "_", ssp, "_ENM_Projection.RData"))
p_df_future <- as.data.frame(rasterToPoints(p))
colnames(p_df_future)[3] <- "habitat_suitability"

future_proj <- ggplot(data = world) + 
  geom_polygon(aes(x = long, y = lat, group = group), fill = "grey90", color = "white") +
  geom_polygon(data = states, aes(x = long, y = lat, group = group), fill = "grey90", color = "white") +
  geom_tile(data = p_df_future, aes(x = x, y = y, fill = habitat_suitability)) +
  coord_sf(xlim = c(-88, -79), ylim = c(25, 31)) +
  labs(
    title = bquote(italic(.(species)) ~ "Florida Scrub"),
    subtitle = "Future Habitat Suitability (+787.9%)", 
    x = "Longitude", y = "Latitude"
  ) +
  scale_fill_viridis(
    name = "Suitability",
    limits = c(0, 1),
    option = "turbo",
    na.value = "white"
  ) +
  theme_minimal(base_size = 10) +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.background = element_blank(),
    plot.background = element_blank(),
    panel.border = element_rect(color = "black", fill = NA, size = 0.5),
    axis.line = element_line(color = "black", size = 0.3),
    axis.ticks = element_line(color = "black", size = 0.3),
    axis.text = element_text(size = 10),
    axis.title = element_text(size = 10),
    legend.position = "bottom")


all_plots[[length(all_plots) + 1]] <- future_proj

# Load binary change raster and plot
current_bin <- raster(paste0("09_NicheComparisons/", spec, "_current_binary.asc"))
future_bin <- raster(paste0("09_NicheComparisons/", spec, "_", model, "_", time_period, "_", ssp, "_binary.asc"))
change <- BIOMOD_RangeSize(current_bin, future_bin)
results <- change$Compt.By.Models
df_change <- as.data.frame(change$Diff.By.Pixel, xy = TRUE)

# Identify the name of the raster column (it's the third column)
raster_col <- colnames(df_change)[3]

# Rename it to "layer" for clarity
colnames(df_change)[3] <- "layer"

# Convert to factor with meaningful labels
df_change$layer <- factor(df_change$layer,
                          levels = c(-2, -1, 0, 1),
                          labels = c("lost", "unchanged", "not occupied", "occupied in the future"))

# Extract pixel change counts
loss_count     <- results[1]
stable0_count  <- results[2]
stable1_count  <- results[3]
gain_count     <- results[4]

color_palette <- viridis::turbo(8)
category_colors <- c("lost" = color_palette[2],
                     "unchanged" = color_palette[6],
                     "not occupied" = color_palette[4],
                     "occupied in the future" = color_palette[7])

# Ensure all factor levels are represented in the data
all_levels <- levels(df_change$layer)
missing_levels <- setdiff(all_levels, unique(df_change$layer))

# Add dummy rows with NA x/y for any missing level
if (length(missing_levels) > 0) {
  dummy_df <- data.frame(
    x = NA,
    y = NA,
    layer = factor(missing_levels, levels = all_levels)
  )
  df_change <- rbind(df_change, dummy_df)
}


comp_plot <- ggplot() +
  geom_polygon(data = states, aes(x = long, y = lat, group = group),
               fill = "grey90", color = "white") +
  geom_tile(data = df_change, aes(x = x, y = y, fill = layer)) +
  coord_sf(xlim = c(-88, -79), ylim = c(25, 31)) +
  labs(
    title = bquote(italic(.(species)) ~ "Florida Scrub"),
    subtitle = expression("Change in Habitat Suitability (1 pixel " %~% "1 km"^2*")"),
    x = "Longitude", y = "Latitude"
  ) +
  scale_fill_manual(
    values = category_colors,
    na.value = "white",
    drop = FALSE,
    name = "Pixel Change",
    guide = guide_legend(nrow = 2, byrow = TRUE),
    labels = c(
      paste0("lost (", loss_count, " pixels)"),
      paste0("remain occupied (", stable1_count, " pixels)"),
      paste0("never occupied (", stable0_count, " pixels)"),
      paste0("gained (", gain_count, " pixels)"))) + 
  theme_minimal(base_size = 10) +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.background = element_blank(),
    plot.background = element_blank(),
    panel.border = element_rect(color = "black", fill = NA, size = 0.5),
    axis.line = element_line(color = "black", size = 0.3),
    axis.ticks = element_line(color = "black", size = 0.3),
    axis.text = element_text(size = 10),
    axis.title = element_text(size = 10),
    legend.position = "bottom")



all_plots[[length(all_plots) + 1]] <- comp_plot

# Load NACP projection
load(paste0("05_FutureProjections/", spec, "_", model, "_", time_period, "_", ssp, "_ENM_CP_Projection.RData"))
p_df_cp <- as.data.frame(rasterToPoints(p))
colnames(p_df_cp)[3] <- "habitat_suitability"

cp_proj <- ggplot(data = world) +
  geom_polygon(aes(x = long, y = lat, group = group), fill = "grey90", color = "white") +
  geom_tile(data = p_df_cp, aes(x = x, y = y, fill = habitat_suitability)) +
  geom_polygon(data = states, aes(x = long, y = lat, group = group), fill = "transparent", color = "white") +
  coord_sf(xlim = c(-100, -73), ylim = c(24, 42)) +
  labs(
    title = bquote(italic(.(species)) ~ "Coastal Plain"),
    subtitle = "Future Habitat Suitability", 
    x = "Longitude", y = "Latitude"
  ) +
  scale_fill_viridis(
    name = "Suitability",
    limits = c(0, 1),
    option = "turbo",
    na.value = "white"
  ) +
  theme_minimal(base_size = 10) +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.background = element_blank(),
    plot.background = element_blank(),
    panel.border = element_rect(color = "black", fill = NA, size = 0.5),
    axis.line = element_line(color = "black", size = 0.3),
    axis.ticks = element_line(color = "black", size = 0.3),
    axis.text = element_text(size = 10),
    axis.title = element_text(size = 10),
    legend.position = "bottom")


all_plots[[length(all_plots) + 1]] <- cp_proj


# Final figure
final_plot <- ggarrange(plotlist = all_plots, 
                        ncol = 4, 
                        nrow = 2,
                        labels = LETTERS[1:8])

ggsave("Figure3_TwoSpecies_ENM.png", plot = final_plot, width = 18, height = 8, dpi = 300)
