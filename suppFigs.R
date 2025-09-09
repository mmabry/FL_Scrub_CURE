### Supplemental figures
## should include plot of occurence points, Plot of current ENM projection, 
## response variables, Future projections, changes, and Southern Coastal Plain projections 

# load libraries
library(ggplot2)
library(sf)
library(raster)
library(dplyr)
library(dismo)
library(tidyr)
library(stringr)
library(tibble)
library(biomod2)

# setwd
setwd("FL_Scrub/share") 


# Load data file
alldf <- read.csv("02_FL_Scrub_Clean/FL_Scrub_maxentready_v3.csv") ## make sure you used your latest version with all cleaned points


# cleaned occurence map
species <- unique(alldf$accepted_name)[1]
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

# read in scrub shapefile
scrub.shape <- st_read("08_shapefile/vcom67_SandPineScrub.shp")
st_geometry_type(scrub.shape)
raster_img <- raster(scrub.shape)
proj4string(raster_img) <- CRS("+proj=longlat +datum=WGS84")

# Convert spec_subset to sf object
spec_sf <- st_as_sf(spec_subset, coords = c("long", "lat"), crs = st_crs(scrub.shape))

# Convert the logical matrix to a vector of row indices
inside_index <- st_intersects(spec_sf, scrub.shape, sparse = FALSE)  # Returns logical matrix
inside_rows <- apply(inside_index, 1, any)  # TRUE if the point is inside any polygon

# Subset only the points that are inside
records_inside <- spec_sf[inside_rows, ]

# Count the number of records per basisOfRecord type within scrub.shape
counts <- records_inside %>%
  group_by(basisOfRecord) %>%
  summarise(count = n())

# Create a named vector for legend labels with counts
legend_labels <- setNames(
  paste0(levels(factor(spec_subset$basisOfRecord)), " (", 
         counts$count[match(levels(factor(spec_subset$basisOfRecord)), counts$basisOfRecord)], ")"),
  levels(factor(spec_subset$basisOfRecord))
)


# Use these bounds in coord_sf
plot <- ggplot(data = world) + 
  geom_polygon(aes(x = long, y = lat, group = group), fill = "grey90", color = "grey50") +
  geom_polygon(data = states, aes(x = long, y = lat, group = group), fill = "grey90", color = "grey70") +
  geom_sf(data = scrub.shape, fill = "grey30", color = "grey30") +
  geom_point(data = spec_subset, aes(x = long, y = lat, color = basisOfRecord), 
             alpha = 0.6, size = 0.5) +
  scale_color_manual(values = c("HUMAN_OBSERVATION" = "blue", 
                                "PRESERVED_SPECIMEN" = "red", 
                                "MATERIAL_SAMPLE" = "green4"),
                     labels = legend_labels) +
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
  
#### response curves
# Load the mod.seq RData object
load(paste0("04_ENM_output/", spec, "_optimalSeq_ENM.RData"))  # Replace with your actual file path

# Convert mod.seq@results to a dataframe and retain row names
var_contributions <- as.data.frame(mod.seq@results) %>%
  rownames_to_column(var = "variable")  # Move row names into a column

# Filter for only "contribution" rows and extract top 3
top_vars <- var_contributions %>%
  filter(str_detect(variable, "contribution")) %>%  # Find only contribution-related rows
  arrange(desc(V1)) %>%  # Assuming the contribution values are in the first column (adjust if needed)
  slice(1:3) %>%
  mutate(variable = gsub("\\.contribution", "", variable)) %>%  # Remove .contribution suffix
  pull(variable)

dismo::response(mod.seq, var = top_vars)


### current projection
# Load in prediction data 
load(paste0("04_ENM_output/", spec, "_FL_Scrub_Prediction_v2.RData"))

### Make p plottable 
p_df <- as.data.frame(rasterToPoints(p))

# Change the name of the third column
colnames(p_df)[3] <- "habitat_suitablity"

# Set basemap
states <- map_data("state")
world <- map_data("world")

# ggplot to make sure it is correct
ggplot(data = world) + 
  geom_polygon(aes(x = long, y = lat, group = group), fill = "grey90", color = "white")+
  geom_polygon(data = states, aes(x = long, y = lat, group = group), fill = "grey90", color = "white") +
  geom_raster(data = p_df, aes(x = x, y = y, fill = habitat_suitability)) +
  coord_sf(xlim = c(-88, -79), ylim = c(25, 31)) + 
  xlab("Longitude") +
  ylab("Latitude") +
  scale_fill_gradientn(colours = viridis::turbo(99),
                       na.value = "white", 
                       breaks=c(0, 0.25, 0.5, 0.75, 1),
                       labels=c("Low",0.25, 0.5, 0.75, "High"),
                       limits=c(0,1)) +
  ggtitle(paste0(species," Current FL Scrub Habitat Suitability")) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black")) +
  theme(axis.text = element_text(size = 12), 
        axis.title = element_text(size = 12)) + #originally 5
  #theme(plot.title = element_text(face = "italic")) + 
  theme(plot.title = element_text(size = 12))

## future projections
models <- c("ACCESS-CM2", "GISS-E2-1-G")
time_periods <- c("2041-2060", "2081-2100")
ssps <- c("ssp245", "ssp370", "ssp585")

# Save the RData file for future reference
load(paste0("05C_FutureProjections/", spec, "_", model, "_", time_period, "_", ssp, "_ENM_Projection.RData"))

# Convert prediction to a plottable dataframe
p_df <- as.data.frame(rasterToPoints(p))
colnames(p_df)[3] <- "habitat_suitability"

# Set up basemap and plot
states <- map_data("state")
world <- map_data("world")

plot <- ggplot(data = world) +
  geom_polygon(aes(x = long, y = lat, group = group), fill = "grey90", color = "white") +
  geom_polygon(data = states, aes(x = long, y = lat, group = group), fill = "grey90", color = "white") +
  geom_raster(data = p_df, aes(x = x, y = y, fill = habitat_suitability)) +
  coord_sf(xlim = c(-88, -79), ylim = c(25, 31)) +
  xlab("Longitude") + ylab("Latitude") +
  scale_fill_gradientn(colours = viridis::turbo(99),
                       na.value = "white",
                       breaks = c(0, 0.25, 0.5, 0.75, 1),
                       labels = c("Low", 0.25, 0.5, 0.75, "High"),
                       limits = c(0, 1)) +
  ggtitle(paste0(species, " ", model, " Model ", time_period, " (", ssp, ") Habitat Suitability")) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black")) +
  theme(axis.text = element_text(size = 12), 
        axis.title = element_text(size = 12)) +
  theme(plot.title = element_text(size = 12))

## comparisons
current_output_file <- paste0("09_NicheComparisons/", spec, "_current_binary.asc")
future_output_file <- paste0("09_NicheComparisons/", spec, "_", model, "_", time_period, "_", ssp, "_binary.asc")

# Read the saved binary rasters
current_binary <- raster(current_output_file)
future_binary <- raster(future_output_file)

# Calculate range size differences
RangeSizeDiff <- BIOMOD_RangeSize(current_binary, future_binary)
results <- RangeSizeDiff$Compt.By.Models

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

# Define fixed colors using the `turbo()` colormap
color_palette <- viridis::turbo(8)

# Assign specific colors to each category
category_colors <- c("-2" = color_palette[2],  # Loss
                     "-1" = color_palette[6],  # Unchanged
                     "0"  = color_palette[4],  # Not occupied
                     "1"  = color_palette[7])  # Occupied in the future

# Generate plot
plot <- ggplot() +
  geom_polygon(data = states, aes(x = long, y = lat, group = group), fill = "grey90", color = "white") +
  geom_raster(data = df, aes(x = x, y = y, fill = fill)) +
  coord_sf(xlim = c(-88, -79), ylim = c(25, 31)) + 
  theme_bw() +
  xlab("Longitude") +
  ylab("Latitude") +
  ggtitle(paste0(species, " Current v ", model, " ", time_period, "(", ssp, ") Change in Suitability")) +
  guides(colour = guide_legend(override.aes = list(size=2))) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black")) +
  # Use scale_fill_manual with custom colors
  scale_fill_manual(values = category_colors, 
                    na.value = "white", 
                    name="Pixel Change",
                    labels=c(
                      paste0("lost (", loss_count, " pixels)"),
                      paste0("unchanged (", stable1_count, " pixels)"),
                      paste0("not occupied (", stable0_count, " pixels)"),
                      paste0("occupied in the future (", gain_count, " pixels)")
                    )) +
  
  theme(legend.title = element_blank(), 
        plot.title = element_text(size = 10)) +
  theme(axis.text = element_text(size = 12), 
        axis.title = element_text(size = 12)) +
  theme(plot.title = element_text(size = 12))
