## table 1
# Load libraries
library(dplyr)
library(tidyr)
library(stringr)
library(readr)

# Set working directory
setwd("09_NicheComparisons/")  

# Total number of pixels in Hardwood forests (used as denominator)
total_scrub_pixels <- 3903

# List all relevant files
files <- list.files(pattern = "_Scrub_Comparison.csv")

# Extract data from each file
extract_values <- function(file) {
  df <- read_csv(file, show_col_types = FALSE)
  
  # Extract metadata from filename
  parts <- str_split(file, "_")[[1]]
  species <- paste(parts[1], parts[2], sep = "_")
  model <- parts[5]
  time <- parts[6]
  ssp <- parts[7]
  
  scenario <- paste(model, time, ssp, sep = "_")
  
  # Calculate % of current suitable habitat
  current <- df$CurrentRangeSize[1]
  current_perc <- round((current / total_scrub_pixels) * 100, 2)
  
  tibble(
    Species = species,
    Scenario = scenario,
    SpeciesRangeChange = df$SpeciesRangeChange[1],
    Current_Habitat_Scrub_Perc = current_perc
  )
}

# Apply across all files
all_data <- bind_rows(lapply(files, extract_values))

# Keep one value per species for current % suitability
current_vals <- all_data %>%
  dplyr::select(Species, Current_Habitat_Scrub_Perc) %>%
  distinct()

# Pivot wider to have one column per scenario
wide_change <- all_data %>%
  dplyr::select(Species, Scenario, SpeciesRangeChange) %>%
  pivot_wider(names_from = Scenario, values_from = SpeciesRangeChange)

# Join together
final_table <- left_join(current_vals, wide_change, by = "Species")

# View result
print(final_table)

# Optional: write to CSV
write_csv(final_table, "Scrub_Habitat_Summary_Perc.csv")

# Load libraries
library(ggplot2)
library(dplyr)
library(tidyr)
library(scales)

# Assuming `final_table` is already created

# Reshape to long format
heatmap_data <- final_table %>%
  pivot_longer(
    cols = -c(Species, Current_Habitat_Scrub_Perc),
    names_to = "Scenario",
    values_to = "SpeciesRangeChange"
  )

# Modify species names to include current %
heatmap_data <- heatmap_data %>%
  mutate(Species = paste0(Species, " (", Current_Habitat_Scrub_Perc, "%)"))


# Process species names: remove underscores, italicize, and order AZ with A at top
heatmap_data <- heatmap_data %>%
  mutate(Species = gsub("_", " ", Species)) %>%
  mutate(Species = factor(Species, levels = sort(unique(Species), decreasing = TRUE)))  # A at top

# Plot
ggplot(heatmap_data, aes(x = Scenario, y = Species, fill = SpeciesRangeChange)) +
  geom_tile(color = "white") +
  geom_text(aes(label = round(SpeciesRangeChange, 1)), size = 3) +
  scale_fill_gradient2(
    low = "blue4",
    mid = "white",
    high = "red",
    midpoint = 0,
    guide = "none"  #  removes legend
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    axis.text.y = element_text(face = "italic"),
    panel.grid = element_blank()
  ) +
  labs(
    x = "Climate Scenario",
    y = "Species",
    title = "Projected % Change in Habitat Suitability in Florida Scrub",
    subtitle = "Blue = habitat loss; Red = habitat gain"
  )

ggsave(
  filename = "habitat_suitability_heatmap.png",
  plot = last_plot(),        # or replace with your plot object if named
  width = 12,                # in inches
  height = 10,
  dpi = 300                  # high resolution for publication
)
