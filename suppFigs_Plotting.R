library(ggplot2)
library(sf)
library(raster)
library(dplyr)
library(dismo)
library(tidyr)
library(stringr)
library(tibble)
library(biomod2)
library(patchwork)
library(ggpubr)
library(cowplot)
library(gridExtra)
library(jpeg)
library(grid)

#setwd("FL_Scrub/share")

# loading plotting function
generate_custom_labels <- function(n) {
  # Start with A to F
  single_letters <- LETTERS[1:6]
  
  # G-R as triplets
  triplet_letters <- LETTERS[7:18]  # G to R
  triplets <- unlist(lapply(triplet_letters, function(x) c(x, paste0(x, "'"), paste0(x, '"'))))
  
  # Combine both
  all_labels <- c(single_letters, triplets)
  
  # Return only as many as needed
  if (n > length(all_labels)) {
    stop("Too many plots: exceeds available custom labels.")
  }
  
  all_labels[1:n]
}


taxonomy_update <- tibble::tribble(
  ~`file_name`, ~`updated_name`,
  "Asclepias curtissii", "Asclepias curtissii",
  "Asimina tetramera", "Asimina tetramera",
  "Bonamia grandiflora", "Bonamia grandiflora",
  "Calamintha ashei", "Calamintha ashei",
  "Carya floridana", "Carya floridana",
  "Ceratiola ericoides", "Ceratiola ericoides",
  "Chamaesyce cumulicola", "Euphorbia cumulicola",
  "Chionanthus pygmaeus", "Chionanthus pygmaeus",
  "Cladonia perforata", "Cladonia perforata",
  "Clitoria fragrans", "Clitoria fragrans",
  "Conradina brevifolia", "Conradina brevifolia",
  "Conradina grandiflora", "Conradina grandiflora",
  "Dicerandra christmanii", "Dicerandra christmanii",
  "Dicerandra frutescens", "Dicerandra frutescens",
  "Eryngium cuneifolium", "Eryngium cuneifolium",
  "Euphorbia floridana", "Euphorbia floridana",
  "Garberia heterophylla", "Garberia heterophylla",
  "Helianthemum nashii", "Crocanthemum nashii",
  "Hypericum cumulicola", "Hypericum cumulicola",
  "Ilex ambigua", "Ilex ambigua",
  "Lechea cernua", "Lechea cernua",
  "Liatris ohlingerae", "Liatris ohlingerae",
  "Nolina brittoniana", "Nolina brittoniana",
  "Osmanthus megacarpus", "Cartrema floridanum",
  "Paronychia chartacea", "Paronychia chartacea",
  "Persea humilis", "Persea borbonia var. humilis",
  "Pinus clausa", "Pinus clausa",
  "Polygala lewtonii", "Polygala lewtonii",
  "Polygonella basiramia", "Polygonum basiramia",
  "Polygonella myriophylla", "Polygonum dentoceras",
  "Prunus geniculata", "Prunus geniculata",
  "Quercus inopina", "Quercus inopina",
  "Sabal etonia", "Sabal etonia",
  "Schrankia microphylla", "Mimosa quadrivalvis var. floridana",
  "Sisyrinchium xerophyllum", "Sisyrinchium xerophyllum",
  "Warea carteri", "Warea carteri"
)

# Load data
alldf <- read.csv("02_FL_Scrub_Clean/FL_Scrub_maxentready_v4.csv")
alldf <- alldf %>%
  left_join(taxonomy_update, by = c("accepted_name" = "file_name"))

unique_species <- unique(alldf$accepted_name)

# Define triplet labels (G to R)
triplet_letters <- LETTERS[7:18]  # G to R

# Define model assignment (ACCESS and GISS alternating)
triplet_models <- rep(c("ACCESS-CM2", "GISS-E2-1-G"), times = 6)

# Optional: time periods and SSPs for matching
triplet_time_periods <- rep(c("2041-2060", "2081-2100"), each = 6)
triplet_ssps <- rep(c("ssp245", "ssp370", "ssp585"), times = 2, each =2)


for (species in unique_species) {
  spec <- gsub(" ", "_", species)
  
  # Look up updated name
  updatedName <- taxonomy_update %>%
    filter(file_name == species) %>%
    pull(updated_name)
  
  # If not found, fall back to original
  if (length(updatedName) == 0) updatedName <- species
  
  
  spec_subset <- dplyr::filter(alldf, accepted_name == species)
  spec_subset$accepted_name <- as.character(spec_subset$accepted_name)
  
  # Basemap
  states <- map_data("state")
  world <- map_data("world")
  lat_range <- range(spec_subset$lat, na.rm = TRUE) + c(-1, 1)
  lon_range <- range(spec_subset$long, na.rm = TRUE) + c(-3, 3)
  
  # Scrub shapefile
  scrub.shape <- st_read("08_shapefile/vcom67_SandPineScrub.shp")
  spec_sf <- st_as_sf(spec_subset, coords = c("long", "lat"), crs = st_crs(scrub.shape))
  
  # Function to plot response curves
  plot_response_curve <- function(mod, var) {
    response_data <- as.data.frame(dismo::response(mod, var = var))  # Extract response curve data
    colnames(response_data) <- c("value", "suitability")  # Ensure proper column names
    
    ggplot(response_data, aes(x = value, y = suitability)) +
      geom_line(color = "blue", size = 1) +
      labs(title = paste("Response Curve"), x = var, y = "Suitability") +
      ylim(0, 1) +  # Force Y-axis limits to 0-1
      theme_minimal()
  }
  
  ##### Photo for A
  # Construct path to species-specific image
  img_path <- file.path("JPG_Images", paste0(spec, ".jpg"))
  
  if (file.exists(img_path)) {
    img <- readJPEG(img_path)
    g <- rasterGrob(img, interpolate = TRUE)
    image_plot <- ggplot() +
      annotation_custom(g, xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = Inf) +
      theme_void()
  } else {
    # fallback grey box if image not found
    image_plot <- ggplot() +
      theme_void() +
      theme(panel.background = element_rect(fill = "grey80"))
  }
  
  
  ##### Occurrence Data Plot for B
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
  
  occ_plot <- ggplot(data = world) + 
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
    labs(title = updatedName) +
    theme(axis.text = element_text(size = 10), 
          axis.title = element_text(size = 10),
          plot.title = element_text(face = "italic", size = 10),
          panel.grid.major = element_blank(), 
          panel.grid.minor = element_blank(),
          panel.background = element_rect(fill = NA, color = NA),
          plot.background = element_rect(fill = NA, color = NA), 
          axis.line = element_line(colour = "black"))+
    theme(legend.title = element_text(size = 8, face = "bold"),
          legend.text = element_text(size = 6),
          legend.key.size = unit(1, "cm"),
          legend.position = "bottom")
  
  # Load Model Results
  load(paste0("04_ENM_output/", spec, "_optimalSeq_ENM.RData"))
  var_contributions <- as.data.frame(mod.seq@results) %>% rownames_to_column(var = "variable")
  top_vars <- var_contributions %>%
    filter(str_detect(variable, "contribution")) %>%
    arrange(desc(V1)) %>%
    slice(1:3) %>%
    mutate(variable = gsub("\\.contribution", "", variable)) %>%
    pull(variable)
  
  # Generate Response Curves for C, D, E
  response_curves <- lapply(top_vars, function(var) {
    plot_response_curve(mod.seq, var)
  })
  
  # Ensure all response curves are ggplot objects
  response_curves <- Filter(function(x) inherits(x, "ggplot"), response_curves)
  
  # Load Current Projection
  load(paste0("04_ENM_output/", spec, "_FL_Scrub_Prediction.RData"))
  p_df <- as.data.frame(rasterToPoints(p))
  colnames(p_df)[3] <- "habitat_suitability"
  
  current_proj <- ggplot(data = world) + 
    geom_polygon(aes(x = long, y = lat, group = group), fill = "grey90", color = "white")+
    geom_polygon(data = states, aes(x = long, y = lat, group = group), fill = "grey90", color = "white") +
    geom_tile(data = p_df, aes(x = x, y = y, fill = habitat_suitability)) +
    coord_sf(xlim = c(-88, -79), ylim = c(25, 31)) + 
    xlab("Longitude") +
    ylab("Latitude") +
    scale_fill_gradientn(colours = viridis::turbo(99),
                         na.value = "white", 
                         breaks=c(0, 0.5, 1),
                         labels=c("Low", 0.5, "High"),
                         limits=c(0,1)) +
    ggtitle("Current Habitat Suitability") +
    theme(panel.grid.major = element_blank(), 
          panel.grid.minor = element_blank(),
          panel.background = element_rect(fill = NA, color = NA),
          plot.background = element_rect(fill = NA, color = NA), 
          axis.line = element_line(colour = "black")) +
    theme(axis.text = element_text(size = 10), 
          axis.title = element_text(size = 10)) +
    theme(plot.title = element_text(size = 10)) +
    theme(legend.title = element_text(size = 8, face = "bold"),
          legend.text = element_text(size = 6),
          legend.key.size = unit(0.4, "cm"),
          legend.position = "bottom")
  
  
  # Future Projections & Comparisons
  time_periods <- c("2041-2060", "2081-2100")
  models <- c("ACCESS-CM2", "GISS-E2-1-G")
  ssps <- c("ssp245", "ssp370", "ssp585")
  
  plot_list <- list()
  
  for (i in seq_along(triplet_letters)) {
    model <- triplet_models[i]
    triplet_label <- triplet_letters[i]
    time_period <- triplet_time_periods[i]
    ssp <- triplet_ssps[i]
    
    # Load future projection
    future_file <- paste0("05_FutureProjections/", spec, "_", model, "_", time_period, "_", ssp, "_ENM_Projection.RData")
    load(future_file)
    
    p_df <- as.data.frame(rasterToPoints(p))
    colnames(p_df)[3] <- "habitat_suitability"
    
    future_proj <- ggplot(data = world) +
      geom_polygon(aes(x = long, y = lat, group = group), fill = "grey90", color = "white") +
      geom_polygon(data = states, aes(x = long, y = lat, group = group), fill = "grey90", color = "white") +
      geom_tile(data = p_df, aes(x = x, y = y, fill = habitat_suitability)) +
      coord_sf(xlim = c(-88, -79), ylim = c(25, 31)) +
      xlab("Longitude") + ylab("Latitude") +
      scale_fill_gradientn(colours = viridis::turbo(99),
                           na.value = "white",
                           breaks = c(0, 0.5, 1),
                           labels = c("Low", 0.5, "High"),
                           limits = c(0, 1)) +
      ggtitle(paste0(model, " Model ", time_period, " (", ssp, ")")) +
      theme(panel.grid.major = element_blank(), 
            panel.grid.minor = element_blank(),
            panel.background = element_rect(fill = NA, color = NA),
            plot.background = element_rect(fill = NA, color = NA), 
            axis.line = element_line(colour = "black")) +
      theme(axis.text = element_text(size = 10), 
            axis.title = element_text(size = 10)) +
      theme(plot.title = element_text(size = 10)) +
      theme(legend.title = element_text(size = 8, face = "bold"),
            legend.text = element_text(size = 6),
            legend.key.size = unit(0.4, "cm"),
            legend.position = "bottom")
    
    # Load binary raster for comparison
    current_output_file <- paste0("09_NicheComparisons/", spec, "_current_binary.asc")
    future_output_file <- paste0("09_NicheComparisons/", spec, "_", model, "_", time_period, "_", ssp, "_binary.asc")
    
    current_binary <- raster(current_output_file)
    future_binary <- raster(future_output_file)
    
    RangeSizeDiff <- BIOMOD_RangeSize(current_binary, future_binary)
    results <- RangeSizeDiff$Compt.By.Models
    df <- as.data.frame(RangeSizeDiff$Diff.By.Pixel, xy = TRUE)
    df$layer <- factor(df[, 3],
                       levels = c(-2, -1, 0, 1),
                       labels = c("lost", "unchanged", "not occupied", "occupied in the future"))
    
    # Extract counts dynamically from the results variable
    loss_count <- results[1]
    stable0_count <- results[2]
    stable1_count <- results[3]
    gain_count <- results[4]
    
    # Define fixed colors using the `turbo()` colormap
    color_palette <- viridis::turbo(8)
    
    # Assign specific colors to each category
    category_colors <- c("lost" = color_palette[2],  # Loss
                         "unchanged" = color_palette[6],  # Unchanged
                         "not occupied"  = color_palette[4],  # Not occupied
                         "occupied in the future"  = color_palette[7])  # Occupied in the future
    
    # Ensure all factor levels are represented in the data
    all_levels <- levels(df$layer)
    
    # Ensure 'layer' in df is a factor with all levels
    df$layer <- factor(df$layer, levels = all_levels)
    
    missing_levels <- setdiff(all_levels, levels(droplevels(df$layer)))
    
    # Add dummy rows with NA x/y for any missing level
    # Only add dummy rows for truly missing levels
    if (length(missing_levels) > 0) {
      dummy_df <- data.frame(
        x = NA,
        y = NA,
        layer = factor(missing_levels, levels = all_levels)
      )
      
      # Ensure all other columns in df are present in dummy_df
      # Create matching columns with NA
      for (col in setdiff(names(df), names(dummy_df))) {
        dummy_df[[col]] <- NA
      }
      
      # Reorder columns to match
      dummy_df <- dummy_df[, names(df)]
      
      # Append
      df <- rbind(df, dummy_df)
    }
    
    comp_plot <- ggplot() +
      geom_polygon(data = states, aes(x = long, y = lat, group = group), fill = "grey90", color = "white") +
      geom_tile(data = df, aes(x = x, y = y, fill = layer)) +
      coord_sf(xlim = c(-88, -79), ylim = c(25, 31)) + 
      theme_bw() +
      xlab("Longitude") +
      ylab("Latitude") +
      ggtitle("Change in Suitability") +
      #guides(colour = guide_legend(override.aes = list(size=2))) +
      theme(panel.grid.major = element_blank(), 
            panel.grid.minor = element_blank(),
            panel.background = element_rect(fill = NA, color = NA),
            plot.background = element_rect(fill = NA, color = NA), 
            axis.line = element_line(colour = "black")) +
      # Use scale_fill_manual with custom colors
      scale_fill_manual(values = category_colors, 
                        na.value = "white", 
                        drop = FALSE,
                        name="Pixel Change",
                        guide = guide_legend(nrow = 2, byrow = TRUE),
                        labels=c(
                          paste0("lost (", loss_count, " pixels)"),
                          paste0("unchanged (", stable1_count, " pixels)"),
                          paste0("not occupied (", stable0_count, " pixels)"),
                          paste0("occupied in the future (", gain_count, " pixels)"))) +
      theme(axis.text = element_text(size = 10), 
            axis.title = element_text(size = 10)) +
      theme(plot.title = element_text(size = 10)) +
      theme(legend.title = element_text(size = 8, face = "bold"),
            legend.text = element_text(size = 6),
            legend.key.size = unit(0.4, "cm"),
            legend.position = "bottom")
      
      # Load the CP projection for the same model/time/ssp
      cp_file <- file.path("05_FutureProjections", paste0(spec, "_", model, "_", time_period, "_", ssp, "_ENM_CP_Projection.RData"))
    
      load(cp_file)  # loads object 'p'
      
      # Convert to data frame for plotting
      p_df_cp <- as.data.frame(rasterToPoints(p))
      colnames(p_df_cp)[3] <- "habitat_suitability"
      
      cp_proj <- ggplot(data = world) +
        geom_polygon(aes(x = long, y = lat, group = group), fill = "grey90", color = "white") +
        #geom_polygon(data = states, aes(x = long, y = lat, group = group), fill = "grey90", color = "white") +
        geom_raster(data = p_df_cp, aes(x = x, y = y, fill = habitat_suitability)) +
        geom_polygon(data = states, aes(x = long, y = lat, group = group), fill = "transparent", color = "white") +
        coord_sf(xlim = c(-100, -68), ylim = c(24, 42), expand = FALSE) +  
        xlab("Longitude") + ylab("Latitude") +
        scale_fill_gradientn(colours = viridis::turbo(99),
                             na.value = "white",
                             breaks = c(0, 0.5, 1),
                             labels = c("Low", 0.5, "High"),
                             limits = c(0, 1)) +
        ggtitle("North American Coastal Plain Suitability") +
        theme(panel.grid.major = element_blank(), 
              panel.grid.minor = element_blank(),
              panel.background = element_rect(fill = NA, color = NA),
              plot.background = element_rect(fill = NA, color = NA), 
              axis.line = element_line(colour = "black")) +
        theme(axis.text = element_text(size = 10), 
              axis.title = element_text(size = 10)) +
        theme(plot.title = element_text(size = 10)) +
        theme(legend.title = element_text(size = 8, face = "bold"),
              legend.text = element_text(size = 6),
              legend.key.size = unit(0.4, "cm"),
              legend.position = "bottom")

      # Only include if all three plots are available
      if (exists("cp_proj")) {
        plot_triplet <- list(future_proj, comp_plot, cp_proj)
      } else {
        plot_triplet <- list(future_proj, comp_plot)
      }
      
      plot_list <- append(plot_list, plot_triplet)
  }
  
  
  # Combine all plots
  all_plots <- c(list(image_plot, occ_plot), response_curves, list(current_proj), plot_list)
  all_plots <- Filter(function(x) inherits(x, "ggplot"), all_plots)
  
  
  ##testing
  # Arrange plots in a 5-row x 6-column grid and save as a full-page PDF
  pdf(paste0("11_SupplementalFigures/", spec, "_SupplementalFigure2.pdf"), height = 20, width = 28)
  arranged_plot <-ggarrange(plotlist = all_plots, 
                            ncol = 6, 
                            nrow = 7, 
                            labels = generate_custom_labels(length(all_plots)))
  grid::grid.draw(arranged_plot)  # This explicitly draws the arranged plot
  dev.off()
  
  print(paste0("Completed: ", species)) ## track progress
}
