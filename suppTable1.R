#Supplemental Table 2
library(tidyverse)

setwd("FL_Scrub/share/")

# Base directories
contrib_dir <- "04_ENM_output"
optseq_dir <- "04_ENM_output"
cor_base_dir <- "06_rasters/SpeciesLayers"

get_species_summary <- function(species) {
  
  spec <- gsub(" ", "_", species)
  
  # --- File paths ---
  contrib_file <- file.path(contrib_dir, paste0(spec, "_variable_importance.txt"))
  opt_file <- file.path(optseq_dir, paste0(spec, "_opt_seq.txt"))
  cor_file <- file.path(cor_base_dir, spec, paste0(spec, "_Pearson_correlations.csv"))
  
  # --- Check if required files exist ---
  if (!file.exists(contrib_file) || !file.exists(cor_file)) return(NULL)
  
  # --- Read and parse contribution file ---
  raw_lines <- read_lines(contrib_file)
  tabbed_lines <- raw_lines[str_detect(raw_lines, "\t")]
  
  split_df <- str_split_fixed(tabbed_lines, "\t", 2) %>%
    as_tibble(.name_repair = "minimal") %>%
    set_names(c("var", "val"))
  
  contrib <- split_df %>%
    filter(str_detect(var, ".contribution")) %>%
    mutate(val = as.numeric(val)) %>%
    filter(!is.na(val) & val > 0) %>%
    arrange(desc(val))
  
  top_vars <- contrib$val[1:min(3, nrow(contrib))]
  names(top_vars) <- contrib$var[1:min(3, nrow(contrib))]
  
  top_vars_raw <- names(top_vars)
  
  # --- Clean variable names for correlation lookup ---
  clean_var <- function(x) {
    x %>%
      str_remove_all('^"|"$') %>%
      str_remove("\\.contribution$") %>%
      str_trim()
  }
  
  top_clean <- clean_var(top_vars_raw)
  
  # --- Read opt_seq file ---
  opt_data <- if (file.exists(opt_file)) {
    read.table(opt_file, header = TRUE, sep = "\t", row.names = 1) %>%
      select(tune.args, auc.val.avg, or.10p.sd, AICc)
  } else {
    tibble(tune.args = NA, auc.val.avg = NA, or.10p.sd = NA, AICc = NA)
  }
  
  # --- Read correlation matrix and set rownames = colnames ---
  cor_df <- read_csv(cor_file, show_col_types = FALSE)
  cor_matrix <- as.matrix(cor_df)
  rownames(cor_matrix) <- colnames(cor_matrix)
  
  # --- Correlation lookup function ---
  get_all_correlated <- function(var, mat, threshold = 0.7) {
    if (!var %in% colnames(mat)) return(NA)
    cor_vals <- mat[var, ]
    cor_vals <- cor_vals[names(cor_vals) != var]
    cor_vals <- cor_vals[abs(cor_vals) >= threshold]
    if (length(cor_vals) == 0) return(NA)
    paste(names(cor_vals), collapse = ", ")
  }
  
  corrs_all <- sapply(top_clean, get_all_correlated, mat = cor_matrix)
  
  # --- Final output row ---
  tibble(
    Species = species,
    Top1 = ifelse(length(top_vars) >= 1, paste0(names(top_vars)[1], " (", round(top_vars[1], 1), "%)"), NA),
    Top2 = ifelse(length(top_vars) >= 2, paste0(names(top_vars)[2], " (", round(top_vars[2], 1), "%)"), NA),
    Top3 = ifelse(length(top_vars) >= 3, paste0(names(top_vars)[3], " (", round(top_vars[3], 1), "%)"), NA),
    Corr_Top1 = ifelse(length(corrs_all) >= 1, corrs_all[1], NA),
    Corr_Top2 = ifelse(length(corrs_all) >= 2, corrs_all[2], NA),
    Corr_Top3 = ifelse(length(corrs_all) >= 3, corrs_all[3], NA),
    tune_args = opt_data$tune.args,
    auc_val_avg = opt_data$auc.val.avg,
    or_10p_sd = opt_data$or.10p.sd,
    AICc = opt_data$AICc
  )
}


alldf <- read.csv("02_FL_Scrub_Clean/FL_Scrub_maxentready_v4.csv")

# Run loop over all species
summary_table_all <- bind_rows(lapply(unique(alldf$accepted_name), get_species_summary))


# View or export
print(summary_table_all)
write_csv(summary_table_all, "SupplementalTable2_full_model_summary_with_cor.csv")
