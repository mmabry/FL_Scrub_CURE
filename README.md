# Florida Scrub Course-Based Undergraduate Research Experience (CURE)

Scripts for the Florida Scrub CURE (Spring 2022, University of Florida): an pipeline for ecological niche modeling of Florida scrub species.

## Contents Overview

A suite of R and shell scripts, organized sequentially to support the complete workflow from raw data acquisition and cleaning through modeling and visualization:

- 01_Download_Records_FL_Scrub.R
- 02_FL_Scrub_taxonomic_names.R
- 03_FL_Scrub_Record_cleaning.R
- 04_FL_Scrub_SpeciesMaps.R
- 05A_FL_Scrub_VIF.R & 05A_FL_Scrub_VIF.sh
- 05B_FL_Scrub_Pearsons.R & 05B_FL_Scrub_Pearsons.sh
- 05_FL_Scrub_SpeciesEnvironmentalVariables.R & its shell counterpart
- 06_FL_Scrub_Ecological_Niche_Modeling.R & shell script
- 07_FL_Scrub_Projections.R
- 08_Future_FL_Scrub_Projections.R & shell script
- 09_FL_Scrub_ProjectionComparisons.R & shell script
- 10_FL_Scrub_SpeciesRichness.R
- 11_CP_Scrub_Projections.R & shell script
- 12_Future_CP_Projections.R & shell script
- Fig2_PredictedChangeHeatmap.R, Fig3_Loss_Gain.R, Fig4_SpeciesRichnessSeaLevel_Mang.R
- Supplementary figures and tables: suppFigs.R, suppFigs_Plotting.R, suppTable1.R (+ shell)

## Purpose

This repository provides a structured, reproducible pipeline for:

- Downloading occurrence records for Florida scrub flora,
- Standardizing taxonomic names,
- Cleaning and filtering georeferencing errors or duplicates,
- Mapping species distributions,
- Calculating environmental variables (including VIF and Pearson correlation analyses to guide variable selection),
- Modeling current and future species distributions via ecological niche models,
- Projecting changes under future climates,
- Mapping species richness and generating key figures for interpretation.

This project was developed as part of a Course-Based Undergraduate Research Experience (CURE) at the University of Florida.

## Prerequisites

- R (≥ 4.x recommended)
- Required R packages, such as (but not limited to): dplyr, tidyr, sf, raster, sp, usdm, biomod2, ggplot2.
- Shell environment for Unix/Linux/macOS to run .sh scripts (Bash-compatible)

## Output

This pipeline produces:

- Cleaned occurrence datasets,
- Species distribution maps,
- Environmental variable correlation diagnostics,
- Ecological niche model outputs (current & future),
- Comparative projections (e.g., differences between current and future ranges),
- Species richness layers,
- Figures for publication-ready interpretation,
- Supplementary materials.

## Organization & Naming Conventions

- Numerical prefixes (e.g., 01_, 02_) ensure execution in intended sequence.
- Shell scripts (.sh) provide a convenient wrapper for running R scripts while tracking execution steps.
- R scripts are modular, each performing a distinct analytical task (e.g., data download, cleaning, mapping, modeling).
- FigX_*.R scripts generate final visual outputs; supplementary scripts (supp*) produce ancillary figures and tables.

## Contributions & Acknowledgments

Developed for the Spring 2022 Florida Scrub CURE at the University of Florida. Contributions are welcome following academic use and appropriate citation.

## Contact

For questions, suggestions, or clarifications, please contact Makenzie via GitHub or through institutional channels.
