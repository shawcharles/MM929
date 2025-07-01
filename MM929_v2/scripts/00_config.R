# MM929 Analysis v2.0 - Configuration File
# ========================================
# This file contains all project-wide settings, file paths, 
# reference levels, and helper functions.

# 1. Load Core Libraries
# ======================
library(tidyverse)
library(janitor)
library(lubridate)
library(scales)
library(viridis)

# 2. Define File Paths
# ====================
# NOTE: Paths are relative to the project root directory (MM929_v2)
DELIVERY_TYPE_FILE <- "../Group 1 project details/converted_data/type_birth_gp1.csv"
BIRTHWEIGHT_FILE <- "../Group 1 project details/converted_data/birthweight_gp1.csv"

# 3. Define Analysis Parameters
# =============================
# Reference category for SIMD quintiles in models
REFERENCE_SIMD <- 3 

# Reference category for maternal age groups in models
REFERENCE_AGE <- "25-34"

# 4. Define Output Directories
# ============================
output_dir <- "output/data_preparation"
plots_dir <- "output/plots"

# 5. Helper Functions
# ===================
# Function to initialize the analysis environment
initialize_analysis <- function() {
  cat("Initializing analysis environment...\n")
  # Create the output directory if it doesn't exist
  if (!dir.exists(output_dir)) {
    dir.create(output_dir, recursive = TRUE)
    cat(sprintf("-> Created output directory: %s\n", output_dir))
  } else {
    cat(sprintf("-> Output directory already exists: %s\n", output_dir))
  }
  # Create the plots directory if it doesn't exist
  if (!dir.exists(plots_dir)) {
    dir.create(plots_dir, recursive = TRUE)
    cat(sprintf("-> Created plots directory: %s\n", plots_dir))
  } else {
    cat(sprintf("-> Plots directory already exists: %s\n", plots_dir))
  }
  cat("Initialization complete.\n\n")
}
