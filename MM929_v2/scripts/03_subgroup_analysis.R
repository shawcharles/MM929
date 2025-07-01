# 03_subgroup_analysis.R
#
# Description:
# This script performs subgroup analyses to enhance the main report.
# It investigates:
# 1. Maternal age patterns in small-for-gestational-age (SGA) births.
# 2. Seasonal variations in delivery methods (caesarean vs. vaginal).
#
# The script generates summary tables (CSV) and plots (PNG) for each analysis.

# --- 1. Setup and Configuration ---
# Load libraries and configuration
if (!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if (!require(lubridate)) install.packages("lubridate", repos = "http://cran.us.r-project.org")
library(tidyverse)
library(lubridate)

# Define file paths
BIRTHWEIGHT_DATA_PATH <- "c:/Users/charl/Documents/MM929/Group 1 project details/birthweight_gp1.RDS"
DELIVERY_DATA_PATH <- "c:/Users/charl/Documents/MM929/Group 1 project details/type_birth_gp1.RDS"
OUTPUT_DIR_DATA <- "c:/Users/charl/Documents/MM929/MM929_v2/output/data_preparation"
OUTPUT_DIR_PLOTS <- "c:/Users/charl/Documents/MM929/MM929_v2/output/plots"

# Create output directories if they don't exist
dir.create(OUTPUT_DIR_DATA, showWarnings = FALSE, recursive = TRUE)
dir.create(OUTPUT_DIR_PLOTS, showWarnings = FALSE, recursive = TRUE)

# --- 2. Load Data ---
birthweight_data <- readRDS(BIRTHWEIGHT_DATA_PATH)
delivery_data <- readRDS(DELIVERY_DATA_PATH)

# --- 3. Subgroup Analysis 1: Maternal Age and SGA Births ---
cat("Running Subgroup Analysis: Maternal Age and SGA Births...\n")

maternal_age_sga_analysis <- birthweight_data %>% 
  filter(AgeGroup != "Unknown") %>%
  filter(BirthweightForGestationalAge %in% c("Small", "Appropriate", "Large")) %>%
  group_by(year, AgeGroup) %>%
  summarise(
    sga_births = sum(Livebirths[BirthweightForGestationalAge == 'Small'], na.rm = TRUE),
    total_births = sum(Livebirths, na.rm = TRUE),
    .groups = 'drop'
  ) %>%
  mutate(sga_proportion = sga_births / total_births) %>%
  select(year, AgeGroup, sga_proportion)

age_output_path <- file.path(OUTPUT_DIR_DATA, "subgroup_maternal_age_sga.csv")
write_csv(maternal_age_sga_analysis, age_output_path)
cat(paste("Maternal age analysis results saved to:", age_output_path, "\n"))

maternal_age_plot <- ggplot(maternal_age_sga_analysis, aes(x = year, y = sga_proportion, color = AgeGroup, group = AgeGroup)) +
  geom_line(linewidth = 1.2) +
  geom_point(size = 2) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  labs(
    title = "Trends in Small-for-Gestational-Age (SGA) Births by Maternal Age",
    subtitle = "Proportion of SGA births from 1997-2022",
    x = "Year",
    y = "Proportion of Births that are SGA",
    color = "Maternal Age Group"
  ) +
  theme_minimal(base_size = 14) +
  theme(legend.position = "bottom", plot.title.position = "plot")

age_plot_path <- file.path(OUTPUT_DIR_PLOTS, "subgroup_maternal_age_trends.png")
ggsave(age_plot_path, plot = maternal_age_plot, width = 10, height = 7, dpi = 300)
cat(paste("Maternal age trend plot saved to:", age_plot_path, "\n"))


# --- 4. Subgroup Analysis 2: Seasonal Variation in Delivery Method ---
cat("\nRunning Subgroup Analysis: Seasonal Variation in Delivery Method...\n")

seasonal_delivery_analysis <- delivery_data %>%
  mutate(
    date = as.Date(date),
    year = year(date),
    quarter = quarter(date)
  ) %>%
  mutate(
    delivery_category = case_when(
      grepl("caesarean", sub_category, ignore.case = TRUE) ~ "Caesarean",
      grepl("vaginal", sub_category, ignore.case = TRUE) ~ "Vaginal",
      TRUE ~ NA_character_
    )
  ) %>%
  filter(!is.na(delivery_category)) %>%
  group_by(year, quarter, delivery_category) %>%
  summarise(total_births = sum(numerator, na.rm = TRUE), .groups = 'drop_last') %>%
  mutate(proportion = total_births / sum(total_births)) %>%
  filter(delivery_category == "Caesarean") %>%
  ungroup() %>%
  select(year, quarter, caesarean_proportion = proportion)

seasonal_output_path <- file.path(OUTPUT_DIR_DATA, "subgroup_seasonal_delivery.csv")
write_csv(seasonal_delivery_analysis, seasonal_output_path)
cat(paste("Seasonal delivery analysis results saved to:", seasonal_output_path, "\n"))

seasonal_plot <- ggplot(seasonal_delivery_analysis, aes(x = factor(quarter), y = caesarean_proportion)) +
  geom_boxplot(fill = "skyblue", alpha = 0.7) +
  geom_jitter(width = 0.1, alpha = 0.5) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  labs(
    title = "Seasonal Variation in Caesarean Delivery Rate",
    subtitle = "Data from 2017-2024, aggregated across years",
    x = "Quarter",
    y = "Proportion of Deliveries by Caesarean Section"
  ) +
  theme_minimal(base_size = 14) +
  theme(plot.title.position = "plot")

seasonal_plot_path <- file.path(OUTPUT_DIR_PLOTS, "subgroup_seasonal_delivery.png")
ggsave(seasonal_plot_path, plot = seasonal_plot, width = 10, height = 7, dpi = 300)
cat(paste("Seasonal delivery trend plot saved to:", seasonal_plot_path, "\n"))

cat("\nSubgroup analyses complete.\n")
