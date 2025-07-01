# 03_subgroup_analysis_debug.R

# Load libraries
library(tidyverse)

# Define file paths
DELIVERY_DATA_PATH <- "c:/Users/charl/Documents/MM929/Group 1 project details/type_birth_gp1.RDS"
OUTPUT_DIR_PLOTS <- "c:/Users/charl/Documents/MM929/MM929_v2/output/plots"

# Load Data
delivery_data <- readRDS(DELIVERY_DATA_PATH)
cat(paste("Initial rows:", nrow(delivery_data), "\n"))

# --- Seasonal Variation in Delivery Method ---
# Step 1: Categorize
categorized_data <- delivery_data %>%
  mutate(
    delivery_category = case_when(
      grepl("caesarean", sub_category, ignore.case = TRUE) ~ "Caesarean",
      grepl("vaginal", sub_category, ignore.case = TRUE) ~ "Vaginal",
      TRUE ~ NA_character_
    )
  )
cat(paste("Rows after categorization:", nrow(categorized_data), "\n"))

# Step 2: Filter NAs
filtered_data <- categorized_data %>%
  filter(!is.na(delivery_category))
cat(paste("Rows after filtering NAs:", nrow(filtered_data), "\n"))

# Step 3: Summarise
summarised_data <- filtered_data %>%
  group_by(year, quarter, delivery_category) %>%
  summarise(total_births = sum(Livebirths, na.rm = TRUE), .groups = 'drop_last')
cat(paste("Rows after summarising:", nrow(summarised_data), "\n"))

# Step 4: Calculate proportion
proportion_data <- summarised_data %>%
  mutate(proportion = total_births / sum(total_births))
cat(paste("Rows after calculating proportion:", nrow(proportion_data), "\n"))

# Step 5: Filter for Caesarean
caesarean_data <- proportion_data %>%
  filter(delivery_category == "Caesarean") %>%
  ungroup() %>%
  select(year, quarter, caesarean_proportion = proportion)
cat(paste("Final rows for plotting:", nrow(caesarean_data), "\n"))

# Step 6: Create and Save Plot
if(nrow(caesarean_data) > 0) {
    seasonal_plot <- ggplot(caesarean_data, aes(x = factor(quarter), y = caesarean_proportion)) +
      geom_boxplot() +
      labs(title = "Seasonal Variation in Caesarean Delivery Rate")
    seasonal_plot_path <- file.path(OUTPUT_DIR_PLOTS, "subgroup_seasonal_delivery.png")
    ggsave(seasonal_plot_path, plot = seasonal_plot, width = 10, height = 7, dpi = 300)
    cat("Plot saved successfully.\n")
} else {
    cat("No data to plot. Plot not created.\n")
}
