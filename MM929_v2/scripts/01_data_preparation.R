# ============================================================================
# MM929 Data Preparation Script - WORKING VERSION
# ============================================================================
# Purpose: Load, clean, and prepare delivery type and birthweight-for-gestational-age data
# Research Questions:
# 1. Delivery type trends (caesarean vs vaginal) since 2017
# 2. Birthweight for gestational age categories over 25 years
# ============================================================================

# Load configuration and initialize environment
source("scripts/00_config.R")
initialize_analysis()

cat("MM929 Data Preparation - Working Version\n")
cat("========================================\n\n")

# 1. Load Raw Data
# ================
cat("1. Loading Raw Data...\n")

delivery_data_raw <- read_csv(DELIVERY_TYPE_FILE, show_col_types = FALSE)
cat(sprintf("-> Delivery data loaded: %d rows, %d columns\n", nrow(delivery_data_raw), ncol(delivery_data_raw)))

birthweight_data_raw <- read_csv(BIRTHWEIGHT_FILE, show_col_types = FALSE)
cat(sprintf("-> Birthweight data loaded: %d rows, %d columns\n\n", nrow(birthweight_data_raw), ncol(birthweight_data_raw)))

# 2. Prepare Delivery Type Data (2017-Present)
# ============================================
delivery_analysis <- delivery_data_raw %>%
  janitor::clean_names() %>%
  # Create a unified delivery group (Vaginal vs Caesarean)
  mutate(
    delivery_group = case_when(
      sub_category %in% c("spontaneous vaginal births", "assisted vaginal births") ~ "Vaginal",
      sub_category %in% c("planned caesarean births", "unplanned caesarean births") ~ "Caesarean",
      TRUE ~ NA_character_
    ),
    year = lubridate::year(date)
  ) %>%
  # Remove any data that doesn't fit the two main categories
  filter(!is.na(delivery_group)) %>%
  # Aggregate from quarterly to yearly counts
  group_by(year, delivery_group) %>%
  summarise(births = sum(numerator, na.rm = TRUE), .groups = "drop_last") %>%
  mutate(total_births = sum(births), proportion = births / total_births) %>%
  ungroup() %>%
  # Pivot to wide format for analysis
  pivot_wider(
    id_cols = c(year, total_births),
    names_from = delivery_group,
    values_from = c(births, proportion)
  ) %>%
  # Clean column names
  rename(
    vaginal_births = births_Vaginal,
    caesarean_births = births_Caesarean,
    prop_vaginal = proportion_Vaginal,
    prop_caesarean = proportion_Caesarean
  ) %>%
  # Add time variable for modeling
  mutate(time = year - min(year))

cat(sprintf("-> Delivery analysis data prepared: %d rows, %d columns\n", nrow(delivery_analysis), ncol(delivery_analysis)))

# Data quality check for delivery data
delivery_prop_check <- delivery_analysis %>%
  mutate(total_prop = prop_vaginal + prop_caesarean) %>%
  summarise(
    min_total = min(total_prop, na.rm = TRUE),
    max_total = max(total_prop, na.rm = TRUE),
    mean_total = mean(total_prop, na.rm = TRUE)
  )

cat(sprintf("-> Quality check - proportions sum to: %.3f to %.3f (mean: %.3f)\n", 
            delivery_prop_check$min_total, delivery_prop_check$max_total, delivery_prop_check$mean_total))

# 3. Prepare Birthweight for Gestational Age Data (25 years)
# ==========================================================
cat("3. Preparing Birthweight for Gestational Age Data...\n")

# Clean and prepare birthweight data
birthweight_analysis <- birthweight_data_raw %>%
  janitor::clean_names() %>%
  # Filter for valid categories and data
  filter(
    birthweight_for_gestational_age %in% c("Small", "Appropriate", "Large"),
    !is.na(simd_quintile),
    !is.na(livebirths),
    livebirths >= 0
  ) %>%
  # Set up factors with reference levels
  mutate(
    simd_quintile = factor(simd_quintile),
    simd_quintile = relevel(simd_quintile, ref = as.character(REFERENCE_SIMD)),
    age_group = factor(age_group),
    age_group = relevel(age_group, ref = REFERENCE_AGE),
    year = as.numeric(year),
    time = year - min(year)
  ) %>%
  # Pivot to wide format for proportions
  pivot_wider(
    names_from = birthweight_for_gestational_age,
    values_from = livebirths,
    values_fill = 0
  ) %>%
  janitor::clean_names() %>%
  # Calculate proportions and modeling variables
  mutate(
    total_births = small + appropriate + large,
    prop_small = small / total_births,
    prop_appropriate = appropriate / total_births,
    prop_large = large / total_births,
    # For binomial modeling (small vs not-small)
    success_small = small,
    failure_small = appropriate + large
  )

cat(sprintf("-> Birthweight analysis data prepared: %d rows, %d columns\n", nrow(birthweight_analysis), ncol(birthweight_analysis)))

# Data quality check for birthweight data
birthweight_prop_check <- birthweight_analysis %>%
  mutate(total_prop = prop_small + prop_appropriate + prop_large) %>%
  summarise(
    min_total = min(total_prop, na.rm = TRUE),
    max_total = max(total_prop, na.rm = TRUE),
    mean_total = mean(total_prop, na.rm = TRUE)
  )

cat(sprintf("-> Quality check - proportions sum to: %.3f to %.3f (mean: %.3f)\n", 
            birthweight_prop_check$min_total, birthweight_prop_check$max_total, birthweight_prop_check$mean_total))

# 4. Save Prepared Datasets
# =========================
cat("4. Saving Prepared Datasets...\n")

write_csv(delivery_analysis, file.path(output_dir, "delivery_analysis_data.csv"))
cat("-> Saved: delivery_analysis_data.csv\n")

write_csv(birthweight_analysis, file.path(output_dir, "birthweight_analysis_data.csv"))
cat("-> Saved: birthweight_analysis_data.csv\n")

# 5. Generate Summary Statistics
# ==============================
cat("5. Generating Summary Statistics...\n")

# Delivery summary statistics
delivery_summary <- delivery_analysis %>%
  summarise(
    years_covered = paste(min(year), "to", max(year)),
    total_years = n(),
    mean_prop_vaginal = weighted.mean(prop_vaginal, total_births, na.rm = TRUE),
    mean_prop_caesarean = weighted.mean(prop_caesarean, total_births, na.rm = TRUE),
    trend_vaginal = cor(year, prop_vaginal, use = "complete.obs"),
    trend_caesarean = cor(year, prop_caesarean, use = "complete.obs")
  )
write_csv(delivery_summary, file.path(output_dir, "summary_delivery_overall.csv"))
cat("-> Saved: summary_delivery_overall.csv\n")

# Birthweight summary statistics
birthweight_summary <- birthweight_analysis %>%
  group_by(year) %>%
  summarise(
    yearly_total_births = sum(total_births, na.rm = TRUE),
    yearly_prop_small = weighted.mean(prop_small, total_births, na.rm = TRUE),
    yearly_prop_appropriate = weighted.mean(prop_appropriate, total_births, na.rm = TRUE),
    yearly_prop_large = weighted.mean(prop_large, total_births, na.rm = TRUE)
  ) %>%
  ungroup() %>%
  summarise(
    years_covered = paste(min(year), "to", max(year)),
    total_observations = nrow(birthweight_analysis),
    total_births = sum(yearly_total_births, na.rm = TRUE),
    mean_prop_small = weighted.mean(yearly_prop_small, yearly_total_births, na.rm = TRUE),
    mean_prop_appropriate = weighted.mean(yearly_prop_appropriate, yearly_total_births, na.rm = TRUE),
    mean_prop_large = weighted.mean(yearly_prop_large, yearly_total_births, na.rm = TRUE)
  )
write_csv(birthweight_summary, file.path(output_dir, "summary_birthweight_overall.csv"))
cat("-> Saved: summary_birthweight_overall.csv\n")

# 6. Create Descriptive Plots
# ===========================
cat("6. Creating Descriptive Plots...\n")

# Plot 1: Vaginal vs Caesarean Delivery Trends
p_delivery <- ggplot(delivery_analysis, aes(x = year)) +
  geom_line(aes(y = prop_vaginal, color = "Vaginal"), linewidth = 1.2) +
  geom_point(aes(y = prop_vaginal, color = "Vaginal"), size = 2.5) +
  geom_line(aes(y = prop_caesarean, color = "Caesarean"), linewidth = 1.2, linetype = "dashed") +
  geom_point(aes(y = prop_caesarean, color = "Caesarean"), size = 2.5) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  scale_color_manual(values = c("Vaginal" = "#0072B2", "Caesarean" = "#D55E00")) +
  labs(
    title = "Delivery Type Trends: Vaginal vs Caesarean Births (2017-Present)",
    subtitle = "Proportion of total births by delivery type",
    x = "Year",
    y = "Proportion of Births",
    color = "Delivery Type"
  ) +
  theme_minimal() +
  theme(legend.position = "bottom")

ggsave("delivery_trends_plot.png", plot = p_delivery, path = output_dir, width = 12, height = 8, dpi = 300)
cat("-> Saved: delivery_trends_plot.png\n")

# Plot 2: Small for Gestational Age by SIMD Quintile
p_birthweight <- birthweight_analysis %>%
  group_by(year, simd_quintile) %>%
  summarise(
    total_births = sum(total_births),
    prop_small = sum(prop_small * total_births) / sum(total_births),
    .groups = "drop"
  ) %>%
  ggplot(aes(x = year, y = prop_small, color = simd_quintile, group = simd_quintile)) +
  geom_line(linewidth = 1.2) +
  geom_point(size = 2.5, alpha = 0.8) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 0.1)) +
  scale_color_viridis_d(name = "SIMD Quintile") +
  labs(
    title = "Small for Gestational Age Births by Deprivation (25-Year Trends)",
    subtitle = "Proportion of births classified as small for gestational age",
    x = "Year",
    y = "Proportion Small for GA"
  ) +
  theme_minimal() +
  theme(legend.position = "bottom")

ggsave("birthweight_trends_plot.png", plot = p_birthweight, path = plots_dir, width = 12, height = 8, dpi = 300)
cat("-> Saved: birthweight_trends_plot.png\n")

# 7. Final Summary
# ===============
cat("\n7. Data Preparation Summary\n")
cat("===========================\n")
cat(sprintf("Delivery data: %d years (%s)\n", nrow(delivery_analysis), 
            paste(range(delivery_analysis$year), collapse = " to ")))
cat(sprintf("Birthweight data: %d observations across %d years (%s)\n", 
            nrow(birthweight_analysis), 
            length(unique(birthweight_analysis$year)),
            paste(range(birthweight_analysis$year), collapse = " to ")))
cat("All datasets, summaries, and plots saved successfully!\n")
cat("Data preparation completed successfully!\n")
