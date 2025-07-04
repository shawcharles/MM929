# ============================================================================
# MM929 Large for Gestational Age (LGA) GLM Analysis
# ============================================================================
# Purpose: GLM analysis for LGA research questions:
# 1. Trend analysis of LGA over 25 years.
# 2. Analysis of socioeconomic inequality in LGA births.
# 3. Analysis of LGA variation by maternal age group.
# ============================================================================

# Load configuration and initialize environment
source("scripts/00_config.R")
initialize_analysis()

cat("MM929 LGA GLM Analysis\n")
cat("========================\n\n")

# Load prepared dataset
birthweight_analysis <- read_csv(file.path(output_dir, "birthweight_analysis_data.csv"), show_col_types = FALSE)

cat(sprintf("Data loaded:\n"))
cat(sprintf("-> Birthweight data: %d observations (%d-%d)\n\n", 
    nrow(birthweight_analysis), min(birthweight_analysis$year), max(birthweight_analysis$year)))

# ============================================================================
# PART 1: LGA ANALYSIS
# ============================================================================

cat("\nPART 1: LGA ANALYSIS\n")
cat("======================\n\n")

# Ensure proper factor levels for birthweight analysis
birthweight_analysis <- birthweight_analysis %>%
  mutate(
    simd_quintile = factor(simd_quintile),
    simd_quintile = relevel(simd_quintile, ref = as.character(REFERENCE_SIMD)),
    age_group = factor(age_group),
    age_group = relevel(age_group, ref = REFERENCE_AGE)
  )

# 1.1 Model Specification and Selection for LGA
cat("1.1 LGA Model Specification\n")
cat("---------------------------\n")

# Define candidate models for Large-for-GA analysis
lga_models_list <- list(
  # Model 1: Main effects only
  "main_effects" = formula(cbind(large, appropriate + small) ~ time + simd_quintile + age_group),
  
  # Model 2: Time-SIMD interaction (primary interest)
  "time_simd_interaction" = formula(cbind(large, appropriate + small) ~ time * simd_quintile + age_group),
  
  # Model 3: Time-Age interaction
  "time_age_interaction" = formula(cbind(large, appropriate + small) ~ time + simd_quintile * age_group),
  
  # Model 4: Full two-way interactions
  "full_interactions" = formula(cbind(large, appropriate + small) ~ time * simd_quintile + time * age_group)
)

# Fit all candidate models
lga_fitted_models <- list()
lga_model_summaries <- list()

cat("Fitting LGA GLM models...\n")

for (model_name in names(lga_models_list)) {
  cat("   - Fitting", model_name, "model...\n")
  
  lga_fitted_models[[model_name]] <- glm(
    formula = lga_models_list[[model_name]],
    family = quasibinomial(link = "logit"),
    data = birthweight_analysis
  )
  
  lga_model_summaries[[model_name]] <- summary(lga_fitted_models[[model_name]])
}

# 1.2 Model Comparison and Selection
cat("\n1.2 LGA Model Comparison\n")
cat("--------------------------\n")

lga_model_comparison <- tibble(
  model = names(lga_fitted_models),
  formula = sapply(lga_models_list, function(x) paste(deparse(x), collapse = " ")),
  deviance = sapply(lga_fitted_models, deviance),
  df_residual = sapply(lga_fitted_models, df.residual),
  dispersion = sapply(lga_model_summaries, function(x) x$dispersion),
  quasi_aic = sapply(lga_fitted_models, function(x) x$deviance + 2 * (length(coef(x)))),
  n_parameters = sapply(lga_fitted_models, function(x) length(coef(x)))
) %>%
  mutate(
    deviance_per_df = deviance / df_residual,
    delta_quasi_aic = quasi_aic - min(quasi_aic)
  ) %>%
  arrange(quasi_aic)

cat("LGA Model Comparison:\n")
print(lga_model_comparison, digits = 3)

# Select primary LGA model
lga_primary_model_name <- lga_model_comparison$model[1]
lga_primary_model <- lga_fitted_models[[lga_primary_model_name]]

cat("\nPrimary LGA model selected:", lga_primary_model_name, "\n")
cat("Formula:", paste(deparse(lga_models_list[[lga_primary_model_name]]), collapse = " "), "\n\n")

# 1.3 Primary LGA Model Analysis
cat("1.3 Primary LGA Model Analysis\n")
cat("------------------------------\n")

cat("Primary LGA Model Summary:\n")
print(summary(lga_primary_model))

# Extract coefficients
lga_coef <- broom::tidy(lga_primary_model) %>%
  mutate(
    odds_ratio = exp(estimate),
    or_lower = exp(estimate - 1.96 * std.error),
    or_upper = exp(estimate + 1.96 * std.error),
    model = "LGA GLM"
  )

cat("\nLGA Coefficients:\n")
print(lga_coef, digits = 4)

# ============================================================================
# PART 2: INEQUALITY ANALYSIS FOR LGA
# ============================================================================

cat("\nPART 2: INEQUALITY ANALYSIS FOR LGA\n")
cat("===================================\n\n")

# 2.1 Socioeconomic Inequality in LGA
cat("2.1 Socioeconomic Inequality in LGA\n")
cat("-----------------------------------\n")

# Calculate predicted probabilities by SIMD quintile over time
lga_inequality_scenarios <- expand_grid(
  time = seq(0, max(birthweight_analysis$time), by = 5),
  simd_quintile = factor(1:5),
  age_group = factor(REFERENCE_AGE)
) %>%
  mutate(
    simd_quintile = relevel(simd_quintile, ref = as.character(REFERENCE_SIMD)),
    year = time + min(birthweight_analysis$year)
  )

# Generate predictions
lga_inequality_pred <- predict(lga_primary_model, newdata = lga_inequality_scenarios, 
                              type = "response", se.fit = TRUE)

lga_inequality_results <- lga_inequality_scenarios %>%
  mutate(
    predicted_prob = lga_inequality_pred$fit,
    se_pred = lga_inequality_pred$se.fit,
    lower_ci = plogis(qlogis(predicted_prob) - 1.96 * se_pred),
    upper_ci = plogis(qlogis(predicted_prob) + 1.96 * se_pred)
  )

# Calculate rate ratios (most deprived vs least deprived)
lga_inequality_analysis <- lga_inequality_results %>%
  select(year, simd_quintile, predicted_prob, lower_ci, upper_ci) %>%
  pivot_wider(
    names_from = simd_quintile,
    values_from = c(predicted_prob, lower_ci, upper_ci),
    names_sep = "_q"
  ) %>%
  mutate(
    rate_ratio = predicted_prob_q1 / predicted_prob_q5,
    rr_lower = lower_ci_q1 / upper_ci_q5,
    rr_upper = upper_ci_q1 / lower_ci_q5,
    absolute_difference = predicted_prob_q1 - predicted_prob_q5
  ) %>%
  select(year, rate_ratio, rr_lower, rr_upper, absolute_difference)

cat("LGA Inequality Analysis Results (Rate Ratios - SIMD 1 vs SIMD 5):\n")
print(lga_inequality_analysis, digits = 3)

# ============================================================================
# PART 3: VISUALIZATION FOR LGA
# ============================================================================

cat("\nPART 3: VISUALIZATION FOR LGA\n")
cat("=============================\n\n")

# 3.1 LGA Trends by SIMD Quintile
cat("3.1 Creating LGA Trends Plot\n")
cat("----------------------------\n")

# Create LGA trends plot
lga_trends_data <- birthweight_analysis %>%
  group_by(year, simd_quintile) %>%
  summarise(
    prop_large = weighted.mean(prop_large, total_births),
    total_births = sum(total_births),
    .groups = "drop"
  )

p_lga_birthweight <- ggplot(lga_trends_data, aes(x = year, y = prop_large, color = simd_quintile)) +
  geom_line(linewidth = 1.2) +
  geom_point(size = 2.5) +
  scale_color_viridis_d(name = "SIMD Quintile", 
                        labels = c("1 (Most Deprived)", "2", "3", "4", "5 (Least Deprived)")) +
  scale_y_continuous(labels = scales::percent_format()) +
  labs(
    title = "Large for Gestational Age Trends by Socioeconomic Deprivation",
    subtitle = "25-year trends (1997-2023) by SIMD quintile",
    x = "Year",
    y = "Proportion Large for Gestational Age",
    caption = "SIMD: Scottish Index of Multiple Deprivation"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 14, face = "bold"),
    plot.subtitle = element_text(size = 12),
    axis.title = element_text(size = 11),
    legend.position = "bottom"
  )

ggsave("lga_trends_simd.png", plot = p_lga_birthweight, path = plots_dir, 
       width = 12, height = 8, dpi = 300)
cat("-> LGA trends plot saved\n")

# 3.2 LGA Inequality Trends Plot
cat("\n3.2 Creating LGA Inequality Trends Plot\n")
cat("---------------------------------------\n")

p_lga_inequality <- ggplot(lga_inequality_analysis, aes(x = year, y = rate_ratio)) +
  geom_line(linewidth = 1.2, color = "#d73027") +
  geom_point(size = 3, color = "#d73027") +
  geom_ribbon(aes(ymin = rr_lower, ymax = rr_upper), alpha = 0.3, fill = "#d73027") +
  geom_hline(yintercept = 1, linetype = "dashed", color = "black") +
  scale_y_continuous(limits = c(0.7, 1.1), breaks = seq(0.7, 1.1, 0.1)) +
  labs(
    title = "Socioeconomic Inequality in Large for Gestational Age Births",
    subtitle = "Rate Ratio: Most Deprived (SIMD 1) vs Least Deprived (SIMD 5)",
    x = "Year",
    y = "Rate Ratio",
    caption = "Shaded area represents 95% confidence interval\nDashed line indicates equality (RR = 1)"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 14, face = "bold"),
    plot.subtitle = element_text(size = 12),
    axis.title = element_text(size = 11)
  )

ggsave("lga_inequality_trends_rate_ratio.png", plot = p_lga_inequality, path = plots_dir, 
       width = 12, height = 8, dpi = 300)
cat("-> LGA inequality trends plot saved\n")

# ============================================================================
# PART 4: SUBGROUP ANALYSIS FOR LGA
# ============================================================================
cat("\nPART 4: SUBGROUP ANALYSIS FOR LGA\n")
cat("=================================\n\n")

maternal_age_lga_analysis <- birthweight_analysis %>% 
  filter(age_group != "Unknown") %>%
  group_by(year, age_group) %>%
  summarise(
    lga_births = sum(large, na.rm = TRUE),
    total_births = sum(total_births, na.rm = TRUE),
    .groups = 'drop'
  ) %>%
  mutate(lga_proportion = lga_births / total_births) %>%
  select(year, age_group, lga_proportion)

age_output_path_lga <- file.path(output_dir, "subgroup_maternal_age_lga.csv")
write_csv(maternal_age_lga_analysis, age_output_path_lga)
cat(paste("LGA maternal age analysis results saved to:", age_output_path_lga, "\n"))

maternal_age_plot_lga <- ggplot(maternal_age_lga_analysis, aes(x = year, y = lga_proportion, color = age_group, group = age_group)) +
  geom_line(linewidth = 1.2) +
  geom_point(size = 2) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  labs(
    title = "Trends in Large-for-Gestational-Age (LGA) Births by Maternal Age",
    subtitle = "Proportion of LGA births from 1997-2022",
    x = "Year",
    y = "Proportion of Births that are LGA",
    color = "Maternal Age Group"
  ) +
  theme_minimal(base_size = 14) +
  theme(legend.position = "bottom", plot.title.position = "plot")

age_plot_path_lga <- file.path(plots_dir, "lga_maternal_age_trends.png")
ggsave(age_plot_path_lga, plot = maternal_age_plot_lga, width = 10, height = 7, dpi = 300)
cat(paste("LGA maternal age trend plot saved to:", age_plot_path_lga, "\n"))


# ============================================================================
# PART 5: SAVE RESULTS
# ============================================================================

cat("\nPART 5: SAVE RESULTS\n")
cat("====================\n\n")

# Save model objects
saveRDS(lga_primary_model, file.path(output_dir, "lga_glm_model.rds"))
saveRDS(lga_fitted_models, file.path(output_dir, "all_lga_glm_models.rds"))

# Save analysis results
write_csv(lga_model_comparison, file.path(output_dir, "lga_model_comparison.csv"))
write_csv(lga_coef, file.path(output_dir, "lga_glm_coefficients.csv"))
write_csv(lga_inequality_analysis, file.path(output_dir, "lga_inequality_analysis.csv"))

cat("LGA GLM Analysis completed successfully!\n")
cat("======================================\n")
cat("Key outputs saved to:", output_dir, "\n")
cat("- LGA analysis by SIMD quintile\n")
cat("- LGA inequality analysis and trends\n")
cat("- LGA by maternal age subgroup analysis\n")
cat("- Model diagnostics and coefficients\n")
cat("- Comprehensive visualizations for LGA\n\n")

cat("Next step: Integrate these findings into the main report.\n")

# Clean workspace
rm(lga_fitted_models, lga_model_summaries)
gc()
