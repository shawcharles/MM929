# ============================================================================
# MM929 Primary GLM Analysis - CORRECTED VERSION
# ============================================================================
# Purpose: GLM analysis for correct MM929 research questions:
# 1. Delivery type trends (caesarean vs vaginal) since 2017
# 2. Forecasting vaginal delivery proportions to 2030
# 3. Birthweight for gestational age analysis over 25 years
# ============================================================================

# Load configuration and initialize environment
source("scripts/00_config.R")
initialize_analysis()

cat("MM929 Primary GLM Analysis - Corrected Version\n")
cat("===============================================\n\n")

# Load prepared datasets
delivery_analysis <- read_csv(file.path(output_dir, "delivery_analysis_data.csv"), show_col_types = FALSE)
birthweight_analysis <- read_csv(file.path(output_dir, "birthweight_analysis_data.csv"), show_col_types = FALSE)

cat(sprintf("Data loaded:\n"))
cat(sprintf("-> Delivery data: %d observations (%d-%d)\n", 
    nrow(delivery_analysis), min(delivery_analysis$year), max(delivery_analysis$year)))
cat(sprintf("-> Birthweight data: %d observations (%d-%d)\n\n", 
    nrow(birthweight_analysis), min(birthweight_analysis$year), max(birthweight_analysis$year)))

# ============================================================================
# PART 1: DELIVERY TYPE ANALYSIS (Research Questions 1 & 2)
# ============================================================================

cat("PART 1: DELIVERY TYPE ANALYSIS\n")
cat("==============================\n\n")

# 1.1 Delivery Type Trend Analysis
cat("1.1 Delivery Type Trend Analysis\n")
cat("---------------------------------\n")

# Fit GLM for vaginal delivery proportions over time
delivery_glm <- glm(
  cbind(vaginal_births, caesarean_births) ~ time + I(time^2),
  family = quasibinomial(link = "logit"),
  data = delivery_analysis
)

cat("Delivery Type GLM Summary:\n")
print(summary(delivery_glm))

# Extract coefficients with confidence intervals
delivery_coef <- broom::tidy(delivery_glm) %>%
  mutate(
    odds_ratio = exp(estimate),
    or_lower = exp(estimate - 1.96 * std.error),
    or_upper = exp(estimate + 1.96 * std.error),
    model = "Delivery Type GLM"
  )

cat("\nDelivery Type Coefficients:\n")
print(delivery_coef, digits = 4)

# 1.2 Forecasting to 2030
cat("\n1.2 Forecasting Vaginal Delivery Proportions to 2030\n")
cat("-----------------------------------------------------\n")

# Create forecast data
forecast_years <- 2017:2030
forecast_data <- tibble(
  year = forecast_years,
  time = year - min(delivery_analysis$year)
)

# Generate predictions with confidence intervals
forecast_pred <- predict(delivery_glm, newdata = forecast_data, 
                        type = "response", se.fit = TRUE)

forecast_results <- forecast_data %>%
  mutate(
    predicted_prop_vaginal = forecast_pred$fit,
    se_pred = forecast_pred$se.fit,
    lower_ci = plogis(qlogis(predicted_prop_vaginal) - 1.96 * se_pred),
    upper_ci = plogis(qlogis(predicted_prop_vaginal) + 1.96 * se_pred),
    is_forecast = year > max(delivery_analysis$year)
  )

cat("Forecast Results for 2030:\n")
forecast_2030 <- forecast_results %>% filter(year == 2030)
cat(sprintf("Predicted vaginal delivery proportion: %.3f (95%% CI: %.3f-%.3f)\n",
    forecast_2030$predicted_prop_vaginal, forecast_2030$lower_ci, forecast_2030$upper_ci))

# Save delivery analysis results
write_csv(delivery_coef, file.path(output_dir, "delivery_glm_coefficients.csv"))
write_csv(forecast_results, file.path(output_dir, "delivery_forecast_2030.csv"))

# ============================================================================
# PART 2: BIRTHWEIGHT FOR GESTATIONAL AGE ANALYSIS (Research Question 3)
# ============================================================================

cat("\nPART 2: BIRTHWEIGHT FOR GESTATIONAL AGE ANALYSIS\n")
cat("================================================\n\n")

# Ensure proper factor levels for birthweight analysis
birthweight_analysis <- birthweight_analysis %>%
  mutate(
    simd_quintile = factor(simd_quintile),
    simd_quintile = relevel(simd_quintile, ref = as.character(REFERENCE_SIMD)),
    age_group = factor(age_group),
    age_group = relevel(age_group, ref = REFERENCE_AGE)
  )

# 2.1 Model Specification and Selection
cat("2.1 Birthweight-for-GA Model Specification\n")
cat("-------------------------------------------\n")

# Define candidate models for Small-for-GA analysis
bw_models_list <- list(
  # Model 1: Main effects only
  "main_effects" = formula(cbind(small, appropriate + large) ~ time + simd_quintile + age_group),
  
  # Model 2: Time-SIMD interaction (primary interest)
  "time_simd_interaction" = formula(cbind(small, appropriate + large) ~ time * simd_quintile + age_group),
  
  # Model 3: Time-Age interaction
  "time_age_interaction" = formula(cbind(small, appropriate + large) ~ time + simd_quintile * age_group),
  
  # Model 4: Full two-way interactions
  "full_interactions" = formula(cbind(small, appropriate + large) ~ time * simd_quintile + time * age_group)
)

# Fit all candidate models
bw_fitted_models <- list()
bw_model_summaries <- list()

cat("Fitting birthweight-for-GA GLM models...\n")

for (model_name in names(bw_models_list)) {
  cat("   - Fitting", model_name, "model...\n")
  
  bw_fitted_models[[model_name]] <- glm(
    formula = bw_models_list[[model_name]],
    family = quasibinomial(link = "logit"),
    data = birthweight_analysis
  )
  
  bw_model_summaries[[model_name]] <- summary(bw_fitted_models[[model_name]])
}

# 2.2 Model Comparison and Selection
cat("\n2.2 Birthweight-for-GA Model Comparison\n")
cat("---------------------------------------\n")

bw_model_comparison <- tibble(
  model = names(bw_fitted_models),
  formula = sapply(bw_models_list, function(x) paste(deparse(x), collapse = " ")),
  deviance = sapply(bw_fitted_models, deviance),
  df_residual = sapply(bw_fitted_models, df.residual),
  dispersion = sapply(bw_model_summaries, function(x) x$dispersion),
  quasi_aic = sapply(bw_fitted_models, function(x) x$deviance + 2 * (length(coef(x)))),
  n_parameters = sapply(bw_fitted_models, function(x) length(coef(x)))
) %>%
  mutate(
    deviance_per_df = deviance / df_residual,
    delta_quasi_aic = quasi_aic - min(quasi_aic)
  ) %>%
  arrange(quasi_aic)

cat("Birthweight-for-GA Model Comparison:\n")
print(bw_model_comparison, digits = 3)

# Select primary birthweight model
bw_primary_model_name <- bw_model_comparison$model[1]
bw_primary_model <- bw_fitted_models[[bw_primary_model_name]]

cat("\nPrimary birthweight model selected:", bw_primary_model_name, "\n")
cat("Formula:", paste(deparse(bw_models_list[[bw_primary_model_name]]), collapse = " "), "\n\n")

# 2.3 Primary Birthweight Model Analysis
cat("2.3 Primary Birthweight Model Analysis\n")
cat("--------------------------------------\n")

cat("Primary Birthweight Model Summary:\n")
print(summary(bw_primary_model))

# Extract coefficients
bw_coef <- broom::tidy(bw_primary_model) %>%
  mutate(
    odds_ratio = exp(estimate),
    or_lower = exp(estimate - 1.96 * std.error),
    or_upper = exp(estimate + 1.96 * std.error),
    model = "Birthweight-for-GA GLM"
  )

cat("\nBirthweight-for-GA Coefficients:\n")
print(bw_coef, digits = 4)

# ============================================================================
# PART 3: INEQUALITY ANALYSIS
# ============================================================================

cat("\nPART 3: INEQUALITY ANALYSIS\n")
cat("===========================\n\n")

# 3.1 Socioeconomic Inequality in Small-for-GA
cat("3.1 Socioeconomic Inequality in Small-for-GA\n")
cat("---------------------------------------------\n")

# Calculate predicted probabilities by SIMD quintile over time
inequality_scenarios <- expand_grid(
  time = seq(0, max(birthweight_analysis$time), by = 5),
  simd_quintile = factor(1:5),
  age_group = factor(REFERENCE_AGE)
) %>%
  mutate(
    simd_quintile = relevel(simd_quintile, ref = as.character(REFERENCE_SIMD)),
    year = time + min(birthweight_analysis$year)
  )

# Generate predictions
inequality_pred <- predict(bw_primary_model, newdata = inequality_scenarios, 
                          type = "response", se.fit = TRUE)

inequality_results <- inequality_scenarios %>%
  mutate(
    predicted_prob = inequality_pred$fit,
    se_pred = inequality_pred$se.fit,
    lower_ci = plogis(qlogis(predicted_prob) - 1.96 * se_pred),
    upper_ci = plogis(qlogis(predicted_prob) + 1.96 * se_pred)
  )

# Calculate rate ratios (most deprived vs least deprived)
inequality_analysis <- inequality_results %>%
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

cat("Inequality Analysis Results (Rate Ratios - SIMD 1 vs SIMD 5):\n")
print(inequality_analysis, digits = 3)

# ============================================================================
# PART 4: VISUALIZATION
# ============================================================================

cat("\nPART 4: VISUALIZATION\n")
cat("=====================\n\n")

# 4.1 Delivery Type Trends and Forecasting Plot
cat("4.1 Creating Delivery Type Trends and Forecasting Plot\n")
cat("------------------------------------------------------\n")

# Combine historical and forecast data for robust plotting
plot_data <- delivery_analysis %>%
  select(year, prop_vaginal) %>%
  full_join(forecast_results, by = "year")

# Create delivery trends plot with forecast
p_delivery <- ggplot(plot_data, aes(x = year)) +
  # Fitted line for historical period (solid blue)
  geom_line(data = . %>% filter(!is_forecast), aes(y = predicted_prop_vaginal), color = "#0571b0", linewidth = 1.2) +
  
  # Forecasted line (dashed red)
  geom_line(data = . %>% filter(is_forecast), aes(y = predicted_prop_vaginal), color = "#ca0020", linetype = "dashed", linewidth = 1.2) +
  
  # Confidence interval ribbon for the forecast
  geom_ribbon(data = . %>% filter(is_forecast), aes(ymin = lower_ci, ymax = upper_ci), fill = "#ca0020", alpha = 0.2) +

  # Observed historical data points (drawn on top with outline for visibility)
  geom_point(data = . %>% filter(!is_forecast), aes(y = prop_vaginal), color = "black", size = 3.5, shape = 21, fill = "#0571b0") +
  
  # Formatting and labels
  scale_y_continuous(labels = scales::percent_format(accuracy = 1), limits = c(0.35, 0.75)) +
  scale_x_continuous(breaks = seq(2017, 2030, by = 2)) +
  labs(
    title = "Trend in Vaginal Delivery Proportion with Forecast to 2030",
    subtitle = "Observed data (2017-2024) with GLM-based forecast and 95% confidence interval",
    x = "Year",
    y = "Proportion of Vaginal Deliveries",
    caption = "Points are observed annual data. Lines represent the fitted model and forecast."
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
    plot.subtitle = element_text(size = 12, hjust = 0.5),
    plot.caption = element_text(face = "italic", size = 9),
    axis.title = element_text(size = 12),
    legend.position = "none",
    panel.grid.minor = element_blank()
  )

ggsave("delivery_trends_forecast.png", plot = p_delivery, path = plots_dir, 
       width = 12, height = 8, dpi = 300)
cat("-> Delivery trends plot saved\n")

# 4.2 Birthweight-for-GA Trends by SIMD Quintile
cat("\n4.2 Creating Birthweight-for-GA Trends Plot\n")
cat("-------------------------------------------\n")

# Create birthweight trends plot
bw_trends_data <- birthweight_analysis %>%
  group_by(year, simd_quintile) %>%
  summarise(
    prop_small = weighted.mean(prop_small, total_births),
    total_births = sum(total_births),
    .groups = "drop"
  )

p_birthweight <- ggplot(bw_trends_data, aes(x = year, y = prop_small, color = simd_quintile)) +
  geom_line(linewidth = 1.2) +
  geom_point(size = 2.5) +
  scale_color_viridis_d(name = "SIMD Quintile", 
                        labels = c("1 (Most Deprived)", "2", "3", "4", "5 (Least Deprived)")) +
  scale_y_continuous(labels = scales::percent_format()) +
  labs(
    title = "Small for Gestational Age Trends by Socioeconomic Deprivation",
    subtitle = "25-year trends (1997-2023) by SIMD quintile",
    x = "Year",
    y = "Proportion Small for Gestational Age",
    caption = "SIMD: Scottish Index of Multiple Deprivation"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 14, face = "bold"),
    plot.subtitle = element_text(size = 12),
    axis.title = element_text(size = 11),
    legend.position = "bottom"
  )

ggsave("birthweight_trends_simd.png", plot = p_birthweight, path = plots_dir, 
       width = 12, height = 8, dpi = 300)
cat("-> Birthweight trends plot saved\n")

# 4.3 Inequality Trends Plot
cat("\n4.3 Creating Inequality Trends Plot\n")
cat("-----------------------------------\n")

p_inequality <- ggplot(inequality_analysis, aes(x = year, y = rate_ratio)) +
  geom_line(linewidth = 1.2, color = "#d73027") +
  geom_point(size = 3, color = "#d73027") +
  geom_ribbon(aes(ymin = rr_lower, ymax = rr_upper), alpha = 0.3, fill = "#d73027") +
  geom_hline(yintercept = 1, linetype = "dashed", color = "black") +
  scale_y_continuous(breaks = seq(1, 3, 0.2)) +
  labs(
    title = "Socioeconomic Inequality in Small for Gestational Age Births",
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

ggsave("inequality_trends_rate_ratio.png", plot = p_inequality, path = plots_dir, 
       width = 12, height = 8, dpi = 300)
cat("-> Inequality trends plot saved\n")

# ============================================================================
# PART 5: MODEL DIAGNOSTICS
# ============================================================================

cat("\nPART 5: MODEL DIAGNOSTICS\n")
cat("=========================\n\n")

# 5.1 Delivery Model Diagnostics
cat("5.1 Delivery Model Diagnostics\n")
cat("------------------------------\n")

# Calculate dispersion parameter for delivery model
delivery_dispersion <- sum(residuals(delivery_glm, type = "pearson")^2) / delivery_glm$df.residual
cat("Delivery model dispersion parameter:", round(delivery_dispersion, 3), "\n")

# 5.2 Birthweight Model Diagnostics
cat("\n5.2 Birthweight Model Diagnostics\n")
cat("---------------------------------\n")

# Calculate dispersion parameter for birthweight model
bw_dispersion <- sum(residuals(bw_primary_model, type = "pearson")^2) / bw_primary_model$df.residual
cat("Birthweight model dispersion parameter:", round(bw_dispersion, 3), "\n")

if (bw_dispersion > 1.5) {
  cat("   -> Moderate overdispersion detected\n")
} else if (bw_dispersion > 2.0) {
  cat("   -> Substantial overdispersion detected\n")
} else {
  cat("   -> Dispersion within acceptable range\n")
}

# Create diagnostic plots for primary birthweight model
cat("\nGenerating birthweight model diagnostic plots...\n")
png(file.path(plots_dir, "birthweight_glm_diagnostics.png"), width = 12, height = 10, units = "in", res = 300)
par(mfrow = c(2, 2))
plot(bw_primary_model)
dev.off()
cat("-> Diagnostic plots saved\n")

# ============================================================================
# PART 6: SAVE RESULTS
# ============================================================================

cat("\nPART 6: SAVE RESULTS\n")
cat("====================\n\n")

# Save model objects
saveRDS(delivery_glm, file.path(output_dir, "delivery_glm_model.rds"))
saveRDS(bw_primary_model, file.path(output_dir, "birthweight_glm_model.rds"))
saveRDS(bw_fitted_models, file.path(output_dir, "all_birthweight_glm_models.rds"))

# Save analysis results
write_csv(bw_model_comparison, file.path(output_dir, "birthweight_model_comparison.csv"))
write_csv(bw_coef, file.path(output_dir, "birthweight_glm_coefficients.csv"))
write_csv(inequality_analysis, file.path(output_dir, "inequality_analysis.csv"))

# Create comprehensive results summary
results_summary <- list(
  analysis_date = Sys.time(),
  delivery_model = list(
    formula = "cbind(vaginal_births, caesarean_births) ~ time + I(time^2)",
    dispersion = delivery_dispersion,
    forecast_2030 = list(
      proportion = forecast_2030$predicted_prop_vaginal,
      lower_ci = forecast_2030$lower_ci,
      upper_ci = forecast_2030$upper_ci
    )
  ),
  birthweight_model = list(
    selected_model = bw_primary_model_name,
    formula = paste(deparse(bw_models_list[[bw_primary_model_name]]), collapse = " "),
    dispersion = bw_dispersion,
    n_observations = nrow(birthweight_analysis)
  ),
  inequality_findings = list(
    latest_year = max(inequality_analysis$year),
    latest_rate_ratio = inequality_analysis$rate_ratio[inequality_analysis$year == max(inequality_analysis$year)],
    trend = if(nrow(inequality_analysis) > 1) {
      if(tail(inequality_analysis$rate_ratio, 1) > head(inequality_analysis$rate_ratio, 1)) "Increasing" else "Decreasing"
    } else "Stable"
  )
)

# Save results summary
jsonlite::write_json(results_summary, file.path(output_dir, "glm_analysis_results_summary.json"), pretty = TRUE)

cat("GLM Analysis completed successfully!\n")
cat("====================================\n")
cat("Key outputs saved to:", output_dir, "\n")
cat("- Delivery trends and 2030 forecast\n")
cat("- Birthweight-for-GA analysis by SIMD quintile\n")
cat("- Inequality analysis and trends\n")
cat("- Model diagnostics and coefficients\n")
cat("- Comprehensive visualizations\n\n")

cat("Next step: Proceed with Phase 3 - Update synthesis and reporting\n")

# Clean workspace
rm(bw_fitted_models, bw_model_summaries, inequality_pred, forecast_pred)
gc()
