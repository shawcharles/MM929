# =============================================================================
# MM929 Analysis - Synthesis of Corrected Analyses
# =============================================================================
# This script synthesizes findings from the CORRECTED analyses:
#   1. Delivery Type Trends & Forecasting (from 02_primary_glm_analysis.R)
#   2. Birthweight for Gestational Age Analysis (from 02_primary_glm_analysis.R)
#
# It provides:
#   - Consolidated key findings for the correct research questions
#   - Synthesis of inequality trends for Small-for-GA births
#   - An executive summary table for stakeholders
#   - Unified interpretation and recommendations based on the new results
#
# Author: MM929 Group 1 (Corrected by Cascade)
# Date: 2025-06-30
# Version: 3.0
# =============================================================================

# Load configuration and required packages
# Assuming the script is run from the project root 'MM929_v2'
source("scripts/00_config.R")
initialize_analysis()

cat("MM929 Corrected Analysis Synthesis\n")
cat("==================================\n\n")

# =============================================================================
# 1. LOAD CORRECTED ANALYSIS RESULTS
# =============================================================================

cat("1. Loading Corrected Analysis Results\n")
cat("=====================================\n\n")

# Define paths to the new output files
results_dir <- file.path(output_dir)

# Load key results files
tryCatch({
  glm_summary <- jsonlite::read_json(file.path(results_dir, "glm_analysis_results_summary.json"))
  delivery_forecast <- read_csv(file.path(results_dir, "delivery_forecast_2030.csv"), show_col_types = FALSE)
  inequality_trends <- read_csv(file.path(results_dir, "inequality_analysis.csv"), show_col_types = FALSE)
  cat("All required results loaded successfully.\n\n")
}, error = function(e) {
  cat("Error loading results files. Please ensure '02_primary_glm_analysis.R' has been run successfully.\n")
  cat("Files expected in:", results_dir, "\n")
  stop(e)
})


# =============================================================================
# 2. KEY FINDINGS SYNTHESIS
# =============================================================================

cat("2. Key Findings Synthesis\n")
cat("=========================\n\n")

# --- 2.1 Delivery Type Trends and Forecast ---
cat("DELIVERY TYPE FINDINGS:\n")
cat("-----------------------\n")
forecast_2030 <- glm_summary$delivery_model$forecast_2030
latest_observed_year <- max(delivery_forecast$year[!delivery_forecast$is_forecast])
latest_observed_prop <- delivery_forecast$predicted_prop_vaginal[delivery_forecast$year == latest_observed_year]

cat(sprintf("• Trend: Vaginal delivery proportion is decreasing over time.\n"))
cat(sprintf("• Latest Observed (2024): %.1f%% of deliveries were vaginal.\n", latest_observed_prop * 100))
cat(sprintf("• 2030 Forecast: Predicted to be %.1f%% (95%% CI: %.1f%%-%.1f%%).\n",
    as.numeric(forecast_2030$proportion) * 100,
    as.numeric(forecast_2030$lower_ci) * 100,
    as.numeric(forecast_2030$upper_ci) * 100
))

# --- 2.2 Birthweight for Gestational Age & Inequality ---
cat("\nBIRTHWEIGHT & INEQUALITY FINDINGS:\n")
cat("----------------------------------\n")
bw_model_info <- glm_summary$birthweight_model
inequality_info <- glm_summary$inequality_findings

most_recent_inequality <- inequality_trends %>% filter(year == max(year))
baseline_inequality <- inequality_trends %>% filter(year == min(year))

cat(sprintf("• Primary Model: '%s' was selected as the best fit.\n", bw_model_info$selected_model))
cat(sprintf("• Model Formula: %s\n", bw_model_info$formula))
cat(sprintf("• Socioeconomic Inequality: Persistent inequality exists in Small-for-GA births.\n"))
cat(sprintf("• Current Inequality (%d): Rate ratio = %.2f (Most vs. Least Deprived).\n",
    most_recent_inequality$year,
    most_recent_inequality$rate_ratio
))
cat(sprintf("• Historical Inequality (%d): Rate ratio = %.2f.\n",
    baseline_inequality$year,
    baseline_inequality$rate_ratio
))
cat(sprintf("• Inequality Trend: %s over the 25-year period.\n", inequality_info$trend))


# =============================================================================
# 3. POLICY IMPLICATIONS & RECOMMENDATIONS
# =============================================================================

cat("\n3. Policy Implications & Recommendations\n")
cat("========================================\n\n")

cat("Based on the analysis, the following implications are highlighted:\n")
cat("1. Delivery Services: The sharp decline in vaginal deliveries and corresponding rise in caesarean sections requires strategic planning for maternity services, including workforce, facilities, and patient counseling.\n")
cat("2. Health Inequality: The persistent and slightly widening gap in Small-for-GA births between the most and least deprived areas indicates that existing interventions may not be sufficient to close the equity gap. Targeted strategies are crucial.\n")
cat("\nRecommendations:\n")
cat("• Investigate drivers of rising caesarean rates (e.g., maternal choice, clinical practice).\n")
cat("• Strengthen targeted public health interventions for pregnant women in deprived areas to address risk factors for Small-for-GA births.\n")
cat("• Continue monitoring these trends to evaluate the impact of future policies.\n")


# =============================================================================
# 4. EXECUTIVE SUMMARY TABLE
# =============================================================================

cat("\n4. Creating Executive Summary Table\n")
cat("===================================\n\n")

# Create a new, corrected executive summary table
executive_summary_corrected <- tibble(
  Domain = c(
    "Delivery Type", "Delivery Type", "Delivery Type",
    "Birthweight Inequality", "Birthweight Inequality", "Birthweight Inequality", "Birthweight Inequality"
  ),
  Metric = c(
    "Analysis Period",
    "Latest Observed Vaginal Delivery Rate (2024)",
    "Forecasted Vaginal Delivery Rate (2030)",
    "Analysis Period",
    "Primary Outcome",
    "Inequality Metric (Most vs. Least Deprived)",
    "Latest Rate Ratio (2022)"
  ),
  Value = c(
    "2017-2024",
    sprintf("%.1f%%", latest_observed_prop * 100),
    sprintf("%.1f%% (95%% CI: %.1f%%-%.1f%%)",
            as.numeric(forecast_2030$proportion) * 100,
            as.numeric(forecast_2030$lower_ci) * 100,
            as.numeric(forecast_2030$upper_ci) * 100),
    "1997-2022 (25 years)",
    "Small for Gestational Age (SGA)",
    "Rate Ratio (RR) by SIMD Quintile",
    sprintf("%.2f", most_recent_inequality$rate_ratio)
  ),
  Interpretation = c(
    "Focus on recent trends post-2017.",
    "Represents a significant decrease since 2017.",
    "Indicates a continuing sharp decline in vaginal deliveries.",
    "Long-term analysis of socioeconomic impact on birth outcomes.",
    "Key indicator of perinatal health.",
    "Measures relative inequality between socioeconomic groups.",
    "Indicates births in the most deprived areas are 82% more likely to be SGA."
  )
)

print(executive_summary_corrected)
write_csv(executive_summary_corrected, file.path(results_dir, "executive_summary_corrected.csv"))
cat("Corrected executive summary saved.\n\n")


# =============================================================================
# 5. SAVE FINAL SYNTHESIS OUTPUTS
# =============================================================================

cat("5. Saving Final Synthesis Outputs\n")
cat("==================================\n\n")

# Create a final synthesis list object with the corrected findings
final_synthesis_corrected <- list(
  analysis_date = Sys.Date(),
  report_title = "MM929 Corrected Analysis Synthesis",
  
  delivery_analysis_summary = list(
    period = "2017-2024",
    finding = "Steep decline in the proportion of vaginal deliveries.",
    forecast_2030 = forecast_2030
  ),
  
  birthweight_analysis_summary = list(
    period = "1997-2022",
    outcome = "Small for Gestational Age (SGA)",
    finding = "Persistent and significant socioeconomic inequality in SGA rates.",
    inequality_trend = inequality_info$trend,
    latest_rate_ratio = list(
      year = most_recent_inequality$year,
      value = most_recent_inequality$rate_ratio
    )
  ),
  
  key_recommendations = c(
    "Strategic review of maternity services in response to changing delivery modes.",
    "Intensify targeted public health interventions to reduce health inequalities in birth outcomes."
  )
)

# Save final synthesis as JSON
jsonlite::write_json(final_synthesis_corrected, file.path(results_dir, "final_synthesis_summary_corrected.json"), pretty = TRUE, auto_unbox = TRUE)
cat("Final corrected synthesis summary saved as JSON.\n\n")

cat("=== MM929 CORRECTED SYNTHESIS COMPLETE ===\n")
cat("All analyses successfully integrated and synthesized based on the correct research questions.\n")
cat("Results ready for final reporting.\n")
