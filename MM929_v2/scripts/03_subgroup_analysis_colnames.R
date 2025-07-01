# 03_subgroup_analysis_colnames.R
# Purpose: Print column names of the delivery data to debug grouping error.

# Load data
delivery_data <- readRDS("c:/Users/charl/Documents/MM929/Group 1 project details/type_birth_gp1.RDS")

# Print column names
cat("--- Delivery Data Column Names ---\n")
print(colnames(delivery_data))
