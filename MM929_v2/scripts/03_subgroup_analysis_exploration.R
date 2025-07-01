# 03_subgroup_analysis_exploration.R

# Load data
delivery_data <- readRDS("c:/Users/charl/Documents/MM929/Group 1 project details/type_birth_gp1.RDS")
birthweight_data <- readRDS("c:/Users/charl/Documents/MM929/Group 1 project details/birthweight_gp1.RDS")

# Inspect delivery data
cat("--- Delivery Data Structure ---\n")
str(delivery_data)
cat("\n--- Delivery Data Head ---\n")
print(head(delivery_data))

# Inspect birthweight data
cat("\n\n--- Birthweight Data Structure ---\n")
str(birthweight_data)
cat("\n--- Birthweight Data Head ---\n")
print(head(birthweight_data))
