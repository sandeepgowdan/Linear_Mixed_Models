# Load necessary libraries
library(dplyr)

# Read the data file containing the covariates and yield measurements
data <- data_ge

# Define the covariates and yield variables
covariates <- c("HM")  # Replace with actual covariate names
yield_var <- "GY"  # Replace with actual yield variable name

# Perform covariate adjustment using linear regression
model <- lm(paste(yield_var, "~", paste(covariates, collapse = "+")), data = data)

# Get the adjusted yield values
data$adjusted_yield <- residuals(model) + mean(data[[yield_var]])

# View the adjusted yield values with genotype names
adjusted_data <- data %>%
  mutate(adjusted_yield = data[[yield_var]] - residuals(model)) %>%
  select(ENV, GEN, REP, HM, GY, adjusted_yield)  # Replace 'Genotype' with the actual column name

# Print the adjusted yield values with genotype names
print(adjusted_data)

# Write the original yield and adjusted yield to a new CSV file
write.csv(adjusted_data, file = "yield_adjustment_results111.csv", row.names = FALSE)

