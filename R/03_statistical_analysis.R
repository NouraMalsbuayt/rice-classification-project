
# PHASE 04: STATISTICAL ANALYSIS

# Loading the required libraries
library(tidyverse)
library(farff)

# Loading the dataset
rice_data <- readARFF("data/Rice_Cammeo_Osmancik.arff")

# Converting Class to factor
rice_data$Class <- as.factor(rice_data$Class)

# Checking the rice varieties
table(rice_data$Class)

# Selecting numerical features
numerical_features <- rice_data %>%
  select(-Class) %>%
  names()

# Performing Welch's t-test for each numerical feature
# Welch's t-tests were performed to compare the numerical features
# between Cammeo and Osmancik rice varieties.

t_test_results <- lapply(numerical_features, function(feature) {
  
  formula <- as.formula(paste(feature, "~ Class"))
  
  test <- t.test(formula, data = rice_data)
  
  data.frame(
    Feature = feature,
    P_Value = test$p.value,
    Mean_Cammeo = mean(rice_data[[feature]][rice_data$Class == "Cammeo"]),
    Mean_Osmancik = mean(rice_data[[feature]][rice_data$Class == "Osmancik"])
  )
})

# Combining the results
t_test_results <- bind_rows(t_test_results)

# Displaying the results
print(t_test_results)


# Calculating Cohen's d to measure effect size
# Cohen's d was calculated to measure the effect size
# of the differences between the two rice varieties.

cohen_d <- function(x, y) {
  
  pooled_sd <- sqrt(
    ((length(x) - 1) * var(x) +
       (length(y) - 1) * var(y)) /
      (length(x) + length(y) - 2)
  )
  
  (mean(x) - mean(y)) / pooled_sd
}

# Calculating effect size for each feature
effect_sizes <- sapply(numerical_features, function(feature) {
  
  x <- rice_data[[feature]][rice_data$Class == "Cammeo"]
  y <- rice_data[[feature]][rice_data$Class == "Osmancik"]
  
  cohen_d(x, y)
})

# Adding effect sizes to the results table
t_test_results$Cohen_d <- effect_sizes

print(t_test_results)


# Adjusting p-values for multiple comparisons
# P-values were adjusted using the Benjamini-Hochberg method
# to account for multiple comparisons.

t_test_results$Adjusted_P_Value <- p.adjust(
  t_test_results$P_Value,
  method = "BH"
)

# Displaying the final statistical results
print(t_test_results)


# Variance, Normality (Q-Q Plot) & Wilcoxon Check
# Research hypotheses, test selection and assumptions

# 1. Examining the variance for each numerical type, grouped by category
# Note: Numerical values only, excluding the 'Class' column
numeric_cols <- rice_data %>% select(where(is.numeric)) %>% names()

variance_by_class <- rice_data %>%
  group_by(Class) %>%
  summarise(across(all_of(numeric_cols), list(mean = mean, sd = sd, var = var)))

print(variance_by_class)

# 2. Visually examining the distribution (normality) using Q-Q plots
ggplot(rice_data, aes(sample = Area)) +
  stat_qq(aes(color = Class), alpha = 0.7) +
  stat_qq_line(color = "gray30", linewidth = 0.8) +
  facet_wrap(~Class) +
  labs(title = "Q-Q Plots for Area by Rice Variety") +
  theme_minimal()

# 3. Applying Welch Two-Sample t-test and Wilcoxon test for 'Area'
t_test_area <- t.test(Area ~ Class, data = rice_data)
wilcox_area <- wilcox.test(Area ~ Class, data = rice_data)

print(t_test_area)
print(wilcox_area)


# Logistic Regression Analysis
# Coefficient Interpretation

# Load the trained logistic regression model
source("R/05_machine_learning.R")

# Extract model coefficients and statistical results
coefficient_summary <- summary(logit_model)$coefficients

# Create a summary table of the regression coefficients
logistic_results <- data.frame(
  Feature = rownames(coefficient_summary),
  Estimate = coefficient_summary[, "Estimate"],
  Std_Error = coefficient_summary[, "Std. Error"],
  Z_Value = coefficient_summary[, "z value"],
  P_Value = coefficient_summary[, "Pr(>|z|)"],
  Odds_Ratio = exp(coefficient_summary[, "Estimate"]),
  row.names = NULL
)

# Display coefficient results
print(logistic_results)

# Save results for the project report
dir.create("results", showWarnings = FALSE)

write.csv(
  logistic_results,
  "results/logistic_regression_coefficients.csv",
  row.names = FALSE
)


# Logistic Regression Diagnostics
# Checking multicollinearity among predictors

library(car)

# Calculate VIF for each predictor
vif_results <- vif(logit_model)

# Display VIF results
print(vif_results)

# Save VIF results for the report
vif_table <- data.frame(
  Feature = names(vif_results),
  VIF = as.numeric(vif_results)
)

write.csv(
  vif_table,
  "results/logistic_regression_vif.csv",
  row.names = FALSE
)
