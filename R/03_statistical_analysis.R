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