
# PHASE 02: DATA WRANGLING

# 1. Loading the necessary libraries
library(tidyverse)
library(farff)

# 2. Reading the data and examining its structure and dimensions (Data Inspection)
rice_data <- readARFF("data/Rice_Cammeo_Osmancik.arff")
str(rice_data)
dim(rice_data) # Verifying dimensions: 3810 rows and 8 columns

# 3. Examining and handling missing values (Missing Values Check)
missing_values_count <- sum(is.na(rice_data))
print(paste("Total missing values in dataset:", missing_values_count))

# 4. Check for duplicate rows (Duplicates Check)
duplicate_rows_count <- sum(duplicated(rice_data))
print(paste("Total duplicate rows:", duplicate_rows_count))

# 5. Variable transformation (Data Type Conversion)
# Transforming the target variable Class to Factor
rice_data$Class <- as.factor(rice_data$Class)

# Display a few rows of the cleaned data to verify
head(rice_data)



# 6. Data Quality Validation

# Select all numerical features for validation
numeric_data <- rice_data %>% select(where(is.numeric))

# Check for non-finite values (NaN, Inf, and -Inf)
# Count invalid values in each numerical feature
non_finite_count <- sapply(numeric_data, function(x) {
  sum(!is.finite(x))
})

print(non_finite_count)

# Check for invalid geometric measurements
# Physical measurements must be greater than zero
positive_features <- c(
  "Area", "Perimeter", "Major_Axis_Length",
  "Minor_Axis_Length", "Convex_Area"
)

invalid_positive <- sapply(rice_data[positive_features], function(x) {
  sum(x <= 0, na.rm = TRUE)
})

print(invalid_positive)

# Check whether shape descriptors are within [0, 1]
# Count values outside the valid range for Eccentricity
invalid_eccentricity <- sum(
  rice_data$Eccentricity < 0 |
    rice_data$Eccentricity > 1,
  na.rm = TRUE
)

# Count values outside the valid range for Extent
invalid_extent <- sum(
  rice_data$Extent < 0 |
    rice_data$Extent > 1,
  na.rm = TRUE
)

print(invalid_eccentricity)
print(invalid_extent)


# 7. Outlier Detection Using IQR

# Detect potential outliers in each numerical feature
# using the 1.5 * IQR rule
outlier_count <- sapply(numeric_data, function(x) {
  Q1 <- quantile(x, 0.25, na.rm = TRUE)
  Q3 <- quantile(x, 0.75, na.rm = TRUE)
  IQR_value <- Q3 - Q1
  
  lower_bound <- Q1 - 1.5 * IQR_value
  upper_bound <- Q3 + 1.5 * IQR_value
  
  sum(x < lower_bound | x > upper_bound, na.rm = TRUE)
})

# Display the number of outliers in each feature
print(outlier_count)


# 8. Outlier Assessment by Rice Class

# Calculate IQR boundaries for Minor_Axis_Length
# using observations from both rice classes
Q1 <- quantile(rice_data$Minor_Axis_Length, 0.25)
Q3 <- quantile(rice_data$Minor_Axis_Length, 0.75)

IQR_value <- Q3 - Q1

lower_bound <- Q1 - 1.5 * IQR_value
upper_bound <- Q3 + 1.5 * IQR_value

# Identify potential outliers without removing observations
minor_axis_outliers <- rice_data %>%
  filter(
    Minor_Axis_Length < lower_bound |
      Minor_Axis_Length > upper_bound
  )

# Count detected outliers by rice class
print(table(minor_axis_outliers$Class))




# 9. Visual Assessment Using Boxplot

# Compare Minor_Axis_Length distributions between rice classes
# The boxplot calculates IQR boundaries separately for each class
boxplot(
  Minor_Axis_Length ~ Class,
  data = rice_data,
  main = "Minor Axis Length by Rice Class",
  xlab = "Rice Class",
  ylab = "Minor Axis Length",
  col = c("lightblue", "lightgreen")
)


# 10. Scaling Assessment

# Examine the ranges of numerical features
scaling_summary <- numeric_data %>%
  summarise(across(
    everything(),
    list(
      Min = min,
      Max = max
    )
  ))

print(scaling_summary)



