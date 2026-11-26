# PHASE 02: DATA WRANGLING
# 1. Data inspection
str(rice_data)
print(dim(rice_data))
# 2. Missing values and duplicates
missing_values_count <- sum(is.na(rice_data))
print(paste("Total missing values in dataset:", missing_values_count))
duplicate_rows_count <- sum(duplicated(rice_data))
print(paste("Total duplicate rows:", duplicate_rows_count))
# 3. Data type conversion
rice_data$Class <- as.factor(rice_data$Class)
print(head(rice_data))
# 4. Data quality validation
numeric_data <- rice_data %>% dplyr::select(where(is.numeric))
non_finite_count <- sapply(numeric_data, function(x) sum(!is.finite(x)))
print(non_finite_count)
non_finite_results <- data.frame(Feature = names(non_finite_count), Non_Finite_Values = as.numeric(non_finite_count))
write.csv(non_finite_results, "results/non_finite_values.csv", row.names = FALSE)
positive_features <- c("Area", "Perimeter", "Major_Axis_Length", "Minor_Axis_Length", "Convex_Area")
invalid_positive <- sapply(rice_data[positive_features], function(x) sum(x <= 0, na.rm = TRUE))
print(invalid_positive)
geometric_validation <- data.frame(Feature = names(invalid_positive), Zero_or_Negative_Values = as.numeric(invalid_positive))
write.csv(geometric_validation, "results/geometric_validation.csv", row.names = FALSE)
invalid_eccentricity <- sum(rice_data$Eccentricity < 0 | rice_data$Eccentricity > 1, na.rm = TRUE)
invalid_extent <- sum(rice_data$Extent < 0 | rice_data$Extent > 1, na.rm = TRUE)
print(invalid_eccentricity)
print(invalid_extent)
shape_validation <- data.frame(Feature = c("Eccentricity", "Extent"), Out_of_Range_Values = c(invalid_eccentricity, invalid_extent))
write.csv(shape_validation, "results/shape_validation.csv", row.names = FALSE)
data_quality_summary <- data.frame(Check = c("Number of observations", "Number of variables", "Missing values", "Duplicate rows", "Non-finite values", "Invalid geometric measurements", "Invalid shape descriptors"), Result = c(nrow(rice_data), ncol(rice_data), missing_values_count, duplicate_rows_count, sum(non_finite_count), sum(invalid_positive), invalid_eccentricity + invalid_extent))
print(data_quality_summary)
write.csv(data_quality_summary, "results/data_quality_summary.csv", row.names = FALSE)
# 5. Outlier detection using IQR
outlier_count <- sapply(numeric_data, function(x) {
  Q1 <- quantile(x, 0.25, na.rm = TRUE)
  Q3 <- quantile(x, 0.75, na.rm = TRUE)
  IQR_value <- Q3 - Q1
  lower_bound <- Q1 - 1.5 * IQR_value
  upper_bound <- Q3 + 1.5 * IQR_value
  sum(x < lower_bound | x > upper_bound, na.rm = TRUE)
})
print(outlier_count)
outlier_results <- data.frame(Feature = names(outlier_count), Outlier_Count = as.numeric(outlier_count))
write.csv(outlier_results, "results/outlier_detection.csv", row.names = FALSE)
# 6. Outlier assessment by rice class
Q1 <- quantile(rice_data$Minor_Axis_Length, 0.25)
Q3 <- quantile(rice_data$Minor_Axis_Length, 0.75)
IQR_value <- Q3 - Q1
lower_bound <- Q1 - 1.5 * IQR_value
upper_bound <- Q3 + 1.5 * IQR_value
minor_axis_outliers <- rice_data %>% dplyr::filter(Minor_Axis_Length < lower_bound | Minor_Axis_Length > upper_bound)
outliers_by_class <- table(minor_axis_outliers$Class)
print(outliers_by_class)
outlier_class_results <- data.frame(Rice_Class = names(outliers_by_class), Outlier_Count = as.numeric(outliers_by_class))
write.csv(outlier_class_results, "results/outliers_by_class.csv", row.names = FALSE)
# 7. Visual assessment
png("figures/minor_axis_length_boxplot.png", width = 8, height = 6, units = "in", res = 300)
boxplot(Minor_Axis_Length ~ Class, data = rice_data, main = "Minor Axis Length by Rice Class", xlab = "Rice Class", ylab = "Minor Axis Length", col = c("lightblue", "lightgreen"))
dev.off()
# 8. Scaling assessment
scaling_summary <- numeric_data %>% summarise(across(everything(), list(Min = min, Max = max)))
print(scaling_summary)
scaling_results <- data.frame(Feature = names(numeric_data), Minimum = sapply(numeric_data, min), Maximum = sapply(numeric_data, max))
print(scaling_results)
write.csv(scaling_results, "results/feature_ranges.csv", row.names = FALSE)
