# PHASE 03: EXPLORATORY DATA ANALYSIS (EDA)

# Loading the required libraries
library(tidyverse)
library(farff)

# Loading the dataset
rice_data <- readARFF("data/Rice_Cammeo_Osmancik.arff")

# Converting the target variable to factor
rice_data$Class <- as.factor(rice_data$Class)



# Creating histograms for all numerical features
# Histograms are used to examine the distribution
# of the numerical features.

rice_numeric <- rice_data %>%
  select(-Class)

rice_long <- rice_numeric %>%
  pivot_longer(
    cols = everything(),
    names_to = "Feature",
    values_to = "Value"
  )

ggplot(rice_long, aes(x = Value)) +
  geom_histogram(bins = 30, fill = "lightblue", color = "black") +
  facet_wrap(~ Feature, scales = "free") +
  labs(
    title = "Distribution of Numerical Features",
    x = "Value",
    y = "Frequency"
  ) +
  theme_minimal()



# Comparing numerical features between Cammeo and Osmancik using boxplots
# Boxplots are used to compare numerical features
# between Cammeo and Osmancik and identify potential outliers.
ggplot(rice_long_class, aes(x = Class, y = Value, fill = Class)) +
  geom_boxplot() +
  facet_wrap(~ Feature, scales = "free") +
  labs(
    title = "Boxplot Comparison of Rice Varieties",
    x = "Rice Variety",
    y = "Feature Value"
  ) +
  theme_minimal()



# Comparing the distribution of numerical features between rice varieties
# Density plots are used to compare the distribution
# of numerical features between the two rice varieties.
rice_long_class <- rice_data %>%
  pivot_longer(
    cols = -Class,
    names_to = "Feature",
    values_to = "Value"
  )

ggplot(rice_long_class, aes(x = Value, color = Class, fill = Class)) +
  geom_density(alpha = 0.3) +
  facet_wrap(~ Feature, scales = "free") +
  labs(
    title = "Feature Distribution Comparison by Rice Variety",
    x = "Value",
    y = "Density",
    color = "Rice Variety",
    fill = "Rice Variety"
  ) +
  theme_minimal()


#Summary Statistics & Class Distribution
# 1. Extract comprehensive descriptive statistics for all numerical variables
summary(rice_data)

# 2. Analysis of rice variety distribution (Class Distribution & Percentages)
class_counts <- table(rice_data$Class)
print("Class Counts:")
print(class_counts)

class_percentages <- prop.table(class_counts) * 100
print("Class Percentages (%):")
print(class_percentages)

# 3. Calculating the means and standard deviation for each feature, grouped by rice variety
rice_summary_stats <- rice_data %>%
  group_by(Class) %>%
  summarise(
    Mean_Area = mean(Area),
    SD_Area = sd(Area),
    Mean_Perimeter = mean(Perimeter),
    SD_Perimeter = sd(Perimeter),
    Mean_Major_Axis = mean(Major_Axis_Length),
    SD_Major_Axis = sd(Major_Axis_Length)
  )

print("Summary Statistics Grouped by Rice Variety:")
print(rice_summary_stats)
