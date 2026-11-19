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