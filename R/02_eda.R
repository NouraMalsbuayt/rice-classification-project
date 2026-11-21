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

rice_long_class <- rice_data %>%
  pivot_longer(
    cols = -Class,
    names_to = "Feature",
    values_to = "Value"
  )

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

# Correlation Analysis
# Examine linear relationships among numerical features

correlation_matrix <- rice_data %>%
  select(where(is.numeric)) %>%
  cor(method = "pearson")

print(round(correlation_matrix, 3))

# Prepare correlation matrix for visualization
correlation_long <- as.data.frame(as.table(correlation_matrix))

colnames(correlation_long) <- c("Feature1", "Feature2", "Correlation")

# Correlation Heatmap
ggplot(correlation_long, aes(x = Feature1, y = Feature2, fill = Correlation)) +
  geom_tile(color = "white") +
  geom_text(aes(label = round(Correlation, 2)), size = 3) +
  scale_fill_gradient2(
    low = "blue",
    mid = "white",
    high = "red",
    midpoint = 0,
    limits = c(-1, 1)
  ) +
  labs(
    title = "Correlation Heatmap of Numerical Features",
    x = "Features",
    y = "Features",
    fill = "Pearson r"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1)
  )

ggsave(
  filename = "figures/correlation_heatmap.png",
  plot = last_plot(),
  width = 10,
  height = 8,
  dpi = 300
)

# Scatterplot Analysis
# Generate all 21 unique pairwise scatterplots
# Split them into 3 figures for better readability

numeric_features <- names(rice_data)[sapply(rice_data, is.numeric)]

scatterplot_pairs <- combn(numeric_features, 2, simplify = FALSE)

scatterplot_data <- purrr::map_dfr(scatterplot_pairs, function(pair) {
  
  rice_data %>%
    transmute(
      X = .data[[pair[1]]],
      Y = .data[[pair[2]]],
      Class = Class,
      Feature_Pair = paste(pair[1], "vs.", pair[2])
    )
  
})

# Assign each pair to one of three figures
pair_names <- unique(scatterplot_data$Feature_Pair)

scatterplot_data <- scatterplot_data %>%
  mutate(
    Figure = ceiling(match(Feature_Pair, pair_names) / 7),
    Feature_Pair = factor(Feature_Pair, levels = pair_names)
  )

dir.create("figures/scatterplots", recursive = TRUE, showWarnings = FALSE)

for (i in 1:3) {
  
  plot_data <- scatterplot_data %>%
    filter(Figure == i) %>%
    mutate(Feature_Pair = droplevels(Feature_Pair))
  
  p <- ggplot(plot_data, aes(x = X, y = Y, color = Class)) +
    geom_point(alpha = 0.35, size = 0.7) +
    facet_wrap(
      ~ Feature_Pair,
      scales = "free",
      ncol = 2
    ) +
    labs(
      title = paste("Pairwise Scatterplots - Part", i),
      x = "Feature Value (X)",
      y = "Feature Value (Y)",
      color = "Rice Variety"
    ) +
    theme_minimal(base_size = 12) +
    theme(
      strip.text = element_text(size = 10, face = "bold"),
      axis.text = element_text(size = 9),
      panel.spacing = grid::unit(1.5, "lines"),
      legend.position = "bottom"
    )
  
  print(p)
  
  ggsave(
    filename = paste0(
      "figures/scatterplots/scatterplots_part_", i, ".png"
    ),
    plot = p,
    width = 13,
    height = 15,
    dpi = 300
  )
}
