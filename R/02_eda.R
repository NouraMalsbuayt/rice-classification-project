# PHASE 03: EXPLORATORY DATA ANALYSIS (EDA)
# 1. Class distribution and percentages
class_counts <- table(rice_data$Class)
print("Class Counts:")
print(class_counts)
class_percentages <- prop.table(class_counts) * 100
print("Class Percentages (%):")
print(class_percentages)
class_distribution <- data.frame(Class = names(class_counts), Count = as.numeric(class_counts), Percentage = as.numeric(class_percentages))
write.csv(class_distribution, "results/class_distribution.csv", row.names = FALSE)
class_plot <- ggplot(rice_data, aes(x = Class, fill = Class)) + geom_bar() + geom_text(stat = "count", aes(label = after_stat(count)), vjust = -0.5) + scale_y_continuous(expand = expansion(mult = c(0, 0.1))) + labs(title = "Distribution of Rice Varieties", x = "Rice Variety", y = "Number of Observations") + theme_minimal() + theme(legend.position = "none")
print(class_plot)
ggsave("figures/class_distribution.png", plot = class_plot, width = 8, height = 6, dpi = 300)
# 2. Overall descriptive statistics
print(summary(rice_data))
overall_sd <- rice_data %>% summarise(across(where(is.numeric), sd))
print(overall_sd)
rice_numeric <- rice_data %>% dplyr::select(-Class)
overall_summary_stats <- data.frame(Feature = names(rice_numeric), Min = sapply(rice_numeric, min), Q1 = sapply(rice_numeric, quantile, probs = 0.25), Median = sapply(rice_numeric, median), Mean = sapply(rice_numeric, mean), Q3 = sapply(rice_numeric, quantile, probs = 0.75), Max = sapply(rice_numeric, max), SD = sapply(rice_numeric, sd))
print(overall_summary_stats)
write.csv(overall_summary_stats, "results/overall_descriptive_statistics.csv", row.names = FALSE)
# 3. Descriptive statistics by rice variety
rice_summary_stats <- rice_data %>% group_by(Class) %>% summarise(across(where(is.numeric), list(Mean = mean, SD = sd, Median = median, IQR = IQR)), .groups = "drop")
print("Summary Statistics Grouped by Rice Variety:")
print(rice_summary_stats, width = Inf)
grouped_summary_stats <- rice_summary_stats %>% pivot_longer(cols = -Class, names_to = c("Feature", ".value"), names_pattern = "(.*)_(Mean|SD|Median|IQR)$")
print(grouped_summary_stats, n = Inf)
write.csv(grouped_summary_stats, "results/descriptive_statistics_by_class.csv", row.names = FALSE)
# 4. Prepare data for visualization
rice_long <- rice_numeric %>% pivot_longer(cols = everything(), names_to = "Feature", values_to = "Value")
rice_long_class <- rice_data %>% pivot_longer(cols = -Class, names_to = "Feature", values_to = "Value")
# 5. Histograms
histogram_plot <- ggplot(rice_long, aes(x = Value)) + geom_histogram(bins = 30, fill = "lightblue", color = "black") + facet_wrap(~ Feature, scales = "free") + labs(title = "Distribution of Numerical Features", x = "Value", y = "Frequency") + theme_minimal()
print(histogram_plot)
ggsave("figures/numerical_feature_histograms.png", plot = histogram_plot, width = 12, height = 9, dpi = 300)
# 6. Boxplots
boxplot_comparison <- ggplot(rice_long_class, aes(x = Class, y = Value, fill = Class)) + geom_boxplot() + facet_wrap(~ Feature, scales = "free") + labs(title = "Boxplot Comparison of Rice Varieties", x = "Rice Variety", y = "Feature Value") + theme_minimal()
print(boxplot_comparison)
ggsave("figures/boxplots_by_class.png", plot = boxplot_comparison, width = 12, height = 9, dpi = 300)
# 7. Density plots
density_plot <- ggplot(rice_long_class, aes(x = Value, color = Class, fill = Class)) + geom_density(alpha = 0.3) + facet_wrap(~ Feature, scales = "free") + labs(title = "Feature Distribution Comparison by Rice Variety", x = "Value", y = "Density", color = "Rice Variety", fill = "Rice Variety") + theme_minimal()
print(density_plot)
ggsave("figures/density_plots_by_class.png", plot = density_plot, width = 12, height = 9, dpi = 300)
# 8. Correlation analysis
correlation_matrix <- rice_data %>% dplyr::select(where(is.numeric)) %>% cor(method = "pearson")
print(round(correlation_matrix, 3))
correlation_results <- data.frame(Feature = rownames(correlation_matrix), correlation_matrix)
write.csv(correlation_results, "results/correlation_matrix.csv", row.names = FALSE)
correlation_long <- as.data.frame(as.table(correlation_matrix))
colnames(correlation_long) <- c("Feature1", "Feature2", "Correlation")
correlation_plot <- ggplot(correlation_long, aes(x = Feature1, y = Feature2, fill = Correlation)) + geom_tile(color = "white") + geom_text(aes(label = round(Correlation, 2)), size = 3) + scale_fill_gradient2(low = "blue", mid = "white", high = "red", midpoint = 0, limits = c(-1, 1)) + labs(title = "Correlation Heatmap of Numerical Features", x = "Features", y = "Features", fill = "Pearson r") + theme_minimal() + theme(axis.text.x = element_text(angle = 45, hjust = 1))
print(correlation_plot)
ggsave("figures/correlation_heatmap.png", plot = correlation_plot, width = 10, height = 8, dpi = 300)
# 9. Pairwise scatterplots
numeric_features <- names(rice_data)[sapply(rice_data, is.numeric)]
scatterplot_pairs <- combn(numeric_features, 2, simplify = FALSE)
scatterplot_data <- purrr::map_dfr(scatterplot_pairs, function(pair) {
  rice_data %>% transmute(X = .data[[pair[1]]], Y = .data[[pair[2]]], Class = Class, Feature_Pair = paste(pair[1], "vs.", pair[2]))
})
pair_names <- unique(scatterplot_data$Feature_Pair)
scatterplot_data <- scatterplot_data %>% mutate(Figure = ceiling(match(Feature_Pair, pair_names) / 7), Feature_Pair = factor(Feature_Pair, levels = pair_names))
dir.create("figures/scatterplots", recursive = TRUE, showWarnings = FALSE)
for (i in 1:3) {
  plot_data <- scatterplot_data %>% dplyr::filter(Figure == i) %>% mutate(Feature_Pair = droplevels(Feature_Pair))
  p <- ggplot(plot_data, aes(x = X, y = Y, color = Class)) + geom_point(alpha = 0.35, size = 0.7) + facet_wrap(~ Feature_Pair, scales = "free", ncol = 2) + labs(title = paste("Pairwise Scatterplots - Part", i), x = "Feature Value (X)", y = "Feature Value (Y)", color = "Rice Variety") + theme_minimal(base_size = 12) + theme(strip.text = element_text(size = 10, face = "bold"), axis.text = element_text(size = 9), panel.spacing = grid::unit(1.5, "lines"), legend.position = "bottom")
  print(p)
  ggsave(paste0("figures/scatterplots/scatterplots_part_", i, ".png"), plot = p, width = 13, height = 15, dpi = 300)
}
