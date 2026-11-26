# PHASE 04: STATISTICAL ANALYSIS
# 1. Set class order for statistical comparisons
rice_data$Class <- factor(rice_data$Class, levels = c("Cammeo", "Osmancik"))
print(table(rice_data$Class))
numerical_features <- rice_data %>% dplyr::select(-Class) %>% names()
numeric_cols <- numerical_features
# 2. Variance analysis
variance_by_class <- rice_data %>% group_by(Class) %>% summarise(across(all_of(numeric_cols), list(mean = mean, sd = sd, var = var)), .groups = "drop")
print(variance_by_class, width = Inf)
variance_results <- variance_by_class %>% pivot_longer(cols = -Class, names_to = c("Feature", ".value"), names_pattern = "(.*)_(mean|sd|var)$")
print(variance_results, n = Inf)
write.csv(variance_results, "results/variance_by_class.csv", row.names = FALSE)
# 3. Normality assessment
rice_qq <- rice_data %>% pivot_longer(cols = all_of(numeric_cols), names_to = "Feature", values_to = "Value")
qq_plot <- ggplot(rice_qq, aes(sample = Value, color = Class)) + stat_qq(alpha = 0.7, size = 0.8) + stat_qq_line(linewidth = 0.8) + facet_wrap(~Feature, scales = "free_y", ncol = 3) + labs(title = "Q-Q Plots of Numerical Features by Rice Variety", x = "Theoretical Quantiles", y = "Sample Quantiles", color = "Rice Variety") + theme_minimal() + theme(legend.position = "bottom", strip.text = element_text(face = "bold"))
print(qq_plot)
ggsave("figures/qq_plots.png", plot = qq_plot, width = 12, height = 9, dpi = 300)
# 4. Welch's two-sample t-test
t_test_results <- lapply(numerical_features, function(feature) {
  formula <- as.formula(paste(feature, "~ Class"))
  test <- t.test(formula, data = rice_data)
  data.frame(Feature = feature, P_Value = test$p.value, Mean_Cammeo = mean(rice_data[[feature]][rice_data$Class == "Cammeo"]), Mean_Osmancik = mean(rice_data[[feature]][rice_data$Class == "Osmancik"]), T_Statistic = unname(test$statistic), DF = unname(test$parameter))
})
t_test_results <- bind_rows(t_test_results)
print(t_test_results)
# 5. Cohen's d
cohen_d <- function(x, y) {
  pooled_sd <- sqrt(((length(x) - 1) * var(x) + (length(y) - 1) * var(y)) / (length(x) + length(y) - 2))
  (mean(x) - mean(y)) / pooled_sd
}
effect_sizes <- sapply(numerical_features, function(feature) {
  x <- rice_data[[feature]][rice_data$Class == "Cammeo"]
  y <- rice_data[[feature]][rice_data$Class == "Osmancik"]
  cohen_d(x, y)
})
t_test_results$Cohen_d <- effect_sizes
print(t_test_results)
# 6. Multiple comparisons adjustment
t_test_results$Adjusted_P_Value <- p.adjust(t_test_results$P_Value, method = "BH")
print(t_test_results)
write.csv(t_test_results, "results/welch_t_test_results.csv", row.names = FALSE)
# 7. Wilcoxon rank-sum test
wilcoxon_results <- lapply(numeric_cols, function(feature) {
  formula <- as.formula(paste(feature, "~ Class"))
  test <- wilcox.test(formula, data = rice_data, exact = FALSE)
  data.frame(Feature = feature, W_Statistic = unname(test$statistic), P_Value = test$p.value)
})
wilcoxon_results <- bind_rows(wilcoxon_results)
wilcoxon_results$Adjusted_P_Value <- p.adjust(wilcoxon_results$P_Value, method = "BH")
print(wilcoxon_results)
write.csv(wilcoxon_results, "results/wilcoxon_test_results.csv", row.names = FALSE)
