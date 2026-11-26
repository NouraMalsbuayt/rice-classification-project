# PHASE 06: MACHINE LEARNING
# 1. Original logistic regression
logit_model <- glm(Class ~ ., data = train_df, family = binomial(link = "logit"))
print(summary(logit_model))
odds_ratios <- exp(coef(logit_model))
print("Odds Ratios (Logistic Regression):")
print(odds_ratios)
# 2. Coefficient analysis
coefficient_summary <- summary(logit_model)$coefficients
logistic_results <- data.frame(Feature = rownames(coefficient_summary), Estimate = coefficient_summary[, "Estimate"], Std_Error = coefficient_summary[, "Std. Error"], Z_Value = coefficient_summary[, "z value"], P_Value = coefficient_summary[, "Pr(>|z|)"], Odds_Ratio = exp(coefficient_summary[, "Estimate"]), row.names = NULL)
print(logistic_results)
write.csv(logistic_results, "results/logistic_regression_coefficients.csv", row.names = FALSE)
# 3. Multicollinearity diagnostics
vif_results <- car::vif(logit_model)
print("Original Model VIF:")
print(vif_results)
vif_table <- data.frame(Feature = names(vif_results), VIF = as.numeric(vif_results))
write.csv(vif_table, "results/logistic_regression_vif.csv", row.names = FALSE)
# 4. Multicollinearity treatment
logit_reduced <- glm(Class ~ . - Area, data = train_df, family = binomial(link = "logit"))
vif_reduced <- car::vif(logit_reduced)
print("VIF After Removing Area:")
print(vif_reduced)
logit_reduced_2 <- glm(Class ~ . - Area - Minor_Axis_Length, data = train_df, family = binomial(link = "logit"))
vif_reduced_2 <- car::vif(logit_reduced_2)
print("VIF After Removing Area and Minor_Axis_Length:")
print(vif_reduced_2)
logit_reduced_3 <- glm(Class ~ . - Area - Minor_Axis_Length - Convex_Area, data = train_df, family = binomial(link = "logit"))
vif_reduced_3 <- car::vif(logit_reduced_3)
print("VIF After Removing Area, Minor_Axis_Length and Convex_Area:")
print(vif_reduced_3)
logit_reduced_4 <- glm(Class ~ Perimeter + Eccentricity + Extent, data = train_df, family = binomial(link = "logit"))
vif_reduced_4 <- car::vif(logit_reduced_4)
print("VIF After Fourth Removal:")
print(vif_reduced_4)
# 5. VIF treatment results
vif_treatment_results <- data.frame(Iteration = c("Original Model", "Step 1", "Step 2", "Step 3", "Step 4"), Removed_Feature = c("None", "Area", "Minor_Axis_Length", "Convex_Area", "Major_Axis_Length"), Remaining_Features = c(length(vif_results), length(vif_reduced), length(vif_reduced_2), length(vif_reduced_3), length(vif_reduced_4)), Maximum_VIF = c(max(vif_results), max(vif_reduced), max(vif_reduced_2), max(vif_reduced_3), max(vif_reduced_4)))
print(vif_treatment_results)
write.csv(vif_treatment_results, "results/vif_treatment_results.csv", row.names = FALSE)
vif_all_steps <- bind_rows(
  data.frame(Iteration = "Original Model", Feature = names(vif_results), VIF = as.numeric(vif_results)),
  data.frame(Iteration = "Step 1", Feature = names(vif_reduced), VIF = as.numeric(vif_reduced)),
  data.frame(Iteration = "Step 2", Feature = names(vif_reduced_2), VIF = as.numeric(vif_reduced_2)),
  data.frame(Iteration = "Step 3", Feature = names(vif_reduced_3), VIF = as.numeric(vif_reduced_3)),
  data.frame(Iteration = "Step 4", Feature = names(vif_reduced_4), VIF = as.numeric(vif_reduced_4))
)
write.csv(vif_all_steps, "results/vif_all_steps.csv", row.names = FALSE)
reduced_vif_table <- data.frame(Feature = names(vif_reduced_4), VIF = as.numeric(vif_reduced_4))
write.csv(reduced_vif_table, "results/reduced_logistic_regression_vif.csv", row.names = FALSE)
# 6. Reduced logistic regression
print(summary(logit_reduced_4))
reduced_odds_ratios <- exp(coef(logit_reduced_4))
print("Odds Ratios of Reduced Logistic Regression:")
print(reduced_odds_ratios)
reduced_coefficient_summary <- summary(logit_reduced_4)$coefficients
reduced_logistic_results <- data.frame(Feature = rownames(reduced_coefficient_summary), Estimate = reduced_coefficient_summary[, "Estimate"], Std_Error = reduced_coefficient_summary[, "Std. Error"], Z_Value = reduced_coefficient_summary[, "z value"], P_Value = reduced_coefficient_summary[, "Pr(>|z|)"], Odds_Ratio = exp(reduced_coefficient_summary[, "Estimate"]), row.names = NULL)
print(reduced_logistic_results)
write.csv(reduced_logistic_results, "results/reduced_logistic_regression_coefficients.csv", row.names = FALSE)
# 7. Random forest with five-fold cross-validation
set.seed(6114)
rf_control <- caret::trainControl(method = "cv", number = 5)
rf_model <- caret::train(Class ~ ., data = train_df, method = "rf", tuneGrid = data.frame(mtry = 1:7), trControl = rf_control, metric = "Accuracy")
print(rf_model)
print(rf_model$bestTune)
# 8. Hyperparameter tuning results
rf_cv_results <- rf_model$results
print(rf_cv_results)
write.csv(rf_cv_results, "results/random_forest_cv_results.csv", row.names = FALSE)
rf_best_tune <- rf_model$bestTune
write.csv(rf_best_tune, "results/random_forest_best_tune.csv", row.names = FALSE)
print("Number of Trees:")
print(rf_model$finalModel$ntree)
# 9. Tuning visualization
rf_tuning_plot <- plot(rf_model)
print(rf_tuning_plot)
png(filename = "figures/random_forest_tuning.png", width = 8, height = 6, units = "in", res = 300)
print(rf_tuning_plot)
dev.off()
