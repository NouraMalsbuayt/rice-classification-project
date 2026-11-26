# PHASE 07: MODEL EVALUATION
# 1. Original logistic regression evaluation
pred_probs <- predict(logit_model, newdata = test_df, type = "response")
pred_class <- factor(ifelse(pred_probs >= 0.5, "Cammeo", "Osmancik"), levels = c("Osmancik", "Cammeo"))
actual_class <- test_df$Class
conf_matrix <- table(Predicted = pred_class, Actual = actual_class)
print("Original Logistic Regression Confusion Matrix:")
print(conf_matrix)
TP <- conf_matrix["Cammeo", "Cammeo"]
TN <- conf_matrix["Osmancik", "Osmancik"]
FP <- conf_matrix["Cammeo", "Osmancik"]
FN <- conf_matrix["Osmancik", "Cammeo"]
accuracy <- (TP + TN) / (TP + TN + FP + FN)
precision <- TP / (TP + FP)
recall <- TP / (TP + FN)
specificity <- TN / (TN + FP)
f1_score <- 2 * ((precision * recall) / (precision + recall))
metrics_table <- data.frame(Model = "Original Logistic Regression", Number_of_Features = 7, Accuracy = round(accuracy, 4), Precision = round(precision, 4), Recall = round(recall, 4), Specificity = round(specificity, 4), F1_Score = round(f1_score, 4), Correct_Predictions = TP + TN, Incorrect_Predictions = FP + FN)
print("Original Logistic Regression Metrics:")
print(metrics_table)
write.csv(metrics_table, "results/logistic_regression_metrics.csv", row.names = FALSE)
logit_cm <- caret::confusionMatrix(pred_class, actual_class, positive = "Cammeo")
print(logit_cm)
# 2. Reduced logistic regression evaluation
reduced_probs <- predict(logit_reduced_4, newdata = test_df, type = "response")
reduced_predictions <- factor(ifelse(reduced_probs >= 0.5, "Cammeo", "Osmancik"), levels = c("Osmancik", "Cammeo"))
reduced_cm <- caret::confusionMatrix(reduced_predictions, actual_class, positive = "Cammeo")
print("Reduced Logistic Regression Confusion Matrix:")
print(reduced_cm)
reduced_metrics <- data.frame(Model = "Reduced Logistic Regression", Number_of_Features = 3, Accuracy = round(as.numeric(reduced_cm$overall["Accuracy"]), 4), Precision = round(as.numeric(reduced_cm$byClass["Precision"]), 4), Recall = round(as.numeric(reduced_cm$byClass["Recall"]), 4), Specificity = round(as.numeric(reduced_cm$byClass["Specificity"]), 4), F1_Score = round(as.numeric(reduced_cm$byClass["F1"]), 4), Correct_Predictions = sum(diag(reduced_cm$table)), Incorrect_Predictions = sum(reduced_cm$table) - sum(diag(reduced_cm$table)))
print("Reduced Logistic Regression Metrics:")
print(reduced_metrics)
write.csv(reduced_metrics, "results/reduced_logistic_metrics.csv", row.names = FALSE)
# 3. Random forest evaluation
rf_predictions <- predict(rf_model, newdata = test_df)
rf_cm <- caret::confusionMatrix(rf_predictions, actual_class, positive = "Cammeo")
print("Random Forest Confusion Matrix:")
print(rf_cm)
rf_metrics <- data.frame(Model = "Random Forest", Number_of_Features = 7, Accuracy = round(as.numeric(rf_cm$overall["Accuracy"]), 4), Precision = round(as.numeric(rf_cm$byClass["Precision"]), 4), Recall = round(as.numeric(rf_cm$byClass["Recall"]), 4), Specificity = round(as.numeric(rf_cm$byClass["Specificity"]), 4), F1_Score = round(as.numeric(rf_cm$byClass["F1"]), 4), Correct_Predictions = sum(diag(rf_cm$table)), Incorrect_Predictions = sum(rf_cm$table) - sum(diag(rf_cm$table)))
print("Random Forest Metrics:")
print(rf_metrics)
write.csv(rf_metrics, "results/random_forest_metrics.csv", row.names = FALSE)
# 4. Save confusion matrices
write.csv(as.data.frame(conf_matrix), "results/logistic_confusion_matrix.csv", row.names = FALSE)
write.csv(as.data.frame(reduced_cm$table), "results/reduced_logistic_confusion_matrix.csv", row.names = FALSE)
write.csv(as.data.frame(rf_cm$table), "results/random_forest_confusion_matrix.csv", row.names = FALSE)
# 5. Confusion matrix visualizations
plot_confusion_matrix <- function(cm, model_name, file_name) {
  cm_data <- as.data.frame(cm)
  colnames(cm_data) <- c("Predicted", "Actual", "Freq")
  p <- ggplot(cm_data, aes(x = Actual, y = Predicted, fill = Freq)) + geom_tile(color = "white", linewidth = 1) + geom_text(aes(label = Freq), size = 6) + scale_fill_gradient(low = "#E3F2FD", high = "#1565C0") + labs(title = paste("Confusion Matrix -", model_name), x = "Actual Class", y = "Predicted Class", fill = "Count") + theme_minimal(base_size = 13) + theme(plot.title = element_text(hjust = 0.5, face = "bold"), panel.grid = element_blank())
  ggsave(filename = file_name, plot = p, width = 7, height = 5, dpi = 300)
  print(p)
}
plot_confusion_matrix(conf_matrix, "Logistic Regression", "figures/logistic_confusion_matrix.png")
plot_confusion_matrix(reduced_cm$table, "Reduced Logistic Regression", "figures/reduced_logistic_confusion_matrix.png")
plot_confusion_matrix(rf_cm$table, "Random Forest", "figures/random_forest_confusion_matrix.png")
# 6. ROC and AUC analysis
logit_roc <- pROC::roc(response = actual_class, predictor = pred_probs, levels = c("Osmancik", "Cammeo"), direction = "<")
reduced_roc <- pROC::roc(response = actual_class, predictor = reduced_probs, levels = c("Osmancik", "Cammeo"), direction = "<")
rf_probs <- predict(rf_model, newdata = test_df, type = "prob")[, "Cammeo"]
rf_roc <- pROC::roc(response = actual_class, predictor = rf_probs, levels = c("Osmancik", "Cammeo"), direction = "<")
auc_table <- data.frame(Model = c("Original Logistic Regression", "Reduced Logistic Regression", "Random Forest"), AUC = c(round(as.numeric(pROC::auc(logit_roc)), 4), round(as.numeric(pROC::auc(reduced_roc)), 4), round(as.numeric(pROC::auc(rf_roc)), 4)))
print("AUC Results:")
print(auc_table)
write.csv(auc_table, "results/auc_results.csv", row.names = FALSE)
# 7. ROC comparison
roc_comparison <- pROC::ggroc(list("Original Logistic Regression" = logit_roc, "Reduced Logistic Regression" = reduced_roc, "Random Forest" = rf_roc), legacy.axes = TRUE, linewidth = 1.2) + geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "gray") + scale_color_manual(values = c("Original Logistic Regression" = "#1565C0", "Reduced Logistic Regression" = "#E65100", "Random Forest" = "#2E7D32")) + labs(title = "ROC Curve Comparison", x = "False Positive Rate (1 - Specificity)", y = "True Positive Rate (Sensitivity)", color = "Model") + theme_minimal(base_size = 13) + theme(plot.title = element_text(hjust = 0.5, face = "bold"), legend.position = "bottom", legend.direction = "vertical")
print(roc_comparison)
ggsave("figures/roc_three_models.png", plot = roc_comparison, width = 8, height = 6, dpi = 300)
# 8. Final model comparison
comparison_table <- rbind(metrics_table, reduced_metrics, rf_metrics)
comparison_table$AUC <- auc_table$AUC[match(comparison_table$Model, auc_table$Model)]
comparison_table <- comparison_table[, c("Model", "Number_of_Features", "Accuracy", "Precision", "Recall", "Specificity", "F1_Score", "AUC", "Correct_Predictions", "Incorrect_Predictions")]
print("Final Model Comparison:")
print(comparison_table)
write.csv(comparison_table, "results/model_comparison.csv", row.names = FALSE)
