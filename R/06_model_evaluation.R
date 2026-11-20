# model evaluation 
#Here are the predictions on  test_df , the confusion matrix, and the performance metrics, following the execution of 05_machine_learning.R :
source("05_machine_learning.R")

pred_probs <- predict(logit_model, newdata = test_df, type = "response")
pred_class <- factor(ifelse(pred_probs >= 0.5, "Cammeo", "Osmancik"),
                     levels = c("Osmancik", "Cammeo"))

actual_class <- test_df$Class

conf_matrix <- table(Predicted = pred_class, Actual = actual_class)
print("Confusion Matrix:")
print(conf_matrix)

TP <- conf_matrix["Cammeo", "Cammeo"]
TN <- conf_matrix["Osmancik", "Osmancik"]
FP <- conf_matrix["Cammeo", "Osmancik"]
FN <- conf_matrix["Osmancik", "Cammeo"]

accuracy    <- (TP + TN) / (TP + TN + FP + FN)
precision   <- TP / (TP + FP)
recall      <- TP / (TP + FN)
specificity <- TN / (TN + FP)
f1_score    <- 2 * ((precision * recall) / (precision + recall))

metrics_table <- data.frame(
  Model       = "Logistic Regression",
  Accuracy    = round(accuracy, 4),
  Precision   = round(precision, 4),
  Recall      = round(recall, 4),
  Specificity = round(specificity, 4),
  F1_Score    = round(f1_score, 4)
)

print("Evaluation Metrics:")
print(metrics_table)
