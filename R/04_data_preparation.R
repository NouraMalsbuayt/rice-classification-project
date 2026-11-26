# PHASE 05: DATA PREPARATION
# 1. Set target variable levels
rice_data$Class <- factor(rice_data$Class, levels = c("Osmancik", "Cammeo"))
# 2. Stratified train/test split (80% / 20%)
set.seed(6114)
train_indices <- unlist(lapply(split(seq_len(nrow(rice_data)), rice_data$Class), function(idx) {
  sample(idx, size = floor(0.80 * length(idx)))
}))
train_raw <- rice_data[train_indices, ]
test_raw <- rice_data[-train_indices, ]
print("Training Set Dimensions:")
print(dim(train_raw))
print("Testing Set Dimensions:")
print(dim(test_raw))
print("Train Class Proportions:")
print(prop.table(table(train_raw$Class)))
print("Test Class Proportions:")
print(prop.table(table(test_raw$Class)))
# 3. Training and testing class distribution
train_counts <- table(train_raw$Class)
test_counts <- table(test_raw$Class)
total_counts <- table(rice_data$Class)
split_distribution <- data.frame(Class = names(total_counts), Training_Count = as.numeric(train_counts), Testing_Count = as.numeric(test_counts), Total_Count = as.numeric(total_counts), Training_Percentage = as.numeric(prop.table(train_counts) * 100), Testing_Percentage = as.numeric(prop.table(test_counts) * 100))
print(split_distribution)
write.csv(split_distribution, "results/train_test_distribution.csv", row.names = FALSE)
# 4. Split validation
test_indices <- setdiff(seq_len(nrow(rice_data)), train_indices)
stopifnot(length(intersect(train_indices, test_indices)) == 0, anyDuplicated(train_indices) == 0, nrow(train_raw) + nrow(test_raw) == nrow(rice_data))
print("Train/Test Split Validation Passed.")
# 5. Shared preprocessing function
preprocess_rice <- function(df, train_ref = NULL) {
  num_cols <- setdiff(names(df)[sapply(df, is.numeric)], "Class")
  if (is.null(train_ref)) {
    means <- sapply(df[num_cols], mean)
    sds <- sapply(df[num_cols], sd)
  } else {
    ref_num <- train_ref[num_cols]
    means <- sapply(ref_num, mean)
    sds <- sapply(ref_num, sd)
  }
  df_scaled <- df
  for (col in num_cols) {
    df_scaled[[col]] <- (df[[col]] - means[col]) / sds[col]
  }
  list(data = df_scaled, params = list(means = means, sds = sds, num_cols = num_cols))
}
# 6. Apply Z-score standardization
train_prep <- preprocess_rice(train_raw)
test_prep <- preprocess_rice(test_raw, train_ref = train_raw)
train_df <- train_prep$data
test_df <- test_prep$data
# 7. Save training standardization parameters
scaling_parameters <- data.frame(Feature = train_prep$params$num_cols, Training_Mean = as.numeric(train_prep$params$means), Training_SD = as.numeric(train_prep$params$sds))
print(scaling_parameters)
write.csv(scaling_parameters, "results/scaling_parameters.csv", row.names = FALSE)
# 8. Validate standardization
numeric_features <- names(train_df)[sapply(train_df, is.numeric)]
scaling_validation <- data.frame(Feature = numeric_features, Mean_After = sapply(train_df[numeric_features], mean), SD_After = sapply(train_df[numeric_features], sd))
print(round(scaling_validation[, c("Mean_After", "SD_After")], 6))
write.csv(scaling_validation, "results/scaling_validation.csv", row.names = FALSE)
