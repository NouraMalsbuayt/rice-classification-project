#  Data prediction  (Split & Scaling)

library(tidyverse)
library(farff)
# Reading data from the 'data' folder within the project 
rice_data <- readARFF("data/Rice_Cammeo_Osmancik.arff")
rice_data$Class <- factor(rice_data$Class, levels = c("Osmancik", "Cammeo"))

# Stratified Train/Test Split (80% / 20%)
set.seed(6114)

train_indices <- unlist(lapply(split(seq_len(nrow(rice_data)), rice_data$Class), function(idx) {
  sample(idx, size = floor(0.80 * length(idx)))
}))

train_raw <- rice_data[train_indices, ]
test_raw  <- rice_data[-train_indices, ]

print("Train Class Proportions:")
print(prop.table(table(train_raw$Class)))

print("Test Class Proportions:")
print(prop.table(table(test_raw$Class)))

# Shared Preprocessing Function (fitted on training data only)
preprocess_rice <- function(df, train_ref = NULL) {
  num_cols <- setdiff(names(df)[sapply(df, is.numeric)], "Class")
  
  if (is.null(train_ref)) {
    means <- sapply(df[num_cols], mean)
    sds   <- sapply(df[num_cols], sd)
  } else {
    ref_num <- train_ref[num_cols]
    means <- sapply(ref_num, mean)
    sds   <- sapply(ref_num, sd)
  }
  
  df_scaled <- df
  for (col in num_cols) {
    df_scaled[[col]] <- (df[[col]] - means[col]) / sds[col]
  }
  
  list(
    data = df_scaled,
    params = list(means = means, sds = sds, num_cols = num_cols)
  )
}

train_prep <- preprocess_rice(train_raw)
test_prep  <- preprocess_rice(test_raw, train_ref = train_raw)

train_df <- train_prep$data
test_df  <- test_prep$data
