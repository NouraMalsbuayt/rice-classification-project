# Machine learning models:
# Building and training the logistic regression model (logit_model) after invoking 04_data_preparation.R
#source("04_data_preparation.R")
source("R/04_data_preparation.R")
logit_model <- glm(Class ~ ., data = train_df, family = binomial(link = "logit"))
print(summary(logit_model))

odds_ratios <- exp(coef(logit_model))
print("Odds Ratios (Logistic Regression):")
print(odds_ratios)








# Random Forest Model
install.packages("caret")
install.packages("randomForest")
library(caret)
library(randomForest)

# Hyperparameter Tuning using 5-Fold Cross Validation

set.seed(6114)

rf_control <- trainControl(
  method = "cv",
  number = 5
)

rf_model <- train(
  Class ~ .,
  data = train_df,
  method = "rf",
  tuneGrid = data.frame(mtry = 1:7),
  trControl = rf_control,
  metric = "Accuracy"
)

# Display the best model
print(rf_model)

# Best hyperparameter
print(rf_model$bestTune)

# Prediction on Test Data
rf_predictions <- predict(rf_model, newdata = test_df)

# Model Evaluation
confusionMatrix(rf_predictions, test_df$Class)


print(rf_model$bestTune)
rf_model
print(rf_model)