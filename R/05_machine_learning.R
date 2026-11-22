
# PHASE 05: MACHINE LEARNING

# Loading the prepared training and test data
source("R/04_data_preparation.R")


# Logistic Regression Model

# Train the logistic regression model
logit_model <- glm(
  Class ~ .,
  data = train_df,
  family = binomial(link = "logit")
)

# Display the model summary
print(summary(logit_model))

# Calculate and display odds ratios
odds_ratios <- exp(coef(logit_model))

print("Odds Ratios (Logistic Regression):")
print(odds_ratios)


# Random Forest Model

# Load the required libraries
library(caret)
library(randomForest)

# Hyperparameter tuning using 5-fold cross-validation
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

# Display the model results
print(rf_model)

# Display the best hyperparameter
print(rf_model$bestTune)

# Predict rice varieties using the test data
rf_predictions <- predict(rf_model, newdata = test_df)

# Evaluate the Random Forest model
print(confusionMatrix(rf_predictions, test_df$Class))
