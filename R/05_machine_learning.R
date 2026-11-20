# Machine learning models:
# Building and training the logistic regression model (logit_model) after invoking 04_data_preparation.R
source("04_data_preparation.R")

logit_model <- glm(Class ~ ., data = train_df, family = binomial(link = "logit"))
print(summary(logit_model))

odds_ratios <- exp(coef(logit_model))
print("Odds Ratios (Logistic Regression):")
print(odds_ratios)
