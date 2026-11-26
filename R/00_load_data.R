# PHASE 01: PROJECT SETUP AND DATA LOADING
required_packages <- c("tidyverse", "farff", "caret", "randomForest", "car", "pROC")
for (pkg in required_packages) {
  if (!requireNamespace(pkg, quietly = TRUE)) install.packages(pkg, repos = "https://cloud.r-project.org")
}
library(tidyverse)
library(farff)
library(caret)
library(randomForest)
library(car)
library(pROC)
rice_data <- readARFF("data/Rice_Cammeo_Osmancik.arff")
print(dim(rice_data))
print(head(rice_data))
dir.create("figures", showWarnings = FALSE)
dir.create("results", showWarnings = FALSE)
