
# PHASE 02: DATA WRANGLING

# 1. Loading the necessary libraries
library(tidyverse)
library(farff)

# 2. Reading the data and examining its structure and dimensions (Data Inspection)
rice_data <- readARFF("data/Rice_Cammeo_Osmancik.arff")
str(rice_data)
dim(rice_data) # Verifying dimensions: 3810 rows and 8 columns

# 3. Examining and handling missing values (Missing Values Check)
missing_values_count <- sum(is.na(rice_data))
print(paste("Total missing values in dataset:", missing_values_count))

# 4. Check for duplicate rows (Duplicates Check)
duplicate_rows_count <- sum(duplicated(rice_data))
print(paste("Total duplicate rows:", duplicate_rows_count))

# 5. Variable transformation (Data Type Conversion)
# Transforming the target variable Class to Factor
rice_data$Class <- as.factor(rice_data$Class)

# Display a few rows of the cleaned data to verify
head(rice_data)
