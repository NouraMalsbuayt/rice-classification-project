# Rice Variety Classification Using Data Analytics and Machine Learning

## Project Overview

This project aims to classify two rice varieties, Cammeo and Osmancik, using seven morphological features extracted from rice grain images. The project applies data wrangling, exploratory data analysis, statistical analysis, and machine learning techniques to investigate differences between the two varieties and evaluate classification performance.

## Dataset

Rice (Cammeo and Osmancik) dataset from the UCI Machine Learning Repository.

- Observations: 3,810
- Numerical features: 7
- Target variable: Class (Cammeo or Osmancik)
- Missing values: 0
- Duplicate rows: 0

**Dataset source:** https://doi.org/10.24432/C5MW4Z

## Project Structure

- `data/` – Original dataset.
- `R/` – R scripts for data analysis and modeling.
- `figures/` – Generated visualizations.
- `results/` – Statistical and machine learning results in CSV format.
- `run_all.R` – Main script for executing the complete analysis workflow.
- `README.md` – Project documentation.
- `rice-classification-project.Rproj` – RStudio project file.

## Analysis Workflow

| Script | Description |
|---|---|
| `00_load_data.R` | Install missing packages, load libraries, import the dataset, and create output directories. |
| `01_data_wrangling.R` | Inspect data, check missing values and duplicates, validate data quality, and detect outliers. |
| `02_eda.R` | Generate descriptive statistics, distribution plots, correlation analysis, and scatterplots. |
| `03_statistical_analysis.R` | Perform variance analysis, Q-Q plots, Welch's t-test, Cohen's d, Wilcoxon rank-sum tests, and p-value adjustment. |
| `04_data_preparation.R` | Perform stratified train/test splitting and Z-score standardization. |
| `05_machine_learning.R` | Train classification models, perform VIF diagnostics and feature removal, and tune Random Forest using cross-validation. |
| `06_model_evaluation.R` | Evaluate and compare the three classification models using performance metrics, confusion matrices, and ROC curves. |

## Tools and Libraries

The project was developed using R and RStudio.

Required R packages:

- tidyverse
- farff
- caret
- randomForest
- car
- pROC

Missing packages are automatically installed during project setup.

## Data Preparation

The dataset was divided using an 80/20 stratified split:

- Training set: 3,048 observations.
- Testing set: 762 observations.
- Random seed: 6114.

Z-score standardization was performed using the mean and standard deviation calculated from the training set.

## Machine Learning Models

Three classification models were developed and evaluated:

1. Original Logistic Regression using all seven numerical features.
2. Reduced Logistic Regression using Perimeter, Eccentricity, and Extent after VIF-based feature removal.
3. Random Forest using all seven numerical features, 500 trees, and five-fold cross-validation for hyperparameter tuning.

## Model Performance

| Metric | Original Logistic Regression | Reduced Logistic Regression | Random Forest |
|---|---:|---:|---:|
| Number of Features | 7 | 3 | 7 |
| Accuracy | 92.52% | 92.91% | 93.31% |
| Precision | 92.97% | 93.59% | 93.93% |
| Recall | 89.26% | 89.57% | 90.18% |
| Specificity | 94.95% | 95.41% | 95.64% |
| F1-Score | 91.08% | 91.54% | 92.02% |
| AUC | 0.9797 | 0.9790 | 0.9736 |

Random Forest achieved the highest test accuracy, while the reduced Logistic Regression model maintained comparable performance using only three features. The original Logistic Regression model achieved the highest AUC.

## How to Run the Project

1. Clone or download the repository.
2. Open `rice-classification-project.Rproj` in RStudio.
3. Ensure that `Rice_Cammeo_Osmancik.arff` is available in the `data/` directory.
4. Run the following command from the project root:

    source("run_all.R")

The main script executes all analysis phases sequentially. An internet connection may be required to install missing R packages.

## Project Outputs

The analysis generates:

- CSV files containing data quality checks, descriptive statistics, statistical tests, model coefficients, VIF results, and model evaluation metrics.
- PNG files containing exploratory visualizations, Q-Q plots, hyperparameter tuning results, confusion matrices, and ROC curves.

All generated tables and visualizations are saved in the `results/` and `figures/` directories, respectively.

## Reproducibility

The project uses a fixed random seed and a sequential analysis workflow to support reproducibility. All scripts, the original dataset, and generated outputs are organized within the repository.

## Collaboration

Each team member contributes to assigned analysis tasks, interpretation of results, and project documentation.