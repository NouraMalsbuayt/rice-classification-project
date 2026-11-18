
# 1. Installing and calling the farff library

install.packages("farff")
library(farff)

# 2. Reading the file
rice_data <- readARFF("data/Rice_Cammeo_Osmancik.arff")

# 3. Verify successful reading and display the dimensions
dim(rice_data)
head(rice_data)
