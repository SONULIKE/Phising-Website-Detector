args <- commandArgs(trailingOnly = TRUE)

# Load required libraries
library(randomForest)
library(readr)

# Load the model
model <- readRDS("C:/coding/cyber crime phising analysis/phishing_model.rds")

# Read the input CSV passed from Python
input_data <- read_csv("C:/coding/cyber crime phising analysis/url_features.csv")

# Make sure column names match and no label is expected
# Make prediction
pred <- predict(model, newdata = input_data, type = "response")
# Print result
cat(pred)
