# phishing_predict.R
# Debugging: Print the input features
cat("Received features:\n")
print(commandArgs(trailingOnly = TRUE))

# Load necessary libraries
suppressWarnings({
  suppressMessages({
    library(caret)
  })
})

# Read arguments
args <- commandArgs(trailingOnly = TRUE)

# Check if we received all 49 features
if (length(args) != 49) {
  stop("Exactly 49 features are required.")
}

# Convert to numeric
features <- as.numeric(args)

# Load models
model <- readRDS("C:/coding/cyber crime phising analysis/R/phishing_model.rds")
preprocess <- readRDS("C:/coding/cyber crime phising analysis/R/preprocess_model.rds")

# Prepare data
features_df <- as.data.frame(t(features))
colnames(features_df) <- colnames(preprocess$mean)  # This assumes preprocessing is from caret::preProcess

# Apply preprocessing
processed <- predict(preprocess, features_df)

# Predict
prediction <- predict(model, processed)

# Output result
cat(prediction)
