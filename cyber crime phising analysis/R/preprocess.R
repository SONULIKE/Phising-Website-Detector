# Load necessary libraries
library(caret)

# Load your actual data (replace with the correct path to your data file)
data <- read.csv("C:/coding/cyber crime phising analysis/dataset.csv")

# Ensure your data only includes the features, and exclude any labels or target columns
# For example, if your data has a 'target' column for classification, remove it
data_features <- data[, -which(names(data) == "target")]  # Replace "target" with the actual label column name, if any.

# Preprocess the data (centering and scaling)
preProcValues <- preProcess(data_features, method = c("center", "scale"))

# Save the preprocessing model to a .rds file
saveRDS(preProcValues, file = "C:/coding/cyber crime phising analysis/R/preprocess_model.rds")

# Output confirmation
print("Preprocessing model saved as 'preprocess_model.rds'.")
