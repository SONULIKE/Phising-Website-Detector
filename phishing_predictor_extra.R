# # Phishing Attack Analysis Using R
# 
# # Load necessary libraries
# library(readr)
# library(dplyr)
# library(ggplot2)
# library(corrplot)
# library(caret)
# library(randomForest)
# library(neuralnet)
# 
# # ----------------------- PHASE 1: DATA LOADING & PREPROCESSING -----------------------
# 
# # Load the dataset
# phishing_data <- read.csv("Phishing_Legitimate_full.csv")
# 
# # View first few rows
# head(phishing_data)
# 
# # Check for missing values
# sum(is.na(phishing_data))
# 
# # Remove missing values
# phishing_data <- na.omit(phishing_data)
# 
# # Convert categorical variables
# phishing_data$Class <- as.factor(phishing_data$Class)
# 
# # Add feature: URL length
# phishing_data$url_length <- nchar(phishing_data$URL)
# 
# # ----------------------- PHASE 2: EXPLORATORY DATA ANALYSIS (EDA) -----------------------
# 
# # Summary statistics
# summary(phishing_data)
# 
# # Class distribution
# ggplot(phishing_data, aes(x = factor(Class))) + 
#   geom_bar(fill = "blue") + 
#   labs(title = "Phishing vs Legitimate Websites", x = "Class", y = "Count")
# 
# # Correlation matrix (numeric features)
# numeric_data <- select(phishing_data, where(is.numeric))
# corr_matrix <- cor(numeric_data)
# corrplot(corr_matrix, method = "circle")
# 
# # ----------------------- PHASE 3: TRAINING MACHINE LEARNING MODELS -----------------------
# 
# # Split data into training and testing
# set.seed(123)
# trainIndex <- createDataPartition(phishing_data$Class, p = 0.8, list = FALSE)
# train_data <- phishing_data[trainIndex, ]
# test_data <- phishing_data[-trainIndex, ]
# 
# # Train Logistic Regression Model
# model <- glm(Class ~ ., data = train_data, family = binomial)
# summary(model)
# 
# # Ensure 'URL' is removed from both datasets
# test_data <- test_data[, !colnames(test_data) %in% c("URL")]
# train_data <- train_data[, !colnames(train_data) %in% c("URL")]
# 
# 
# library(ROSE)
# 
# train_data <- ROSE(Class ~ ., data = train_data, seed = 42)$data
# 
# 
# # Ensure 'Class' is numeric
# train_data$Class <- as.numeric(as.character(train_data$Class))
# test_data$Class <- as.numeric(as.character(test_data$Class))
# 
# # Make predictions and evaluate
# predictions <- predict(model, test_data, type = "response")
# predicted_class <- ifelse(predictions > 0.5, 1, 0)
# confusionMatrix(factor(predicted_class), test_data$Class)
# 
# # Train Random Forest Model
# rf_model <- randomForest(Class ~ ., data = train_data, ntree = 100)
# rf_predictions <- predict(rf_model, test_data)
# confusionMatrix(rf_predictions, test_data$Class)
# 
# # Feature Importance (Random Forest)
# varImpPlot(rf_model)
# 
# # ----------------------- PHASE 4: ADVANCED DEEP LEARNING MODEL -----------------------
# 
# # Normalize data for Neural Network
# normalize <- function(x) {(x - min(x)) / (max(x) - min(x))}
# norm_data <- as.data.frame(lapply(select(phishing_data, -Class, -URL), normalize))
# norm_data$Class <- phishing_data$Class
# 
# # Split Data
# train_nn <- norm_data[trainIndex, ]
# test_nn <- norm_data[-trainIndex, ]
# 
# # Train Neural Network Model
# nn_model <- neuralnet(Class ~ ., data = train_nn, hidden = c(5,3), linear.output = FALSE)
# plot(nn_model)
# 
# # Make Predictions
# nn_predictions <- compute(nn_model, test_nn[,-ncol(test_nn)])
# nn_pred_class <- ifelse(nn_predictions$net.result > 0.5, 1, 0)
# confusionMatrix(factor(nn_pred_class), test_nn$Class)
# 
# # ----------------------- PHASE 5: REPORTING & VISUALIZATION -----------------------
# 
# # Generate Markdown Report
# library(rmarkdown)
# render("Phishing_Analysis_Report.Rmd")
# 
# # Save the final dataset
# write.csv(phishing_data, "processed_phishing_data.csv")
# 
# # End of Script
# 
# 
# # Load necessary libraries
# library(caret)
# 
# # Load the dataset
# phishing_data <- read.csv("Phishing_Legitimate_full.csv")  # Change filename as needed
# 
# # Rename target variable
# colnames(phishing_data)[colnames(phishing_data) == "CLASS_LABEL"] <- "Class"
# 
# # Convert target variable to factor
# phishing_data$Class <- as.factor(phishing_data$Class)
# 
# # Verify class distribution
# table(phishing_data$Class)
# 
# # Split data into training (80%) and testing (20%)
# set.seed(42)
# trainIndex <- createDataPartition(phishing_data$Class, p = 0.8, list = FALSE)
# train_data <- phishing_data[trainIndex, ]
# test_data <- phishing_data[-trainIndex, ]
# 
# # Train a logistic regression model
# model <- glm(Class ~ ., family = binomial, data = train_data)
# 
# # Summarize model
# summary(model)
# 
# # Make predictions
# predictions <- predict(model, newdata = test_data, type = "response")
# 
# # Convert probabilities to class labels
# predicted_class <- ifelse(predictions > 0.5, 1, 0)
# predicted_class <- factor(predicted_class, levels = levels(test_data$Class))
# 
# # Evaluate model performance
# conf_matrix <- confusionMatrix(predicted_class, test_data$Class)
# print(conf_matrix)
# 
# 
# #Set directory path
# setwd("C:/coding/cyber crime phising analysis")
# getwd()
# # Load necessary libraries
# library(caret)  # For data splitting and model evaluation
# library(ggplot2) 
#  # For plotting
# library(PRROC) # For Precision-Recall Curve
# library(pROC)  # For ROC curve
# 
# # Load and preprocess the data
# phishing_data <- read.csv("Phishing_Legitimate_full.csv")
# 
# # Rename target variable
# colnames(phishing_data)[colnames(phishing_data) == "CLASS_LABEL"] <- "Class"
# 
# # Convert target variable to factor
# phishing_data$Class <- as.factor(phishing_data$Class)
# 
# # Verify class distribution
# table(phishing_data$Class)
# 
# # Split data into training (80%) and testing (20%)
# set.seed(42)
# trainIndex <- createDataPartition(phishing_data$Class, p = 0.8, list = FALSE)
# train_data <- phishing_data[trainIndex, ]
# test_data <- phishing_data[-trainIndex, ]
# 
# # Train a logistic regression model
# model <- glm(Class ~ ., family = binomial, data = train_data)
# 
# # Summarize model
# summary(model)
# 
# # Make predictions
# predictions <- predict(model, newdata = test_data, type = "response")
# 
# # Convert probabilities to class labels
# predicted_class <- ifelse(predictions > 0.5, 1, 0)
# predicted_class <- factor(predicted_class, levels = levels(test_data$Class))
# 
# # Evaluate model performance
# conf_matrix <- confusionMatrix(predicted_class, test_data$Class)
# print(conf_matrix)
# 
# # Plot Confusion Matrix
# # Convert confusion matrix to a data frame for plotting
# cm_table <- as.data.frame(conf_matrix$table)
# colnames(cm_table) <- c("Reference", "Prediction", "Freq")
# 
# # Plot confusion matrix
# ggplot(data = cm_table, aes(x = Reference, y = Prediction, fill = Freq)) +
#   geom_tile() +
#   geom_text(aes(label = Freq), color = "white", size = 6) +
#   scale_fill_gradient(low = "blue", high = "red") +
#   ggtitle("Confusion Matrix") +
#   theme_minimal()
# 
# 
# # ROC Curve
# roc_curve <- roc(as.numeric(test_data$Class) - 1, predictions)
# plot(roc_curve, col = "blue", main = "ROC Curve")
# abline(a = 0, b = 1, lty = 2, col = "red")  # Random classifier line
# auc_score <- auc(roc_curve)
# print(paste("AUC Score:", auc_score))
# 
# # Precision-Recall Curve
# # Compute Precision-Recall Curve
# pr <- pr.curve(scores.class0 = predictions, weights.class0 = as.numeric(test_data$Class) - 1, curve = TRUE)
# 
# # Plot Precision-Recall Curve
# plot(pr$curve[, 1], pr$curve[, 2], type = "l", col = "green",
#      xlab = "Recall", ylab = "Precision", main = "Precision-Recall Curve")
# 
# # Assuming model is your trained logistic regression model
# saveRDS(model, file = "phishing_model.rds")
# 
# 
# 
# Set working directory (Ensure it's correct)
setwd("C:/coding/phising_guard")

# Load necessary libraries
library(caret)

# Load the trained model
model <- readRDS("phishing_model.rds")

# Read the input CSV provided by Flask
args <- commandArgs(trailingOnly = TRUE)
input_file <- args[1]

# Load input data
input_data <- read.csv(input_file)

# Ensure the same preprocessing steps (Factor conversion)
input_data$Class <- as.factor(input_data$Class)

# Make predictions
predictions <- predict(model, newdata = input_data, type = "response")

# Convert probability to binary class (1 = Phishing, 0 = Legitimate)
predicted_class <- ifelse(predictions > 0.5, 1, 0)

# Print the result (important: single numeric output)
cat(predicted_class)

