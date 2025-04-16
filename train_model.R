# # Set working directory
# setwd("C:/coding/phising_guard")  # Change this to your actual directory
# 
# # Load necessary libraries
# library(caret)
# library(pROC)
# 
# # Load dataset
# data <- read.csv("Phishing_Legitimate_full.csv")
# 
# # Rename target variable
# colnames(data)[colnames(data) == "CLASS_LABEL"] <- "Class"
# 
# # Convert target variable to factor
# data$Class <- as.factor(data$Class)
# 
# # Split data into training (80%) and testing (20%)
# set.seed(42)
# trainIndex <- createDataPartition(data$Class, p = 0.8, list = FALSE)
# train_data <- data[trainIndex, ]
# test_data <- data[-trainIndex, ]
# table(train_data$label)
# # Train logistic regression model
# 
# model <- glm(Class ~ . , family = binomial, data = train_data)
# # 
# # # Save trained model
# # saveRDS(model, "phishing_model.rds")
# 
# summary(model)
# 
# 
# predict(model, newdata = new_df, type = "response")


# Set working directory
setwd("C:/coding/phising_guard")  # Change to your actual path

# Load libraries
library(caret)
library(pROC)
library(dplyr)

# Load dataset
data <- read.csv("Phishing_Legitimate_full.csv")

# Rename and format target
colnames(data)[colnames(data) == "CLASS_LABEL"] <- "Class"
data$Class <- as.factor(data$Class)

# Check class balance
cat("Class balance in dataset:\n")
print(table(data$Class))
print(prop.table(table(data$Class)))

# Handle imbalance (optional: upsampling)
set.seed(42)
trainIndex <- createDataPartition(data$Class, p = 0.8, list = FALSE)
train_data <- data[trainIndex, ]
test_data <- data[-trainIndex, ]

# Optional upsampling
cat("Upsampling...\n")
train_bal <- upSample(x = train_data[, -ncol(train_data)],
                      y = train_data$Class,
                      yname = "Class")

# Scale numeric features
cat("Scaling features...\n")
preProc <- preProcess(train_bal[, -ncol(train_bal)], method = c("center", "scale"))
train_scaled <- predict(preProc, train_bal[, -ncol(train_bal)])
test_scaled <- predict(preProc, test_data[, -ncol(test_data)])

train_final <- cbind(train_scaled, Class = train_bal$Class)
test_final <- cbind(test_scaled, Class = test_data$Class)

# Train logistic regression model
cat("Training model...\n")
model <- glm(Class ~ ., data = train_final, family = binomial, control = list(maxit = 100))


# Save model and preProcess for use in Flask
saveRDS(model, "phishing_model.rds")
saveRDS(preProc, "preprocess_model.rds")

# Evaluate on test set
cat("Evaluating model...\n")
probs <- predict(model, newdata = test_final, type = "response")
preds <- ifelse(probs > 0.5, "1", "0")
confusion <- confusionMatrix(as.factor(preds), test_final$Class)
auc <- roc(test_final$Class, probs)

# Print metrics
print(confusion)
cat("AUC:", auc$auc, "\n")



new_test<-read.csv("legit_test_data.csv")
p<- predict(model, newdata = new_test, type = "response")