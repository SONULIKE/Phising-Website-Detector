# Phishing Website Detection Model
# This script builds and evaluates a Random Forest model to distinguish between phishing and legitimate websites
# using the dataset.csv file in the current directory
install.packages("randomForest")
install.packages("caret")
install.packages("dplyr")
install.packages("ggplot2")
# Load required libraries
library(randomForest)
library(caret)
library(dplyr)
library(ggplot2)

# ===== MODEL BUILDING AND EVALUATION =====

# Function to build and evaluate the phishing detection model
build_phishing_model <- function(data_path = "dataset.csv", target_column = "Result") {
  # Step 1: Load data
  cat("Loading dataset from", data_path, "...\n")
  phishing_data <- read.csv(data_path)

  # Print basic information about the dataset
  cat("Dataset dimensions:", dim(phishing_data)[1], "rows and",
      dim(phishing_data)[2], "columns\n")
  cat("Column names in dataset:\n")
  print(colnames(phishing_data))

  # Check for missing values
  missing_values <- colSums(is.na(phishing_data))
  if(sum(missing_values) > 0) {
    cat("Warning: Dataset contains missing values!\n")
    print(missing_values[missing_values > 0])
    cat("Removing rows with missing values...\n")
    phishing_data <- na.omit(phishing_data)
  }

  # Step 2: Prepare target variable
  # Check unique values in the target column
  unique_vals <- unique(phishing_data[[target_column]])
  cat("Unique values in target column:", paste(unique_vals, collapse = ", "), "\n")

  # Convert target to factor with informative labels
  # Common encoding: 1 = phishing, -1 or 0 = legitimate
  if(all(unique_vals %in% c(1, -1))) {
    phishing_data[[target_column]] <- factor(phishing_data[[target_column]],
                                             levels = c(-1, 1),
                                             labels = c("legitimate", "phishing"))
  } else if(all(unique_vals %in% c(1, 0))) {
    phishing_data[[target_column]] <- factor(phishing_data[[target_column]],
                                             levels = c(0, 1),
                                             labels = c("legitimate", "phishing"))
  } else {
    # If different encoding, convert to factor without changing labels
    phishing_data[[target_column]] <- factor(phishing_data[[target_column]])
    cat("Note: Using original levels for target variable\n")
  }

  # Print class distribution
  class_dist <- table(phishing_data[[target_column]])
  cat("Class distribution:\n")
  print(class_dist)

  # Step 3: Split data into training and testing sets
  set.seed(123)  # For reproducibility
  train_index <- createDataPartition(phishing_data[[target_column]], p = 0.7, list = FALSE)
  train_data <- phishing_data[train_index, ]
  test_data <- phishing_data[-train_index, ]

  cat("Training set size:", nrow(train_data), "samples\n")
  cat("Testing set size:", nrow(test_data), "samples\n")

  # Step 4: Train Random Forest model
  cat("\nTraining Random Forest model...\n")
  formula <- as.formula(paste(target_column, "~ ."))

  model <- randomForest(formula,
                        data = train_data,
                        ntree = 200,
                        importance = TRUE)

  # Print model summary
  cat("\nModel Summary:\n")
  print(model)

  # Step 5: Evaluate model on test data
  cat("\nEvaluating model on test data...\n")
  predictions <- predict(model, test_data)
  conf_matrix <- confusionMatrix(predictions, test_data[[target_column]])

  cat("Confusion Matrix:\n")
  print(conf_matrix$table)
  cat("\nModel Performance Metrics:\n")
  cat("Accuracy:", round(conf_matrix$overall["Accuracy"] * 100, 2), "%\n")
  cat("Sensitivity (True Positive Rate):", round(conf_matrix$byClass["Sensitivity"] * 100, 2), "%\n")
  cat("Specificity (True Negative Rate):", round(conf_matrix$byClass["Specificity"] * 100, 2), "%\n")
  cat("Precision (Positive Predictive Value):", round(conf_matrix$byClass["Pos Pred Value"] * 100, 2), "%\n")
  cat("F1 Score:", round(conf_matrix$byClass["F1"] * 100, 2), "%\n")

  # Step 6: Feature importance
  cat("\nFeature Importance (Top 10):\n")
  importance_df <- as.data.frame(importance(model))
  importance_df$Feature <- rownames(importance_df)
  importance_df <- importance_df[order(importance_df$MeanDecreaseGini, decreasing = TRUE), ]
  print(head(importance_df[, c("Feature", "MeanDecreaseGini")], 10))

  # Create feature importance plot
  cat("\nCreating feature importance plot...\n")
  top_features <- head(importance_df, 15)

  importance_plot <- ggplot(top_features, aes(x = reorder(Feature, MeanDecreaseGini), y = MeanDecreaseGini)) +
    geom_bar(stat = "identity", fill = "steelblue") +
    coord_flip() +
    theme_minimal() +
    labs(x = "Features", y = "Importance (Mean Decrease in Gini)",
         title = "Top 15 Most Important Features for Phishing Detection")

  print(importance_plot)

  # Save plot
  ggsave("feature_importance_plot.png", importance_plot, width = 10, height = 6, dpi = 300)
  cat("Feature importance plot saved as 'feature_importance_plot.png'\n")

  # Return model and evaluation results
  return(list(
    model = model,
    confusion_matrix = conf_matrix,
    feature_importance = importance_df,
    test_data = test_data,
    importance_plot = importance_plot
  ))
}

# ===== MODEL EXPORT FUNCTIONS =====

# Function to save the model and related information for your teammate
export_model <- function(model_results, output_dir = ".") {
  # Create directory if it doesn't exist
  if (!dir.exists(output_dir)) {
    dir.create(output_dir, recursive = TRUE)
  }

  # Save the model
  model_path <- file.path(output_dir, "phishing_detection_model.rds")
  saveRDS(model_results$model, model_path)

  # Save feature importance
  importance_path <- file.path(output_dir, "feature_importance.csv")
  write.csv(model_results$feature_importance, importance_path, row.names = FALSE)

  # Create a simple documentation file
  doc_path <- file.path(output_dir, "model_documentation.txt")
  sink(doc_path)
  cat("PHISHING WEBSITE DETECTION MODEL\n")
  cat("================================\n\n")
  cat("Model Type: Random Forest\n")
  cat("Created On:", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "\n\n")
  cat("Performance Metrics:\n")
  cat("- Accuracy:", round(model_results$confusion_matrix$overall["Accuracy"] * 100, 2), "%\n")
  cat("- Sensitivity:", round(model_results$confusion_matrix$byClass["Sensitivity"] * 100, 2), "%\n")
  cat("- Specificity:", round(model_results$confusion_matrix$byClass["Specificity"] * 100, 2), "%\n")
  cat("- Precision:", round(model_results$confusion_matrix$byClass["Pos Pred Value"] * 100, 2), "%\n")
  cat("- F1 Score:", round(model_results$confusion_matrix$byClass["F1"] * 100, 2), "%\n\n")
  cat("Top 5 Most Important Features:\n")
  for (i in 1:5) {
    cat(i, ". ", model_results$feature_importance$Feature[i],
        " (Importance: ", round(model_results$feature_importance$MeanDecreaseGini[i], 4), ")\n", sep="")
  }
  cat("\n")
  cat("For using this model with new data, ensure the input features match those used during training.\n")
  cat("See the feature_importance.csv file for the complete list of features and their importance.\n")
  sink()

  cat("Model and related files saved to", output_dir, "\n")
  cat("- Model: phishing_detection_model.rds\n")
  cat("- Feature Importance: feature_importance.csv\n")
  cat("- Documentation: model_documentation.txt\n")

  return(list(
    model_path = model_path,
    importance_path = importance_path,
    doc_path = doc_path
  ))
}

# ===== TEST MODEL FUNCTION =====

# Function to test the model with sample data
test_model_with_samples <- function(model, sample_count = 5, test_data = NULL) {
  cat("\nTesting the model with", sample_count, "random samples from test data...\n")

  if (is.null(test_data) || nrow(test_data) == 0) {
    stop("No test data provided. Cannot run sample tests.")
  }

  # Select random samples
  set.seed(42) # For reproducible samples
  sample_indices <- sample(1:nrow(test_data), min(sample_count, nrow(test_data)))
  samples <- test_data[sample_indices, ]

  # Make predictions
  cat("\nSample Test Results:\n")
  cat("====================\n")

  for (i in 1:nrow(samples)) {
    # Get actual class
    actual_class <- samples$Result[i]

    # Remove the Result column for prediction
    sample_features <- samples[i, names(samples) != "Result", drop = FALSE]

    # Make prediction
    prediction <- predict(model, sample_features, type = "class")
    prediction_prob <- predict(model, sample_features, type = "prob")

    # Display results
    cat("\nSample", i, ":\n")
    cat("Actual class: ", as.character(actual_class), "\n", sep="")
    cat("Predicted class: ", as.character(prediction), "\n", sep="")
    cat("Prediction probabilities:\n")
    print(prediction_prob)
    cat("Correct prediction:", prediction == actual_class, "\n")

    # Show a few key features of this sample
    imp_features <- rownames(importance(model))[1:5]  # Top 5 important features
    cat("Key feature values:\n")
    for (feat in imp_features) {
      if (feat %in% names(sample_features)) {
        cat("- ", feat, ": ", sample_features[[feat]], "\n", sep="")
      }
    }
    cat("----------------------------------\n")
  }
}

# ===== PREDICTION FUNCTION FOR YOUR TEAMMATE =====

# This is the function your teammate will use with newly extracted features
predict_website_type <- function(feature_vector, model_path = "phishing_detection_model.rds") {
  # Load the saved model
  if (!file.exists(model_path)) {
    stop("Model file not found at: ", model_path)
  }

  model <- readRDS(model_path)

  # Get expected feature names from the model
  if (inherits(model, "randomForest")) {
    expected_features <- setdiff(colnames(model$forest$xlevels), "Result")
  } else {
    # Fallback approach if feature names can't be extracted directly
    warning("Could not extract feature names directly from model. Using standard feature names.")
    expected_features <- c("having_IP_Address", "URL_Length", "Shortining_Service", "having_At_Symbol",
                           "double_slash_redirecting", "Prefix_Suffix", "having_Sub_Domain",
                           "SSLfinal_State", "Domain_registeration_length", "Favicon", "port",
                           "HTTPS_token", "Request_URL", "URL_of_Anchor", "Links_in_tags",
                           "SFH", "Submitting_to_email", "Abnormal_URL", "Redirect",
                           "on_mouseover", "RightClick", "popUpWidnow", "Iframe",
                           "age_of_domain", "DNSRecord", "web_traffic", "Page_Rank",
                           "Google_Index", "Links_pointing_to_page", "Statistical_report")
  }

  # Check if feature vector has the correct format
  if (length(feature_vector) != length(expected_features)) {
    stop("Feature vector has ", length(feature_vector),
         " elements but model expects ", length(expected_features), " features.")
  }

  # Create a data frame with the correct feature names
  website_data <- as.data.frame(t(feature_vector))
  colnames(website_data) <- expected_features

  # Make prediction
  prediction_class <- predict(model, website_data, type = "class")
  prediction_prob <- predict(model, website_data, type = "prob")

  return(list(
    prediction = prediction_class,
    probabilities = prediction_prob,
    is_phishing = prediction_class == "phishing"
  ))
}

# ===== EXAMPLE FUNCTION TO MANUALLY TEST THE MODEL =====

# Function to demonstrate how to use the model with a manually crafted example
test_with_manual_example <- function(model) {
  cat("\nTesting model with manually crafted examples...\n")

  # 1. Get the ACTUAL feature names from YOUR model
  feature_names <- names(model$forest$xlevels)
  feature_names <- feature_names[feature_names != "Result"]  # Remove target variable
  cat("Model expects these features:\n")
  print(feature_names)

  # 2. Create test data that MATCHES YOUR MODEL'S FEATURES EXACTLY
  # Note: Using the exact feature names from your model output
  test_data <- data.frame(
    index = 1,  # Adding index since your model expects it
    having_IPhaving_IP_Address = -1,
    URLURL_Length = 1,
    Shortining_Service = -1,
    having_At_Symbol = -1,
    double_slash_redirecting = -1,
    Prefix_Suffix = -1,
    having_Sub_Domain = -1,
    SSLfinal_State = 1,
    Domain_registeration_length = 1,
    Favicon = 1,
    port = 1,
    HTTPS_token = 1,
    Request_URL = 1,
    URL_of_Anchor = 1,
    Links_in_tags = 1,
    SFH = 1,
    Submitting_to_email = -1,
    Abnormal_URL = -1,
    Redirect = -1,
    on_mouseover = -1,
    RightClick = 1,
    popUpWidnow = -1,
    Iframe = -1,
    age_of_domain = 1,
    DNSRecord = 1,
    web_traffic = 1,
    Page_Rank = 1,
    Google_Index = 1,
    Links_pointing_to_page = 1,
    Statistical_report = -1
  )

  # 3. Select ONLY the features your model needs
  test_data <- test_data[, feature_names]

  # 4. Create phishing example (invert values)
  phishing_data <- test_data
  phishing_data[] <- lapply(phishing_data, function(x) ifelse(x == 1, -1, 1))

  # 5. Test legitimate example
  cat("\nTesting legitimate website features:\n")
  legitimate_pred <- predict(model, newdata = test_data)
  legitimate_prob <- predict(model, newdata = test_data, type = "prob")
  cat("Prediction:", as.character(legitimate_pred), "\n")
  cat("Probabilities:\n")
  print(legitimate_prob)

  # 6. Test phishing example
  cat("\nTesting phishing website features:\n")
  phishing_pred <- predict(model, newdata = phishing_data)
  phishing_prob <- predict(model, newdata = phishing_data, type = "prob")
  cat("Prediction:", as.character(phishing_pred), "\n")
  cat("Probabilities:\n")
  print(phishing_prob)

  return(list(
    legitimate = list(prediction = legitimate_pred, probabilities = legitimate_prob),
    phishing = list(prediction = phishing_pred, probabilities = phishing_prob)
  ))
}
# Function to directly test the model without saving it
test_model_directly <- function(model, test_data) {
  # Run standard tests
  test_model_with_samples(model, sample_count = 3, test_data = test_data)

  # Run manual examples test
  test_with_manual_example(model)
}

# ===== JUST RUN EVERYTHING AUTOMATICALLY =====

# Build and evaluate the model
cat("=== PHISHING WEBSITE DETECTION MODEL ===\n\n")
cat("Building model from dataset.csv...\n")
model_results <- build_phishing_model()

# Test the model
cat("\n=== MODEL TESTING ===\n")
test_model_directly(model_results$model, model_results$test_data)

# Export the model for your teammate
cat("\n=== EXPORTING MODEL FOR TEAMMATE ===\n")
export_paths <- export_model(model_results)

cat("\n=== INSTRUCTIONS FOR YOUR TEAMMATE ===\n")
cat("1. Use the 'phishing_detection_model.rds' file in your feature extraction code\n")
cat("2. Import the model with: model <- readRDS('phishing_detection_model.rds')\n")
cat("3. Use the predict_website_type() function to classify websites\n")
cat("4. See model_documentation.txt for details on model performance\n")

cat("\nProcess complete! You can now share the files with your teammate.\n")









# ===== 100% WORKING TEST CODE =====
cat("\n=== FINAL WORKING TEST ===\n")

test_website <- function(model, url = "www.google.com") {
  # 1. Get ACTUAL feature names from YOUR model
  required_features <- names(model$forest$xlevels)
  required_features <- required_features[required_features != "Result"]

  cat("Your model requires these features:\n")
  print(required_features)

  # 2. Create list of ALL possible features with default legitimate values
  all_features <- list(
    index = 0,
    having_IPhaving_IP_Address = -1,         # Google uses domain, not IP
    URLURL_Length = -1,                       # Short domain = safe
    Shortining_Service = -1,                  # Not a shortened URL
    having_At_Symbol = -1,                    # No '@'
    double_slash_redirecting = -1,
    Prefix_Suffix = -1,
    having_Sub_Domain = -1,                   # google.com is root
    SSLfinal_State = 1,                       # Valid SSL
    Domain_registeration_length = 1,          # Long-term domain
    Favicon = 1,
    port = -1,                                 # Default port used
    HTTPS_token = -1,
    Request_URL = -1,                          # Most requests are on same domain
    URL_of_Anchor = -1,                        # Safe anchor usage
    Links_in_tags = -1,
    SFH = -1,
    Submitting_to_email = -1,
    Abnormal_URL = -1,
    Redirect = -1,
    on_mouseover = -1,
    RightClick = -1,
    popUpWidnow = -1,
    Iframe = -1,
    age_of_domain = 1,
    DNSRecord = 1,
    web_traffic = 1,                          # High traffic
    Page_Rank = 1,
    Google_Index = 1,
    Links_pointing_to_page = 1,
    Statistical_report = -1
  )


  # 3. Match EXACT feature names (case-sensitive)
  matched_features <- all_features[names(all_features) %in% required_features]

  # 4. Create test data with PROPER column order
  test_data <- as.data.frame(matched_features)[required_features]

  # 5. Make prediction
  pred <- predict(model, newdata = test_data)
  prob <- predict(model, newdata = test_data, type = "prob")

  # 6. Print results
  cat("\n=== PREDICTION RESULTS ===\n")
  cat("URL:", url, "\n")
  cat("Prediction:", as.character(pred), "\n")
  cat("Phishing Probability:", round(prob[,"phishing"], 5), "\n")
  cat("Legitimate Probability:", round(prob[,"legitimate"], 5), "\n")

  return(list(
    url = url,
    prediction = pred,
    probabilities = prob,
    features_used = test_data
  ))
}

# RUN THE TEST
result <- test_website(model_results$model)

# ===== END TEST CODE =====
