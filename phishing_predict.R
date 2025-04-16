
# Load required library
library(readr)



# Read input features from CSV
input <- read.csv("C:/coding/phising_guard/input_data.csv")


# Load pre-trained model
model <- readRDS("C:/coding/phising_guard/phishing_model.rds")

# Make prediction
prediction <- predict(model, input,type="response")

# Print output so Flask/Python can capture it
cat(prediction)
