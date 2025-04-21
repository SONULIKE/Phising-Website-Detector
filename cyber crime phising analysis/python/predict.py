import pandas as pd
from extract_features import extract_features
import rpy2.robjects as robjects
from rpy2.robjects import pandas2ri
import os

# Add your actual R installation path here
os.environ['R_HOME'] = r"C:/PROGRA~1/R/R-44~1.2"  # change this to your R version path
os.environ['R_USER'] = os.environ['USERPROFILE']  # optional, avoids permission issues

# Now import rpy2
import rpy2.robjects as robjects

# Your code continues...

# Load and prepare
url = input("Enter a URL: ")
features = extract_features(url)
df = pd.DataFrame([features])

# Activate automatic conversion
pandas2ri.activate()

# Load R model and predict
robjects.r('model <- readRDS("R/phishing_model.rds")')
robjects.globalenv['newdata'] = df

# Predict
prediction = robjects.r('predict(model, newdata, type="class")')
print(f"Prediction for '{url}':", prediction[0])
