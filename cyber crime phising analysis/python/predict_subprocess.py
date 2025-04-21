import subprocess

def get_features_from_python(url):
    result = subprocess.run(
        ["python", "C:/coding/cyber crime phising analysis/python/extract_features.py", url],
        capture_output=True,
        text=True
    )
    
    # Raw output from extract_features.py (before splitting into list)
    print("Raw output from extract_features.py:")
    print(result.stdout.strip())
    
    # Split the output into a list of features
    features = result.stdout.strip().split()  # Split into list
    print("Features as list after splitting:")
    print(features)  # Print the list of features
    
    # Trim to exactly 49 features if necessary
    features = features[:49]
    
    print(f"Trimmed features to 49: {features}")
    
    return features

def predict_with_r(features):
    print(f"Features passed to R script: {features}")  # Debug: Print the features passed to the R script
    
    if len(features) != 49:
        print(f"Error: Expected 49 features, got {len(features)}")
        return

    # Now, the features are passed as a list, so we directly use them in the command
    cmd = ["Rscript", "C:/coding/cyber crime phising analysis//R/phishing_predict.R"] + features  # List is unpacked directly into the command
    r_output = subprocess.run(cmd, capture_output=True, text=True)
    
    print("Prediction Output:")
    print(r_output.stdout.strip())
    
    if r_output.stderr:
        print("R Error:")
        print(r_output.stderr.strip())

if __name__ == "__main__":
    url = input("Enter a URL: ")
    features = get_features_from_python(url)  # Now features is a list
    predict_with_r(features)

