from flask import Flask, request, render_template
import subprocess
from extract_features import extract_features


import pandas as pd

app = Flask(__name__)

@app.route("/", methods=["GET", "POST"])
def index():
    prediction = None
    if request.method == "POST":
        url = request.form.get("url")
        features = extract_features(url)
        print("URL received:", url)
        print("Extracted features:", features)
        columns = ["index","having_IPhaving_IP_Address","URLURL_Length","Shortining_Service","having_At_Symbol","double_slash_redirecting","Prefix_Suffix","having_Sub_Domain",
                   "SSLfinal_State","Domain_registeration_length","Favicon","port","HTTPS_token","Request_URL","URL_of_Anchor","Links_in_tags",
                   "SFH","Submitting_to_email","Abnormal_URL","Redirect","on_mouseover","RightClick","popUpWidnow","Iframe","age_of_domain","DNSRecord",
                   "web_traffic","Page_Rank","Google_Index","Links_pointing_to_page","Statistical_report"]

        # Convert to DataFrame
        features_df = pd.DataFrame([features], columns=columns)
       
       
        features_df.to_csv("C:/coding/cyber crime phising analysis/url_features.csv", index=False)
        
        print("Saved features:\n", features_df.head())  # Already doing this — good!
        print("CSV content:")
        with open("C:/coding/cyber crime phising analysis/url_features.csv", "r") as f:
            print(f.read())
        # Call R script
        result = subprocess.run(
            [r"C:/Program Files/R/R-4.4.2/bin/Rscript.exe", "C:/coding/cyber crime phising analysis/model_inference.R", "C:/coding/cyber crime phising analysis/url_features.csv"],
            capture_output=True, text=True
        )
        print("STDOUT from R script:", result.stdout)
        print("STDERR from R script:", result.stderr)
        if result.stdout.strip() == '':
            print("Error: No output received from R script.")
        else:
            try:
                output = result.stdout.strip()
                if not output:
                    raise ValueError("R script returned no output.")
                pred = float(output)
            except ValueError as ve:
                    print(f"Prediction failed: {ve}")
                    pred = -1  # Or handle accordingly

        
        pred = float(result.stdout.strip())
        prediction = "Phishing Website" if pred > 0.5 else "Legitimate Website"

    return render_template("index.html", prediction=prediction)

if __name__ == "__main__":
    app.run(debug=True)
