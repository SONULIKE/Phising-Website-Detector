from flask import Flask, request, render_template
import subprocess
import os
import pandas as pd
from extract_features import extract_features

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

        features_df = pd.DataFrame([features], columns=columns)

        # Save the CSV
        features_df.to_csv("url_features.csv", index=False)

        print("Saved features:\n", features_df.head())

        # Call the R script
        result = subprocess.run(
            [r"C:/Program Files/R/R-4.4.3/bin/Rscript.exe", 
              "model_inference.R","url_features.csv"
             ],
            capture_output=True, text=True
        )

        # Capture the output from the R script
        output = result.stdout.strip()
        print("Raw output from R script:", output)

        try:
            # Convert the output to a float
            pred = float(output)
            
            # Map the output to prediction labels
            if pred == 2:
                prediction = "Phishing Website"
            elif pred == 1 or pred == 0:
                prediction = "Legitimate Website"
            else:
                prediction = "Error: Unexpected model output."
        except ValueError:
            print(f"Unexpected output from R script: {output}")
            prediction = "Error: Could not get prediction from model."

    return render_template("index.html", prediction=prediction)


if __name__ == "__main__":
    app.run(debug=True)
