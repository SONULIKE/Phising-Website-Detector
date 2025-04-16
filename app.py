from flask import Flask, request, render_template, jsonify
import pandas as pd
from feature_extractor import extract_features
import subprocess

app = Flask(__name__)

@app.route("/", methods=["GET", "POST"])
def home():
    prediction = None
    if request.method == "POST":
        url = request.form.get("url")
        features = extract_features(url)

        # Define the column names in the exact order used in the model
        columns = ["id",
        "NumDots", "SubdomainLevel", "PathLevel", "UrlLength", "NumDash", "NumDashInHostname",
        "AtSymbol", "TildeSymbol", "NumUnderscore", "NumPercent", "NumQueryComponents",
        "NumAmpersand", "NumHash", "NumNumericChars", "NoHttps", "RandomString", "IpAddress",
        "DomainInSubdomains", "DomainInPaths", "HttpsInHostname", "HostnameLength", "PathLength",
        "QueryLength", "DoubleSlashInPath", "NumSensitiveWords", "EmbeddedBrandName",
        "PctExtHyperlinks", "PctExtResourceUrls", "ExtFavicon", "InsecureForms", "RelativeFormAction",
        "ExtFormAction", "AbnormalFormAction", "PctNullSelfRedirectHyperlinks",
        "FrequentDomainNameMismatch", "FakeLinkInStatusBar", "RightClickDisabled", "PopUpWindow",
        "SubmitInfoToEmail", "IframeOrFrame", "MissingTitle", "ImagesOnlyInForm", "SubdomainLevelRT",
        "UrlLengthRT", "PctExtResourceUrlsRT", "AbnormalExtFormActionR", "ExtMetaScriptLinkRT",
        "PctExtNullSelfRedirectHyperlinksRT"]

        # Convert to DataFrame
        features_df = pd.DataFrame([features], columns=columns)
        
        '''
        # Add an `id` column as expected by the model
        features_df.insert(0, "id", 1)
        '''
        features_df['PctExtNullSelfRedirectHyperlinksRT'] = features_df['PctExtNullSelfRedirectHyperlinksRT'].replace(-1, 0)
        # Save to CSV
        features_df.to_csv("C:/coding/phising_guard/input_data.csv", index=False)
        
        print("Saved features:\n", features_df.head())  # Already doing this — good!
        print("CSV content:")
        with open("C:/coding/phising_guard/input_data.csv", "r") as f:
            print(f.read())

        # Call R script for prediction
        result = subprocess.run([r"C:/Program Files/R/R-4.4.2/bin/Rscript.exe", "C:/coding/phising_guard/phishing_predict.R", "C:/coding/phising_guard/input_data.csv"],capture_output=True,text=True)
        print("STDOUT from R script:", result.stdout)
        print("STDERR from R script:", result.stderr)
        if result.stdout.strip() == '':
            print("Error: No output received from R script.")
        else:
            try:
                pred = float(result.stdout.strip())
                print(f"Prediction: {pred} (1 = Phishing, 0 = Legitimate)")
                if pred >= 0.5:
                    print("Phishing")
                else:
                    print("Legitimate")

            except ValueError:
                print("Error: Output from R script is not a valid number.")
        
        
        pred = float(result.stdout.strip())
        prediction = "Phishing Website" if pred > 0.5 else "Legitimate Website"

        return render_template("index.html", prediction=prediction, url=url)
    return render_template("index.html")

@app.route("/chatbot", methods=["POST"])
def chatbot():
    user_question = request.form.get("message")
    url = request.form.get("url")

    if "title" in user_question.lower():
        try:
            from bs4 import BeautifulSoup
            import requests
            response = requests.get(url, timeout=5)
            soup = BeautifulSoup(response.text, "html.parser")
            title = soup.title.string if soup.title else "No title found."
            return jsonify({"reply": f"The title of the website is: {title}"})
        except:
            return jsonify({"reply": "Sorry, I couldn’t fetch data from the website."})
    
    return jsonify({"reply": "I'm a simple bot. I can tell you about the title of the site for now."})

if __name__ == "__main__":
    app.run(debug=True)
