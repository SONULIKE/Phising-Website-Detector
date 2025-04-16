'''

import re
from urllib.parse import urlparse

def count_digits(s):
    return sum(c.isdigit() for c in s)

def extract_features(url):
    parsed = urlparse(url)

    hostname = parsed.hostname or ""
    path = parsed.path or ""
    query = parsed.query or ""

    features = {"id":0,
        "NumDots": url.count('.'),
        "SubdomainLevel": hostname.count('.') - 1 if hostname else 0,
        "PathLevel": path.count('/'),
        "UrlLength": len(url),
        "NumDash": url.count('-'),
        "NumDashInHostname": hostname.count('-'),
        "AtSymbol": int('@' in url),
        "TildeSymbol": int('~' in url),
        "NumUnderscore": url.count('_'),
        "NumPercent": url.count('%'),
        "NumQueryComponents": query.count('='),
        "NumAmpersand": url.count('&'),
        "NumHash": url.count('#'),
        "NumNumericChars": count_digits(url),
        "NoHttps": int(not url.startswith("https")),
        "RandomString": 0,  # Placeholder (requires entropy calculation)
        "IpAddress": int(bool(re.match(r'\d+\.\d+\.\d+\.\d+', hostname))),
        "DomainInSubdomains": int('example' in hostname.split('.')[:-2]),  # Placeholder logic
        "DomainInPaths": int('example' in path),  # Placeholder logic
        "HttpsInHostname": int('https' in hostname),
        "HostnameLength": len(hostname),
        "PathLength": len(path),
        "QueryLength": len(query),
        "DoubleSlashInPath": int('//' in path),
        "NumSensitiveWords": sum(word in url.lower() for word in ['secure', 'account', 'webscr', 'login', 'ebayisapi', 'signin', 'banking']),
        "EmbeddedBrandName": 0,  # Placeholder
        "PctExtHyperlinks": 0.0,  # Placeholder
        "PctExtResourceUrls": 0.0,  # Placeholder
        "ExtFavicon": 0,  # Placeholder
        "InsecureForms": 0,  # Placeholder
        "RelativeFormAction": 0,  # Placeholder
        "ExtFormAction": 0,  # Placeholder
        "AbnormalFormAction": 0,  # Placeholder
        "PctNullSelfRedirectHyperlinks": 0.0,  # Placeholder
        "FrequentDomainNameMismatch": 0,  # Placeholder
        "FakeLinkInStatusBar": 0,  # Placeholder
        "RightClickDisabled": 0,  # Placeholder
        "PopUpWindow": 0,  # Placeholder
        "SubmitInfoToEmail": 0,  # Placeholder
        "IframeOrFrame": 0,  # Placeholder
        "MissingTitle": 0,  # Placeholder
        "ImagesOnlyInForm": 0,  # Placeholder
        "SubdomainLevelRT": 1,  # Placeholder
        "UrlLengthRT": 1,  # Placeholder
        "PctExtResourceUrlsRT": 0,  # Placeholder
        "AbnormalExtFormActionR": 1,  # Placeholder
        "ExtMetaScriptLinkRT": 1,  # Placeholder
        "PctExtNullSelfRedirectHyperlinksRT": -1,  # Placeholder
    }

    return list(features.values())

'''


import re
import math
import requests
import ipaddress
from urllib.parse import urlparse
from bs4 import BeautifulSoup

def count_digits(s):
    return sum(c.isdigit() for c in s)

def calculate_entropy(s):
    if not s:
        return 0
    prob = [float(s.count(c)) / len(s) for c in set(s)]
    return round(-sum([p * math.log(p, 2) for p in prob]), 2)

def is_ip(hostname):
    try:
        ipaddress.ip_address(hostname)
        return True
    except:
        return False

def extract_features(url):
    parsed = urlparse(url)
    hostname = parsed.hostname or ""
    path = parsed.path or ""
    query = parsed.query or ""

    # HTML-based flags
    iframe_or_frame = 0
    right_click_disabled = 0
    ext_favicon = 0
    submit_to_email = 0
    missing_title = 0

    try:
        response = requests.get(url, timeout=5, headers={"User-Agent": "Mozilla/5.0"})
        if response.status_code == 200:
            soup = BeautifulSoup(response.text, 'html.parser')

            iframe_or_frame = int(bool(soup.find_all(['iframe', 'frame'])))
            if "event.button==2" in response.text or "contextmenu" in response.text.lower():
                right_click_disabled = 1

            favicon = soup.find("link", rel=lambda x: x and "icon" in x.lower())
            if favicon and "href" in favicon.attrs:
                if hostname not in favicon['href']:
                    ext_favicon = 1

            for form in soup.find_all("form", action=True):
                if "mailto:" in form['action'].lower():
                    submit_to_email = 1
                    break

            title = soup.find("title")
            if not title or not title.text.strip():
                missing_title = 1
    except:
        pass

    features = {
        "id": 0,
        "NumDots": url.count('.'),
        "SubdomainLevel": hostname.count('.') - 1 if hostname else 0,
        "PathLevel": path.count('/'),
        "UrlLength": len(url),
        "NumDash": url.count('-'),
        "NumDashInHostname": hostname.count('-'),
        "AtSymbol": int('@' in url),
        "TildeSymbol": int('~' in url),
        "NumUnderscore": url.count('_'),
        "NumPercent": url.count('%'),
        "NumQueryComponents": query.count('='),
        "NumAmpersand": url.count('&'),
        "NumHash": url.count('#'),
        "NumNumericChars": count_digits(url),
        "NoHttps": int(not url.startswith("https")),
        "RandomString": calculate_entropy(url),
        "IpAddress": int(is_ip(hostname)),
        "DomainInSubdomains": int(any(hostname.split('.')[-2] in part for part in hostname.split('.')[:-2])),
        "DomainInPaths": int(hostname.split('.')[-2] in path),
        "HttpsInHostname": int('https' in hostname),
        "HostnameLength": len(hostname),
        "PathLength": len(path),
        "QueryLength": len(query),
        "DoubleSlashInPath": int('//' in path),
        "NumSensitiveWords": sum(word in url.lower() for word in ['secure', 'account', 'webscr', 'login', 'signin', 'banking']),
        "EmbeddedBrandName": 0,  # Placeholder
        "PctExtHyperlinks": 0.0,  # Placeholder
        "PctExtResourceUrls": 0.0,  # Placeholder
        "ExtFavicon": ext_favicon,
        "InsecureForms": 0,  # Placeholder
        "RelativeFormAction": 0,  # Placeholder
        "ExtFormAction": 0,  # Placeholder
        "AbnormalFormAction": 0,  # Placeholder
        "PctNullSelfRedirectHyperlinks": 0.0,  # Placeholder
        "FrequentDomainNameMismatch": 0,  # Placeholder
        "FakeLinkInStatusBar": 0,  # Placeholder
        "RightClickDisabled": right_click_disabled,
        "PopUpWindow": 0,  # Placeholder
        "SubmitInfoToEmail": submit_to_email,
        "IframeOrFrame": iframe_or_frame,
        "MissingTitle": missing_title,
        "ImagesOnlyInForm": 0,  # Placeholder
        "SubdomainLevelRT": 1,  # Placeholder
        "UrlLengthRT": 1,  # Placeholder
        "PctExtResourceUrlsRT": 0,  # Placeholder
        "AbnormalExtFormActionR": 1,  # Placeholder
        "ExtMetaScriptLinkRT": 1,  # Placeholder
        "PctExtNullSelfRedirectHyperlinksRT": -1  # Placeholder
    }

    return list(features.values())
