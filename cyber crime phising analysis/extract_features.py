import re
import socket
import urllib.request
from urllib.parse import urlparse
from bs4 import BeautifulSoup
import whois
import datetime
import requests

def has_ip(url):
    try:
        ip = re.findall(r'[0-9]+(?:\.[0-9]+){3}', url)
        return 1 if ip else -1
    except:
        return -1

def url_length(url):
    return 1 if len(url) < 54 else (0 if len(url) <= 75 else -1)

def is_shortened(url):
    shortening_services = r"bit\.ly|goo\.gl|shorte\.st|go2l\.ink|x\.co|ow\.ly|tinyurl|tr\.im|is\.gd|cli\.gs|yfrog\.com|migre\.me|ff\.im|tiny\.cc|url4\.eu|twit\.ac|su\.pr|twurl\.nl|snipurl\.com|short\.to|BudURL\.com|ping\.fm|post\.ly|Just\.as|bkite\.com|snipr\.com|fic\.kr|loopt\.us|doiop\.com|short\.ie|kl\.am|wp\.me|rubyurl\.com|om\.ly|to\.ly|bit\.do|t\.co|lnkd\.in|db\.tt|qr\.ae|adf\.ly|bitly\.com|cur\.lv|tinyurl\.com|ow\.ly|bit\.ly|ity\.im|q\.gs|is\.gd|po\.st|bc\.vc|twitthis\.com|u\.to|j\.mp|buzurl\.com|cutt\.us|u\.bb|yourls\.org|prettylinkpro\.com|scrnch\.me|filoops\.info|vzturl\.com|qr\.net|1url\.com|tweez\.me|v\.gd|tr\.im|link\.zip\.net"
    return 1 if re.search(shortening_services, url) else -1

def has_at_symbol(url):
    return 1 if "@" in url else -1

def double_slash_redirecting(url):
    return 1 if url.rfind("//") > 6 else -1

def prefix_suffix(url):
    return -1 if '-' in urlparse(url).netloc else 1

def having_sub_domain(url):
    domain = urlparse(url).netloc
    dots = domain.split('.')
    if len(dots) <= 3:
        return 1
    elif len(dots) == 4:
        return 0
    else:
        return -1

def ssl_final_state(url):
    try:
        if re.match(r"^https", url):
            return 1
        else:
            return -1
    except:
        return -1

def domain_registration_length(domain):
    try:
        expiration_date = domain.expiration_date
        today = datetime.datetime.now()
        if isinstance(expiration_date, list):
            expiration_date = expiration_date[0]
        age = (expiration_date - today).days
        return 1 if age >= 365 else -1
    except:
        return -1

def has_favicon(url):
    try:
        soup = BeautifulSoup(requests.get(url, timeout=5).text, "html.parser")
        for link in soup.find_all('link', rel='shortcut icon'):
            if urlparse(link['href']).netloc != urlparse(url).netloc:
                return -1
        return 1
    except:
        return -1

def port_check(url):
    try:
        port = urlparse(url).port
        return -1 if port and port not in [80, 443] else 1
    except:
        return -1

def https_token(url):
    domain = urlparse(url).netloc
    return -1 if 'https' in domain else 1

def request_url(url):
    try:
        response = requests.get(url, timeout=5)
        soup = BeautifulSoup(response.text, 'html.parser')
        imgs = soup.find_all('img', src=True)
        total = len(imgs)
        valid = 0
        for img in imgs:
            src = img['src']
            if urlparse(src).netloc == '' or urlparse(src).netloc in url:
                valid += 1
        percent = valid / total if total > 0 else 1
        return 1 if percent > 0.6 else (0 if percent >= 0.3 else -1)
    except:
        return -1

def url_of_anchor(url):
    try:
        response = requests.get(url, timeout=5)
        soup = BeautifulSoup(response.text, 'html.parser')
        anchors = soup.find_all('a', href=True)
        total = len(anchors)
        valid = 0
        for anchor in anchors:
            href = anchor['href']
            if not href.startswith('#') and not re.search('javascript:void', href) and urlparse(href).netloc in url:
                valid += 1
        percent = valid / total if total > 0 else 1
        return 1 if percent > 0.6 else (0 if percent >= 0.3 else -1)
    except:
        return -1

def links_in_tags(url):
    try:
        response = requests.get(url, timeout=5)
        soup = BeautifulSoup(response.text, 'html.parser')
        meta = soup.find_all('meta')
        link = soup.find_all('link')
        script = soup.find_all('script')
        tags = meta + link + script
        total = len(tags)
        valid = 0
        for tag in tags:
            try:
                if tag.has_attr('href') or tag.has_attr('src'):
                    attr = tag.get('href') or tag.get('src')
                    if urlparse(attr).netloc in url:
                        valid += 1
            except:
                continue
        percent = valid / total if total > 0 else 1
        return 1 if percent > 0.6 else (0 if percent >= 0.3 else -1)
    except:
        return -1

def sfh_check(url):
    try:
        soup = BeautifulSoup(requests.get(url, timeout=5).text, 'html.parser')
        forms = soup.find_all('form', action=True)
        for form in forms:
            action = form['action']
            if action == "" or action == "about:blank":
                return -1
            elif urlparse(action).netloc not in url:
                return 0
        return 1
    except:
        return -1

def submitting_to_email(url):
    try:
        soup = BeautifulSoup(requests.get(url, timeout=5).text, 'html.parser')
        if re.findall(r"[mailto:?]", soup.text):
            return 1
        else:
            return -1
    except:
        return -1

def abnormal_url(url):
    try:
        whois_res = whois.whois(urlparse(url).netloc)
        return 1 if whois_res.domain_name else -1
    except:
        return -1

def redirect_count(url):
    try:
        response = requests.get(url, timeout=5)
        return 1 if len(response.history) <= 1 else (0 if len(response.history) <= 4 else -1)
    except:
        return -1

def mouse_over(url):
    try:
        soup = BeautifulSoup(requests.get(url, timeout=5).text, 'html.parser')
        if re.findall(r"onmouseover", str(soup)):
            return 1
        else:
            return -1
    except:
        return -1

def right_click(url):
    try:
        soup = BeautifulSoup(requests.get(url, timeout=5).text, 'html.parser')
        if re.findall(r"event.button ?== ?2", str(soup)):
            return 1
        else:
            return -1
    except:
        return -1

def popup_window(url):
    try:
        soup = BeautifulSoup(requests.get(url, timeout=5).text, 'html.parser')
        if re.findall(r"alert\(", str(soup)):
            return 1
        else:
            return -1
    except:
        return -1

def iframe_check(url):
    try:
        soup = BeautifulSoup(requests.get(url, timeout=5).text, 'html.parser')
        if soup.find('iframe'):
            return 1
        else:
            return -1
    except:
        return -1

def age_of_domain(domain):
    try:
        creation = domain.creation_date
        if isinstance(creation, list):
            creation = creation[0]
        today = datetime.datetime.now()
        age = (today - creation).days
        return 1 if age >= 180 else -1
    except:
        return -1

def dns_record(domain):
    try:
        _ = socket.gethostbyname(domain)
        return 1
    except:
        return -1

# You can add web_traffic, page_rank, google_index, links_pointing, statistical_report if you have APIs or datasets

def extract_features(url):
    try:
        domain_info = whois.whois(urlparse(url).netloc)
    except:
        domain_info = None

    features = [1,
        has_ip(url),
        url_length(url),
        is_shortened(url),
        has_at_symbol(url),
        double_slash_redirecting(url),
        prefix_suffix(url),
        having_sub_domain(url),
        ssl_final_state(url),
        domain_registration_length(domain_info) if domain_info else -1,
        has_favicon(url),
        port_check(url),
        https_token(url),
        request_url(url),
        url_of_anchor(url),
        links_in_tags(url),
        sfh_check(url),
        submitting_to_email(url),
        abnormal_url(url),
        redirect_count(url),
        mouse_over(url),
        right_click(url),
        popup_window(url),
        iframe_check(url),
        age_of_domain(domain_info) if domain_info else -1,
        dns_record(urlparse(url).netloc),
        1,  # placeholder for web_traffic
        1,  # placeholder for page_rank
        1,  # placeholder for google_index
        1,  # placeholder for links_pointing_to_page
        1   # placeholder for statistical_report
    ]
    return features
