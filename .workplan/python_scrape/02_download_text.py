# %%
# Import necessary libraries
import requests
from bs4 import BeautifulSoup
import os
import pandas as pd
import re
import shutil
from urllib.parse import urljoin, urlparse

# Suppress warning from verify=True; I don't know how to fix it
import warnings
from urllib3.exceptions import InsecureRequestWarning

warnings.simplefilter("ignore", InsecureRequestWarning)

os.chdir("W:/arthur/202404_heat_ai")

#%%
# List of pages from which to download all links (not just iframes)
greenlist = ["https://www.aei.org/housing-supply-case-studies/"]


# %%
# Text extraction
# Function to save text to file
def save_text_to_file(text, folder_name):
    # basename = os.path.basename(folder_name)
    basename = "article_text.txt"
    text_file_path = os.path.join(folder_name, basename)
    with open(text_file_path, "w", encoding="utf-8") as text_file:
        text_file.write(text)


# %%
# Text extraction
# Function to extract title and date from article content
def extract_title_and_date(article_content):
    title_tag = article_content.find(re.compile(r"h[1-6]"))
    title = title_tag.get_text(strip=True) if title_tag else "No Title Found"

    # Patterns to search for date
    date_patterns = [
        r"\b(?:Jan(?:uary)?|Feb(?:ruary)?|Mar(?:ch)?|Apr(?:il)?|May|Jun(?:e)?|Jul(?:y)?|Aug(?:ust)?|Sep(?:tember)?|Oct(?:ober)?|Nov(?:ember)?|Dec(?:ember)?)\b \d{1,2}, \d{4}",
        r"\b(?:Jan(?:uary)?|Feb(?:ruary)?|Mar(?:ch)?|Apr(?:il)?|May|Jun(?:e)?|Jul(?:y)?|Aug(?:ust)?|Sep(?:tember)?|Oct(?:ober)?|Nov(?:ember)?)\b \d{4}",
        r"\bWeek \d{1,2}, \d{4}",
        r"\bWeek \d{1,2}(?: & \d{1,2})?, \d{4}",
        r"\bQ[1-4] \d{4}\b",
        r"\b\d{4}\b",
        r"\b\d{8}\b",
    ]

    # Attempt to find date in various HTML tags
    date = "No Date Found"
    possible_date_tags = article_content.find_all(["time", "p", "span", "div"])

    for tag in possible_date_tags:
        tag_text = tag.get_text(strip=True)
        for pattern in date_patterns:
            date_match = re.search(pattern, tag_text)
            if date_match:
                date = date_match.group(0)
                break
        if date != "No Date Found":
            break

    # Fallback to searching in the title if no date found in content
    if date == "No Date Found":
        for pattern in date_patterns:
            date_match = re.search(pattern, title)
            if date_match:
                date = date_match.group(0)
                break

    return title, date

def extract_author(article_content):
    """
    Extracts the author from the article content.
    Searches for tags with attributes indicating author information.
    """
    author = "No Author Found"
    possible_author_tags = article_content.find_all(["span", "p", "div", "meta"], class_=re.compile(r"author", re.IGNORECASE))
    for tag in possible_author_tags:
        tag_text = tag.get_text(strip=True)
        if tag_text:
            author = tag_text
            break
    # Perform string replacements
    author = author.lstrip("By ").lstrip("With ")  # Remove "By" or "With" from the front
    author = author.replace("|", ", ")  # Replace "|" with ", "
    return author

# get the page name to create a product folder
def get_page_name(url):
    path = urlparse(url).path
    # Remove beginning and trailing slashes if present
    path = path.strip("/")

    path_parts = path.split("/")

    # This dictates the name of the subfolder and a file name based on the remaining parts of the URL
    if len(path_parts) > 1:
        first_part = path_parts[0]
        remaining_parts = "-".join(path_parts[1:])
        path = f"{first_part}/{remaining_parts}"
    return path


# Function to extract text content from a URL
def extract_text_content(article_content):
    paragraphs = article_content.find_all(["p", "li", re.compile(r"h\d")])
    text_content = "\n".join([p.get_text() for p in paragraphs])
    return text_content


# %%
# download PDFs
def download_pdf(pdf_url, save_path):
    response = requests.get(pdf_url, verify=False, timeout=20)
    with open(save_path, "wb") as file:
        file.write(response.content)


# Function to download PDFs from article content
def download_pdfs_on_page(article_content, base_url, folder_name):
    n = 1
    # Find all iframe elements
    for iframe in article_content.find_all("iframe"):
        src = iframe.get("src")
        if src and ".pdf" in src.lower():
            pdf_url = urljoin(base_url, src)
            pdf_file_name = f"{n}.pdf"
            pdf_file_path = os.path.join(folder_name, pdf_file_name)
            if not os.path.exists(pdf_file_path):
                download_pdf(pdf_url, pdf_file_path)
            n += 1
    if(base_url in greenlist):
        for link in article_content.find_all("a"):
            href = link.get("href")
            if href and ".pdf" in href.lower():
                pdf_url = urljoin(base_url, href)
                pdf_file_name = f"{n}.pdf"
                pdf_file_path = os.path.join(folder_name, pdf_file_name)
                if not os.path.exists(pdf_file_path):
                    download_pdf(pdf_url, pdf_file_path)
                n += 1



# duplicate all of the downloaded PDFs for convenience
def copy_pdfs_to_backup():
    pdf_list = []
    for root, dirs, files in os.walk("data/intermed/aei_search_results/"):
        for file in files:
            if file.endswith(".pdf"):
                pdf_list.append(os.path.join(root, file))

    for pdf in pdf_list:
        src = pdf
        dst = f"R:/archive/Published PDFs/{os.path.basename(pdf)}"
        shutil.copy2(src, dst)


# %%
# Function to extract and save content from a URL
def extract_and_save_content(url):
    page_name = get_page_name(url)
    title = "No title found"
    date = "No date found"
    author = "No author found"

    folder_name = os.path.join("data/intermed/aei_search_results/", page_name)

    # Create folder if not exists
    if not os.path.exists(folder_name):
        os.makedirs(folder_name)

    response = requests.get(url, verify=False, timeout=20)
    soup = BeautifulSoup(response.content, "html.parser")

    # Extract text from <p> and <li> inside <article> within <main>
    main_content = soup.find("main")
    if main_content:
        article_content = main_content.find("article")
        if article_content:
            text_content = extract_text_content(article_content)
            save_text_to_file(text_content, folder_name)
            download_pdfs_on_page(article_content, url, folder_name)

            title, date = extract_title_and_date(article_content)
            author = extract_author(article_content)
        if(url in greenlist):
            download_pdfs_on_page(main_content, url, folder_name)

    return title, date, author


# %%
# Main function to read CSV and process URLs
def main():
    csv_file = "data/intermed/aei_search_results/scraped_links.csv"

    df = pd.read_csv(csv_file)
    urls = df["links"]

    titles = []
    dates = []
    authors = []
    for i, url in enumerate(urls):
        title, date, author = extract_and_save_content(url)
        titles.append(title)
        dates.append(date)
        authors.append(author)
        if (i + 1) % 10 == 0:
            print(f"{round((i + 1) / len(urls) * 100)}% of URLs complete")

    # Update DataFrame columns after processing all URLs
    df["title"] = titles
    df["date"] = dates
    df["author"] = authors

    output_csv_file = "data/intermed/aei_search_results/updated_scraped_links.csv"
    df.to_csv(output_csv_file, index=False)

    copy_pdfs_to_backup()


# %%
# Run the main function
if __name__ == "__main__":
    main()

# %%
