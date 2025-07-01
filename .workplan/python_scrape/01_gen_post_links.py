# Grab all post links from Ed and Tobias

# %%
import requests
from bs4 import BeautifulSoup
import pandas as pd
import os
import warnings
from urllib3.exceptions import InsecureRequestWarning
from urllib.parse import urlparse
import shutil

# Suppress warning from verify=True; I don't know how to fix it
warnings.simplefilter("ignore", InsecureRequestWarning)

os.chdir("W:/arthur/202404_heat_ai")

# %%
base_url = "https://www.aei.org/search-results/?wpsolr_fq%5B0%5D=author_str:"
authors = ["Edward%20J.%20Pinto", "Tobias%20Peter"]

url_list = [base_url + author + "&wpsolr_page=" for author in authors]

links = []

greenlist = ["https://www.aei.org/housing-supply-case-studies/"]


def get_links_from_page(page_number, url):
    """
    Retrieves the links from a specific page.

    Args:
      page_number (int): The page number to retrieve links from.

    Returns:
      list: A list of links found on the page.
    """
    page_url = url + str(page_number)
    response = requests.get(page_url, verify=False, timeout=20)
    soup = BeautifulSoup(response.content, "html.parser")
    post_divs = soup.find_all("div", class_="post post-search")
    page_links = [a["href"] for div in post_divs for a in div.find_all("a", href=True)]
    return page_links


# %%
# Function to create a valid folder name from a URL (with ONE subfolder nest)
def create_folder_name(url):
    parsed_url = urlparse(url)
    path = parsed_url.path.strip("/")
    path_parts = path.split("/", 1)
    if len(path_parts) > 1:
        path_parts[1] = path_parts[1].replace("/", "-")
    return "/".join(path_parts)


# %%
# Loop through the search results pages and extract the links
page_number = 1

for url in url_list:
    while (
        page_number < 2000
    ):  # currently they have about 40 each - a just-in-case cap (stop early to run for a sample)
        print(f"Scraping page {page_number}")
        page_links = get_links_from_page(page_number, url)
        if not page_links:  # Stop if no more links are found
            break
        links.extend(page_links)
        page_number += 1
links.extend(greenlist)

print(f"Scraped {len(links)} links")

# %%
# Save the links to a CSV file
df = pd.DataFrame(links, columns=["links"])


# rm links matching profile or uploads or ...
df = df[~df["links"].str.contains("profile/|uploads/")]
df = df[df["links"].str.startswith("https://www.aei.org")]


df["folder_name"] = df["links"].apply(create_folder_name)

df.to_csv("data/intermed/aei_search_results/scraped_links.csv", index=False)
df.head()

# %%
df = pd.read_csv("data/intermed/aei_search_results/scraped_links.csv")
