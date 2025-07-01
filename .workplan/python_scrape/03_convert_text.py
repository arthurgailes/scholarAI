# %%
# Import necessary libraries
import os
import re
import json
from PyPDF2 import PdfFileReader
import pandas as pd

os.chdir("W:/arthur/202404_heat_ai")


# %%
# Function to extract text from PDF
def extract_text_from_pdf(pdf_path):
    text = ""
    with open(pdf_path, "rb") as file:
        reader = PdfFileReader(file)
        for page_num in range(reader.numPages):
            page = reader.getPage(page_num)
            text += page.extract_text()
    return text


# %%
# Function to create a valid JSON object and write it to a file
# TODO: output as a text file without the metadata
def create_text(folder_path, link, body_text, author, date):
    folder_name = os.path.basename(folder_path)
    data = {
        "id": folder_name,
        "title": folder_name.replace("-", " ").title(),
        "link": link,
        "author": author,
        "date": date,
        "body": preprocess_text(body_text),
    }
    json_path = os.path.join(folder_path, "output" + ".json")
    with open(json_path, "w", encoding="utf-8") as json_file:
        json.dump(data, json_file, indent=4)


# remove extraneous text
def preprocess_text(text):
    # Remove non-printable characters
    text = re.sub(r"[\x00-\x1F\x7F-\x9F]", "", text)
    # Replace multiple newlines with a single newline
    text = re.sub(r"\n+", "\n", text)
    # Strip leading and trailing whitespace
    text = text.strip()
    return text


# %%
# Function to process each folder
def process_folder(folder_path, link, author, date):
    folder_name = os.path.basename(folder_path)
    body_text = ""

    # Prepend text from article_text.txt if it exists
    article_text_file = os.path.join(folder_path, "article_text.txt")
    if os.path.exists(article_text_file):
        with open(article_text_file, "r", encoding="utf-8") as file:
            article_text = file.read()
            body_text = article_text + "\n" + body_text

    # Read existing _web_text file if it exists
    web_text_file = os.path.join(folder_path, f"{folder_name}_web_text.txt")
    if os.path.exists(web_text_file):
        with open(web_text_file, "r", encoding="utf-8") as file:
            body_text += file.read()

    # Extract text from each PDF and save to a new text file
    for file_name in os.listdir(folder_path):
        if file_name.endswith(".pdf"):
            try:
                pdf_path = os.path.join(folder_path, file_name)
                pdf_text = extract_text_from_pdf(pdf_path)
                txt_path = os.path.join(folder_path, file_name.replace(".pdf", ".txt"))
                with open(txt_path, "w", encoding="utf-8") as txt_file:
                    txt_file.write(pdf_text)
                body_text += "\n" + pdf_text
            except Exception as e:
                print(f"Error processing {pdf_path}: {e}")

    # Create JSON file
    create_text(folder_path, link, body_text, author, date)


# %%
# Main function to read CSV and process each folder
def main():
    base_dir = "data/intermed/aei_search_results/"
    csv_file = os.path.join(base_dir, "updated_scraped_links.csv")

    df = pd.read_csv(csv_file).dropna()
    total_folders = len(df)

    for index, row in df.iterrows():
        folder_path = os.path.join(base_dir, row["folder_name"])
        if os.path.exists(folder_path):
            process_folder(folder_path, row["links"], row["author"], row["date"])
            print(f"Processed {index + 1}/{total_folders} folders ({round((index + 1) / total_folders * 100, 2)}%)")

    print("Processing complete!")
# %%
# Run the main function
if __name__ == "__main__":
    main()

# %%
