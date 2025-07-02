# Test script for PDF extraction from housing market indicators page
# This script focuses specifically on the example URL mentioned

# Load the package
devtools::load_all()

# Clear any existing test data
test_dir <- "data/test_housing_pdf"
if (dir.exists(test_dir)) unlink(test_dir, recursive = TRUE)
dir.create(test_dir, recursive = TRUE, showWarnings = FALSE)

# The specific URL mentioned in the issue
url <- "https://www.aei.org/research-products/report/aei-housing-market-indicators-april-2025/"

# Extract and save content from this URL
result <- extract_and_save(url, test_dir)

# Check if PDFs were saved
folder_path <- file.path(test_dir, create_folder_name(url))
pdf_files <- list.files(folder_path, pattern = "\\.pdf$", full.names = TRUE)

cat("PDF files found:", length(pdf_files), "\n")
if (length(pdf_files) > 0) {
  cat("PDF files:\n")
  print(basename(pdf_files))
  cat("\nPDF sizes (bytes):\n")
  print(file.size(pdf_files))
} else {
  cat("No PDF files found in", folder_path, "\n")
}

# Also check for article text
text_file <- file.path(folder_path, "article_text.txt")
if (file.exists(text_file)) {
  cat("\nArticle text file exists with size:", file.size(text_file), "bytes\n")
} else {
  cat("\nNo article text file found\n")
}
