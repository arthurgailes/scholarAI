library(scholarAI)

# Test URL with PDF in iframe
test_url <- "https://www.aei.org/research-products/report/aei-housing-market-indicators-april-2025/"

# Create a clean test directory
test_dir <- "data/test_pdf_extraction"
if (dir.exists(test_dir)) unlink(test_dir, recursive = TRUE)
dir.create(test_dir, recursive = TRUE, showWarnings = FALSE)

# Run the extraction
result <- scholarAI:::extract_and_save(
  test_url,
  output_root = test_dir,
  greenlist = "https://www.aei.org/housing-supply-case-studies/"
)

# Check if PDFs were saved
folder_path <- file.path(test_dir, scholarAI:::create_folder_name(test_url))
pdf_files <- list.files(folder_path, pattern = "\\.pdf$", full.names = TRUE)

cat("PDF files found:", length(pdf_files), "\n")
if (length(pdf_files) > 0) {
  cat("PDF files:\n")
  print(basename(pdf_files))
  cat("\nPDF sizes:\n")
  print(file.size(pdf_files))
} else {
  cat("No PDF files found in", folder_path, "\n")
  
  # Debug: Check what HTML we're working with
  cat("\nChecking HTML structure...\n")
  res <- httr::GET(test_url, httr::config(ssl_verifypeer = FALSE), httr::timeout(20))
  html <- xml2::read_html(res)
  
  # Find main article element
  main_element <- rvest::html_element(html, "main")
  main_article <- rvest::html_element(main_element, "article")
  if (is.na(main_article)) main_article <- html
  
  # Check for iframes
  iframe_elements <- rvest::html_elements(main_article, "iframe")
  cat("Number of iframes found:", length(iframe_elements), "\n")
  
  if (length(iframe_elements) > 0) {
    iframe_srcs <- rvest::html_attr(iframe_elements, "src")
    cat("iframe sources:\n")
    print(iframe_srcs)
  }
  
  # Check for PDF links
  a_elements <- rvest::html_elements(main_article, "a")
  a_hrefs <- rvest::html_attr(a_elements, "href")
  pdf_hrefs <- a_hrefs[grepl("\\.pdf$", a_hrefs, ignore.case = TRUE)]
  
  cat("Number of PDF links found:", length(pdf_hrefs), "\n")
  if (length(pdf_hrefs) > 0) {
    cat("PDF links:\n")
    print(pdf_hrefs)
  }
}
