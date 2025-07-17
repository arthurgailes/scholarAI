# Tests for PDF extraction functionality
suppressWarnings({
  library(testthat)
  library(xml2)
  library(rvest)
  library(httr)
})
## -------------------------------------------------------------------------
context("PDF extraction from iframes and links")

test_that("handle_pdfs extracts PDFs from iframes", {
  skip_on_cran()
  skip_if_offline(host = "www.aei.org")

  # Create a temporary directory for the test
  test_dir <- file.path(testthat::test_path(), "test_data", "pdf_test")
  if (dir.exists(test_dir)) {
    unlink(test_dir, recursive = TRUE)
  }
  dir.create(test_dir, recursive = TRUE, showWarnings = FALSE)

  # Test URL with direct PDF links
  report_url <- "https://www.aei.org/research-products/report/college-in-the-time-of-coronavirus-challenges-facing-american-higher-education/"

  # Download the page
  res <- httr::GET(
    report_url,
    httr::config(ssl_verifypeer = FALSE),
    httr::timeout(5)
  )
  expect_equal(
    httr::status_code(res),
    200,
    info = "Should be able to download the report page"
  )

  html <- xml2::read_html(res)

  # Extract the main article element
  main_element <- rvest::html_element(html, "main")
  main_article <- rvest::html_element(main_element, "article")
  if (is.na(main_article)) main_article <- html

  # Call handle_pdfs
  pdf_saved <- scholarAI:::handle_pdfs(
    main_article,
    report_url,
    test_dir,
    greenlist = "https://www.aei.org/research-products"
  )

  # Check that at least one PDF was saved
  expect_true(pdf_saved, "handle_pdfs should return TRUE when PDFs are saved")

  # Check that PDF files exist in the directory
  pdf_files <- list.files(test_dir, pattern = "\\.pdf$")
  expect_true(length(pdf_files) > 0, "At least one PDF file should be saved")

  # Check that the first PDF file has content
  if (length(pdf_files) > 0) {
    first_pdf_path <- file.path(test_dir, pdf_files[1])
    file_info <- file.info(first_pdf_path)
    expect_true(
      file_info$size > 1000,
      "PDF file should have meaningful content (>1KB)"
    )
  }
})

test_that("extract_and_save includes pdf_saved status", {
  skip_on_cran()
  skip_if_offline(host = "www.aei.org")

  # Create a temporary directory for the test
  test_dir <- file.path(testthat::test_path(), "test_data", "pdf_extract_test")
  if (dir.exists(test_dir)) {
    unlink(test_dir, recursive = TRUE)
  }
  dir.create(test_dir, recursive = TRUE, showWarnings = FALSE)

  # Test URL with direct PDF links
  report_url <- "https://www.aei.org/research-products/report/college-in-the-time-of-coronavirus-challenges-facing-american-higher-education/"

  # Call extract_and_save
  result <- scholarAI:::extract_and_save(
    url = report_url,
    output_root = test_dir,
    greenlist = "https://www.aei.org/research-products"
  )

  # Check that pdf_saved field exists and is TRUE
  expect_true(
    "pdf_saved" %in% names(result),
    "extract_and_save result should include pdf_saved status"
  )
  expect_true(
    result$pdf_saved,
    "pdf_saved should be TRUE for report page with PDF links"
  )

  # Check folder structure
  folder_name <- scholarAI:::create_folder_name(report_url)
  folder_path <- file.path(test_dir, folder_name)

  expect_true(dir.exists(folder_path), "Article folder should be created")

  # Check that PDF files exist in the directory
  pdf_files <- list.files(folder_path, pattern = "\\.pdf$")
  expect_true(
    length(pdf_files) > 0,
    "At least one PDF file should be saved in the article folder"
  )

  # Check that PDF text is appended to article_text.txt
  article_text_path <- file.path(folder_path, "article_text.txt")
  expect_true(file.exists(article_text_path), "article_text.txt should exist")
  text_content <- readLines(article_text_path, warn = FALSE)
  # Look for the PDF separator and at least some content
  sep_line <- grep("--- PDF TEXT ---", text_content, fixed = TRUE)
  expect_true(
    length(sep_line) == 1,
    "PDF text separator should be present in article_text.txt"
  )
  expect_true(
    any(nzchar(text_content[(sep_line + 1):length(text_content)])),
    "There should be non-empty PDF text after the separator in article_text.txt"
  )

  # Check that metadata.json exists
  meta_path <- file.path(folder_path, "metadata.json")
  expect_true(file.exists(meta_path), "metadata.json should exist")
  meta_json <- yyjsonr::read_json_file(meta_path)
})

test_that("copy_pdfs correctly copies PDFs to backup directory", {
  skip_on_cran()

  # Create test directories
  source_dir <- file.path(testthat::test_path(), "test_data", "pdf_source")
  target_dir <- file.path(testthat::test_path(), "test_data", "pdf_backup")

  # Clean up any existing test directories
  if (dir.exists(source_dir)) unlink(source_dir, recursive = TRUE)
  if (dir.exists(target_dir)) unlink(target_dir, recursive = TRUE)

  # Create source directory structure with dummy PDFs
  dir.create(file.path(source_dir, "article1"), recursive = TRUE)
  dir.create(file.path(source_dir, "article2"), recursive = TRUE)

  # Create dummy PDF files
  writeLines("PDF content 1", file.path(source_dir, "article1", "1.pdf"))
  writeLines("PDF content 2", file.path(source_dir, "article1", "2.pdf"))
  writeLines("PDF content 3", file.path(source_dir, "article2", "1.pdf"))

  # Call copy_pdfs
  copied <- scholarAI:::copy_pdfs(source_dir, target_dir)

  # Check that files were copied
  expect_identical(copied, 3L, "Should have copied 3 PDF files")
  expect_true(file.exists(file.path(target_dir, "article1_1.pdf")))
  expect_true(file.exists(file.path(target_dir, "article1_2.pdf")))
  expect_true(file.exists(file.path(target_dir, "article2_1.pdf")))

  # Check content of copied files
  expect_equal(
    readLines(file.path(target_dir, "article1_1.pdf")),
    "PDF content 1"
  )
  expect_equal(
    readLines(file.path(target_dir, "article2_1.pdf")),
    "PDF content 3"
  )
})
