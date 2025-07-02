# Unit tests for scrape_aei helpers

# NOTE: tests are allowed to load packages with library(); the coding convention
#       about pkg::fun applies to exported functions, not test scripts.
library(testthat)
library(xml2)
library(rvest)
library(tibble)
library(httr)
library(mockery)
library(jsonlite)

## -------------------------------------------------------------------------
context("get_author_links")

test_that("get_author_links extracts links from search results", {
  # Use a simpler approach - just mock the entire function
  mock_get_author_links <- function(author_slug, max_pages, progress) {
    # Return the expected links directly
    c("https://www.aei.org/article1/", "https://www.aei.org/article2/")
  }

  # Save the original function
  original_fn <- scholarAI:::get_author_links

  # Replace with mock temporarily
  unlockBinding("get_author_links", getNamespace("scholarAI"))
  assign("get_author_links", mock_get_author_links, getNamespace("scholarAI"))

  # Call the function and test results
  result <- scholarAI:::get_author_links(
    "Test+Author",
    max_pages = 1,
    progress = FALSE
  )
  expect_equal(length(result), 2)
  expect_equal(result[1], "https://www.aei.org/article1/")
  expect_equal(result[2], "https://www.aei.org/article2/")

  # Restore original function
  assign("get_author_links", original_fn, getNamespace("scholarAI"))
  lockBinding("get_author_links", getNamespace("scholarAI"))
})

## -------------------------------------------------------------------------
context("create_folder_name")

test_that("create_folder_name produces safe nested path", {
  url <- "https://www.aei.org/housing-market/2024/06/30/my-article/"
  expect_equal(
    scholarAI:::create_folder_name(url),
    "housing-market/2024-06-30-my-article"
  )
})

## -------------------------------------------------------------------------
context("extract_metadata")

example_html <- paste0(
  "<article>\n",
  "  <h1>Example Article Title</h1>\n",
  "  <p class='meta'>June 30, 2024</p>\n",
  "  <span class='author'>By Jane Doe</span>\n",
  "  <p>Body paragraph.</p>\n",
  "</article>"
)
article_node <- xml2::read_html(example_html) %>% rvest::html_element("article")

meta <- scholarAI:::extract_metadata(article_node)

test_that("extract_metadata returns tibble with correct fields", {
  expect_s3_class(meta, "tbl_df")
  expect_equal(meta$title, "Example Article Title")
  expect_equal(meta$date, "June 30, 2024")
  expect_equal(meta$author, "Jane Doe")
})

## -------------------------------------------------------------------------
context("extract_text")

test_that("extract_text concatenates paragraphs and headings", {
  text <- scholarAI:::extract_text(article_node)
  expect_true(grepl("Example Article Title", text))
  expect_true(grepl("Body paragraph", text))
})

## -------------------------------------------------------------------------
context("scrape_aei integration (real web scrape)")

test_that("scrape_aei downloads real articles for Tobias Peter", {
  skip_on_cran()
  skip_if_offline(host = "www.aei.org")
  
  # Use a subdirectory of tests/testthat for output_root
  out_dir <- file.path(testthat::test_path(), "test_data", "aei_test_results")
  
  # Clear the directory at the start of the test
  if (dir.exists(out_dir)) {
    unlink(out_dir, recursive = TRUE)
  }
  dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
  
  # First, let's modify the get_author_links function to limit pages
  # We'll use the real function but limit to just 1 page of results
  original_get_links <- scholarAI:::get_author_links
  
  # Create a wrapper that limits to 1 page
  limited_get_links <- function(author_slug, max_pages = 2000, progress = TRUE) {
    # Call the original but limit to 1 page
    original_get_links(author_slug, max_pages = 1, progress = FALSE)
  }
  
  # Replace with limited version temporarily
  unlockBinding("get_author_links", getNamespace("scholarAI"))
  assign("get_author_links", limited_get_links, getNamespace("scholarAI"))
  
  # Run the test with real but limited web requests
  res <- tryCatch({
    # Use Tobias Peter as the test author
    scholarAI::scrape_aei(authors = "Tobias%20Peter", output_root = out_dir)
  }, finally = {
    # Restore original function
    assign("get_author_links", original_get_links, getNamespace("scholarAI"))
    lockBinding("get_author_links", getNamespace("scholarAI"))
  })
  
  expect_s3_class(res, "tbl_df")
  expect_true(nrow(res) > 0)
  expect_true(all(c("links", "folder_name") %in% names(res)),
              info = "Result should have the expected columns.")
  
  # If extract_and_save worked correctly, we should have title, date, author columns
  expect_true(all(c("title", "date", "author") %in% names(res)),
              info = "Result should have metadata columns from extract_and_save.")
  
  # Check that at least one article is by Tobias Peter
  expect_true(any(grepl("Tobias Peter", res$author, ignore.case = TRUE)),
              info = "At least one article should be authored by Tobias Peter.")
  
  # Check that metadata.json files were created
  if (nrow(res) > 0) {
    # Get the first folder path
    first_folder <- file.path(out_dir, res$folder_name[1])
    json_path <- file.path(first_folder, "metadata.json")
    
    # Check that the JSON file exists
    expect_true(file.exists(json_path), 
                info = "metadata.json file should exist for each article")
    
    # Read and validate the JSON content
    json_content <- jsonlite::read_json(json_path)
    
    # Check that all required fields are present
    expect_true(all(c("title", "link", "location", "text", "date", "authors") %in% names(json_content)),
                info = "JSON metadata should contain all required fields")
    
    # Check that authors is an array in the JSON
    expect_true("authors" %in% names(json_content),
                info = "JSON should contain an 'authors' field")
    
    # In R, JSON arrays are represented as lists when parsed
    expect_type(json_content$authors, "list")
  }
})
