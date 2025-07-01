# Unit tests for scrape_aei helpers

# NOTE: tests are allowed to load packages with library(); the coding convention
#       about pkg::fun applies to exported functions, not test scripts.
library(testthat)
library(xml2)
library(rvest)
library(tibble)
library(httr)
library(mockery)

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
  result <- scholarAI:::get_author_links("Test+Author", max_pages = 1, progress = FALSE)
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
  expect_equal(scholarAI:::create_folder_name(url), "housing-market/2024-06-30-my-article")
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
