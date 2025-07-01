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
  # Create mock HTML with search results
  search_html <- paste0(
    "<html><body>",
    "<div class='post post-search'><a href='https://www.aei.org/article1/'>Article 1</a></div>",
    "<div class='post post-search'><a href='https://www.aei.org/article2/'>Article 2</a></div>",
    "</body></html>"
  )
  
  # Create a mock response object
  mock_resp <- structure(
    list(status_code = 200, content = charToRaw(search_html)),
    class = c("response", "list")
  )
  
  # Create a stub function that returns our mock response
  get_stub <- function(...) mock_resp
  
  # Mock the GET function with our stub function
  mockery::stub(scholarAI:::get_author_links, "httr::GET", get_stub)
  
  # Mock httr::http_error to always return FALSE
  mockery::stub(scholarAI:::get_author_links, "httr::http_error", function(...) FALSE)
  
  # Call the function with a test author and max_pages=1 to avoid looping
  result <- scholarAI:::get_author_links("Test+Author", max_pages = 1, progress = FALSE)
  
  # Check results
  expect_equal(length(result), 2)
  expect_equal(result[1], "https://www.aei.org/article1/")
  expect_equal(result[2], "https://www.aei.org/article2/")
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
