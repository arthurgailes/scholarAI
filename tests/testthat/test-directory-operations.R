# Tests for directory operations in scrape_aei

library(testthat)
library(mockery)
library(withr)

## -------------------------------------------------------------------------
context("directory_operations")

test_that("create_folder_name creates directory structure for URLs", {
  # Test the folder name creation separately
  url <- "https://www.aei.org/housing-market/2024/06/30/my-article/"
  folder_name <- scholarAI:::create_folder_name(url)
  expect_equal(folder_name, "housing-market/2024-06-30-my-article")
})

test_that("directory creation functions work correctly", {
  skip_if_not_installed("withr")
  
  withr::with_tempdir({
    # Test basic directory creation
    dir.create("test_dir/nested", recursive = TRUE)
    expect_true(dir.exists("test_dir/nested"))
    
    # Create a test file
    writeLines("test content", "test_dir/nested/test.txt")
    expect_true(file.exists("test_dir/nested/test.txt"))
  })
})

test_that("scrape_aei creates required directories", {
  skip_if_not_installed("withr")
  
  tryCatch({
    withr::with_tempdir({
      # Mock the functions used by scrape_aei
      mockery::stub(scholarAI::scrape_aei, "get_author_links", function(...) {
        c("https://www.aei.org/test-article/")
      })
      
      mockery::stub(scholarAI::scrape_aei, "extract_and_save", function(...) {
        list(title = "Test", date = "2024-06-30", author = "Test Author")
      })
      
      mockery::stub(scholarAI::scrape_aei, "copy_pdfs", function(...) NULL)
      
      # Run the function
      result <- scholarAI::scrape_aei(authors = "Test", output_root = "test_dir")
      
      # Verify directories and files
      expect_true(dir.exists("test_dir"))
      expect_true(file.exists("test_dir/scraped_links.csv"))
      expect_true(file.exists("test_dir/updated_scraped_links.csv"))
    })
  }, error = function(e) {
    fail(paste("Test failed with error:", e$message))
  })
})

test_that("copy_pdfs handles missing directories gracefully", {
  skip_if_not_installed("withr")
  
  tryCatch({
    withr::with_tempdir({
      # Create test directories and file
      dir.create("source_dir")
      writeLines("fake pdf content", "source_dir/test.pdf")
      
      # Call the function
      scholarAI:::copy_pdfs("source_dir", "backup_dir")
      
      # Verify results
      expect_true(dir.exists("backup_dir"))
      expect_true(file.exists("backup_dir/test.pdf"))
      
      # Check content was copied
      content <- readLines("backup_dir/test.pdf")
      expect_equal(content, "fake pdf content")
    })
  }, error = function(e) {
    fail(paste("Test failed with error:", e$message))
  })
})
