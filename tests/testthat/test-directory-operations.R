# Tests for directory operations in scrape_aei

library(testthat)
library(mockery)
library(withr)

## -------------------------------------------------------------------------
context("directory_operations")

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
      mockery::stub(scholarAI::scrape_aei, "scholarAI::get_author_links", function(...) {
        c("https://www.aei.org/test-article/")
      })
      
      # Now extract_and_save is in aei_extract.R, so we need to mock the fully qualified name
      mockery::stub(scholarAI::scrape_aei, "scholarAI::extract_and_save", function(...) {
        list(title = "Test", date = "2024-06-30", author = "Test Author", pdf_saved = FALSE)
      })
      
      mockery::stub(scholarAI::scrape_aei, "scholarAI::copy_pdfs", function(...) NULL)
      
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
