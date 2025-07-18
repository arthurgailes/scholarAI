test_that("build_ai_scholar function structure works", {
  # Mock the component functions to verify they're called correctly
  mockery::stub(build_ai_scholar, "scrape_aei", function(...) {
    data.frame(title = "Test Article")
  })
  mockery::stub(build_ai_scholar, "text_corpus_to_df", function(...) {
    data.frame(id = 1, content = "test content")
  })
  mockery::stub(build_ai_scholar, "save_corpus_metadata", function(...) {
    "metadata.json"
  })
  mockery::stub(build_ai_scholar, "corpus_to_duckdb", function(...) {
    "corpus.duckdb"
  })
  mockery::stub(build_ai_scholar, "corpus_embeddings", function(...) NULL)
  mockery::stub(build_ai_scholar, "build_scholar_prompt", function(...) {
    "instructions.md"
  })
  mockery::stub(build_ai_scholar, "generate_scholar_functions", function(...) {
    list(custom_file = "custom.R", scholar_functions = c("askTest"))
  })

  # Create a temp directory for testing
  temp_dir <- file.path(tempdir(), "test_scholar_build")
  if (dir.exists(temp_dir)) {
    unlink(temp_dir, recursive = TRUE)
  }
  dir.create(temp_dir, recursive = TRUE, showWarnings = FALSE)

  # Run the function with progress disabled to avoid console output in tests
  result <- build_ai_scholar(
    authors = "Test%20Author",
    output_dir = temp_dir,
    progress = FALSE
  )

  # Verify the return structure
  expect_type(result, "list")
  expect_equal(result$output_dir, temp_dir)
  expect_equal(result$db_path, "corpus.duckdb")
  expect_equal(result$metadata_path, "metadata.json")
  expect_equal(result$prompt_path, "instructions.md")
  expect_true(is.data.frame(result$scrape_results))

  # Clean up
  unlink(temp_dir, recursive = TRUE)
})

test_that("build_ai_scholar validates inputs", {
  # Should error when authors is missing
  expect_error(build_ai_scholar(output_dir = tempdir()), "author")

  # Should error when no results
  expect_error(build_ai_scholar(authors = "Test"), "not found")

  # Should pass with minimal valid inputs (but mock the implementation)
  mockery::stub(build_ai_scholar, "scrape_aei", function(...) data.frame())
  mockery::stub(build_ai_scholar, "text_corpus_to_df", function(...) {
    data.frame()
  })
  mockery::stub(build_ai_scholar, "save_corpus_metadata", function(...) "")
  mockery::stub(build_ai_scholar, "corpus_to_duckdb", function(...) "")
  mockery::stub(build_ai_scholar, "corpus_embeddings", function(...) NULL)
  mockery::stub(build_ai_scholar, "build_scholar_prompt", function(...) "")
  mockery::stub(build_ai_scholar, "generate_scholar_functions", function(...) {
    list(custom_file = "custom.R", scholar_functions = c("askTest"))
  })

  # Create a temp directory for this test
  test_temp_dir <- file.path(tempdir(), "test_scholar_build2")
  if (dir.exists(test_temp_dir)) {
    unlink(test_temp_dir, recursive = TRUE)
  }
  dir.create(test_temp_dir, recursive = TRUE, showWarnings = FALSE)

  # This should not error with proper mocking
  expect_no_error(build_ai_scholar(
    authors = "Test",
    output_dir = tempdir(),
    progress = FALSE
  ))
})
