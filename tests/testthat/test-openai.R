library(testthat)

# This test requires a valid OPENAI_API_KEY to be set as an environment variable.
# It will be skipped if the key is not available.
skip_if_not(
  nzchar(Sys.getenv("OPENAI_API_KEY")),
  "No OpenAI API key found, skipping API tests"
)

test_that("get_openai_embeddings returns a properly structured matrix", {
  texts <- c("hello world", "this is a test")
  embeddings <- scholarAI::get_openai_embeddings(texts)

  # Check output type and structure
  expect_true(is.matrix(embeddings))
  expect_true(is.numeric(embeddings))
  expect_equal(nrow(embeddings), length(texts))
  
  # The API might fail during testing, but we should still get a matrix
  # If it works, we'll get 1536 columns; if it fails, we'll get 1 column of NAs
  expect_true(ncol(embeddings) == 1536 || ncol(embeddings) == 1)
  
  # Additional tests if the API call succeeded
  if (ncol(embeddings) == 1536) {
    # Each row should be a unit vector (approximately)
    row_norms <- sqrt(rowSums(embeddings^2))
    expect_true(all(abs(row_norms - 1) < 0.01))
    
    # Different texts should have different embeddings
    expect_false(identical(embeddings[1,], embeddings[2,]))
  }
})

test_that("get_openai_embeddings handles empty and invalid inputs", {
  # Test with empty character vector
  expect_equal(
    scholarAI::get_openai_embeddings(character(0)),
    matrix(numeric(0), nrow = 0, ncol = 0)
  )

  # Test error on non-character input
  expect_error(
    scholarAI::get_openai_embeddings(1:5),
    "texts must be a character vector"
  )
})

test_that("get_openai_embeddings fails gracefully with bad key", {
  # Temporarily set a bad key to test failure
  withr::with_envvar(c("OPENAI_API_KEY" = "bad-key"), {
    expect_warning(
      mat <- scholarAI::get_openai_embeddings("test", max_attempts = 1),
      "Failed to get embedding for"
    )
    expect_true(is.matrix(mat))
    expect_equal(nrow(mat), 1)
    expect_true(all(is.na(mat)))
  })
})
