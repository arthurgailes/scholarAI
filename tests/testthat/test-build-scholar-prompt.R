test_that("build_scholar_prompt creates markdown instructions from corpus", {
  # Skip if ellmer not available
  skip_if_not_installed("ellmer")
  skip_if_not_installed("cli")

  # Create temp directory for test
  temp_dir <- tempfile("prompt_test_")
  dir.create(temp_dir)
  on.exit(unlink(temp_dir, recursive = TRUE), add = TRUE)

  # Create mock corpus files
  doc1_dir <- file.path(temp_dir, "doc1")
  doc2_dir <- file.path(temp_dir, "doc2")
  dir.create(doc1_dir)
  dir.create(doc2_dir)

  writeLines(
    "This is a scholarly article about economic policy. The author argues that fiscal restraint
    is necessary during periods of economic growth to prevent inflation. The writing style is
    formal and data-driven, with frequent references to historical precedents.",
    file.path(doc1_dir, "text.txt")
  )

  writeLines(
    "In this analysis of housing markets, the author presents a case for zoning reform.
    The writing is persuasive and includes statistical evidence. The author frequently
    uses phrases like 'the data suggests' and 'evidence indicates' throughout the text.",
    file.path(doc2_dir, "text.txt")
  )

  # Create output path
  output_path <- file.path(temp_dir, "instructions.md")

  # Mock the call_ai_model function to avoid actual API calls
  mock_call_ai_model <- function(prompt, model_name, api_key, verbose) {
    return("# Scholar Style Guide\n\nThis scholar writes in a formal, data-driven style with the following characteristics:\n\n- Relies heavily on statistical evidence\n- Uses phrases like 'the data suggests' and 'evidence indicates'\n- Focuses on economic policy and housing markets\n- Makes references to historical precedents\n- Presents arguments for fiscal restraint and zoning reform")
  }

  # Save original function and replace with mock
  original_fn <- scholarAI:::call_ai_model

  # Replace with mock temporarily
  unlockBinding("call_ai_model", getNamespace("scholarAI"))
  assign("call_ai_model", mock_call_ai_model, getNamespace("scholarAI"))

  # Run the function
  result <- build_scholar_prompt(
    corpus_path = temp_dir,
    output_path = output_path,
    batch_size = 1,
    verbose = FALSE
  )

  # Restore original function
  assign("call_ai_model", original_fn, getNamespace("scholarAI"))
  lockBinding("call_ai_model", getNamespace("scholarAI"))

  # Test that output file was created
  expect_true(file.exists(output_path))

  # Test that content was written
  content <- readLines(output_path)
  expect_true(length(content) > 0)
  expect_true(any(grepl("Scholar Style Guide", content)))
})

test_that("build_scholar_prompt handles DuckDB input", {

  # Create temp directory for test
  temp_dir <- tempfile("prompt_test_db_")
  dir.create(temp_dir)
  on.exit(unlink(temp_dir, recursive = TRUE), add = TRUE)

  # Create mock DuckDB database
  db_path <- file.path(temp_dir, "corpus.duckdb")
  con <- DBI::dbConnect(duckdb::duckdb(), db_path)
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE), add = TRUE)

  # Create mock corpus table
  corpus_data <- data.frame(
    title = c("Article 1", "Article 2"),
    content = c(
      "This is a scholarly article about economic policy with a formal style.",
      "This is an analysis of housing markets with statistical evidence."
    ),
    stringsAsFactors = FALSE
  )

  DBI::dbWriteTable(con, "corpus", corpus_data)

  # Create output path
  output_path <- file.path(temp_dir, "instructions.md")

  # Mock the call_ai_model function to avoid actual API calls
  mock_call_ai_model <- function(prompt, model_name, api_key, verbose) {
    return("# Scholar Style Guide\n\nThis scholar writes in a formal style with the following characteristics:\n\n- Focuses on economic policy and housing markets\n- Uses statistical evidence")
  }

  # Save original function and replace with mock
  original_fn <- scholarAI:::call_ai_model

  # Replace with mock temporarily
  unlockBinding("call_ai_model", getNamespace("scholarAI"))
  assign("call_ai_model", mock_call_ai_model, getNamespace("scholarAI"))

  # Run the function
  result <- build_scholar_prompt(
    corpus_path = db_path,
    output_path = output_path,
    batch_size = 1,
    verbose = FALSE
  )

  # Restore original function
  assign("call_ai_model", original_fn, getNamespace("scholarAI"))
  lockBinding("call_ai_model", getNamespace("scholarAI"))

  # Test that output file was created
  expect_true(file.exists(output_path))

  # Test that content was written
  content <- readLines(output_path)
  expect_true(length(content) > 0)
  expect_true(any(grepl("Scholar Style Guide", content)))
})

test_that("build_scholar_prompt handles empty corpus gracefully", {
  # Skip if required packages not available
  skip_if_not_installed("ellmer")
  skip_if_not_installed("cli")

  # Create temp directory for test
  temp_dir <- tempfile("prompt_test_empty_")
  dir.create(temp_dir)
  on.exit(unlink(temp_dir, recursive = TRUE), add = TRUE)

  # Create output path
  output_path <- file.path(temp_dir, "instructions.md")

  # Expect error when no text files are found
  expect_error(
    build_scholar_prompt(
      corpus_path = temp_dir,
      output_path = output_path,
      verbose = FALSE
    ),
    "No text files found"
  )
})

test_that("build_scholar_prompt respects token limits", {
  # Skip if required packages not available
  skip_if_not_installed("ellmer")
  skip_if_not_installed("cli")

  # Create temp directory for test
  temp_dir <- tempfile("prompt_test_token_")
  dir.create(temp_dir)
  on.exit(unlink(temp_dir, recursive = TRUE), add = TRUE)

  # Create a large text file
  doc_dir <- file.path(temp_dir, "doc")
  dir.create(doc_dir)

  # Generate large text (approximately 10K tokens)
  large_text <- paste(rep("This is a test sentence with about ten tokens. ", 1000), collapse = "")
  writeLines(large_text, file.path(doc_dir, "text.txt"))

  # Create output path
  output_path <- file.path(temp_dir, "instructions.md")

  # Mock the call_ai_model function to check if text was truncated
  truncation_detected <- FALSE
  mock_call_ai_model <- function(prompt, model_name, api_key, verbose) {
    # Check if truncation message is in the prompt
    if (grepl("Content truncated due to length", prompt)) {
      truncation_detected <<- TRUE
    }
    return("# Scholar Style Guide\n\nThis is a truncated analysis.")
  }

  # Save original function and replace with mock
  original_fn <- scholarAI:::call_ai_model

  # Replace with mock temporarily
  unlockBinding("call_ai_model", getNamespace("scholarAI"))
  assign("call_ai_model", mock_call_ai_model, getNamespace("scholarAI"))

  # Run the function with a small token limit
  result <- build_scholar_prompt(
    corpus_path = temp_dir,
    output_path = output_path,
    max_token_per_batch = 1000,  # Small limit to force truncation
    verbose = FALSE
  )

  # Restore original function
  assign("call_ai_model", original_fn, getNamespace("scholarAI"))
  lockBinding("call_ai_model", getNamespace("scholarAI"))

  # Test that truncation was detected
  expect_true(truncation_detected)

  # Test that output file was created despite truncation
  expect_true(file.exists(output_path))
})

test_that("build_scholar_prompt handles custom model name", {
  # Skip if required packages not available
  skip_if_not_installed("ellmer")
  skip_if_not_installed("cli")

  # Create temp directory for test
  temp_dir <- tempfile("prompt_test_model_")
  dir.create(temp_dir)
  on.exit(unlink(temp_dir, recursive = TRUE), add = TRUE)

  # Create a simple text file
  doc_dir <- file.path(temp_dir, "doc")
  dir.create(doc_dir)
  writeLines("Sample text for testing custom model.", file.path(doc_dir, "text.txt"))

  # Create output path
  output_path <- file.path(temp_dir, "instructions.md")

  # Mock the call_ai_model function to check if custom model name is used
  model_name_used <- NULL
  mock_call_ai_model <- function(prompt, model_name, api_key, verbose) {
    model_name_used <<- model_name
    return("# Scholar Style Guide\n\nCustom model analysis.")
  }

  # Save original function and replace with mock
  original_fn <- scholarAI:::call_ai_model

  # Replace with mock temporarily
  unlockBinding("call_ai_model", getNamespace("scholarAI"))
  assign("call_ai_model", mock_call_ai_model, getNamespace("scholarAI"))

  # Run the function with a custom model name
  custom_model <- "anthropic/claude-3-opus"
  result <- build_scholar_prompt(
    corpus_path = temp_dir,
    output_path = output_path,
    model_name = custom_model,
    verbose = FALSE
  )

  # Restore original function
  assign("call_ai_model", original_fn, getNamespace("scholarAI"))
  lockBinding("call_ai_model", getNamespace("scholarAI"))

  # Test that the custom model name was used
  expect_equal(model_name_used, custom_model)
})
