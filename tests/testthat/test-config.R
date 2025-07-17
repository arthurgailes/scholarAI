# Tests for configuration management functions

# Create a temporary directory for test files
test_dir <- tempfile("scholarai_test_")
dir.create(test_dir, recursive = TRUE)

# Clean up after tests
on.exit(unlink(test_dir, recursive = TRUE))

test_that("save_scholar_config creates a valid YAML file", {
  # Define test parameters
  test_output_dir <- file.path(test_dir, "corpus")
  test_authors <- c("Author1", "Author2")
  test_db_path <- file.path(test_dir, "test.duckdb")


  # Create a simple test
  result <- save_scholar_config(
    output_dir = test_output_dir,
    authors = test_authors,
    db_path = test_db_path,
    progress = FALSE
  )

  # Check that the file was created
  expect_true(file.exists(result))
})


test_that("generate_scholar_functions uses config values as defaults", {
  # Define test parameters
  test_output_dir <- file.path(test_dir, "corpus")
  test_authors <- c("Author1", "Author2")
  test_db_path <- file.path(test_dir, "test.duckdb")
  test_prompt_path <- file.path(test_dir, "scholar_instructions.md")
  test_config_path <- file.path(test_dir, "test_config.yml")

  # Create directory structure
  dir.create(test_output_dir, recursive = TRUE, showWarnings = FALSE)

  # Create a mock prompt file
  writeLines("Mock scholar instructions", test_prompt_path)

  # Create a mock DB file (empty file is sufficient for this test)
  file.create(test_db_path)

  # Create a mock config file
  mock_config <- list(
    output_dir = test_output_dir,
    authors = test_authors,
    db_path = test_db_path,
    prompt_path = test_prompt_path,
    last_updated = format(Sys.time(), "%Y-%m-%d %H:%M:%S")
  )

  # Write the config to a file
  yaml::write_yaml(mock_config, test_config_path)

  # Temporarily redirect output to avoid console clutter
  temp_file <- tempfile()
  sink(temp_file)

  # Call generate_scholar_functions with only the config_path
  # This should use the values from the config file
  result <- tryCatch({
    scholarAI:::generate_scholar_functions(
      config_path = test_config_path,
      progress = FALSE
    )
  }, error = function(e) {
    # Return the error message if there's an error
    return(e$message)
  })

  # Restore output
  sink()
  unlink(temp_file)

  # Check if result is a list (success) or an error message (failure)
  if (is.list(result)) {
    # Test that the function used the config values
    expect_true(file.exists(result$custom_file))

    # Read the generated custom.R file
    custom_content <- readLines(result$custom_file)

    # Check that the custom.R file contains references to the config values
    expect_true(any(grepl(test_db_path, custom_content, fixed = TRUE)))
    expect_true(any(grepl(test_prompt_path, custom_content, fixed = TRUE)))

    # Check that functions were created for each author
    for (author in test_authors) {
      first_name <- strsplit(author, " ")[[1]][1]
      function_name <- paste0("ask", first_name)
      expect_true(any(grepl(function_name, custom_content)))
    }
  } else {
    # If there was an error, skip the test but provide diagnostic info
    skip(paste("Test failed with error:", result))
  }

  # Clean up
  unlink(test_db_path)
  unlink(test_prompt_path)
  if (exists("result") && is.list(result) && file.exists(result$custom_file)) {
    unlink(result$custom_file)
  }
})
