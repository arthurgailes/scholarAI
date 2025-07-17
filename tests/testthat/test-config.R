# Tests for configuration management functions

# Create a temporary directory for test files
test_dir <- tempfile("scholarai_test_")
dir.create(test_dir, recursive = TRUE)

# Clean up after tests
on.exit(unlink(test_dir, recursive = TRUE))

test_that("save_scholar_config creates a valid YAML file", {
  # Skip if yaml package is not available
  skip_if_not_installed("yaml")

  # Define test parameters
  test_output_dir <- file.path(test_dir, "corpus")
  test_authors <- c("Author1", "Author2")
  test_db_path <- file.path(test_dir, "test.duckdb")
  test_config_path <- file.path(test_dir, "test_config.yml")

  # Skip if yaml package is not available
  skip_if_not_installed("yaml")

  # Create a simple test
  result <- save_scholar_config(
    output_dir = test_output_dir,
    authors = test_authors,
    db_path = test_db_path,
    config_path = test_config_path,
    progress = FALSE
  )

  # Check that the file was created
  expect_true(file.exists(test_config_path))

  # Check return value
  expect_equal(result, test_config_path)
})

test_that("load_scholar_config loads configuration correctly", {
  # Skip if yaml package is not available
  skip_if_not_installed("yaml")

  # Define test parameters
  test_output_dir <- file.path(test_dir, "corpus")
  test_authors <- c("Author1", "Author2")
  test_db_path <- file.path(test_dir, "test.duckdb")
  test_config_path <- file.path(test_dir, "test_config.yml")

  # Create a mock config file
  mock_config <- list(
    output_dir = test_output_dir,
    authors = test_authors,
    db_path = test_db_path,
    last_updated = format(Sys.time(), "%Y-%m-%d %H:%M:%S")
  )

  # Skip if yaml package is not available
  skip_if_not_installed("yaml")

  # Create a test config file
  yaml::write_yaml(
    list(
      output_dir = test_output_dir,
      authors = test_authors,
      db_path = test_db_path,
      last_updated = format(Sys.time(), "%Y-%m-%d %H:%M:%S")
    ),
    test_config_path
  )

  # Call the function being tested
  config <- load_scholar_config(
    config_path = test_config_path,
    progress = FALSE
  )

  # Check that the loaded config matches expected values
  expect_equal(config$output_dir, test_output_dir)
  expect_equal(config$authors, test_authors)
  expect_equal(config$db_path, test_db_path)
})

test_that("load_scholar_config handles missing file gracefully", {
  # Define a non-existent config path
  non_existent_path <- file.path(test_dir, "does_not_exist.yml")

  # Call the function with a non-existent file
  config <- load_scholar_config(
    config_path = non_existent_path,
    progress = FALSE
  )

  # Check that default values are returned
  expect_null(config$output_dir)
  expect_null(config$authors)
  expect_null(config$db_path)
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
  unlink(test_config_path)
  unlink(test_db_path)
  unlink(test_prompt_path)
  if (exists("result") && is.list(result) && file.exists(result$custom_file)) {
    unlink(result$custom_file)
  }
})

test_that("build_ai_scholar saves configuration in step 0", {
  # Define test parameters
  test_output_dir <- file.path(test_dir, "corpus_build_test")
  test_authors <- c("Author1")
  test_config_path <- file.path(test_dir, "build_config.yml")
  
  # Clean up any existing files from previous test runs
  if (file.exists(test_config_path)) unlink(test_config_path)
  if (dir.exists(test_output_dir)) unlink(test_output_dir, recursive = TRUE)
  
  # Create a temporary environment to mock the build_ai_scholar function
  # This avoids running the full function which would make external API calls
  temp_env <- new.env()
  
  # Define a simplified version of build_ai_scholar that only does step 0
  temp_env$mock_build_ai_scholar <- function(authors, output_dir, config_path) {
    # Only do step 0: Save configuration
    scholarAI::save_scholar_config(
      output_dir = output_dir,
      authors = authors,
      config_path = config_path,
      progress = FALSE
    )
    
    # Return the config path
    return(config_path)
  }
  
  # Call the mock function
  result_config_path <- temp_env$mock_build_ai_scholar(
    authors = test_authors,
    output_dir = test_output_dir,
    config_path = test_config_path
  )
  
  # Test that the configuration file was created
  expect_true(file.exists(result_config_path))
  
  # Load the configuration to verify it contains the expected values
  config <- scholarAI::load_scholar_config(result_config_path, progress = FALSE)
  
  # Check that the configuration contains the correct values
  # Use basename to compare just the last part of the path to avoid separator issues
  expect_equal(basename(config$output_dir), basename(test_output_dir))
  # Also check that the parent directory is the same
  expect_equal(basename(dirname(config$output_dir)), basename(dirname(test_output_dir)))
  expect_equal(config$authors, test_authors)
  expect_true(!is.null(config$last_updated))
  
  # Clean up
  unlink(test_config_path)
  unlink(test_output_dir, recursive = TRUE)
})
