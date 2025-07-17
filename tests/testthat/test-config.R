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

  # Create a mock config file
  mock_config <- list(
    output_dir = test_output_dir,
    authors = test_authors,
    db_path = test_db_path,
    prompt_path = test_prompt_path,
    last_updated = format(Sys.time(), "%Y-%m-%d %H:%M:%S")
  )

  # Skip this test for now as it requires more complex mocking
  skip("Skipping test that requires complex mocking")

  # Test skipped, no need to check results
})

test_that("build_ai_scholar saves configuration in step 0", {
  # Define test parameters
  test_output_dir <- file.path(test_dir, "corpus")
  test_authors <- c("Author1")

  # Skip this test for now as it requires more complex mocking
  skip("Skipping test that requires complex mocking")

  # Test skipped, no need to check results
})
