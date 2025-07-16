# Real-world implementation of the scholarAI workflow (Part 2)
# This file runs steps 5-7 of the pipeline with real data
# 5. Generate corpus embeddings
# 6. Build scholar prompt
# 7. Generate scholar functions
#
# This file depends on the output from test-full-implementation-part1.R

# Try to source setup.R, but continue if not found
# try(source("setup.R"), silent = TRUE)

# Define paths based on the same convention as part 1
out_dir <- file.path(
  testthat::test_path(),
  "test_data",
  "real_world_implementation"
)

# Infer the database path - should be in the corpus directory
db_path <- file.path(out_dir, "corpus.duckdb")
metadata_path <- file.path(out_dir, "corpus_metadata.json")

# Load configuration from part 1
config_path <- file.path(out_dir, "scholarai_config.yml")

# Define the prompt path
prompt_path <- file.path(out_dir, "scholar_instructions.md")

# Create a variable to hold embedding count - make it accessible to the find_similar_documents function
assign("embedding_count", 0, envir = .GlobalEnv)

# Ensure the config file exists in the test directory
if (!file.exists(config_path)) {
  # Create the directory if it doesn't exist
  dir.create(dirname(config_path), recursive = TRUE, showWarnings = FALSE)

  # Create a configuration file
  scholarAI::save_scholar_config(
    output_dir = out_dir,
    authors = "Tobias%20Peter",
    db_path = db_path,
    config_path = config_path,
    progress = FALSE
  )
}


test_that("env is ready", {
  expect_true(
    Sys.getenv("OPENAI_API_KEY") != "",
    "OpenAI API key not available"
  )
  expect_true(
    Sys.getenv("OPENROUTER_API_KEY") != "",
    "OpenAI API key not available"
  )

  expect_true(file.exists(db_path), "Need to create db")
  expect_true(dir.exists(out_dir), "Need to create directory")
})

test_that("Part 1 results are available", {
  # Verify the loaded paths
  expect_true(dir.exists(out_dir), "Output directory not found")
  expect_true(file.exists(db_path), "DuckDB database not found")
  expect_true(file.exists(metadata_path), "Metadata file not found")
})

# Step 5: Generate corpus embeddings
message("STEP 5: Generating corpus embeddings")

# Generate embeddings and run tests in the same test block
test_that("Corpus embeddings can be generated", {
  # Skip if OpenAI API key is not available
  skip_if_not(
    Sys.getenv("OPENAI_API_KEY") != "",
    "OpenAI API key not available"
  )

  # This test depends on the successful creation of the database in part 1
  skip_if_not(
    file.exists(db_path),
    "Database file not found, skipping embedding generation."
  )

  # Generate corpus embeddings
  scholarAI::corpus_embeddings(db_path)

  # Connect to database with embeddings
  embeddings_con <- DBI::dbConnect(
    duckdb::duckdb(),
    db_path,
    array = "matrix"
  )
  on.exit(DBI::dbDisconnect(embeddings_con, shutdown = TRUE), add = TRUE)

  # Check if embeddings table was created
  tables <- DBI::dbListTables(embeddings_con)
  expect_true("embeddings" %in% tables, "Embeddings table not created")

  # Count embeddings and store in global environment for other tests
  embedding_count_result <- DBI::dbGetQuery(
    embeddings_con,
    "SELECT COUNT(*) as count FROM embeddings"
  )$count
  assign("embedding_count", embedding_count_result, envir = .GlobalEnv)
  message("Generated ", embedding_count, " embeddings")

  # Verify we have embeddings
  expect_gt(embedding_count, 0, "No embeddings were generated")

  # Test similarity search
  query <- "how can we solve the housing crisis with light-touch density"
  message("Testing similarity search with query: '", query, "'")

  similar_docs <- scholarAI::find_similar_documents(
    embeddings_con,
    query,
    limit = 3
  )

  # Should find at least one similar document
  expect_true(nrow(similar_docs) > 0, "No similar documents found")

  if (nrow(similar_docs) > 0) {
    message(
      "Top match: ",
      similar_docs$title[1],
      " (similarity: ",
      round(similar_docs$similarity[1], 3),
      ")"
    )
  }

  # Test assertions for similarity search
  expect_true(is.data.frame(similar_docs), "similar_docs is not a data frame")
  expect_true(
    "similarity" %in% names(similar_docs),
    "similarity column missing"
  )
})

# Step 6: Build scholar prompt
message("STEP 6: Building scholar prompt")

# Build scholar prompt and test in the same block
test_that("Scholar prompt can be built", {
  # Build the scholar prompt
  prompt_path <- scholarAI::build_scholar_prompt(
    corpus_path = out_dir,
    authors = "Tobias Peter",
    model = "google/gemini-2.5-flash",
    output_path = file.path(out_dir, "scholar_instructions.md")
  )

  # Print prompt file info
  message("Scholar instructions created at: ", prompt_path)

  # Test assertions
  expect_true(file.exists(prompt_path), "Prompt file not created")
  expect_true(file.size(prompt_path) > 0, "Prompt file is empty")
})

# Step 7: Generate scholar functions
message("STEP 7: Generating scholar functions")

# Generate scholar functions and test in the same block
test_that("Scholar functions can be generated", {
  # Skip if yaml package is not available
  skip_if_not_installed("yaml")

  # Create a config file if it doesn't exist
  if (!file.exists(config_path)) {
    # Create a configuration file
    scholarAI::save_scholar_config(
      output_dir = out_dir,
      authors = "Tobias%20Peter",
      db_path = db_path,
      prompt_path = prompt_path,
      config_path = config_path,
      progress = FALSE
    )
  }

  # Check that the configuration file exists
  expect_true(file.exists(config_path), "Configuration file not found")

  # Load the configuration
  config <- scholarAI::load_scholar_config(config_path, progress = FALSE)
  expect_false(is.null(config), "Configuration could not be loaded")

  # Create a custom output file path in the test directory
  custom_output_file <- file.path(out_dir, "custom.R")

  # Generate scholar functions using the configuration
  result <- scholarAI::generate_scholar_functions(
    authors = "Tobias%20Peter",
    db_path = db_path,
    prompt_path = prompt_path,
    config_path = config_path,
    output_dir = out_dir, # Specify output directory explicitly
    custom_file = custom_output_file, # Specify custom file path explicitly
    progress = FALSE
  )

  # Verify the custom file was created in the expected location
  expect_true(
    file.exists(custom_output_file),
    "Custom file was not created in the expected location"
  )

  # Verify the custom file contains the expected content
  custom_content <- readLines(custom_output_file)
  expect_true(
    any(grepl("askTobias", custom_content)),
    "Custom file does not contain the expected function"
  )

  # Print function generation info
  message(
    "Scholar functions generated: ",
    paste(result$scholar_functions, collapse = ", ")
  )
  message("Custom file created at: ", result$custom_file)

  # Test assertions
  expect_true(file.exists(result$custom_file), "Custom file not created")
  expect_true(
    length(result$scholar_functions) > 0,
    "No scholar functions generated"
  )

  # Check that the function is available in the global environment
  # Note: In a test environment, functions aren't assigned to .GlobalEnv
  # So we'll just check the file content
  custom_content <- readLines(result$custom_file)
  expect_true(
    any(grepl("askTobias", custom_content)),
    "Scholar function not found in custom file"
  )
})

# Print final directory structure summary
file_count <- length(list.files(out_dir, recursive = TRUE))
message("Final directory structure contains ", file_count, " files")

# Print summary of the implementation
message("Real-world implementation part 2 completed successfully")
message("Output directory: ", out_dir)
message("Scholar functions generated successfully")
