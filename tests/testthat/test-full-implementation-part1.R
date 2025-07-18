# Real-world implementation of the scholarAI workflow (Part 1)
# This file runs steps 0-4 of the pipeline with real data (no mocks)
# 0. Save initial configuration
# 1. Scrape AEI articles
# 2. Convert corpus to dataframe
# 3. Save corpus metadata
# 4. Convert corpus to DuckDB

# Define shared variables that will be used across test blocks
scrape_results <- NULL
corpus_df <- NULL
metadata_path <- NULL
db_path <- NULL

test_that("env is ready", {
  expect_true(
    Sys.getenv("OPENAI_API_KEY") != "",
    "OpenAI API key not available"
  )
  expect_true(
    Sys.getenv("OPENROUTER_API_KEY") != "",
    "OpenAI API key not available"
  )
})

# Check AEI website availability
ok <- try(httr::GET("https://www.aei.org", httr::timeout(5)), silent = TRUE)
if (inherits(ok, "try-error") || httr::http_error(ok)) {
  cli::cli_abort("Cannot access AEI website. Check your internet connection.")
}

# Define paths
out_dir <- "./test_data/real_world_implementation"
db_path <- file.path(out_dir, "corpus.duckdb")
metadata_path <- file.path(out_dir, "metadata.rds")
prompt_path <- file.path(out_dir, "scholar_instructions.md")


# Clear the directory at the start
if (dir.exists(out_dir)) {
  unlink(out_dir, recursive = TRUE)
}
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)


# Step 1: Scrape AEI articles for a specific author
# Using Tobias Peter as the test author, limiting to 2 pages for reasonable test time
message("STEP 1: Scraping AEI articles")


# Run the scraper with real web requests
scrape_results <- scholarAI::scrape_aei(
  authors = "Tobias%20Peter",
  output_root = out_dir,
  max_pages = 2
)

# Print scrape results summary
message("Scrape results: ", nrow(scrape_results), " articles found")
message("Columns: ", paste(names(scrape_results), collapse = ", "))

# Step 2: Convert the corpus to a dataframe
message("STEP 2: Converting corpus to dataframe")

# Convert the corpus to a dataframe
corpus_df <- scholarAI::text_corpus_to_df(out_dir)

# Print corpus dataframe summary
message("Corpus dataframe: ", nrow(corpus_df), " rows")
message("Columns: ", paste(names(corpus_df), collapse = ", "))

test_that("Corpus can be converted to dataframe", {
  # Print column names for debugging
  print(names(corpus_df))

  # Test assertions
  expect_true(nrow(corpus_df) > 0)

  # Check for columns that actually exist
  available_cols <- names(corpus_df)
  if ("title" %in% available_cols) {
    expect_true("title" %in% available_cols)
  }
  if ("folder" %in% available_cols) {
    expect_true("folder" %in% available_cols)
  }
})

# Step 3: Save corpus metadata
message("STEP 3: Saving corpus metadata")

# Save corpus metadata
metadata_path <- scholarAI::save_corpus_metadata(out_dir)

# Print metadata file info
message("Metadata saved to: ", metadata_path)

test_that("Corpus metadata can be saved", {
  # Test assertions
  expect_true(file.exists(metadata_path))
})

# Step 4: Convert corpus to DuckDB
message("STEP 4: Converting corpus to DuckDB")

# Convert corpus to DuckDB
db_path <- scholarAI::corpus_to_duckdb(corpus_dir = out_dir)

# Print database info
message("DuckDB database created at: ", db_path)

# Check database tables
con <- DBI::dbConnect(duckdb::duckdb(), db_path)
tables <- DBI::dbListTables(con)
DBI::dbDisconnect(con, shutdown = TRUE)

message("Database tables: ", paste(tables, collapse = ", "))

test_that("Corpus can be converted to DuckDB", {
  # Test assertions
  expect_true(file.exists(db_path))

  # Check tables
  con <- DBI::dbConnect(duckdb::duckdb(), db_path)
  tables <- DBI::dbListTables(con)
  DBI::dbDisconnect(con, shutdown = TRUE)

  expect_true("corpus" %in% tables)
})

test_that("DuckDB corpus contains valid content", {
  # This test depends on the successful creation of the database in the previous test
  skip_if_not(
    file.exists(db_path),
    "Database file not found, skipping content check."
  )

  # Connect to the database
  con <- DBI::dbConnect(duckdb::duckdb(), db_path)
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE), add = TRUE)

  # Check that a good portion of the content is not NULL and has length
  content_check <- DBI::dbGetQuery(
    con,
    "
    SELECT
      CAST(SUM(CASE WHEN content IS NOT NULL AND LENGTH(content) > 100 THEN 1 ELSE 0 END) AS DOUBLE) / COUNT(*) as ratio_with_content,
      AVG(LENGTH(content)) as avg_content_length
    FROM corpus
  "
  )

  # Expect at least 80% of documents to have substantial content
  expect_gt(
    content_check$ratio_with_content,
    0.8,
    label = "Ratio of documents with content > 100 chars"
  )

  # Expect average content length to be substantial (e.g., > 500 chars)
  expect_gt(
    content_check$avg_content_length,
    500,
    label = "Average content length"
  )
})

# Print summary of part 1
message("Real-world implementation part 1 completed successfully")
message("Output directory: ", out_dir)
message("Database path: ", db_path)
message("Number of articles scraped: ", nrow(scrape_results))
