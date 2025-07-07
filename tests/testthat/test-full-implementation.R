# Real-world implementation of the scholarAI workflow
# This file runs the entire pipeline with real data (no mocks)

# This script demonstrates the full scholarAI workflow with real data
# It is not meant to be run in CI/CD pipelines but as a demonstration
# of the full functionality with real web scraping
# Check if we're online and can reach AEI
tryCatch(
  {
    response <- httr::GET("https://www.aei.org", httr::timeout(5))
    if (httr::http_error(response)) {
      stop(
        "Cannot access AEI website. HTTP status: ",
        httr::status_code(response)
      )
    }
  },
  error = function(e) {
    stop(
      "This script requires internet access and the AEI website to be available: ",
      e$message
    )
  }
)

# Create a directory for our real-world implementation
out_dir <- file.path(
  testthat::test_path(),
  "test_data",
  "real_world_implementation"
)

# Clear the directory at the start
if (dir.exists(out_dir)) {
  unlink(out_dir, recursive = TRUE)
}
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

# Step 1: Scrape AEI articles for a specific author
# Using Tobias Peter as the test author, limiting to 2 pages for reasonable test time
message("STEP 1: Scraping AEI articles")

# Create a wrapper that limits to 2 pages
original_get_links <- scholarAI:::get_author_links
limited_get_links <- function(
  author_slug,
  max_pages = 2000,
  progress = TRUE
) {
  # Call the original but limit to 2 pages
  original_get_links(author_slug, max_pages = 2, progress = FALSE)
}

# Replace with limited version temporarily
unlockBinding("get_author_links", getNamespace("scholarAI"))
assign("get_author_links", limited_get_links, getNamespace("scholarAI"))

# Run the scraper with real web requests
scrape_results <- tryCatch(
  {
    scholarAI::scrape_aei(
      authors = "Tobias%20Peter",
      output_root = out_dir
    )
  },
  finally = {
    # Restore original function
    assign("get_author_links", original_get_links, getNamespace("scholarAI"))
    lockBinding("get_author_links", getNamespace("scholarAI"))
  }
)

# Print scrape results summary
message("Scrape results: ", nrow(scrape_results), " articles found")
message("Columns: ", paste(names(scrape_results), collapse = ", "))

# Step 2: Convert the corpus to a dataframe
message("STEP 2: Converting corpus to dataframe")
corpus_df <- scholarAI::text_corpus_to_df(out_dir)

# Print corpus dataframe summary
message("Corpus dataframe: ", nrow(corpus_df), " rows")
message("Columns: ", paste(names(corpus_df), collapse = ", "))

# Step 3: Save corpus metadata
message("STEP 3: Saving corpus metadata")
metadata_path <- scholarAI::save_corpus_metadata(out_dir)

# Print metadata file info
message("Metadata saved to: ", metadata_path)

# Step 4: Convert corpus to DuckDB
message("STEP 4: Converting corpus to DuckDB")
tryCatch(
  {
    # Only run if duckdb is available
    if (
      requireNamespace("duckdb", quietly = TRUE) &&
        requireNamespace("DBI", quietly = TRUE)
    ) {
      db_path <- scholarAI::corpus_to_duckdb(out_dir)

      # Print database info
      message("DuckDB database created at: ", db_path)

      # Check database tables
      con <- DBI::dbConnect(duckdb::duckdb(), db_path)
      tables <- DBI::dbListTables(con)
      DBI::dbDisconnect(con, shutdown = TRUE)

      message("Database tables: ", paste(tables, collapse = ", "))
    } else {
      message("Skipping DuckDB conversion - packages not available")
    }
  },
  error = function(e) {
    message("Error in DuckDB conversion: ", e$message)
  }
)

# Step 5: Build scholar prompt
message("STEP 5: Building scholar prompt")
tryCatch(
  {
    prompt_path <- scholarAI::build_scholar_prompt(
      corpus_path = out_dir,
      output_path = file.path(out_dir, "scholar_instructions.md")
    )

    # Print prompt file info
    message("Scholar instructions created at: ", prompt_path)
  },
  error = function(e) {
    message("Error in scholar prompt generation: ", e$message)
  }
)

# Print final directory structure summary
file_count <- length(list.files(out_dir, recursive = TRUE))
message("Final directory structure contains ", file_count, " files")

# Print summary of the implementation
message("Real-world implementation completed successfully")
message("Output directory: ", out_dir)
message("Number of articles scraped: ", nrow(scrape_results))
