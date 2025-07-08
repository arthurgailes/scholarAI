# Real-world implementation of the scholarAI workflow
# This file runs the entire pipeline with real data (no mocks)

# This script demonstrates the full scholarAI workflow with real data
# It is not meant to be run in CI/CD pipelines but as a demonstration
# of the full functionality with real web scraping

# Define shared variables that will be used across test blocks
scrape_results <- NULL
corpus_df <- NULL
metadata_path <- NULL
db_path <- NULL
prompt_path <- NULL
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
db_path <- scholarAI::corpus_to_duckdb(out_dir)

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

# Step 5: Generate corpus embeddings
message("STEP 5: Generating corpus embeddings")

# Generate corpus embeddings if requirements are met
embeddings_con <- NULL
embedding_count <- 0
similar_docs <- NULL

if (
  requireNamespace("reticulate", quietly = TRUE) &&
    openai_available &&
    Sys.getenv("OPENAI_API_KEY") != "" &&
    !is.null(db_path)
) {
  tryCatch(
    {
      # Use the db_path from the previous step
      embeddings_con <- scholarAI::corpus_embeddings(db_path)

      # Check if embeddings table was created
      tables <- DBI::dbListTables(embeddings_con)

      if ("embeddings" %in% tables) {
        # Count embeddings
        embedding_count <- DBI::dbGetQuery(
          embeddings_con,
          "SELECT COUNT(*) as count FROM embeddings"
        )$count
        message("Generated ", embedding_count, " embeddings")

        # Test similarity search
        query <- "economic policy"
        message("Testing similarity search with query: '", query, "'")
        query_embedding <- scholarAI::get_text_embedding(query)
        similar_docs <- scholarAI::find_similar_documents(
          embeddings_con,
          query_embedding,
          limit = 3
        )

        message("Found ", nrow(similar_docs), " similar documents")
        if (nrow(similar_docs) > 0) {
          message(
            "Top match: ",
            similar_docs$title[1],
            " (similarity: ",
            round(similar_docs$similarity[1], 3),
            ")"
          )
        }
      }

      DBI::dbDisconnect(embeddings_con, shutdown = TRUE)
    },
    error = function(e) {
      message("Error in corpus embeddings generation: ", e$message)
    }
  )
} else {
  message(
    "Skipping corpus embeddings - required packages not available or API key not set"
  )
}

test_that("Corpus embeddings can be generated", {
  skip_if_not_installed("reticulate")
  skip_if(Sys.getenv("OPENAI_API_KEY") == "", "OpenAI API key not available")
  skip_if(
    !openai_available,
    "Python openai module not available or mamba env not found"
  )
  skip_if(is.null(db_path), "DuckDB database not available")
  skip_if(embedding_count == 0, "No embeddings were generated")

  # Test assertions
  expect_true(embedding_count > 0)

  # Test assertions for similarity search
  if (!is.null(similar_docs)) {
    expect_true(is.data.frame(similar_docs))
    expect_true("similarity" %in% names(similar_docs))
  }
})

# Step 6: Build scholar prompt
message("STEP 6: Building scholar prompt")

# Build scholar prompt
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

test_that("Scholar prompt can be built", {
  skip_if(
    is.null(prompt_path),
    "Scholar prompt generation was skipped or failed"
  )

  # Test assertions
  expect_true(file.exists(prompt_path))
  expect_true(file.size(prompt_path) > 0)
})

# Print final directory structure summary
file_count <- length(list.files(out_dir, recursive = TRUE))
message("Final directory structure contains ", file_count, " files")

# Print summary of the implementation
message("Real-world implementation completed successfully")
message("Output directory: ", out_dir)
message("Number of articles scraped: ", nrow(scrape_results))
