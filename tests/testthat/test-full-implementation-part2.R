# Real-world implementation of the scholarAI workflow (Part 2)
# This file runs steps 5-6 of the pipeline with real data
# 5. Generate corpus embeddings
# 6. Build scholar prompt
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
  # Generate corpus embeddings
  scholarAI::corpus_embeddings(db_path)

  # Connect to database with embeddings
  embeddings_con <- DBI::dbConnect(
    duckdb::duckdb(),
    db_path,
    array = "matrix"
  )

  # Check if embeddings table was created
  tables <- DBI::dbListTables(embeddings_con)
  expect_true("embeddings" %in% tables, "Embeddings table not created")

  # Count embeddings
  embedding_count <- DBI::dbGetQuery(
    embeddings_con,
    "SELECT COUNT(*) as count FROM embeddings"
  )$count
  message("Generated ", embedding_count, " embeddings")

  # Verify we have embeddings
  expect_gt(embedding_count, 0, "No embeddings were generated")

  # Test similarity search
  query <- "economic policy"
  message("Testing similarity search with query: '", query, "'")
  query_embedding <- scholarAI::get_text_embedding(query)
  similar_docs <- scholarAI::find_similar_documents(
    embeddings_con,
    query_embedding,
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

  # Close connection
  DBI::dbDisconnect(embeddings_con, shutdown = TRUE)
})

# Step 6: Build scholar prompt
message("STEP 6: Building scholar prompt")

# Build scholar prompt and test in the same block
test_that("Scholar prompt can be built", {
  # Build the scholar prompt
  prompt_path <- scholarAI::build_scholar_prompt(
    corpus_path = out_dir,
    model = "google/gemini-2.0-flash-001",
    output_path = file.path(out_dir, "scholar_instructions.md")
  )

  # Print prompt file info
  message("Scholar instructions created at: ", prompt_path)

  # Test assertions
  expect_true(file.exists(prompt_path), "Prompt file not created")
  expect_true(file.size(prompt_path) > 0, "Prompt file is empty")
})

# Print final directory structure summary
file_count <- length(list.files(out_dir, recursive = TRUE))
message("Final directory structure contains ", file_count, " files")

# Print summary of the implementation
message("Real-world implementation part 2 completed successfully")
message("Output directory: ", out_dir)
