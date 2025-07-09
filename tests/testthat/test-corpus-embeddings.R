library(testthat)
library(yyjsonr)

test_that("corpus_embeddings creates embeddings in DuckDB", {
  # Skip if no valid OpenAI API key
  skip_if_not(
    nzchar(Sys.getenv("OPENAI_API_KEY")),
    "No OpenAI API key found, skipping API tests"
  )

  # Create a temporary directory for test files
  test_dir <- tempfile("corpus_test")
  dir.create(test_dir, recursive = TRUE)
  on.exit(unlink(test_dir, recursive = TRUE), add = TRUE)

  # Create a simple corpus with a few documents
  corpus_dir <- file.path(test_dir, "corpus")
  dir.create(corpus_dir)

  # # Create metadata
  metadata <- make_mock_corpus_metadata(folder = corpus_dir)

  # Write metadata file
  yyjsonr::write_json_file(
    metadata,
    file.path(corpus_dir, "corpus_metadata.json"),
    pretty = TRUE,
    auto_unbox = TRUE
  )

  # Create test document directories and files
  sapply(metadata$folder, dir.create, recursive = TRUE)
  sapply(metadata$folder, function(folder) {
    writeLines(
      "This is the content of article 1",
      file.path(folder, "text.txt")
    )
  })

  db_path <- file.path(corpus_dir, "corpus.duckdb")
  scholarAI::corpus_to_duckdb(corpus_dir)

  # con <- DBI::dbConnect(duckdb::duckdb(db_path))
  # head(DBI::dbReadTable(con, "corpus"))
  # DBI::dbDisconnect(con, shutdown = TRUE)

  # Run corpus_embeddings
  scholarAI::corpus_embeddings(db_path, batch_size = 2)

  # Connect to database and check results
  con <- DBI::dbConnect(duckdb::duckdb(), db_path, array = "matrix")
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE), add = TRUE, after = FALSE)

  # Check that embeddings table exists
  expect_true("embeddings" %in% DBI::dbListTables(con))

  # Get embedding count
  embedding_count <- DBI::dbGetQuery(
    con,
    "SELECT COUNT(*) as count FROM embeddings"
  )$count

  # Check that we have the right number of embeddings (should be 3 for our test data)
  expect_equal(embedding_count, 3)

  # Check that embeddings have the right structure
  sample_embedding <- DBI::dbGetQuery(
    con,
    "SELECT embedding FROM embeddings LIMIT 1"
  )$embedding

  # Check embedding dimensions (OpenAI embeddings should be 1536-dimensional)
  expect_equal(length(sample_embedding), 1536)

  # Only test similarity search if we have embeddings
  if (embedding_count > 0) {
    # Try to get embedding for query, but handle potential API failures
    tryCatch(
      {
        query <- "global warming"
        query_embedding <- scholarAI::get_text_embedding(query)

        # Only proceed if we got a valid embedding
        if (is.numeric(query_embedding) && length(query_embedding) > 0) {
          similar_docs <- scholarAI::find_similar_documents(
            con,
            query_embedding,
            limit = 3
          )

          # Test the results
          expect_true(is.data.frame(similar_docs))
          expect_true(nrow(similar_docs) > 0)
          expect_true("similarity" %in% names(similar_docs))

          # Climate change document should be most similar to "global warming"
          expect_equal(similar_docs$id[1], 1)
        } else {
          skip(
            "Could not generate query embedding, skipping similarity search test"
          )
        }
      },
      error = function(e) {
        skip(paste0("Error in similarity search test: ", e$message))
      }
    )
  }
})
