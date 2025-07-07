test_that("corpus_embeddings functions work", {
  # Skip if no API key available
  skip_if(Sys.getenv("OPENAI_API_KEY") == "", 
          "Skipping test: No OpenAI API key available")
  
  # Skip if Python openai module not available
  skip_if(!reticulate::py_module_available("openai"),
          "Skipping test: Python 'openai' module not available")
  
  # Create a temporary directory for test files
  test_dir <- tempfile("corpus_test")
  dir.create(test_dir)
  on.exit(unlink(test_dir, recursive = TRUE), add = TRUE)
  
  # Create a simple corpus with a few documents
  dir.create(file.path(test_dir, "corpus"))
  
  # Create metadata file
  metadata <- data.frame(
    id = 1:3,
    title = c("Climate Change", "Economic Policy", "Foreign Relations"),
    url = c("https://example.com/1", "https://example.com/2", "https://example.com/3"),
    filename = c("doc1.txt", "doc2.txt", "doc3.txt"),
    stringsAsFactors = FALSE
  )
  
  write.csv(metadata, file.path(test_dir, "metadata.csv"), row.names = FALSE)
  
  # Create text files
  writeLines("Climate change is a pressing global issue that requires immediate action.",
             file.path(test_dir, "corpus", "doc1.txt"))
  writeLines("Economic policies should focus on sustainable growth and reducing inequality.",
             file.path(test_dir, "corpus", "doc2.txt"))
  writeLines("Foreign relations between nations are complex and require diplomatic approaches.",
             file.path(test_dir, "corpus", "doc3.txt"))
  
  # Create DuckDB corpus
  db_path <- file.path(test_dir, "test_corpus.duckdb")
  corpus_to_duckdb(
    metadata_path = file.path(test_dir, "metadata.csv"),
    corpus_dir = file.path(test_dir, "corpus"),
    db_path = db_path
  )
  
  # Test get_text_embedding function
  test_text <- "This is a test"
  embedding <- get_text_embedding(test_text)
  
  expect_is(embedding, "numeric")
  expect_true(length(embedding) > 0)
  
  # Test corpus_embeddings function with a small batch size
  con <- corpus_embeddings(db_path, batch_size = 2)
  
  # Check that embeddings table was created
  tables <- DBI::dbListTables(con)
  expect_true("embeddings" %in% tables)
  
  # Check that embeddings were generated for all documents
  embedding_count <- DBI::dbGetQuery(con, "SELECT COUNT(*) as count FROM embeddings")$count
  expect_equal(embedding_count, 3)
  
  # Test find_similar_documents function
  query <- "climate environmental policy"
  query_embedding <- get_text_embedding(query)
  
  similar_docs <- find_similar_documents(con, query_embedding, limit = 3)
  
  expect_is(similar_docs, "data.frame")
  expect_true(nrow(similar_docs) > 0)
  expect_true("similarity" %in% names(similar_docs))
  
  # Clean up
  DBI::dbDisconnect(con, shutdown = TRUE)
})
