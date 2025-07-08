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
  
  # Create metadata file with proper folder structure
  # Create subdirectories for each document
  doc_dirs <- file.path(test_dir, "corpus", paste0("doc", 1:3))
  sapply(doc_dirs, dir.create, recursive = TRUE)
  
  metadata <- data.frame(
    id = 1:3,
    title = c("Climate Change", "Economic Policy", "Foreign Relations"),
    url = c("https://example.com/1", "https://example.com/2", "https://example.com/3"),
    filename = c("text.txt", "text.txt", "text.txt"),
    folder = doc_dirs,  # Use full paths to document folders
    stringsAsFactors = FALSE
  )
  
  write.csv(metadata, file.path(test_dir, "metadata.csv"), row.names = FALSE)
  
  # Create text files with proper paths matching metadata
  # Each document should have its text in folder/text.txt
  writeLines("Climate change is a pressing global issue that requires immediate action.",
             file.path(doc_dirs[1], "text.txt"))
  writeLines("Economic policies should focus on sustainable growth and reducing inequality.",
             file.path(doc_dirs[2], "text.txt"))
  writeLines("Foreign relations between nations are complex and require diplomatic approaches.",
             file.path(doc_dirs[3], "text.txt"))
  
  # Create DuckDB corpus
  # First create metadata JSON in the corpus directory
  metadata_json <- jsonlite::toJSON(metadata, auto_unbox = TRUE)
  writeLines(metadata_json, file.path(test_dir, "corpus", "corpus_metadata.json"))
  
  # Now call corpus_to_duckdb with the correct signature
  corpus_dir <- file.path(test_dir, "corpus")
  corpus_to_duckdb(corpus_dir = corpus_dir)
  
  # Set the correct db_path based on corpus_to_duckdb's default behavior
  db_path <- file.path(corpus_dir, "corpus.duckdb")
  
  # Test get_text_embedding function
  test_text <- "This is a test"
  embedding <- get_text_embedding(test_text)
  
  expect_is(embedding, "numeric")
  expect_true(length(embedding) > 0)
  
  # Test corpus_embeddings function with a small batch size
  # Connect to DuckDB with array support enabled
  con <- DBI::dbConnect(duckdb::duckdb(), db_path, array = "matrix")
  attr(con, "dbdir") <- db_path
  
  # Debug: Check corpus table contents before generating embeddings
  corpus_data <- DBI::dbGetQuery(con, "SELECT rowid, title, content FROM corpus")
  print(corpus_data)
  
  # Check for NULL content
  null_content <- sum(is.na(corpus_data$content))
  print(paste("Documents with NULL content:", null_content))
  
  # Generate embeddings - use batch_size = 1 to process one document at a time
  # This helps isolate any issues with batch processing
  corpus_embeddings(db_path, batch_size = 1)
  
  # Check that embeddings table was created
  tables <- DBI::dbListTables(con)
  expect_true("embeddings" %in% tables)
  
  # Check that embeddings were generated for all documents
  embedding_count <- DBI::dbGetQuery(con, "SELECT COUNT(*) as count FROM embeddings")$count
  expect_equal(embedding_count, 3)
  
  # Test find_similar_documents function
  # Use a query that closely matches one of our test documents
  query <- "climate change global issue"
  query_embedding <- get_text_embedding(query)
  
  # Use a lower similarity threshold to ensure we get results in the test
  similar_docs <- find_similar_documents(con, query_embedding, limit = 3, min_similarity = 0.5)
  
  expect_is(similar_docs, "data.frame")
  expect_true(nrow(similar_docs) > 0)
  expect_true("similarity" %in% names(similar_docs))
  
  # Clean up
  DBI::dbDisconnect(con, shutdown = TRUE)
})
