test_that("corpus_to_duckdb creates database with expected structure", {
  # Skip if duckdb not available
  skip_if_not_installed("duckdb")
  skip_if_not_installed("DBI")
  
  # Create temp directory for test
  temp_dir <- tempfile("corpus_test_")
  dir.create(temp_dir)
  on.exit(unlink(temp_dir, recursive = TRUE), add = TRUE)
  
  # Create mock metadata
  mock_metadata <- data.frame(
    title = c("Article 1", "Article 2"),
    url = c("http://example.com/1", "http://example.com/2"),
    authors = c("Author A", "Author B, Author C"),
    date = c("2023-01-01", "2023-01-02"),
    folder = c(file.path(temp_dir, "article1"), file.path(temp_dir, "article2")),
    stringsAsFactors = FALSE
  )
  
  # Create article directories and files
  for (i in seq_len(nrow(mock_metadata))) {
    dir.create(mock_metadata$folder[i], recursive = TRUE)
    writeLines(
      paste("This is the content of article", i),
      file.path(mock_metadata$folder[i], "text.txt")
    )
  }
  
  # Write metadata CSV
  metadata_path <- file.path(temp_dir, "corpus_metadata.csv")
  write.csv(mock_metadata, metadata_path, row.names = FALSE)
  
  # Run the function
  db_path <- corpus_to_duckdb(
    metadata_path = metadata_path,
    corpus_dir = temp_dir,
    db_path = file.path(temp_dir, "corpus.duckdb")
  )
  
  # Test that database was created
  expect_true(file.exists(db_path))
  
  # Connect to database
  con <- DBI::dbConnect(duckdb::duckdb(), db_path)
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE), add = TRUE)
  
  # Check table exists
  tables <- DBI::dbListTables(con)
  expect_true("corpus" %in% tables)
  
  # Check corpus content
  corpus_table <- DBI::dbReadTable(con, "corpus")
  expect_equal(nrow(corpus_table), 2)
  expect_equal(corpus_table$title, mock_metadata$title)
  expect_true(all(grepl("This is the content of article", corpus_table$content)))
  
  # Test that we can query by folder
  folder_query <- DBI::dbGetQuery(con, 
    "SELECT * FROM corpus WHERE folder = ?", 
    params = list(mock_metadata$folder[1])
  )
  
  # Check that the query returns the expected result
  expect_equal(nrow(folder_query), 1)
  expect_equal(folder_query$folder[1], mock_metadata$folder[1])
})

test_that("corpus_to_duckdb handles missing files gracefully", {
  # Skip if duckdb not available
  skip_if_not_installed("duckdb")
  skip_if_not_installed("DBI")
  
  # Create temp directory for test
  temp_dir <- tempfile("corpus_test_")
  dir.create(temp_dir)
  on.exit(unlink(temp_dir, recursive = TRUE), add = TRUE)
  
  # Create mock metadata with one missing text file
  mock_metadata <- data.frame(
    title = c("Article 1", "Article 2"),
    url = c("http://example.com/1", "http://example.com/2"),
    folder = c(file.path(temp_dir, "article1"), file.path(temp_dir, "article2")),
    stringsAsFactors = FALSE
  )
  
  # Create only one article directory with text
  dir.create(mock_metadata$folder[1], recursive = TRUE)
  writeLines("This is the content of article 1", file.path(mock_metadata$folder[1], "text.txt"))
  dir.create(mock_metadata$folder[2], recursive = TRUE)
  # Deliberately not creating text.txt for the second article
  
  # Write metadata CSV
  metadata_path <- file.path(temp_dir, "corpus_metadata.csv")
  write.csv(mock_metadata, metadata_path, row.names = FALSE)
  
  # Run function with warnings
  expect_warning(
    db_path <- corpus_to_duckdb(
      metadata_path = metadata_path,
      corpus_dir = temp_dir,
      db_path = file.path(temp_dir, "corpus.duckdb")
    ),
    "Text file not found"
  )
  
  # Connect to database
  con <- DBI::dbConnect(duckdb::duckdb(), db_path)
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE), add = TRUE)
  
  # Check corpus content - should have one row with content, one without
  corpus_table <- DBI::dbReadTable(con, "corpus")
  expect_equal(nrow(corpus_table), 2)
  expect_equal(sum(!is.na(corpus_table$content)), 1)
  expect_equal(sum(is.na(corpus_table$content)), 1)
})

test_that("corpus_to_duckdb handles custom batch size", {
  # Skip if duckdb not available
  skip_if_not_installed("duckdb")
  skip_if_not_installed("DBI")
  
  # Create temp directory for test
  temp_dir <- tempfile("corpus_test_")
  dir.create(temp_dir)
  on.exit(unlink(temp_dir, recursive = TRUE), add = TRUE)
  
  # Create mock metadata with 5 articles
  mock_metadata <- data.frame(
    title = paste("Article", 1:5),
    url = paste0("http://example.com/", 1:5),
    folder = file.path(temp_dir, paste0("article", 1:5)),
    stringsAsFactors = FALSE
  )
  
  # Create article directories and files
  for (i in seq_len(nrow(mock_metadata))) {
    dir.create(mock_metadata$folder[i], recursive = TRUE)
    writeLines(
      paste("This is the content of article", i),
      file.path(mock_metadata$folder[i], "text.txt")
    )
  }
  
  # Write metadata CSV
  metadata_path <- file.path(temp_dir, "corpus_metadata.csv")
  write.csv(mock_metadata, metadata_path, row.names = FALSE)
  
  # Run the function with a small batch size 
  db_path <- corpus_to_duckdb(
    metadata_path = metadata_path,
    corpus_dir = temp_dir,
    db_path = file.path(temp_dir, "corpus.duckdb"),
    batch_size = 2
  )
  
  # Connect to database
  con <- DBI::dbConnect(duckdb::duckdb(), db_path)
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE), add = TRUE)
  
  # Check all 5 articles were processed correctly
  corpus_table <- DBI::dbReadTable(con, "corpus")
  expect_equal(nrow(corpus_table), 5)
  expect_equal(sum(!is.na(corpus_table$content)), 5)
})

test_that("corpus_to_duckdb handles custom db_name", {
  # Skip if duckdb not available
  skip_if_not_installed("duckdb")
  skip_if_not_installed("DBI")
  
  # Create temp directory for test
  temp_dir <- tempfile("corpus_test_")
  dir.create(temp_dir)
  on.exit(unlink(temp_dir, recursive = TRUE), add = TRUE)
  
  # Create mock metadata
  mock_metadata <- data.frame(
    title = c("Article 1"),
    url = c("http://example.com/1"),
    folder = c(file.path(temp_dir, "article1")),
    stringsAsFactors = FALSE
  )
  
  # Create article directory and file
  dir.create(mock_metadata$folder[1], recursive = TRUE)
  writeLines("This is the content of article 1", file.path(mock_metadata$folder[1], "text.txt"))
  
  # Write metadata CSV
  metadata_path <- file.path(temp_dir, "corpus_metadata.csv")
  write.csv(mock_metadata, metadata_path, row.names = FALSE)
  
  # Run the function with custom db name
  custom_db_name <- "custom_corpus.duckdb"
  db_path <- corpus_to_duckdb(
    metadata_path = metadata_path,
    corpus_dir = temp_dir,
    db_path = file.path(temp_dir, custom_db_name)
  )
  
  # Test that database was created with custom name
  expect_equal(basename(db_path), custom_db_name)
  expect_true(file.exists(db_path))
})

test_that("corpus_to_duckdb errors on missing metadata", {
  # Skip if duckdb not available
  skip_if_not_installed("duckdb")
  skip_if_not_installed("DBI")
  
  # Create temp directory for test
  temp_dir <- tempfile("corpus_test_")
  dir.create(temp_dir)
  on.exit(unlink(temp_dir, recursive = TRUE), add = TRUE)
  
  # No metadata.json file created
  
  # Function should error
  expect_error(
    corpus_to_duckdb(
      metadata_path = file.path(temp_dir, "corpus_metadata.csv"),
      corpus_dir = temp_dir,
      db_path = file.path(temp_dir, "corpus.duckdb")
    ), 
    "Corpus metadata file not found"
  )
})

test_that("corpus_to_duckdb handles large text content", {
  skip_if_not_installed("duckdb")
  skip_if_not_installed("DBI")
  
  # Create temp directory
  temp_dir <- file.path(tempdir(), "corpus_large")
  dir.create(temp_dir, recursive = TRUE, showWarnings = FALSE)
  on.exit(unlink(temp_dir, recursive = TRUE), add = TRUE)
  
  # Create mock metadata
  mock_metadata <- data.frame(
    folder = file.path(temp_dir, "article1"),
    title = "Test Article 1",
    stringsAsFactors = FALSE
  )
  
  # Create article directory
  dir.create(mock_metadata$folder, recursive = TRUE, showWarnings = FALSE)
  
  # Create large text content (about 100KB)
  large_text <- paste(rep("This is a test of large content. ", 5000), collapse = "")
  
  # Write text file
  writeLines(large_text, file.path(mock_metadata$folder, "text.txt"))
  
  # Write metadata CSV
  metadata_path <- file.path(temp_dir, "corpus_metadata.csv")
  write.csv(mock_metadata, metadata_path, row.names = FALSE)
  
  # Run function
  db_path <- corpus_to_duckdb(
    metadata_path = metadata_path,
    corpus_dir = temp_dir,
    db_path = file.path(temp_dir, "corpus.duckdb")
  )
  
  # Connect to database
  con <- DBI::dbConnect(duckdb::duckdb(), db_path)
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE), add = TRUE)
  
  # Check text content was stored correctly
  corpus_table <- DBI::dbReadTable(con, "corpus")
  expect_equal(nrow(corpus_table), 1)
  expect_equal(nchar(corpus_table$content[1]), nchar(large_text))
})

test_that("corpus_to_duckdb creates database that can be reopened and modified", {
  skip_if_not_installed("duckdb")
  skip_if_not_installed("DBI")
  
  # Create temp directory
  temp_dir <- file.path(tempdir(), "corpus_reopen")
  dir.create(temp_dir, recursive = TRUE, showWarnings = FALSE)
  on.exit(unlink(temp_dir, recursive = TRUE), add = TRUE)
  
  # Create mock metadata
  mock_metadata <- data.frame(
    folder = file.path(temp_dir, "article1"),
    title = "Test Article 1",
    stringsAsFactors = FALSE
  )
  
  # Create article directory
  dir.create(mock_metadata$folder, recursive = TRUE, showWarnings = FALSE)
  
  # Write text file
  writeLines("Initial content", file.path(mock_metadata$folder, "text.txt"))
  
  # Write metadata CSV
  metadata_path <- file.path(temp_dir, "corpus_metadata.csv")
  write.csv(mock_metadata, metadata_path, row.names = FALSE)
  
  # Run function to create database
  db_path <- corpus_to_duckdb(
    metadata_path = metadata_path,
    corpus_dir = temp_dir,
    db_path = file.path(temp_dir, "corpus.duckdb")
  )
  
  # Verify database was created and properly closed
  expect_true(file.exists(db_path))
  
  # Use a separate block for the first connection
  {
    # Connect to the database again
    con <- DBI::dbConnect(duckdb::duckdb(), db_path)
    
    # Read initial data
    initial_data <- DBI::dbReadTable(con, "corpus")
    expect_equal(nrow(initial_data), 1)
    expect_equal(initial_data$content[1], "Initial content")
    
    # Add new data
    new_data <- data.frame(
      folder = file.path(temp_dir, "article2"),
      title = "Test Article 2",
      file_path = file.path(temp_dir, "article2", "text.txt"),
      content = "New content",
      stringsAsFactors = FALSE
    )
    
    # Append to the table
    DBI::dbAppendTable(con, "corpus", new_data)
    
    # Read updated data
    updated_data <- DBI::dbReadTable(con, "corpus")
    expect_equal(nrow(updated_data), 2)
    
    # Close connection
    DBI::dbDisconnect(con, shutdown = TRUE)
  }
  
  # Reopen connection to verify changes persisted
  con2 <- DBI::dbConnect(duckdb::duckdb(), db_path)
  on.exit(DBI::dbDisconnect(con2, shutdown = TRUE), add = TRUE)
  
  # Verify data persisted
  final_data <- DBI::dbReadTable(con2, "corpus")
  expect_equal(nrow(final_data), 2)
  expect_equal(final_data$title[2], "Test Article 2")
  expect_equal(final_data$content[2], "New content")
})
