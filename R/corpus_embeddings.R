#' Generate OpenAI embeddings for corpus text
#'
#' This function connects to a DuckDB corpus database, retrieves text content,
#' and generates OpenAI embeddings using the OpenAI API.
#' The embeddings are stored back in the DuckDB database for future use.
#'
#' @param db_path Path to the DuckDB database containing the corpus
#' @param api_key OpenAI API key. If NULL, will look for OPENAI_API_KEY environment variable
#' @param model Embedding model to use (default: "text-embedding-3-small")
#' @param batch_size Number of texts to process in each batch (default: 100)
#' @param dimensions Number of dimensions for the embeddings (default: 1536)
#' @param add_vss Whether to add Vector Similarity Search extension to DuckDB (default: TRUE)
#' @return Invisibly returns the DuckDB connection
#'
#' @examples
#' \dontrun{
#' # Set API key in environment variable first
#' Sys.setenv(OPENAI_API_KEY = "your-api-key")
#'
#' # Generate embeddings for corpus
#' con <- corpus_embeddings("path/to/corpus.duckdb")
#'
#' # Query similar documents
#' query <- "climate change policy"
#' query_embedding <- get_text_embedding(query)
#' similar_docs <- find_similar_documents(con, query_embedding)
#' }
#' @export
corpus_embeddings <- function(
  db_path,
  api_key = NULL,
  model = "text-embedding-3-small",
  batch_size = 100,
  dimensions = 1536,
  add_vss = TRUE
) {
  # Check if db_path exists
  if (!file.exists(db_path)) {
    cli::cli_abort(c(
      "x" = "DuckDB database file does not exist.",
      "i" = paste0("Checked: ", db_path)
    ))
  }

  # Connect to DuckDB with array support enabled
  cli::cli_alert_info("Connecting to DuckDB database at {.file {db_path}}")
  con <- DBI::dbConnect(duckdb::duckdb(), db_path, array = "matrix")
  # Store the database path as an attribute for reconnection if needed
  attr(con, "dbdir") <- db_path

  on.exit(DBI::dbDisconnect(con, shutdown = TRUE), add = TRUE)

  # Check if corpus table exists
  tables <- DBI::dbListTables(con)
  if (!"corpus" %in% tables) {
    cli::cli_abort(c(
      "x" = "Corpus table not found in database.",
      "i" = "Make sure you've run corpus_to_duckdb() first."
    ))
  }

  corpus_has_id <- any(DBI::dbListFields(con, "corpus") == "id")
  if (!corpus_has_id) stop("Corpus needs an 'id' column")

  # Check for API key
  if (is.null(api_key)) {
    api_key <- Sys.getenv("OPENAI_API_KEY")
  }
  if (!is.character(api_key) || !nzchar(api_key)) {
    cli::cli_abort(c(
      "x" = "OpenAI API key not provided.",
      "i" = "Either provide api_key parameter or set OPENAI_API_KEY environment variable."
    ))
  }
  # Add Vector Similarity Search extension if requested
  if (add_vss) {
    vss_available <- load_vss(con)

    attr(con, "vss_available") <- vss_available
  }

  # Create embeddings table if it doesn't exist
  if ("embeddings" %in% DBI::dbListTables(con)) {
    cli::cli_alert_info("Dropping existing embeddings table")
    DBI::dbExecute(con, "DROP TABLE embeddings")
  }

  DBI::dbExecute(
    con,
    paste0(
      "CREATE TABLE embeddings (",
      "id INTEGER, ",
      "embedding FLOAT[",
      dimensions,
      "]",
      ")"
    )
  )

  # Get total number of documents
  total_docs <- DBI::dbGetQuery(
    con,
    "SELECT COUNT(*) as count FROM corpus"
  )$count
  batches <- ceiling(total_docs / batch_size)

  cli::cli_alert_info("Processing {total_docs} documents in {batches} batches")

  # Track total embeddings generated
  total_embeddings_generated <- 0

  # Process each batch
  for (i in seq_len(batches)) {
    # Calculate batch range for logging
    start_idx <- (i - 1) * batch_size + 1
    end_idx <- min(i * batch_size, total_docs)

    cli::cli_alert_info(
      "Processing batch {i}/{batches} (documents {start_idx}-{end_idx})"
    )

    # Get batch data - don't filter out NULL content yet
    # Note: DuckDB ids are 1-based in our implementation
    query <- paste0(
      "SELECT id, title, content FROM corpus ",
      "WHERE id BETWEEN ", start_idx, " AND ", end_idx
    )
    batch_data <- DBI::dbGetQuery(con, query)

    # Skip empty batch
    if (nrow(batch_data) == 0) {
      cli::cli_alert_warning("No documents found in batch {i}")
      next
    }

    # Filter out NULL content
    valid_indices <- !is.na(batch_data$content) & batch_data$content != ""

    # Log which documents are being skipped
    if (sum(!valid_indices) > 0) {
      skipped_ids <- batch_data$id[!valid_indices]
      cli::cli_alert_warning(
        "Skipping documents with NULL/empty content: ids {paste(skipped_ids, collapse = ', ')}"
      )
      # Log skipped ids to console for debugging
      print(paste("Skipped ids:", paste(skipped_ids, collapse = ", ")))
    }

    # Skip batch if no valid content
    if (sum(valid_indices) == 0) {
      cli::cli_alert_warning("No valid content in batch {i}")
      next
    }

    # Filter to valid data
    valid_data <- batch_data[valid_indices, ]

    # Generate embeddings for batch
    cli::cli_alert_info(
      "Generating embeddings for {nrow(valid_data)} documents"
    )
    embeddings_matrix <- generate_embeddings(valid_data$content, model, api_key)

    # Convert matrix to a list of vectors for insertion
    embedding_arrays <- lapply(
      seq_len(nrow(embeddings_matrix)),
      function(i) embeddings_matrix[i, ]
    )

    # Filter out rows where embedding failed (are all NA)
    valid_embeddings_indices <- !sapply(
      embedding_arrays,
      function(e) all(is.na(e))
    )
    embedding_arrays <- embedding_arrays[valid_embeddings_indices]
    valid_data <- valid_data[valid_embeddings_indices, ]

    if (length(embedding_arrays) == 0) {
      cli::cli_alert_warning(
        "Embedding generation failed for all documents in batch {i}"
      )
      next
    }

    # Align IDs with successfully generated embeddings after filtering
    filtered_ids <- valid_data$id[valid_embeddings_indices]
    embedding_df <- data.frame(
      id = filtered_ids,
      stringsAsFactors = FALSE
    )

    # Add embeddings as a list column (same length as filtered_ids)
    embedding_df$embedding <- embedding_arrays

    # Log embedding data before insertion
    cli::cli_alert_info(
      "Attempting to insert {nrow(embedding_df)} embeddings for batch {i}"
    )
    cli::cli_alert_info(
      "ids to insert: {paste(embedding_df$id, collapse = ', ')}"
    )

    # Use dbAppendTable for direct insertion
    tryCatch(
      {
        # Use DBI's append mechanism which handles list columns properly with DuckDB
        DBI::dbAppendTable(con, "embeddings", embedding_df)
        cli::cli_alert_success(
          "Successfully inserted {nrow(embedding_df)} embeddings"
        )
        total_embeddings_generated <- total_embeddings_generated +
          nrow(embedding_df)
      },
      error = function(e) {
        cli::cli_alert_warning("Bulk insert failed: {e$message}")
        cli::cli_alert_info("Falling back to individual inserts")

        # Fallback to individual inserts – skip ids already present to avoid duplicates
        existing_ids <- DBI::dbGetQuery(
          con,
          sprintf(
            "SELECT id FROM embeddings WHERE id IN (%s)",
            paste(embedding_df$id, collapse = ",")
          )
        )$id
        success_count <- 0
        for (j in seq_len(nrow(embedding_df))) {
          if (embedding_df$id[j] %in% existing_ids) {
            cli::cli_alert_info(
              "Skipping duplicate id {embedding_df$id[j]} detected during fallback"
            )
            next
          }
          tryCatch(
            {
              # Fallback should use the same prepared data frame logic if possible
              # but dbAppendTable is generally robust. This is a placeholder for a more
              # refined fallback if needed.
              sql <- paste0(
                "INSERT INTO embeddings VALUES (",
                embedding_df$id[j],
                ", ",
                paste0(
                  "[",
                  paste(embedding_df$embedding[[j]], collapse = ","),
                  "]"
                ),
                ")"
              )
              DBI::dbExecute(con, sql)
              success_count <- success_count + 1
            },
            error = function(e2) {
              cli::cli_alert_danger(
                "Error inserting embedding {j} (id {embedding_df$id[j]}): {e2$message}"
              )
            }
          )
        }
        cli::cli_alert_info(
          "Inserted {success_count} out of {nrow(embedding_df)} embeddings"
        )
        total_embeddings_generated <- total_embeddings_generated + success_count
      }
    )
  }

  # Final verification of embeddings count
  final_count <- DBI::dbGetQuery(
    con,
    "SELECT COUNT(*) as count FROM embeddings"
  )$count
  cli::cli_alert_info(
    "Final embeddings count: {final_count} (expected {total_docs})"
  )

  # Check for missing embeddings
  if (final_count < total_docs) {
    cli::cli_alert_warning(
      "{total_docs - final_count} documents are missing embeddings"
    )
    # Get ids that are missing embeddings
    missing_query <- "SELECT c.id, c.title FROM corpus c LEFT JOIN embeddings e ON c.id = e.id WHERE e.id IS NULL"
    missing_docs <- DBI::dbGetQuery(con, missing_query)
    cli::cli_alert_info("Documents missing embeddings:")
    print(missing_docs)
  } else if (final_count > total_docs) {
    cli::cli_alert_warning(
      "More embeddings than documents! This suggests duplicate embeddings."
    )
  } else {
    cli::cli_alert_success("All documents have embeddings!")
  }

  cli::cli_alert_success(
    "Successfully generated embeddings for corpus in {.file {db_path}}"
  )

  # Create index for faster similarity search if VSS is available
  # This is done as the last step to avoid potential database corruption
  vss_available <- isTRUE(attr(con, "vss_available"))

  if (vss_available) {
    cli::cli_alert_info("Creating vector similarity search index")
    tryCatch(
      {
        # First check if the index already exists
        index_query <- tryCatch(
          DBI::dbGetQuery(
            con,
            "SELECT * FROM duckdb_indexes() WHERE table_name = 'embeddings' AND index_name = 'embedding_idx'"
          ),
          error = function(e) data.frame()
        )

        if (nrow(index_query) == 0) {
          # Enable HNSW persistence for non-memory databases
          DBI::dbExecute(con, "SET hnsw_enable_experimental_persistence = true")
          DBI::dbExecute(
            con,
            paste0(
              "CREATE INDEX IF NOT EXISTS embedding_idx ",
              "ON embeddings USING HNSW (embedding) ",
              "WITH (metric = 'cosine')"
            )
          )
          cli::cli_alert_success(
            "Successfully created HNSW index for vector similarity search"
          )
        } else {
          cli::cli_alert_info("Vector similarity search index already exists")
        }
      },
      error = function(e) {
        cli::cli_warn(c(
          "!" = "Failed to create vector similarity search index.",
          "i" = "Vector similarity searches may be slower.",
          "i" = "Error: {as.character(e)}"
        ))
      }
    )
  } else {
    cli::cli_warn(c(
      "!" = "VSS extension not available. Skipping index creation.",
      "i" = "Vector similarity searches will use the fallback method."
    ))
  }
  
  # Return the path to the database invisibly
  invisible(db_path)
}

#' Chunk text into smaller pieces based on token limits
#'
#' @param text Text to chunk
#' @param max_tokens Maximum tokens per chunk
#' @param overlap_tokens Number of tokens to overlap between chunks
#' @return Character vector of text chunks
#' @keywords internal
chunk_text <- function(text, max_tokens = 6000, overlap_tokens = 200) {
  if (is.na(text) || text == "") {
    return(character(0))
  }
  
  if (n_token(text) <= max_tokens) {
    return(text)
  }
  
  chunks <- character()
  start <- 1
  text_length <- nchar(text)
  
  while (start <= text_length) {
    # Start with a chunk that might be close to max_tokens
    # Use a rough estimate to avoid checking token count for every character
    end <- min(start + (max_tokens * 2) - 1, text_length)
    chunk <- substr(text, start, end)
    
    # If the chunk is too large, reduce its size until it fits
    while (n_token(chunk) > max_tokens && nchar(chunk) > 1) {
      end <- end - 100  # Reduce by 100 chars at a time for efficiency
      chunk <- substr(text, start, end)
    }
    
    chunks <- append(chunks, chunk)
    
    # Move start position, accounting for overlap
    chars_to_move <- nchar(chunk) - (overlap_tokens * 2)  # Rough char equivalent
    start <- start + max(1, chars_to_move)  # Ensure we always advance
    if (start > text_length) break
  }
  
  # Process each chunk and handle recursive chunking
  result_chunks <- character()
  for (chunk in chunks) {
    estimated_tokens <- n_token(chunk)
    if (estimated_tokens > 7000) {
      # Recursively chunk large chunks
      sub_chunks <- chunk_text(chunk, max_tokens = 5000, overlap_tokens = 100)
      result_chunks <- c(result_chunks, sub_chunks)
    } else {
      result_chunks <- c(result_chunks, chunk)
    }
  }
  
  return(result_chunks)
}

#' Generate embeddings for text using OpenAI API
#'
#' @param texts Character vector of texts to generate embeddings for
#' @param model Model to use for embeddings
#' @param api_key OpenAI API key
#' @return Matrix of embeddings
#' @keywords internal
generate_embeddings <- function(texts, model = "text-embedding-3-small", api_key = NULL) {
  # Initialize empty matrix
  result_matrix <- matrix(nrow = length(texts), ncol = 1536)
  
  # Process each text
  for (i in seq_along(texts)) {
    text <- texts[i]
    
    # Skip empty text
    if (is.na(text) || text == "") {
      result_matrix[i, ] <- NA_real_
      next
    }
    
    # Use token estimation
    estimated_tokens <- n_token(text)
    
    # Handle large documents by chunking
    if (estimated_tokens > 6000) {
      # Use chunking function
      text_chunks <- chunk_text(text, max_tokens = 5000)
      
      # Verify we have chunks
      if (length(text_chunks) == 0) {
        result_matrix[i, ] <- NA_real_
        next
      }
      
      # Try to get embeddings for chunks
      chunk_embeddings <- tryCatch({
        get_openai_embeddings(
          text_chunks,
          model = model,
          openai_key = api_key
        )
      }, error = function(e) {
        # Try each chunk individually if batch fails
        chunk_embeddings <- matrix(nrow = length(text_chunks), ncol = 1536)
        for (j in seq_along(text_chunks)) {
          chunk_embeddings[j, ] <- tryCatch({
            get_openai_embeddings(
              text_chunks[j],
              model = model,
              openai_key = api_key
            )[1, ]
          }, error = function(e) {
            return(rep(NA_real_, 1536))
          })
        }
        return(chunk_embeddings)
      })
      
      # Remove any NA rows
      valid_rows <- !apply(chunk_embeddings, 1, function(row) all(is.na(row)))
      if (sum(valid_rows) == 0) {
        result_matrix[i, ] <- NA_real_
        next
      }
      
      # Average the embeddings of valid chunks
      valid_embeddings <- chunk_embeddings[valid_rows, , drop = FALSE]
      result_matrix[i, ] <- colMeans(valid_embeddings, na.rm = TRUE)
    } else {
      # For smaller documents, get embedding directly
      embedding_vector <- tryCatch({
        get_openai_embeddings(
          text,
          model = model,
          openai_key = api_key
        )
      }, error = function(e) {
        return(matrix(NA_real_, nrow = 1, ncol = 1536))
      })
      
      result_matrix[i, ] <- embedding_vector[1, ]
    }
  }
  
  return(result_matrix)
}

#' Get embedding for a single text
#'
#' @param text Text to generate embedding for
#' @param model Embedding model to use (default: "text-embedding-3-small")
#' @param max_attempts Maximum number of attempts for API calls (default: 3)
#' @param pause_sec Seconds to pause between retry attempts (default: 0.1)
#' @return Numeric vector representing the embedding
#' @export
get_text_embedding <- function(
  text,
  model = "text-embedding-3-small",
  max_attempts = 3,
  pause_sec = 0.1
) {
  # Get API key
  api_key <- Sys.getenv("OPENAI_API_KEY")
  if (!nzchar(api_key)) {
    cli::cli_abort(c(
      "x" = "OpenAI API key not set.",
      "i" = "Set OPENAI_API_KEY environment variable."
    ))
  }

  # Ensure text is properly formatted
  if (!is.character(text)) {
    cli::cli_alert_warning("Converting non-character input to character")
    text <- as.character(text)
  }

  # Generate embedding
  cli::cli_alert_info("Generating embedding for text: {substr(text, 1, 50)}...")

  embedding_matrix <- get_openai_embeddings(
    texts = text,
    model = model,
    openai_key = api_key,
    max_attempts = max_attempts,
    pause_sec = pause_sec
  )

  # Check for failure
  if (nrow(embedding_matrix) == 0 || all(is.na(embedding_matrix[1, ]))) {
    cli::cli_abort(c(
      "x" = "Failed to generate embedding.",
      "i" = "Check OpenAI API key and network connection."
    ))
  }

  # Return the first (and only) embedding
  return(embedding_matrix[1, ])
}
