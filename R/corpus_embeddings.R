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
    cli::cli_alert_info("Adding Vector Similarity Search extension to DuckDB")
    vss_available <- FALSE

    # First check if VSS is already installed
    extensions_query <- tryCatch(
      DBI::dbGetQuery(
        con,
        "SELECT name FROM duckdb_extensions() WHERE name = 'vss'"
      ),
      error = function(e) data.frame(name = character(0))
    )

    if (nrow(extensions_query) > 0) {
      # VSS extension exists, try to load it
      tryCatch(
        {
          DBI::dbExecute(con, "LOAD vss")
          vss_available <- TRUE
          cli::cli_alert_success(
            "Successfully loaded Vector Similarity Search extension"
          )
        },
        error = function(e) {
          cli::cli_warn(c(
            "!" = "Failed to load Vector Similarity Search extension.",
            "i" = "Error: {as.character(e)}"
          ))
        }
      )
    } else {
      # Try to install VSS extension
      tryCatch(
        {
          DBI::dbExecute(con, "INSTALL vss")
          DBI::dbExecute(con, "LOAD vss")
          vss_available <- TRUE
          cli::cli_alert_success(
            "Successfully installed and loaded Vector Similarity Search extension"
          )
        },
        error = function(e) {
          cli::cli_warn(c(
            "!" = "Failed to install Vector Similarity Search extension.",
            "i" = "You may need to manually install it or use a newer version of DuckDB.",
            "i" = "Error: {as.character(e)}"
          ))
        }
      )
    }

    # Store VSS availability for later use
    attr(con, "vss_available") <- vss_available
  }

  # Create embeddings table if it doesn't exist
  if ("embeddings" %in% DBI::dbListTables(con)) {
    cli::cli_alert_info("Dropping existing embeddings table")
    DBI::dbExecute(con, "DROP TABLE embeddings")
  }

  cli::cli_alert_info("Creating embeddings table")
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

  # Log all rowids and content lengths for debugging
  all_docs <- DBI::dbGetQuery(
    con,
    "SELECT rowid, title, LENGTH(content) as content_length FROM corpus"
  )
  cli::cli_alert_info("All documents in corpus:")
  print(all_docs)

  # Track total embeddings generated
  total_embeddings_generated <- 0

  # Process each batch
  for (i in seq_len(batches)) {
    # Calculate batch range for logging
    batch_start <- (i - 1) * batch_size + 1
    batch_end <- min(i * batch_size, total_docs)

    cli::cli_alert_info(
      "Processing batch {i}/{batches} (documents {batch_start}-{batch_end})"
    )

    # Get batch data - don't filter out NULL content yet
    # Note: DuckDB rowids are 1-based in our implementation
    query <- paste0(
      "SELECT rowid, title, content FROM corpus ",
      "LIMIT ",
      batch_size,
      " OFFSET ",
      (i - 1) * batch_size
    )
    batch_data <- DBI::dbGetQuery(con, query)

    # Debug: Print batch data
    cli::cli_alert_info("Batch {i} retrieved {nrow(batch_data)} rows")
    for (j in seq_len(nrow(batch_data))) {
      if (is.na(batch_data$content[j]) || batch_data$content[j] == "") {
        cli::cli_alert_danger(
          "Document {j} (rowid {batch_data$rowid[j]}) has NULL or empty content"
        )
      } else {
        cli::cli_alert_info(
          "Document {j} (rowid {batch_data$rowid[j]}): Content length = {nchar(batch_data$content[j])}"
        )
      }
    }

    # Skip empty batch
    if (nrow(batch_data) == 0) {
      cli::cli_alert_warning("No documents found in batch {i}")
      next
    }

    # Filter out NULL content
    valid_indices <- !is.na(batch_data$content) & batch_data$content != ""

    # Log which documents are being skipped
    if (sum(!valid_indices) > 0) {
      skipped_rowids <- batch_data$rowid[!valid_indices]
      cli::cli_alert_warning(
        "Skipping documents with NULL/empty content: rowids {paste(skipped_rowids, collapse = ', ')}"
      )
      # Log skipped rowids to console for debugging
      print(paste("Skipped rowids:", paste(skipped_rowids, collapse = ", ")))
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

    # Create data frame for insertion
    embedding_df <- data.frame(
      id = valid_data$rowid,
      stringsAsFactors = FALSE
    )

    # Add embeddings as a list column
    embedding_df$embedding <- embedding_arrays

    # Log embedding data before insertion
    cli::cli_alert_info(
      "Attempting to insert {nrow(embedding_df)} embeddings for batch {i}"
    )
    cli::cli_alert_info(
      "Rowids to insert: {paste(embedding_df$id, collapse = ', ')}"
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

        # Fallback to individual inserts
        success_count <- 0
        for (j in seq_len(nrow(embedding_df))) {
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
                "Error inserting embedding {j} (rowid {embedding_df$id[j]}): {e2$message}"
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
    # Get rowids that are missing embeddings
    missing_query <- "SELECT c.rowid, c.title FROM corpus c LEFT JOIN embeddings e ON c.rowid = e.id WHERE e.id IS NULL"
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
        # Check if this is a persistence-related error
        is_persistence_error <- grepl(
          "in-memory|persistence",
          tolower(as.character(e)),
          ignore.case = TRUE
        )

        if (is_persistence_error) {
          cli::cli_warn(c(
            "!" = "Failed to create vector similarity search index due to persistence settings.",
            "i" = "Vector similarity searches may be slower.",
            "i" = "Error: {as.character(e)}",
            "i" = "The HNSW index requires either an in-memory database or the 'hnsw_enable_experimental_persistence' option."
          ))
        } else {
          cli::cli_warn(c(
            "!" = "Failed to create vector similarity search index.",
            "i" = "Vector similarity searches may be slower.",
            "i" = "Error: {as.character(e)}"
          ))
        }
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


#' Generate embeddings for text using OpenAI API
#'
#' @param texts Character vector of texts to generate embeddings for
#' @param model Model to use for embeddings, defaults to "text-embedding-3-small"
#' @param api_key OpenAI API key.
#' @return A numeric matrix of embeddings.
#' @keywords internal

# Function to split text into chunks
chunk_text <- function(text, max_tokens = 8000, overlap_tokens = 200) {
  # A more conservative character count as a proxy for tokens (1 token ~ 3 chars)
  max_chars <- max_tokens * 3
  overlap_chars <- overlap_tokens * 3

  if (nchar(text) <= max_chars) {
    return(list(text))
  }

  chunks <- list()
  start <- 1
  while (start <= nchar(text)) {
    end <- min(start + max_chars - 1, nchar(text))
    chunks <- append(chunks, substr(text, start, end))
    start <- start + max_chars - overlap_chars
    if (start > nchar(text)) break
  }
  return(chunks)
}

generate_embeddings <- function(
  texts,
  model = "text-embedding-3-small",
  api_key
) {
  # This will hold the final embedding for each document (text)
  final_embeddings <- list()

  for (text in texts) {
    if (is.na(text) || text == "") {
      final_embeddings <- append(final_embeddings, list(NA))
      next
    }

    # Estimate tokens and chunk if necessary
    # A simple proxy for token count
    # TODO: Replace with a proper tokenizer if available
    estimated_tokens <- nchar(text) / 4

    if (estimated_tokens > 8190) {
      # text-embedding-3-small max is 8191
      text_chunks <- chunk_text(text, max_tokens = 8000)

      # Get embeddings for all chunks at once
      chunk_embeddings_matrix <- get_openai_embeddings(
        text_chunks,
        model = model,
        openai_key = api_key
      )

      # Filter out failed chunks (all NA rows)
      valid_chunk_embeddings <- chunk_embeddings_matrix[
        !apply(chunk_embeddings_matrix, 1, function(row) all(is.na(row))),
        ,
        drop = FALSE
      ]

      if (nrow(valid_chunk_embeddings) > 0) {
        # Average the embeddings of the chunks
        avg_embedding <- colMeans(valid_chunk_embeddings, na.rm = TRUE)
        final_embeddings <- append(final_embeddings, list(avg_embedding))
      } else {
        cli::cli_warn("Failed to embed any chunk for a large document.")
        final_embeddings <- append(final_embeddings, list(NA))
      }
    } else {
      # Document is small enough, embed directly
      embedding_vector <- get_openai_embeddings(
        text,
        model = model,
        openai_key = api_key
      )
      if (is.matrix(embedding_vector) && nrow(embedding_vector) > 0) {
        final_embeddings <- append(
          final_embeddings,
          list(embedding_vector[1, ])
        )
      } else {
        final_embeddings <- append(final_embeddings, list(NA))
      }
    }
  }

  # Determine embedding dimension from the first successful embedding
  first_valid_embedding <- Filter(
    function(x) !is.null(x) && !all(is.na(x)),
    final_embeddings
  )
  if (length(first_valid_embedding) == 0) {
    # If all embeddings failed, we can't determine dimension. Return empty matrix.
    # The calling function should handle this gracefully.
    return(matrix(numeric(0), 0, 0))
  }
  emb_dim <- length(first_valid_embedding[[1]])

  # Convert list to matrix, handling NAs
  embedding_matrix <- t(sapply(final_embeddings, function(emb) {
    if (is.numeric(emb) && length(emb) == emb_dim) {
      emb
    } else {
      rep(NA_real_, emb_dim)
    }
  }))

  return(embedding_matrix)
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

#' Find documents similar to a query embedding
#'
#' @param con DuckDB connection
#' @param query_embedding Numeric vector representing the query embedding
#' @param limit Maximum number of results to return (default: 10)
#' @param min_similarity Minimum similarity score (0-1) to include in results (default: 0.7)
#' @return Data frame with document IDs and similarity scores
#' @export
find_similar_documents <- function(
  con,
  query_embedding,
  limit = 10,
  min_similarity = 0.5
) {
  # Check if embeddings table exists
  tables <- DBI::dbListTables(con)
  if (!"embeddings" %in% tables) {
    cli::cli_abort(c(
      "x" = "Embeddings table not found in database.",
      "i" = "Run corpus_embeddings() first to generate embeddings."
    ))
  }

  # Ensure we have the database path stored for reconnection if needed
  db_path <- attr(con, "dbdir")
  if (is.null(db_path)) {
    # Try to extract the path from connection info
    conn_info <- DBI::dbGetInfo(con)
    if (!is.null(conn_info$dbname)) {
      db_path <- conn_info$dbname
      attr(con, "dbdir") <- db_path
    } else {
      cli::cli_alert_warning("Cannot determine database path for reconnection")
    }
  }

  # Check if array support is enabled
  if (!identical(attr(con, "array"), "matrix") && !is.null(db_path)) {
    cli::cli_alert_info("Reconnecting with array support enabled")
    con <- DBI::dbConnect(duckdb::duckdb(), db_path, array = "matrix")
    attr(con, "dbdir") <- db_path
  }

  # Check if VSS extension is loaded
  vss_loaded <- tryCatch(
    {
      DBI::dbGetQuery(con, "SELECT vss_version() as version")
      TRUE
    },
    error = function(e) {
      FALSE
    }
  )

  # If VSS is not loaded, inform the user
  if (!vss_loaded) {
    cli::cli_alert_warning(
      "Vector Similarity Search extension not loaded. Will use fallback method if needed."
    )
  }

  # Format query embedding as a proper array string for DuckDB
  query_embedding_str <- paste0(
    "[",
    paste(query_embedding, collapse = ","),
    "]"
  )

  # Debug: Check how many embeddings we have
  embedding_count <- DBI::dbGetQuery(
    con,
    "SELECT COUNT(*) as count FROM embeddings"
  )$count
  cli::cli_alert_info("Found {embedding_count} embeddings in database")

  # Return early if no embeddings exist
  if (embedding_count == 0) {
    cli::cli_alert_warning("No embeddings found in database")
    return(data.frame(
      id = integer(0),
      title = character(0),
      url = character(0),
      similarity = numeric(0)
    ))
  }

  # Try using VSS with HNSW index for fast similarity search
  query <- paste0(
    "SELECT e.id, c.title, c.url, 1 - array_cosine_distance(e.embedding, ",
    query_embedding_str,
    ") AS similarity ",
    "FROM embeddings e JOIN corpus c ON e.id = c.rowid ",
    "WHERE 1 - array_cosine_distance(e.embedding, ",
    query_embedding_str,
    ") >= ",
    min_similarity,
    " ",
    "ORDER BY array_cosine_distance(e.embedding, ",
    query_embedding_str,
    ") ASC LIMIT ",
    limit
  )

  cli::cli_alert_info(
    "Running similarity search with threshold {min_similarity}"
  )

  results <- tryCatch(
    {
      DBI::dbGetQuery(con, query)
    },
    error = function(e) {
      cli::cli_alert_warning(
        "SQL query error during similarity search. Falling back to slower method."
      )
      cli::cli_alert_info("Error: {e$message}")
      NULL
    }
  )

  # If VSS query failed, fall back to manual calculation
  if (is.null(results)) {
    # Reconnect with array support if needed
    if (!identical(attr(con, "array"), "matrix") && !is.null(db_path)) {
      cli::cli_alert_info("Reconnecting with array support for fallback method")
      con <- DBI::dbConnect(duckdb::duckdb(), db_path, array = "matrix")
      attr(con, "dbdir") <- db_path
    }

    # Get all embeddings and metadata
    all_embeddings <- tryCatch(
      {
        DBI::dbGetQuery(con, "SELECT e.id, e.embedding FROM embeddings e")
      },
      error = function(e) {
        cli::cli_alert_error("Failed to retrieve embeddings: {e$message}")
        return(data.frame(id = integer(0), embedding = list()))
      }
    )

    if (nrow(all_embeddings) == 0) {
      cli::cli_alert_warning("No embeddings found in database")
      return(data.frame(
        id = integer(0),
        title = character(0),
        url = character(0),
        similarity = numeric(0)
      ))
    }

    # Calculate cosine similarity manually
    similarities <- sapply(all_embeddings$embedding, function(emb) {
      # Calculate cosine similarity: dot product / (norm(a) * norm(b))
      dot_product <- sum(emb * query_embedding)
      norm_a <- sqrt(sum(emb^2))
      norm_b <- sqrt(sum(query_embedding^2))
      dot_product / (norm_a * norm_b)
    })

    cli::cli_alert_info(
      "Calculated similarities: {paste(round(similarities, 3), collapse=', ')}"
    )

    # Filter by minimum similarity, but use a very low threshold if we don't find any matches
    matches <- which(similarities >= min_similarity)

    if (length(matches) == 0) {
      cli::cli_alert_warning(
        "No documents found with similarity >= {min_similarity}, lowering threshold"
      )
      # Use a very low threshold to ensure we get at least some results
      min_similarity <- 0.1
      matches <- which(similarities >= min_similarity)

      if (length(matches) == 0) {
        cli::cli_alert_warning(
          "Still no matches found, returning top matches regardless of threshold"
        )
        # Just return the top matches regardless of threshold
        matches <- order(similarities, decreasing = TRUE)[seq_len(min(
          limit,
          length(similarities)
        ))]
      }
    }

    # Get metadata for the documents
    all_metadata <- DBI::dbGetQuery(con, "SELECT rowid, title, url FROM corpus")

    # Create results data frame
    results <- data.frame(
      id = all_embeddings$id,
      similarity = similarities
    )

    # Sort by similarity and limit results
    results <- results[order(results$similarity, decreasing = TRUE), ]
    if (nrow(results) > 0) {
      # Use seq_len instead of 1:min to handle edge cases properly
      results <- results[seq_len(min(limit, nrow(results))), ]

      # Join with metadata
      results <- merge(results, all_metadata, by.x = "id", by.y = "rowid")
    } else {
      results <- data.frame(
        id = integer(0),
        title = character(0),
        url = character(0),
        similarity = numeric(0)
      )
    }
  }

  # Filter by minimum similarity if needed
  if (min_similarity > 0 && nrow(results) > 0) {
    results <- results[results$similarity >= min_similarity, ]
  }

  if (nrow(results) == 0) {
    cli::cli_alert_info(
      "No similar documents found above similarity threshold {min_similarity}."
    )
  }

  return(results)
}

#' Calculate cosine similarity between two vectors
#'
#' @param a First vector
#' @param b Second vector
#' @return Similarity score between 0 and 1
#' @keywords internal
cosine_similarity <- function(a, b) {
  # Convert inputs to numeric vectors if needed
  a <- as.numeric(a)
  b <- as.numeric(b)

  # Calculate cosine similarity
  dot_product <- sum(a * b)
  norm_a <- sqrt(sum(a^2))
  norm_b <- sqrt(sum(b^2))

  similarity <- dot_product / (norm_a * norm_b)

  return(similarity)
}
