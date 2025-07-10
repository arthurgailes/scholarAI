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
      # First check if the extension exists
      extensions <- DBI::dbGetQuery(con, "SELECT name FROM duckdb_extensions() WHERE name = 'vss'")
      
      if (nrow(extensions) > 0) {
        # Try to load it if it exists
        DBI::dbExecute(con, "LOAD vss")
        
        # Verify it's loaded by checking for a VSS function
        DBI::dbGetQuery(con, "SELECT array_cosine_distance([1,2,3], [4,5,6]) as test")
        TRUE
      } else {
        FALSE
      }
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

  # Ensure query embedding is numeric
  query_embedding <- as.numeric(query_embedding)

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

  # Format query embedding as a string for DuckDB SQL
  # This is more reliable than parameterized queries for array functions
  query_embedding_str <- paste0(
    "[",
    paste(query_embedding, collapse = ","),
    "]"
  )
  
  # Try using VSS with HNSW index for fast similarity search
  query <- paste0(
    "SELECT e.id, c.title, c.url, ",
    "1 - array_cosine_distance(e.embedding, ", query_embedding_str, ") AS similarity ",
    "FROM embeddings e JOIN corpus c ON e.id = c.rowid ",
    "WHERE 1 - array_cosine_distance(e.embedding, ", query_embedding_str, ") >= ", min_similarity, " ",
    "ORDER BY array_cosine_distance(e.embedding, ", query_embedding_str, ") ASC LIMIT ", limit
  )

  cli::cli_alert_info(
    "Running similarity search with threshold {min_similarity}"
  )

  results <- tryCatch(
    {
      # Execute the SQL query directly
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

    # Return early because similarity filtering already handled above
    return(results)
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
