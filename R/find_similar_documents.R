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
  vss_loaded <- load_vss(con)

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
    "]::FLOAT[",
    length(query_embedding), # should always be 1536 for openAI
    "]"
  )

  # Try using VSS with HNSW index for fast similarity search
  query <- paste(
    "SELECT c.*,",
    "array_inner_product(embedding, ", query_embedding_str, ") AS similarity ",
    "FROM embeddings e JOIN corpus c ON e.id = c.id ",
    "WHERE similarity >= ", min_similarity,
    "ORDER BY similarity DESC LIMIT ", limit
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

  if (nrow(results) == 0) {
    cli::cli_alert_info(
      "No similar documents found above similarity threshold {min_similarity}."
    )
  }

  return(results)
}


