#' Find documents similar to a query embedding
#'
#' @param con DuckDB connection
#' @param query A string
#' @param limit Maximum number of results to return (default: 10)
#' @param min_similarity Minimum similarity score (0-1) to include in results (default: 0.7)
#' @return Data frame with document IDs and similarity scores
#' @export
find_similar_documents <- function(
  con,
  query,
  limit = 10,
  min_similarity = 0.05
) {
  # Check if embeddings table exists
  tables <- DBI::dbListTables(con)
  if (!"embeddings" %in% tables) {
    cli::cli_abort(c(
      "x" = "Embeddings table not found in database.",
      "i" = "Run corpus_embeddings() first to generate embeddings."
    ))
  }

  # Check if VSS extension is loaded
  vss_loaded <- load_vss(con)

  # If VSS is not loaded, inform the user
  if (!vss_loaded) {
    cli::cli_alert_warning(
      "Vector Similarity Search extension not loaded. Will use fallback method if needed."
    )
  }

  # generate query embeddings
  query_embedding <- get_text_embedding(query)

  # Return early if no embeddings exist
  if (length(query_embedding) == 0) {
    stop("Unable to generate embeddings for query")
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
  sql_query <- paste(
    "SELECT c.*,",
    "array_inner_product(embedding, ",
    query_embedding_str,
    ") AS similarity ",
    "FROM embeddings e JOIN corpus c ON e.id = c.id ",
    "WHERE similarity >= ",
    min_similarity,
    "ORDER BY similarity DESC LIMIT ",
    limit
  )

  cli::cli_alert_info(
    "Running similarity search with threshold {min_similarity}"
  )

  results <- tryCatch(
    {
      # Execute the SQL query directly
      DBI::dbGetQuery(con, sql_query)
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
