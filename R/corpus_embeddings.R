# Python dependencies are declared in .onLoad at the bottom of this file

#' Generate OpenAI embeddings for corpus text using Python/reticulate
#'
#' This function connects to a DuckDB corpus database, retrieves text content,
#' and generates OpenAI embeddings using Python's OpenAI library via reticulate.
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
  # Check for required packages
  if (!requireNamespace("duckdb", quietly = TRUE)) {
    cli::cli_abort(c(
      "x" = "Package 'duckdb' is required but not installed.",
      "i" = "Install it with: install.packages('duckdb')"
    ))
  }

  if (!requireNamespace("DBI", quietly = TRUE)) {
    cli::cli_abort(c(
      "x" = "Package 'DBI' is required but not installed.",
      "i" = "Install it with: install.packages('DBI')"
    ))
  }

  # Check if db_path exists
  if (!file.exists(db_path)) {
    cli::cli_abort(c(
      "x" = "DuckDB database file does not exist.",
      "i" = paste0("Checked: ", db_path)
    ))
  }

  # Connect to DuckDB
  cli::cli_alert_info("Connecting to DuckDB database at {.file {db_path}}")
  con <- DBI::dbConnect(duckdb::duckdb(), db_path)
  on.exit(DBI::dbDisconnect(con, shutdown = FALSE), add = TRUE)

  # Check if corpus table exists
  tables <- DBI::dbListTables(con)
  if (!"corpus" %in% tables) {
    cli::cli_abort(c(
      "x" = "Corpus table not found in database.",
      "i" = "Make sure you've run corpus_to_duckdb() first."
    ))
  }

  # Set up Python environment and OpenAI
  cli::cli_alert_info("Setting up Python environment for OpenAI")

  # Import OpenAI (py_require called in .onLoad)
  openai <- reticulate::import("openai", convert = FALSE)

  # Set API key
  if (!is.null(api_key)) {
    openai$api_key <- api_key
  } else if (Sys.getenv("OPENAI_API_KEY") != "") {
    # API key already set in environment
  } else {
    cli::cli_abort(c(
      "x" = "OpenAI API key not provided.",
      "i" = "Either provide api_key parameter or set OPENAI_API_KEY environment variable."
    ))
  }

  # Add Vector Similarity Search extension if requested
  if (add_vss) {
    cli::cli_alert_info("Adding Vector Similarity Search extension to DuckDB")
    tryCatch(
      {
        DBI::dbExecute(con, "INSTALL VSS")
        DBI::dbExecute(con, "LOAD VSS")
      },
      error = function(e) {
        cli::cli_warn(c(
          "!" = "Failed to install or load VSS extension.",
          "i" = "You may need to manually install it or use a newer version of DuckDB."
        ))
      }
    )
  }

  # Check if embeddings table exists, create if not
  if (!"embeddings" %in% tables) {
    cli::cli_alert_info("Creating embeddings table")
    DBI::dbExecute(
      con,
      paste0(
        "CREATE TABLE embeddings (",
        "id INTEGER, ",
        "embedding FLOAT[]",
        ")"
      )
    )
  }

  # Get total number of documents
  total_docs <- DBI::dbGetQuery(
    con,
    "SELECT COUNT(*) as count FROM corpus"
  )$count
  batches <- ceiling(total_docs / batch_size)

  cli::cli_alert_info("Processing {total_docs} documents in {batches} batches")

  # Create progress bar
  cli::cli_progress_bar(
    total = batches,
    format = "Processing batch {cli::pb_current}/{cli::pb_total} [{cli::pb_percent}%]",
    format_done = "Processed {cli::pb_total} batches [{cli::pb_elapsed}]",
    clear = FALSE
  )

  # Process in batches to keep memory usage low
  for (i in seq_len(batches)) {
    start_idx <- (i - 1) * batch_size + 1
    end_idx <- min(i * batch_size, total_docs)

    # Get batch of text content
    query <- paste0(
      "SELECT rowid, content FROM corpus ",
      "WHERE rowid BETWEEN ",
      start_idx,
      " AND ",
      end_idx,
      " AND content IS NOT NULL"
    )
    batch_data <- DBI::dbGetQuery(con, query)

    if (nrow(batch_data) > 0) {
      # Generate embeddings for batch
      embeddings <- generate_embeddings(batch_data$content, model, openai)

      # Prepare data for insertion
      embedding_data <- data.frame(
        id = batch_data$rowid,
        embedding = I(embeddings)
      )

      # Insert embeddings into database
      DBI::dbWriteTable(con, "embeddings", embedding_data, append = TRUE)
    }

    # Update progress bar
    cli::cli_progress_update(set = i)
  }

  # Create index for faster similarity search if VSS is available
  tryCatch(
    {
      cli::cli_alert_info("Creating vector similarity search index")
      DBI::dbExecute(
        con,
        paste0(
          "CREATE INDEX IF NOT EXISTS embedding_idx ",
          "ON embeddings USING VSS (embedding) ",
          "WITH (dimensions=",
          dimensions,
          ")"
        )
      )
    },
    error = function(e) {
      cli::cli_warn(c(
        "!" = "Failed to create VSS index.",
        "i" = "Vector similarity searches may be slower."
      ))
    }
  )

  cli::cli_alert_success(
    "Successfully generated embeddings for corpus in {.file {db_path}}"
  )

  invisible(con)
}


#' Generate embeddings for a batch of texts
#'
#' @param texts Character vector of texts to generate embeddings for
#' @param model Embedding model to use
#' @param openai OpenAI Python module
#' @return List of numeric vectors representing embeddings
#' @keywords internal
generate_embeddings <- function(
  texts,
  model = "text-embedding-3-small",
  openai = NULL
) {
  # Import OpenAI if not provided
  if (is.null(openai)) {
    openai <- reticulate::import("openai", convert = FALSE)
  }

  # Generate embeddings
  response <- openai$embeddings$create(
    input = texts,
    model = model
  )

  # Extract embeddings from response
  # Use py_to_r to convert Python objects to R
  embeddings <- reticulate::py_to_r(response$data)

  # Convert to list of numeric vectors
  embedding_list <- lapply(embeddings, function(item) {
    item$embedding
  })

  return(embedding_list)
}

#' Get embedding for a single text
#'
#' @param text Text to generate embedding for
#' @param model Embedding model to use (default: "text-embedding-3-small")
#' @return Numeric vector representing the embedding
#' @export
get_text_embedding <- function(text, model = "text-embedding-3-small") {
  # Import OpenAI (py_require called in .onLoad)
  openai <- reticulate::import("openai", convert = FALSE)

  # Check for API key
  if (Sys.getenv("OPENAI_API_KEY") == "") {
    cli::cli_abort(c(
      "x" = "OpenAI API key not set.",
      "i" = "Set OPENAI_API_KEY environment variable."
    ))
  }

  # Generate embedding
  embeddings <- generate_embeddings(list(text), model, openai)

  # Return the first (and only) embedding
  return(embeddings[[1]])
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
  min_similarity = 0.7
) {
  # Check if embeddings table exists
  tables <- DBI::dbListTables(con)
  if (!"embeddings" %in% tables) {
    cli::cli_abort(c(
      "x" = "Embeddings table not found in database.",
      "i" = "Run corpus_embeddings() first to generate embeddings."
    ))
  }

  # Convert embedding to string format for DuckDB
  embedding_str <- paste0(
    "[",
    paste(query_embedding, collapse = ","),
    "]"
  )

  # Try using VSS for fast similarity search
  tryCatch(
    {
      query <- paste0(
        "SELECT e.id, c.title, c.url, ",
        "cosine_similarity(e.embedding, ",
        embedding_str,
        ") AS similarity ",
        "FROM embeddings e ",
        "JOIN corpus c ON e.id = c.rowid ",
        "WHERE cosine_similarity(e.embedding, ",
        embedding_str,
        ") >= ",
        min_similarity,
        " ",
        "ORDER BY similarity DESC ",
        "LIMIT ",
        limit
      )

      results <- DBI::dbGetQuery(con, query)

      if (nrow(results) == 0) {
        cli::cli_alert_info(
          "No similar documents found above similarity threshold {min_similarity}."
        )
      }

      return(results)
    },
    error = function(e) {
      cli::cli_warn(c(
        "!" = "VSS similarity search failed. Falling back to slower method.",
        "i" = "Error: {as.character(e)}"
      ))

      # Fallback to manual calculation if VSS fails
      query <- paste0(
        "SELECT e.id, e.embedding ",
        "FROM embeddings e"
      )

      all_embeddings <- DBI::dbGetQuery(con, query)

      # Calculate similarities manually
      similarities <- sapply(seq_len(nrow(all_embeddings)), function(i) {
        emb <- unlist(all_embeddings$embedding[i])
        cosine_similarity(query_embedding, emb)
      })

      # Create results data frame
      results <- data.frame(
        id = all_embeddings$id,
        similarity = similarities
      )

      # Filter by minimum similarity
      results <- results[results$similarity >= min_similarity, ]

      # Sort by similarity (descending)
      results <- results[order(results$similarity, decreasing = TRUE), ]

      # Limit results
      if (nrow(results) > limit) {
        results <- results[1:limit, ]
      }

      # Join with corpus data
      if (nrow(results) > 0) {
        ids_str <- paste(results$id, collapse = ",")
        corpus_data <- DBI::dbGetQuery(
          con,
          paste0(
            "SELECT rowid, title, url FROM corpus ",
            "WHERE rowid IN (",
            ids_str,
            ")"
          )
        )

        results <- merge(results, corpus_data, by.x = "id", by.y = "rowid")
      } else {
        cli::cli_alert_info(
          "No similar documents found above similarity threshold {min_similarity}."
        )
      }

      return(results)
    }
  )
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

#' Register Python module requirements and hooks in .onLoad
#' @keywords internal
.onLoad <- function(libname, pkgname) {
  # Declare Python package dependencies
  reticulate::py_require("openai")
}
