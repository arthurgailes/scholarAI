#' After generating the text corpus, this function converts it to a DuckDB database.
#'
#' Creates a DuckDB database containing both the corpus metadata and the actual text content
#' from the file paths. Uses DuckDB's native JSON functions to efficiently load data.
#' Stores everything in a single table for efficient columnar storage.
#'
#' @param corpus_dir Directory containing the corpus text files
#' @param batch_size Number of documents to process in each batch (default: 50)
#' @return Invisibly returns the path to the created DuckDB database
#'         Use `DBI::dbConnect(duckdb::duckdb(), "path/to/database.duckdb")` to connect to the database
#'
#' @examples
#' # corpus_to_duckdb(corpus_dir = "corpus")
#' @export
corpus_to_duckdb <- function(
  corpus_dir,
  batch_size = 50
) {
  # Check if corpus_dir exists
  if (!dir.exists(corpus_dir)) {
    cli::cli_abort(c(
      "x" = "Corpus directory does not exist.",
      "i" = paste0("Checked: ", corpus_dir)
    ))
  }

  # Set default paths
  metadata_path <- file.path(corpus_dir, "corpus_metadata.json")
  db_path <- file.path(corpus_dir, "corpus.duckdb")

  if (file.exists(db_path)) message("Overwriting existing db")
  unlink(db_path)

  # Check for corpus metadata
  if (!file.exists(metadata_path)) {
    cli::cli_abort(c(
      "x" = "Corpus metadata file not found.",
      "i" = paste("Expected: ", metadata_path, ". Run save_corpus_metadata")
    ))
  }

  # Connect to DuckDB
  cli::cli_alert_info("Creating DuckDB database at {.file {db_path}}")
  con <- DBI::dbConnect(duckdb::duckdb(), db_path)
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE), add = TRUE)

  # Load metadata directly into DuckDB
  cli::cli_alert_info("Loading corpus metadata directly into DuckDB")

  # Determine file type based on extension
  file_ext <- tolower(tools::file_ext(metadata_path))

  if (file_ext == "json") {
    # Create corpus table using DuckDB's JSON functions
    cli::cli_alert_info("Creating corpus table from JSON")

    # Load DuckDB JSON extension
    tryCatch(
      {
        DBI::dbExecute(con, "LOAD json;")
      },
      error = function(e) {
        # If the extension isn't loaded, try to install it
        DBI::dbExecute(con, "INSTALL json;")
        DBI::dbExecute(con, "LOAD json;")
      }
    )

    # Create the table from the JSON file
    create_table_sql <- sprintf(
      "CREATE TABLE corpus AS SELECT * FROM read_json('%s', auto_detect=true);",
      metadata_path
    )
    DBI::dbExecute(con, create_table_sql)

    # Add columns for file_path and content
    DBI::dbExecute(
      con,
      "ALTER TABLE corpus ADD COLUMN IF NOT EXISTS file_path VARCHAR;"
    )
    DBI::dbExecute(
      con,
      "ALTER TABLE corpus ADD COLUMN IF NOT EXISTS content VARCHAR;"
    )
  } else if (file_ext == "csv") {
    # Create corpus table using DuckDB's CSV functions
    cli::cli_alert_info("Creating corpus table from CSV")

    # Create the table from the CSV file
    create_table_sql <- sprintf(
      "CREATE TABLE corpus AS SELECT * FROM read_csv_auto('%s');",
      metadata_path
    )
    DBI::dbExecute(con, create_table_sql)

    # Add columns for file_path and content
    DBI::dbExecute(
      con,
      "ALTER TABLE corpus ADD COLUMN IF NOT EXISTS file_path VARCHAR;"
    )
    DBI::dbExecute(
      con,
      "ALTER TABLE corpus ADD COLUMN IF NOT EXISTS content VARCHAR;"
    )
  } else {
    # Fall back to R-based loading for other formats
    cli::cli_alert_info("Loading metadata using R")

    if (file_ext == "json") {
      metadata <- yyjsonr::read_json_file(metadata_path)
    } else {
      metadata <- utils::read.csv(metadata_path, stringsAsFactors = FALSE)
    }

    # Add placeholder columns
    metadata$file_path <- NA_character_
    metadata$content <- NA_character_

    # Write to DuckDB
    DBI::dbWriteTable(con, "corpus", metadata, overwrite = TRUE)
  }

  # Prepare for batch processing
  # Get the total number of documents from the database
  total_docs <- DBI::dbGetQuery(con, "SELECT COUNT(*) FROM corpus")[[1]]
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
    # Calculate offset for SQL query (SQL uses 0-based indexing for OFFSET)
    offset <- (i - 1) * batch_size

    # Get the batch of metadata from the database
    batch_query <- sprintf(
      "SELECT * FROM corpus LIMIT %d OFFSET %d",
      batch_size,
      offset
    )
    batch_data <- DBI::dbGetQuery(con, batch_query)

    # Process each document in the batch
    for (j in seq_len(nrow(batch_data))) {
      if ("folder" %in% names(batch_data)) {
        folder <- batch_data$folder[j]
        text_file <- file.path(folder, "text.txt")

        if (file.exists(text_file)) {
          # Read text content
          text_content <- readLines(text_file, warn = FALSE)
          text_content <- paste(text_content, collapse = "\n")

          # Update batch data
          batch_data$file_path[j] <- text_file
          batch_data$content[j] <- text_content
        } else {
          cli::cli_warn(c(
            "!" = "Text file not found for document {offset + j}.",
            "i" = paste0("Expected: ", text_file)
          ))
        }
      } else {
        cli::cli_warn(c(
          "!" = "Missing 'folder' column in metadata.",
          "i" = "Cannot locate text files without folder information."
        ))
        break
      }
    }

    # Update the database with the content
    for (j in seq_len(nrow(batch_data))) {
      # Use parameterized query to safely handle text content
      update_query <- "UPDATE corpus SET file_path = ?, content = ? WHERE folder = ?;"
      DBI::dbExecute(
        con,
        update_query,
        params = list(
          batch_data$file_path[j],
          batch_data$content[j],
          batch_data$folder[j]
        )
      )
    }

    # Report progress
    cli::cli_progress_update(set = i)
  }

  cli::cli_alert_success(
    "Successfully created DuckDB database at {.file {db_path}}"
  )
  cli::cli_alert_info("The database contains:")
  cli::cli_ul(c(
    "corpus table: Combined metadata and document content"
  ))

  cli::cli_alert_info(
    "Database created at {.path {db_path}}. Connect with: {.code con <- DBI::dbConnect(duckdb::duckdb(), '{db_path}')}"
  )
  invisible(db_path)
}
