#' After generating the text corpus, this function converts it to a DuckDB database.
#'
#' Creates a DuckDB database at the output directory root containing both the corpus metadata
#' and the actual text content from the file paths. Uses a lazy approach to avoid loading
#' all text into memory at once. Stores everything in a single table for efficient columnar storage.
#'
#' @param output_dir The directory containing the corpus metadata and text files
#' @param db_name The name of the DuckDB database file (default: "corpus.duckdb")
#' @param batch_size Number of documents to process in each batch (default: 100)
#' @return Invisibly returns the path to the created DuckDB database
#'         Use `DBI::dbConnect(duckdb::duckdb(), "path/to/database.duckdb")` to connect to the database
#'
#' @examples
#' # corpus_to_duckdb("my_results")
#' @export
corpus_to_duckdb <- function(
  output_dir,
  db_name = "corpus.duckdb",
  batch_size = 100
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

  # Check if output_dir exists
  if (!dir.exists(output_dir)) {
    cli::cli_abort(c(
      "x" = "Output directory does not exist.",
      "i" = paste0("Checked: ", output_dir)
    ))
  }

  # Check for corpus metadata
  metadata_path <- file.path(output_dir, "corpus_metadata.json")
  if (!file.exists(metadata_path)) {
    cli::cli_abort(c(
      "x" = "Corpus metadata file not found.",
      "i" = paste0("Expected: ", metadata_path),
      "i" = "Run save_corpus_metadata() first."
    ))
  }

  # Create database path
  db_path <- file.path(output_dir, db_name)

  # Connect to DuckDB
  cli::cli_alert_info("Creating DuckDB database at {.file {db_path}}")
  con <- DBI::dbConnect(duckdb::duckdb(), db_path)
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE), add = TRUE)

  # Load metadata
  cli::cli_alert_info("Loading corpus metadata")
  metadata <- jsonlite::read_json(metadata_path, simplifyVector = TRUE)

  # Create corpus table
  cli::cli_alert_info("Creating corpus table")

  # First create an empty table with the right schema
  # We'll add text content to this table in batches
  metadata$file_path <- NA_character_
  metadata$content <- NA_character_

  DBI::dbWriteTable(con, "corpus", metadata, overwrite = TRUE)

  # Remove the placeholder rows
  DBI::dbExecute(con, "DELETE FROM corpus")

  # Prepare for batch processing
  total_docs <- nrow(metadata)
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
    batch_metadata <- metadata[start_idx:end_idx, ]

    # Get the batch of metadata
    batch_data <- batch_metadata

    # Add columns for file_path and content
    batch_data$file_path <- NA_character_
    batch_data$content <- NA_character_

    # Process each document in the batch
    for (j in seq_len(nrow(batch_data))) {
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
          "!" = "Text file not found for document {start_idx + j - 1}.",
          "i" = paste0("Expected: ", text_file)
        ))
      }
    }

    # Insert batch into database
    DBI::dbAppendTable(con, "corpus", batch_data)

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
