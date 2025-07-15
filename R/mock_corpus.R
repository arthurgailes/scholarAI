#' Create a mock corpus metadata data.frame for testing
#'
#' Generates a data.frame with columns matching those expected by corpus_to_duckdb, save_corpus_metadata, and related tests.
#' @param n Number of rows to generate
#' @param title Optional vector of titles
#' @param url Optional vector of URLs
#' @param authors Optional vector/list of authors
#' @param date Optional vector of publication dates
#' @param folder Optional vector of folder paths
#' @param filename Optional vector of filenames (default: "text.txt")
#' @param ... Additional named columns to add/override
#' @return data.frame suitable for use as corpus metadata in tests
make_mock_corpus_metadata <- function(
  n = 3,
  title = NULL,
  url = NULL,
  authors = NULL,
  date = NULL,
  folder = NULL,
  filename = NULL,
  root_dir = NULL,
  ...
) {
  # Sensible defaults for reproducible tests
  title <- title %||% paste("Article", seq_len(n))
  url <- url %||% paste0("https://example.com/", seq_len(n))
  authors <- authors %||% rep("Author A", n)
  date <- date %||% format(Sys.Date() - seq_len(n), "%Y-%m-%d")
  root_dir <- root_dir %||% tempdir()
  folder <- folder %||% file.path(root_dir, paste0("article", seq_len(n)))
  filename <- filename %||% rep("text.txt", n)

  df <- data.frame(
    id = seq_len(n),
    title = title,
    url = url,
    authors = authors,
    date = date,
    folder = folder,
    filename = filename,
    stringsAsFactors = FALSE
  )
  # Allow additional/override columns
  dots <- list(...)
  if (length(dots)) {
    for (nm in names(dots)) {
      df[[nm]] <- rep_len(dots[[nm]], n)
    }
  }
  df
}

make_mock_corpus_df <- function(..., content) {
  df <- make_mock_corpus_metadata()
  df$content <- paste0("Document ", seq_len(nrow(df)), " about topic.")
  return(df)
}

# Internal infix helper for defaulting
`%||%` <- function(x, y) if (is.null(x)) y else x
