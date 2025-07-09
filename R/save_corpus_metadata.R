#' Save the corpus metadata as a JSON file in the output root
#'
#' Reads all metadata.json files under the given root directory, combines into a dataframe,
#' and writes the result as corpus_metadata.json in the root using yyjsonr::write_json_file.
#'
#' @param root_dir The root directory containing article subfolders with metadata.json files.
#' @param file_name The name of the output file. Default is "corpus_metadata.json".
#' @param id_col The column to use as the id. Default is "seq" for sequential ids.
#'
#' @return Invisibly returns the metadata as a data.frame
#'
#' @examples
#' # save_corpus_metadata("my_results")
#' @export
save_corpus_metadata <- function(
  root_dir,
  file_name = "corpus_metadata.json",
  id_col = "seq"
) {
  df <- text_corpus_to_df(root_dir)

  # Add id column
  if (identical(id_col, "seq")) {
    df$id <- seq_len(nrow(df))
  } else if (id_col %in% names(df)) {
    df$id <- df[[id_col]]
  } else {
    stop("id_col must be 'seq' or a valid column name")
  }

  if (anyDuplicated(df$id) != 0) {
    stop("corpus id column is not unique")
  }

  out_path <- file.path(root_dir, file_name)
  yyjsonr::write_json_file(df, out_path, pretty = TRUE, auto_unbox = TRUE)
  invisible(df)
}


#' Convert a directory of AEI article folders into a dataframe via metadata.json
#'
#' Recursively searches for all metadata.json files under the given root directory and
#' combines their contents into a single dataframe. Each row corresponds to one article's metadata.
#'
#' @param root_dir The root directory containing article subfolders with metadata.json files.
#'
#' @return A dataframe (tibble) with one row per metadata.json and columns for all metadata fields.
#'
#' @examples
#' # Suppose you have run scrape_aei() with output_root = "my_results"
#' # df <- text_corpus_to_df("my_results")
#' # save_corpus_metadata("my_results")
#' @export
text_corpus_to_df <- function(
  root_dir
) {
  # Find all metadata.json files recursively
  meta_files <- list.files(
    root_dir,
    pattern = "^metadata\\.json$",
    recursive = TRUE,
    full.names = TRUE
  )

  if (length(meta_files) == 0) {
    stop("! No metadata.json files found in ", root_dir)
  }

  # Helper to safely read one metadata.json
  read_meta <- function(path) {
    tryCatch(
      {
        meta <- yyjsonr::read_json_file(path)
        # Always add a column for the folder path
        meta$folder <- dirname(path)
        meta
      },
      error = function(e) {
        cli::cli_warn(c(
          paste0("! Failed to read metadata: ", path),
          "i" = e$message
        ))
        NULL
      }
    )
  }

  # Read all metadata
  meta_list <- lapply(meta_files, read_meta)
  meta_list <- Filter(Negate(is.null), meta_list)

  if (length(meta_list) == 0) {
    cli::cli_warn("! No valid metadata.json files could be read.")
    return(data.frame())
  }

  # Bind to dataframe; handle list-columns (e.g. authors)
  df <- as.data.frame(
    do.call(
      rbind,
      lapply(meta_list, function(x) {
        # Ensure all fields present as columns
        fields <- unique(unlist(lapply(meta_list, names)))
        x[setdiff(fields, names(x))] <- NA
        # Flatten authors if it's a list
        if (is.list(x$authors) && !is.character(x$authors)) {
          x$authors <- paste(unlist(x$authors), collapse = ", ")
        }
        x[fields]
      })
    ),
    stringsAsFactors = FALSE
  )

  rownames(df) <- NULL
  df
}
