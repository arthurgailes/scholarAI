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
    cli::cli_warn(c(
      "! No metadata.json files found in directory.",
      "i" = paste0("Checked: ", root_dir)
    ))
    return(data.frame())
  }

  # Helper to safely read one metadata.json
  read_meta <- function(path) {
    tryCatch(
      {
        meta <- jsonlite::read_json(path, simplifyVector = TRUE)
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

#' Save the corpus metadata as a JSON file in the output root
#'
#' Reads all metadata.json files under the given root directory, combines into a dataframe,
#' and writes the result as corpus_metadata.json in the root using yyjsonr::write_json_file.
#'
#' @param root_dir The root directory containing article subfolders with metadata.json files.
#' @param file_name The name of the output file. Default is "corpus_metadata.json".
#'
#' @return Invisibly returns the path to the written file.
#'
#' @examples
#' # save_corpus_metadata("my_results")
#' @export
save_corpus_metadata <- function(
  root_dir,
  file_name = "corpus_metadata.json"
) {
  df <- text_corpus_to_df(root_dir)
  out_path <- file.path(root_dir, file_name)
  yyjsonr::write_json_file(df, out_path, pretty = TRUE, auto_unbox = TRUE)
  invisible(out_path)
}
