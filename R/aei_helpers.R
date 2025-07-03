#' AEI scraping helpers and utilities
#'
#' @description
#' Helper and utility functions for AEI scraping pipeline (folder name, regex, etc).
#' Not exported.
#'
#' @keywords internal
NULL

#' Convert a URL path into a safe folder name (one nested level allowed)
#'
#' @param url Character URL.
#' @return Character folder path relative to output directory.
create_folder_name <- function(url) {
  parsed_url <- urltools::url_parse(url)
  path <- parsed_url$path
  path <- gsub("^/|/$", "", path)
  parts <- strsplit(path, "/", fixed = TRUE)[[1]]
  if (length(parts) == 0) return(path)
  if (length(parts) >= 2) {
    first_part <- parts[1]
    rest_parts <- paste(parts[-1], collapse = "-")
    return(paste(first_part, rest_parts, sep = "/"))
  }
  path
}

#' Get date pattern regex for AEI article dates
#'
#' @return A regex pattern string for matching various date formats in AEI articles
#' @export
date_patterns <- function() {
  paste0(
    "(",
    paste(
      c(
        "(?:Jan(?:uary)?|Feb(?:ruary)?|Mar(?:ch)?|Apr(?:il)?|May|Jun(?:e)?|Jul(?:y)?|Aug(?:ust)?|",
        "Sep(?:tember)?|Oct(?:ober)?|Nov(?:ember)?|Dec(?:ember)?)[[:space:]]+\\d{1,2},[[:space:]]+\\d{4}",
        "(?:Jan(?:uary)?|Feb(?:ruary)?|Mar(?:ch)?|Apr(?:il)?|May|Jun(?:e)?|Jul(?:y)?|Aug(?:ust)?|",
        "Sep(?:tember)?|Oct(?:ober)?|Nov(?:ember)?|Dec(?:ember)?)[[:space:]]+\\d{4}",
        "Week[[:space:]]+\\d{1,2}(?:[[:space:]]+&[[:space:]]+\\d{1,2})?,[[:space:]]+\\d{4}",
        "Q[1-4][[:space:]]+\\d{4}",
        "\\d{4}",
        "\\d{8}"
      ),
      collapse = "|"
    ),
    ")"
  )
}

#' Return first element of x or default if empty
#' @keywords internal
first_or <- function(x, default) if (length(x) == 0) default else x[[1]]
