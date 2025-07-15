#' Miscellaneous helper functions for ScholarAI
#'
#' TODO: optionally use `tokenizer` to count actual tokens
n_token <- function(text, method = 'approximate') {
  if (method == 'approximate') return(nchar(text) / 4) else
    stop("Method not recognized")
}

#' Attempt to install and load VSS (HNSW index) extension
#'
#' @keywords internal
load_vss <- function(con){
  vss_loaded <- tryCatch(
    {
      # First check if the extension exists
      extensions <- DBI::dbGetQuery(con, "SELECT * FROM duckdb_extensions() WHERE extension_name = 'vss'")

      if (nrow(extensions) > 0) {
        # Try to load it if it exists
        DBI::dbExecute(con, "INSTALL vss")
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
  return(vss_loaded)
}
