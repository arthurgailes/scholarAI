#' Miscellaneous helper functions for ScholarAI
#'
#' @description Token estimation function for text processing
#' @param text Text to estimate token count for
#' @return Estimated token count
#' @keywords internal
n_token <- function(text) {
  if (is.na(text) || text == "") return(0)
  
  # Conservative approximation (1 token ~ 2 chars)
  # This helps prevent exceeding token limits
  return(ceiling(nchar(text) / 2))
}

#' Attempt to install and load VSS (HNSW index) extension
#'
#' @keywords internal
load_vss <- function(con){
  vss_loaded <- tryCatch(
    {
      # First check if the extension exists
      extensions <- DBI::dbGetQuery(con, "SELECT * FROM duckdb_extensions() WHERE extension_name = 'vss'")

      if (nrow(extensions) == 0 || isFALSE(extensions$loaded)) {
        DBI::dbExecute(con, "INSTALL vss")
        DBI::dbExecute(con, "LOAD vss")
      }
      TRUE
    },
    error = function(e) {
      FALSE
    }
  )
  return(vss_loaded)
}
