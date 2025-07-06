#' Miscellaneous helper functions for ScholarAI
#'
#' TODO: optionally use `tokenizer` to count actual tokens
n_token <- function(text, method = 'approximate') {
  if (method == 'approximate') return(nchar(text) / 4) else
    stop("Method not recognized")
}
