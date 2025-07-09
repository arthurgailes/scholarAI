#' Get OpenAI Embeddings for a Character Vector
#'
#' @description
#' Retrieves embeddings from the OpenAI API for an arbitrary character vector, with transparent retry and failure logic.
#'
#' @param texts Character vector of texts to embed.
#' @param model OpenAI embedding model to use. Default is 'text-embedding-3-small'.
#' @param openai_key OpenAI API key. If NULL, will use OPENAI_API_KEY env var.
#' @param max_attempts Maximum number of attempts per request (default 3).
#' @param pause_sec Seconds to pause between retries (default 2).
#' @return A numeric matrix of embeddings (rows = input texts).
#' @examples
#' \dontrun{
#'   emb <- get_openai_embeddings(c("hello", "world"), openai_key = "sk-...yourkey...")
#' }
#' @export
get_openai_embeddings <- function(
  texts,
  model = "text-embedding-3-small",
  openai_key = Sys.getenv("OPENAI_API_KEY"),
  max_attempts = 3,
  pause_sec = 0.1
) {
  check_embedding_input(texts, openai_key, max_attempts, pause_sec)

  if (length(texts) == 0) {
    return(matrix(numeric(0), nrow = 0, ncol = 0))
  }

  results <- lapply(texts, function(txt) {
    get_single_embedding(txt, model, openai_key, max_attempts, pause_sec)
  })

  emb_dim <- max(vapply(
    results,
    function(x) if (is.numeric(x)) length(x) else 0,
    numeric(1)
  ))
  mat <- t(vapply(
    results,
    function(x)
      if (is.numeric(x) && length(x) == emb_dim) x else rep(NA_real_, emb_dim),
    numeric(emb_dim)
  ))
  rownames(mat) <- NULL
  
  mat
}

#' function for single embedding
get_single_embedding <- function(
  txt,
  model = "text-embedding-3-small",
  openai_key = Sys.getenv("OPENAI_API_KEY"),
  max_attempts = 3,
  pause_sec = 0.1
) {
  attempt <- 1
  repeat {
    resp <- tryCatch(
      httr::POST(
        url = "https://api.openai.com/v1/embeddings",
        httr::add_headers(Authorization = paste("Bearer", openai_key)),
        httr::content_type_json(),
        body = yyjsonr::write_json_str(
          list(input = txt, model = model),
          auto_unbox = TRUE
        )
      ),
      error = function(e) e
    )
    if (inherits(resp, "response") && httr::status_code(resp) == 200) {
      return(as.numeric(httr::content(resp)$data[[1]]$embedding))
    }
    if (attempt >= max_attempts) {
      warning(sprintf(
        "Failed to get embedding for '%s' after %d attempts; returning NA.",
        txt, max_attempts
      ))
      return(NA)
    }
    attempt <- attempt + 1
    Sys.sleep(pause_sec)
  }
}

check_embedding_input <- function(texts, openai_key, max_attempts, pause_sec) {
  if (!is.character(texts)) stop("texts must be a character vector")
  if (!nzchar(openai_key)) stop("No OpenAI API key provided.")
  if (!is.numeric(max_attempts) || max_attempts < 1)
    stop("max_attempts must be >= 1")
  if (!is.numeric(pause_sec) || pause_sec < 0) stop("pause_sec must be >= 0")
}
