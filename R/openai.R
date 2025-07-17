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
#' @param timeout_sec Maximum time in seconds to wait for a single embedding request (default 5).
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
  pause_sec = 0.1,
  timeout_sec = 2
) {
  check_embedding_input(texts, openai_key, max_attempts, pause_sec, timeout_sec)

  if (length(texts) == 0) {
    return(matrix(numeric(0), nrow = 0, ncol = 0))
  }

  results <- get_openai_embedding_batch(
    texts = texts,
    model = model,
    openai_key = openai_key,
    max_attempts = max_attempts,
    pause_sec = pause_sec,
    timeout_sec = timeout_sec
  )

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

# Internal: batch POST/retry logic for OpenAI embeddings
get_openai_embedding_batch <- function(
  texts,
  model,
  openai_key,
  max_attempts,
  pause_sec,
  timeout_sec
) {
  batch_limit <- 96
  batches <- split(seq_along(texts), ceiling(seq_along(texts)/batch_limit))
  results <- vector("list", length(texts))
  for (b in batches) {
    batch_texts <- texts[b]
    attempt <- 1
    repeat {
      resp <- tryCatch(
        httr::POST(
          url = "https://api.openai.com/v1/embeddings",
          httr::add_headers(Authorization = paste("Bearer", openai_key)),
          httr::content_type_json(),
          httr::timeout(timeout_sec),
          body = yyjsonr::write_json_str(
            list(input = batch_texts, model = model),
            auto_unbox = TRUE
          )
        ),
        error = function(e) {
          cli::cli_warn(c(
            "!" = paste("HTTP error in OpenAI batch API call:", as.character(e)),
            "i" = paste("Attempt", attempt, "/", max_attempts)
          ))
          return(e)
        }
      )
      if (inherits(resp, "error")) {
        if (attempt >= max_attempts) {
          cli::cli_warn(c(
            "!" = "Failed to get embedding batch after max attempts due to HTTP error",
            "i" = paste0("Batch texts: ", paste(substr(batch_texts, 1, 30), collapse = ", "))
          ))
          for (i in seq_along(b)) results[[b[i]]] <- NA
          break
        }
      } else if (inherits(resp, "response")) {
        if (httr::status_code(resp) == 200) {
          content <- tryCatch(httr::content(resp), error = function(e) NULL)
          if (!is.null(content) && !is.null(content$data) && length(content$data) == length(batch_texts)) {
            for (i in seq_along(batch_texts)) {
              emb <- content$data[[i]]$embedding
              results[[b[i]]] <- if (!is.null(emb)) as.numeric(emb) else NA
            }
            break
          } else {
            cli::cli_warn(c(
              "!" = "API response missing expected embedding data (batch)",
              "i" = "Response structure may have changed"
            ))
            for (i in seq_along(b)) results[[b[i]]] <- NA
            break
          }
        } else {
          status_code <- httr::status_code(resp)
          content <- tryCatch(httr::content(resp), error = function(e) NULL)
          error_message <- if (!is.null(content) && !is.null(content$error)) content$error$message else "Unknown error"
          cli::cli_warn(c(
            "!" = paste("API returned status code", status_code),
            "i" = paste("Error:", error_message),
            "i" = paste("Attempt", attempt, "/", max_attempts)
          ))
        }
      }
      if (attempt >= max_attempts) {
        for (i in seq_along(b)) results[[b[i]]] <- rep(NA_real_, 1536)
        break
      }
      attempt <- attempt + 1
      Sys.sleep(pause_sec)
    }
  }
  results
}



#' function for single embedding
get_single_embedding <- function(
  txt,
  model = "text-embedding-3-small",
  openai_key = Sys.getenv("OPENAI_API_KEY"),
  max_attempts = 3,
  pause_sec = 0.1,
  timeout_sec = 2
) {
  if (is.na(txt) || txt == "") {
    cli::cli_warn("Empty or NA text provided to get_single_embedding")
    return(NA)
  }

  # Truncate very long text for warning messages (used in error messages below)
  display_txt <- if (nchar(txt) > 50) paste0(substr(txt, 1, 50), "...") else txt

  attempt <- 1
  repeat {
    resp <- tryCatch(
      httr::POST(
        url = "https://api.openai.com/v1/embeddings",
        httr::add_headers(Authorization = paste("Bearer", openai_key)),
        httr::content_type_json(),
        httr::timeout(timeout_sec),
        body = yyjsonr::write_json_str(
          list(input = txt, model = model),
          auto_unbox = TRUE
        )
      ),
      error = function(e) {
        cli::cli_warn(c(
          "!" = "HTTP error in OpenAI API call: {as.character(e)}",
          "i" = "Attempt {attempt}/{max_attempts}"
        ))
        return(e)
      }
    )

    # Check if response is an error object
    if (inherits(resp, "error")) {
      if (attempt >= max_attempts) {
        cli::cli_warn(c(
          "!" = "Failed to get embedding after {max_attempts} attempts due to HTTP error",
          "i" = "Text: '{display_txt}'"
        ))
        return(NA)
      }
    } else if (inherits(resp, "response")) { # Check if response is a valid HTTP response
      if (httr::status_code(resp) == 200) {
        # Success case
        content <- tryCatch(
          httr::content(resp),
          error = function(e) {
            cli::cli_warn(c(
              "!" = "Failed to parse API response: {as.character(e)}",
              "i" = "Status code was 200 but content parsing failed"
            ))
            return(NULL)
          }
        )

        if (
          !is.null(content) &&
            !is.null(content$data) &&
            length(content$data) > 0 &&
            !is.null(content$data[[1]]$embedding)
        ) {
          return(as.numeric(content$data[[1]]$embedding))
        } else {
          cli::cli_warn(c(
            "!" = "API response missing expected embedding data",
            "i" = "Response structure may have changed"
          ))
        }
      } else {
        # Non-200 status code
        status_code <- httr::status_code(resp)
        content <- tryCatch(httr::content(resp), error = function(e) NULL)
        error_message <- if (!is.null(content) && !is.null(content$error)) {
          content$error$message
        } else {
          "Unknown error"
        }

        # Use the variables in the warning message to avoid lint warnings
        cli::cli_warn(c(
          "!" = "API returned status code {status_code}",
          "i" = "Error: {error_message}",
          "i" = "Attempt {attempt}/{max_attempts}"
        ))
      }
    }

    if (attempt >= max_attempts) {
      display_txt <- substr(txt, 1, 50)
      if (nchar(txt) > 50) display_txt <- paste0(display_txt, "...")
      cli::cli_warn(c(
        "!" = "Failed to get embedding after {max_attempts} attempts",
        "i" = "Text: '{display_txt}'"
      ))
      return(rep(NA_real_, 1536))
    }

    attempt <- attempt + 1
    Sys.sleep(pause_sec)
  }
}

check_embedding_input <- function(
  texts,
  openai_key,
  max_attempts,
  pause_sec,
  timeout_sec
) {
  if (!is.character(texts)) {
    cli::cli_abort(
      c(
        "x" = "{.arg texts} must be a character vector.",
        "!" = "You supplied an object of type {.cls {class(texts)}}.",
        "i" = "First few values: {.val {head(substr(texts, 1, 50), 3)}}"
      )
    )
  }
  if (!nzchar(openai_key)) stop("No OpenAI API key provided.")
  if (!is.numeric(max_attempts) || max_attempts < 1)
    stop("max_attempts must be >= 1")
  if (!is.numeric(pause_sec) || pause_sec < 0) stop("pause_sec must be >= 0")
  if (!is.numeric(timeout_sec) || timeout_sec <= 0)
    stop("timeout_sec must be > 0")
}
