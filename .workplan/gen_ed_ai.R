# Ed Pinto AI
INSTR_PATH <- "exploration/large_context/ask_ed_ai/instructions.md"
TPL_PATH <- list.files(
  "W:/Amanda/Projects/Arthur/Scenario Analysis for Smaller Lots/playbook",
  "housing_playbook_v.*.qmd",
  full.names = TRUE
) |>
  rev() |>
  head(1)
PLAYBOOK_QS <- "W:/arthur/misc/202506_as_built_shiny/data/playbook_master_data.qs"
TXT_DIR <- "W:/arthur/202404_heat_ai/data/intermed/aei_search_results/"

# Regex/constants --------------------------------------------------------------
EXCLUSIONS_RE <- "(events|multimedia|press|podcasts)"
OPED_RE <- "op-eds"


# ---- User-facing API ---------------------------------------------------------
#' Edward Pinto AI helper – dispatcher.
#'
#' @param chat_text Text query to send to the AI
#' @param type Type of response to generate, either "generic" or "oped"
#' @param geoid Geographic ID to use for context (default: "0")
#' @param target_words Target word count for the response (default: NULL)
#' @param context_type Type of context to use (default: "auto")
#' @param n_docs Number of documents to use for context (default: 25)
#' @param temperature Model temperature parameter (default: 0.7)
#' @param ... Additional parameters passed to the underlying methods
#' @export

#' Generate an Edward Pinto op-ed
#' @inheritParams gen_ed_ai
#' @export

#' Generate an Edward Pinto op-ed.
#'
#' This is the *oped* method for `gen_ed_ai()`. It first constructs the op-ed
#' style prompt and then delegates to `.gen_ed_process()`.
#' @inheritParams gen_ed_ai
#' @export
#' @examples
#' gen_ed_oped("19153", "Housing affordability is worsening…")
gen_ed_oped <- function(chat_text, geoid = "0", target_words = 800, ...) {
  prompt <- glue::glue(
    "You are Edward Pinto. You have just finished writing an op-ed in response \n    to the following query: {chat_text}. Provide the full and final text of that\n    op-ed, starting with the headline. The output should be *only* the op-ed."
  )

  .gen_ed_process(
    prompt,
    type = "oped",
    geoid = geoid,
    target_words = target_words,
    ...
  )
}


#' Generic chat helper (fallback method for `gen_ed_ai`).
#' @inheritParams gen_ed_ai
#' @export
gen_ed_generic <- function(chat_text, geoid = "0", target_words = NULL, ...) {
  .gen_ed_process(
    chat_text,
    type = "generic",
    geoid = geoid,
    target_words = target_words,
    ...
  )
}


#' Edward Pinto AI helper – dispatcher.
#' (Roxygen above)
#' @inheritParams gen_ed_ai
#' @export
gen_ed_ai <- function(chat_text, type = c("generic", "oped"), ...) {
  switch(
    match.arg(type),
    oped = gen_ed_oped(chat_text, ...),
    generic = gen_ed_generic(chat_text, ...)
  )
}


# ---- Helpers -----------------------------------------------------------------
#' @keywords internal
.sample_context_files <- function(type, n_docs, txt_dir) {
  all_files <- fs::dir_ls(txt_dir, regexp = "\\.txt$", recurse = TRUE)
  files_pool <- all_files[!grepl(EXCLUSIONS_RE, all_files, ignore.case = TRUE)]
  opeds <- grep(OPED_RE, files_pool, value = TRUE, ignore.case = TRUE)

  if (type == "oped") {
    n_op <- min(round(n_docs * 0.75), length(opeds))
    c(sample(opeds, n_op), sample(setdiff(files_pool, opeds), n_docs - n_op))
  } else {
    sample(files_pool, min(n_docs, length(files_pool)))
  }
}

#' @keywords internal
.add_len_limit <- function(txt, n) {
  if (is.null(n)) return(txt)
  glue::glue("{txt}\n\n(Please limit your response to about {n} words.)")
}

# ---- Additional internal helpers ---------------------------------------------
#' @keywords internal
.safe_playbook <- function(geoid) {
  tryCatch(
    {
      if (!is.null(geoid)) {
        qs::qread(PLAYBOOK_QS) |>
          dplyr::filter(geoid == !!geoid) |>
          yyjsonr::write_json_str()
      }
    },
    error = function(e) NULL
  )
}

#' @keywords internal
.build_system_prompt <- function(context_blob) {
  paste0(
    if (fs::file_exists(INSTR_PATH)) readr::read_file(INSTR_PATH) else
      stop("Missing instructions"),
    "\n\nContext docs:\n\n",
    context_blob,
    "\n\nHousing Playbook template:\n\n",
    if (fs::file_exists(TPL_PATH)) readr::read_file(TPL_PATH) else
      stop("Missing playbook template")
  )
}


# ---- Internal core process ---------------------------------------------------
#' Internal helper that does the heavy lifting once the user prompt has been
#' constructed. **Not exported**.
.gen_ed_process <- function(
  chat_text,
  type = c("generic", "oped"),
  geoid = "0",
  n_docs = 25,
  txt_dir = TXT_DIR,
  model = "google/gemini-2.5-pro",
  target_words = NULL,
  api_args = list(temperature = 0.7),
  api_key = ellmer:::openrouter_key(),
  ...
) {
  type <- match.arg(type)

  # --------------------------------------------------------------------------
  # 1. Build context corpus ---------------------------------------------------
  # --------------------------------------------------------------------------
  samp <- .sample_context_files(type, n_docs, txt_dir)

  text_blob <- paste(purrr::map_chr(samp, readr::read_file), collapse = "\n\n")

  # --------------------------------------------------------------------------
  # 2. Playbook JSON & system prompt ------------------------------------------
  # --------------------------------------------------------------------------
  pb_json <- .safe_playbook(geoid)
  sys <- .build_system_prompt(text_blob)

  chat <- ellmer::chat_openrouter(
    model,
    system_prompt = sys,
    api_args = api_args,
    api_key = api_key,
    ...
  )

  # Append playbook data for op-eds ------------------------------------------
  if (type == "oped" && !is.null(pb_json))
    chat_text <- glue::glue("{chat_text}\n\nPlaybook data: {pb_json}")

  # Optional length constraint -----------------------------------------------
  chat_text <- .add_len_limit(chat_text, target_words)

  list(chat = chat, response = chat$chat(chat_text))
}
