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
HMI_RE <- "(hmi|housing-market-indicators)"


# ---- User-facing API ---------------------------------------------------------
#' Edward Pinto AI helper – dispatcher.
#'
#' @param chat_text Text query to send to the AI
#' @param type Type of response to generate, either "generic", "playbook", "oped", or "hmi"
#' @param target_words Target word count for the response (default: NULL)
#' @param context_type Type of context to use (default: "auto")
#' @param n_docs Number of documents to use for context (default: 25)
#' @param api_args Model API arguments (default: list(temperature = 0.7))
#' @param api_key API key (default: ellmer:::openrouter_key())
#' @param system_prompt Custom system prompt (default: NULL, uses instructions from file)
#' @param ... Additional parameters passed to the underlying methods
#' @export
gen_ed_ai <- function(
  chat_text,
  type = c("generic", "playbook", "oped", "hmi"),
  target_words = NULL,
  context_type = "auto",
  n_docs = 25,
  api_args = list(temperature = 0.7),
  api_key = ellmer:::openrouter_key(),
  system_prompt = NULL,
  ...
) {
  switch(
    match.arg(type),
    oped = gen_ed_oped(chat_text, target_words = target_words, n_docs = n_docs, api_args = api_args, api_key = api_key, system_prompt = system_prompt, ...),
    playbook = gen_ed_playbook(chat_text, target_words = target_words, n_docs = n_docs, api_args = api_args, api_key = api_key, system_prompt = system_prompt, ...),
    hmi = gen_ed_hmi(chat_text, target_words = target_words, n_docs = n_docs, api_args = api_args, api_key = api_key, system_prompt = system_prompt, ...),
    generic = gen_ed_generic(chat_text, target_words = target_words, n_docs = n_docs, api_args = api_args, api_key = api_key, system_prompt = system_prompt, ...)
  )
}

#' Generate an Edward Pinto op-ed.
#'
#' This is the *oped* method for `gen_ed_ai()`. It first constructs the op-ed
#' style prompt and then delegates to `.gen_ed_process()`.
#' @inheritParams gen_ed_ai
#' @export
#' @examples
#' gen_ed_oped("19153", "Housing affordability is worsening…")
gen_ed_oped <- function(chat_text, ...) {
  prompt <- glue::glue(
    "You are Edward Pinto. You have just finished writing an op-ed in response \n    to the following query: {chat_text}. Provide the full and final text of that\n    op-ed, starting with the headline. The output should be *only* the op-ed."
  )

  .gen_ed_process(
    prompt,
    type = "oped",
    ...
  )
}


#' Generic chat helper (fallback method for `gen_ed_ai`).
#' This version doesn't use the playbook data.
#' @inheritParams gen_ed_ai
#' @export
gen_ed_generic <- function(
  chat_text,
  n_docs = 25,
  txt_dir = TXT_DIR,
  model = "google/gemini-2.5-pro",
  target_words = NULL,
  api_args = list(temperature = 0.7),
  api_key = ellmer:::openrouter_key(),
  system_prompt = NULL,
  ...
) {
  # Sample context files
  samp <- .sample_context_files("generic", n_docs, txt_dir)
  text_blob <- paste(purrr::map_chr(samp, readr::read_file), collapse = "\n\n")

  # Build system prompt without playbook
  sys <- if (!is.null(system_prompt)) {
    # If custom system prompt is provided, use it as-is
    system_prompt
  } else if (fs::file_exists(INSTR_PATH)) {
    paste0(readr::read_file(INSTR_PATH), "\n\nContext docs:\n\n", text_blob)
  } else {
    stop("Missing instructions and no custom system prompt provided")
  }

  # Create chat instance
  # Remove system_prompt from dots to avoid duplicate parameter
  dots <- list(...)
  dots$system_prompt <- NULL
  
  chat <- do.call(
    ellmer::chat_openrouter,
    c(
      list(
        model = model,
        system_prompt = sys,
        api_args = api_args,
        api_key = api_key
      ),
      dots
    )
  )

  # Optional length constraint
  chat_text <- .add_len_limit(chat_text, target_words)

  list(chat = chat, response = chat$chat(chat_text))
}


#' Playbook-based chat helper.
#' This version uses the playbook data like the original generic function.
#' @inheritParams gen_ed_ai
#' @param geoid Geographic ID to use for context (default: "0")
#' @export
gen_ed_playbook <- function(chat_text, geoid = "0", ...) {
  .gen_ed_process(
    chat_text,
    type = "generic",
    geoid = geoid,
    ...
  )
}


#' Housing Market Indicators (HMI) chat helper.
#' This version filters documents to only include those with 'hmi' or 'housing-market-indicators'.
#' @inheritParams gen_ed_ai
#' @export
gen_ed_hmi <- function(chat_text, ...) {
  .gen_ed_process(
    chat_text,
    type = "hmi",
    ...
  )
}


# ---- Helpers -----------------------------------------------------------------
#' @keywords internal
.sample_context_files <- function(type, n_docs, txt_dir) {
  all_files <- fs::dir_ls(txt_dir, regexp = "\\.txt$", recurse = TRUE)
  files_pool <- all_files[!grepl(EXCLUSIONS_RE, all_files, ignore.case = TRUE)]
  opeds <- grep(OPED_RE, files_pool, value = TRUE, ignore.case = TRUE)
  hmi_files <- grep(HMI_RE, files_pool, value = TRUE, ignore.case = TRUE)

  if (type == "oped") {
    n_op <- min(round(n_docs * 0.75), length(opeds))
    c(sample(opeds, n_op), sample(setdiff(files_pool, opeds), n_docs - n_op))
  } else if (type == "hmi") {
    if (length(hmi_files) == 0) {
      warning("No HMI files found. Using generic files instead.")
      sample(files_pool, min(n_docs, length(files_pool)))
    } else {
      sample(hmi_files, min(n_docs, length(hmi_files)))
    }
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
.safe_playbook <- function(p_geoid) {
  df <- qs::qread(PLAYBOOK_QS) |> collapse::fsubset(geoid == p_geoid)
  if (nrow(df) == 0) stop(paste("no playbook data for geoid", p_geoid))
  yyjsonr::write_json_str(df)
}

#' @keywords internal
.build_system_prompt <- function(context_blob, pb_json, system_prompt = NULL) {
  # If a custom system prompt is provided, use it as-is without adding context
  if (!is.null(system_prompt)) {
    return(system_prompt)
  }
  
  # Otherwise, build the default system prompt with all context
  paste0(
    if (fs::file_exists(INSTR_PATH)) readr::read_file(INSTR_PATH) else
      stop("Missing instructions and no custom system prompt provided"),
    "\n\nContext docs:\n\n",
    context_blob,
    "\n\nHere is the housing supply playbook template, which we use for :\n\n",
    if (fs::file_exists(TPL_PATH)) readr::read_file(TPL_PATH) else
      stop("Missing playbook template"),
    "\n\nPlaybook data:\n\n",
    pb_json
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
  system_prompt = NULL,
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
  sys <- .build_system_prompt(text_blob, pb_json, system_prompt)

  # Remove system_prompt from dots to avoid duplicate parameter
  dots <- list(...)
  dots$system_prompt <- NULL
  
  chat <- do.call(
    ellmer::chat_openrouter,
    c(
      list(
        model = model,
        system_prompt = sys,
        api_args = api_args,
        api_key = api_key
      ),
      dots
    )
  )

  # Optional length constraint -----------------------------------------------
  chat_text <- .add_len_limit(chat_text, target_words)

  list(chat = chat, response = chat$chat(chat_text))
}
