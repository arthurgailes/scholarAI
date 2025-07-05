#' Introspect the corpus for the author's style and build a prompt for ScholarAI
#'
#' Analyzes a corpus of documents to extract an author's writing style, topics, and voice.
#' Processes the corpus in chunks if it's large, and progressively refines a markdown file
#' with instructions to replicate the scholar's style. The final output is a concise set
#' of instructions (max 1000 words) that captures the essence of the scholar.
#'
#' @param corpus_path Path to the corpus directory or DuckDB database
#' @param output_path Path where the markdown instructions file will be saved
#' @param model_name Name of the AI model to use (default: "google/gemini-2.5-pro")
#' @param batch_size Number of documents to process in each batch (default: 10)
#' @param max_token_per_batch Maximum number of tokens to include in each batch (default: 8000)
#' @param api_key Optional API key for the model service
#' @param verbose Whether to show progress information (default: TRUE)
#'
#' @return Invisibly returns the path to the created markdown file
#'
#' @examples
#' \dontrun{
#' # Process a corpus and generate scholar instructions
#' build_scholar_prompt("path/to/corpus", "scholar_instructions.md")
#' }
#' @export
build_scholar_prompt <- function(
  corpus_path,
  output_path = "scholar_instructions.md",
  model_name = "google/gemini-2.5-pro",
  batch_size = 10,
  max_token_per_batch = 8000,
  api_key = NULL,
  verbose = TRUE
) {
  # Check for required packages
  if (!requireNamespace("cli", quietly = TRUE)) {
    stop("Package 'cli' is required but not installed.")
  }

  # Validate inputs
  if (!dir.exists(corpus_path) && !file.exists(corpus_path)) {
    cli::cli_abort(c(
      "x" = "Corpus path does not exist.",
      "i" = paste0("Checked: ", corpus_path)
    ))
  }

  # Initialize progress reporting
  if (verbose) {
    cli::cli_alert_info("Starting analysis of scholar corpus")
  }

  # Determine if corpus_path is a DuckDB database or directory
  is_duckdb <- grepl("\\.duckdb$", corpus_path)

  # Load corpus data
  corpus_data <- load_corpus_data(corpus_path, is_duckdb)

  # Process corpus in batches
  instructions <- process_corpus_in_batches(
    corpus_data,
    batch_size,
    max_token_per_batch,
    model_name,
    api_key,
    verbose
  )

  # Write final instructions to file
  writeLines(instructions, output_path)

  if (verbose) {
    cli::cli_alert_success(
      "Scholar instructions written to {.file {output_path}}"
    )
  }

  invisible(output_path)
}

#' Load corpus data from either a DuckDB database or directory
#' @keywords internal
load_corpus_data <- function(corpus_path, is_duckdb) {
  if (is_duckdb) {
    if (
      !requireNamespace("DBI", quietly = TRUE) ||
        !requireNamespace("duckdb", quietly = TRUE)
    ) {
      cli::cli_abort(c(
        "x" = "Packages 'DBI' and 'duckdb' are required for database operations.",
        "i" = "Install them with: install.packages(c('DBI', 'duckdb'))"
      ))
    }

    # Connect to DuckDB and load corpus data
    con <- DBI::dbConnect(duckdb::duckdb(), corpus_path)
    on.exit(DBI::dbDisconnect(con, shutdown = TRUE), add = TRUE)

    # Check if corpus table exists
    if (!"corpus" %in% DBI::dbListTables(con)) {
      cli::cli_abort(c(
        "x" = "No 'corpus' table found in the database.",
        "i" = "Make sure the database was created with corpus_to_duckdb()."
      ))
    }

    # Read corpus data
    corpus_data <- DBI::dbReadTable(con, "corpus")
  } else {
    # Handle directory-based corpus
    # Look for text files in the corpus directory
    text_files <- list.files(
      corpus_path,
      pattern = "\\.txt$",
      recursive = TRUE,
      full.names = TRUE
    )

    if (length(text_files) == 0) {
      cli::cli_abort(c(
        "x" = "No text files found in the corpus directory.",
        "i" = paste0("Checked: ", corpus_path)
      ))
    }

    # Create a data frame with file paths
    corpus_data <- data.frame(
      file_path = text_files,
      stringsAsFactors = FALSE
    )

    # Add content column (will be populated during processing)
    corpus_data$content <- NA_character_
  }

  return(corpus_data)
}

#' Process corpus in batches and generate scholar instructions
#' @keywords internal
process_corpus_in_batches <- function(
  corpus_data,
  batch_size,
  max_token_per_batch,
  model_name,
  api_key,
  verbose
) {
  # Initialize instructions
  instructions <- ""
  total_docs <- nrow(corpus_data)
  batches <- ceiling(total_docs / batch_size)

  if (verbose) {
    cli::cli_alert_info(
      "Processing {total_docs} documents in {batches} batches"
    )

    # Create progress bar
    cli::cli_progress_bar(
      total = batches,
      format = "Processing batch {cli::pb_current}/{cli::pb_total} [{cli::pb_percent}%]",
      format_done = "Processed {cli::pb_total} batches [{cli::pb_elapsed}]",
      clear = FALSE
    )
  }

  # Process in batches
  for (i in seq_len(batches)) {
    start_idx <- (i - 1) * batch_size + 1
    end_idx <- min(i * batch_size, total_docs)
    batch_data <- corpus_data[start_idx:end_idx, ]

    # Load content for files if needed
    if (is.na(batch_data$content[1])) {
      for (j in seq_len(nrow(batch_data))) {
        if (file.exists(batch_data$file_path[j])) {
          text_content <- readLines(batch_data$file_path[j], warn = FALSE)
          batch_data$content[j] <- paste(text_content, collapse = "\n")
        }
      }
    }

    # Filter out any rows with NA content
    batch_data <- batch_data[!is.na(batch_data$content), ]

    if (nrow(batch_data) == 0) {
      if (verbose) {
        cli::cli_warn("Batch {i} contains no valid documents, skipping")
      }
      next
    }

    # Truncate content to respect token limits
    batch_text <- prepare_batch_text(batch_data$content, max_token_per_batch)

    # Update instructions with this batch
    instructions <- update_instructions(
      batch_text,
      instructions,
      model_name,
      api_key,
      verbose
    )

    # Update progress
    if (verbose) {
      cli::cli_progress_update(set = i)
    }
  }

  # Final refinement to ensure instructions are concise (<=1000 words)
  instructions <- finalize_instructions(
    instructions,
    model_name,
    api_key,
    verbose
  )

  return(instructions)
}

#' Prepare batch text by truncating to respect token limits
#' @keywords internal
prepare_batch_text <- function(content_list, max_token_per_batch) {
  combined_text <- paste(content_list, collapse = "\n\n---\n\n")

  # Approximate token count
  token_count <- n_token(combined_text)

  # Truncate if necessary
  if (token_count > max_token_per_batch) {
    # Simple truncation - in a real implementation, this could be more sophisticated
    truncation_ratio <- max_token_per_batch / token_count
    truncated_length <- floor(nchar(combined_text) * truncation_ratio)
    combined_text <- substr(combined_text, 1, truncated_length)
    combined_text <- paste0(
      combined_text,
      "\n\n[Content truncated due to length]"
    )
  }

  return(combined_text)
}

#' Update instructions based on new batch of text
#' @keywords internal
update_instructions <- function(
  batch_text,
  current_instructions,
  model_name,
  api_key,
  verbose
) {
  # First batch - generate initial instructions
  if (current_instructions == "") {
    prompt <- paste0(
      "Analyze the following text from a scholar's writings. ",
      "Extract the author's writing style, voice, topics of interest, and unique characteristics. ",
      "Create a markdown document with instructions (max 1000 words) that would help someone replicate this scholar's style and voice. ",
      "Focus on what makes their writing unique and recognizable.\n\n",
      "TEXT:\n",
      batch_text
    )
  } else {
    # Subsequent batches - refine existing instructions
    prompt <- paste0(
      "You previously created the following instructions to replicate a scholar's writing style:\n\n",
      current_instructions,
      "\n\nNow analyze this additional text from the same scholar and update your instructions. ",
      "Maintain or improve clarity while keeping the total length under 1000 words.\n\n",
      "ADDITIONAL TEXT:\n",
      batch_text
    )
  }

  # Call AI model to generate/update instructions
  response <- call_ai_model(prompt, model_name, api_key, verbose)

  # Extract markdown content from response
  new_instructions <- extract_markdown_content(response)

  return(new_instructions)
}

#' Finalize instructions to ensure they are concise and clear
#' @keywords internal
finalize_instructions <- function(instructions, model_name, api_key, verbose) {
  if (verbose) {
    cli::cli_alert_info("Finalizing scholar instructions")
  }

  # Count words in current instructions
  word_count <- length(strsplit(instructions, "\\s+")[[1]])

  # If already under 1000 words, just do a final polish
  if (word_count <= 1000) {
    prompt <- paste0(
      "Polish the following instructions for replicating a scholar's writing style. ",
      "Ensure they are clear, concise, and well-organized while maintaining all key insights. ",
      "Keep the total length under 1000 words.\n\n",
      instructions
    )
  } else {
    # Need to condense
    prompt <- paste0(
      "The following instructions for replicating a scholar's writing style are too long. ",
      "Condense them to under 1000 words while preserving all key insights about the scholar's ",
      "voice, style, topics, and unique characteristics.\n\n",
      instructions
    )
  }

  # Call AI model for final refinement
  response <- call_ai_model(prompt, model_name, api_key, verbose)

  # Extract markdown content
  final_instructions <- extract_markdown_content(response)

  return(final_instructions)
}

#' Call AI model with prompt
#' @keywords internal
call_ai_model <- function(prompt, model_name, api_key, verbose) {
  if (verbose) {
    cli::cli_alert_info("Calling AI model: {model_name}")
  }

  # Set up API arguments
  api_args <- list(temperature = 0.2) # Lower temperature for more consistent output

  # Create chat instance
  chat_args <- list(
    model = model_name,
    system_prompt = "You are an expert at analyzing scholarly writing and creating concise instructions to replicate a scholar's style.",
    api_args = api_args
  )

  # Add API key if provided
  if (!is.null(api_key)) {
    chat_args$api_key <- api_key
  }

  # Create chat instance and send prompt
  tryCatch(
    {
      chat <- do.call(ellmer::chat_openrouter, chat_args)
      response <- chat$chat(prompt)
      return(response)
    },
    error = function(e) {
      cli::cli_abort(c(
        "x" = "Error calling AI model.",
        "i" = paste0("Error message: ", e$message),
        "i" = "Check your API key and model name."
      ))
    }
  )
}

#' Extract markdown content from AI response
#' @keywords internal
extract_markdown_content <- function(response) {
  # Simple extraction - in a real implementation, this could be more sophisticated
  # to handle various response formats
  return(response)
}
