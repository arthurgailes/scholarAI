#' Scholar function generation utilities
#'
#' This file contains functions for generating scholar-specific query functions
#' and managing the custom.R file that contains these functions.

#' Generate a custom function for querying a specific scholar
#'
#' Creates a function that wraps ask_scholar with pre-configured parameters for a specific scholar.
#'
#' @param scholar_name Character string with the name of the scholar
#' @param db_path Path to the DuckDB database containing the corpus and embeddings
#' @param prompt_path Path to the scholar instructions file
#' @param model Character string specifying the LLM model to use
#'
#' @return A function that can be called to query the scholar
#' @keywords internal
generate_scholar_function <- function(
  scholar_name,
  db_path,
  prompt_path,
  model = "anthropic/claude-sonnet-4"
) {
  # Input validation
  if (!is.character(scholar_name) || length(scholar_name) != 1) {
    stop("Scholar name must be a single character string")
  }
  if (!file.exists(db_path)) {
    stop("Database path must be provided and exist")
  }
  if (!file.exists(prompt_path)) {
    stop("Prompt path must be provided and exist")
  }

  # Extract first name from scholar_name
  first_name <- strsplit(scholar_name, " ")[[1]][1]
  fn <- function(query, limit = 5, temperature = 0.7, progress = TRUE) {
    ask_scholar(
      query = query,
      db_path = db_path,
      prompt_path = prompt_path,
      model = model,
      limit = limit,
      temperature = temperature,
      progress = progress
    )
  }

  # Set function attributes for better documentation
  attr(fn, "scholar_name") <- scholar_name
  attr(fn, "first_name") <- first_name
  attr(fn, "db_path") <- db_path
  attr(fn, "prompt_path") <- prompt_path
  attr(fn, "model") <- model

  # Return the function
  return(fn)
}

#' Generate scholar-specific functions and create custom.R file
#'
#' Creates scholar-specific functions for each author, adds them to the global environment,
#' and writes them to a custom.R file in the project root directory.
#'
#' @param authors Character vector of author names. If NULL, will try to load from config.
#' @param db_path Path to the DuckDB database. If NULL, will try to load from config.
#' @param prompt_path Path to the scholar instructions file. If NULL, will try to load from config.
#' @param prompt_model Character string specifying the LLM model to use
#' @param output_dir Directory where the corpus is stored. If NULL, will try to load from config.
#' @param config_path Path to the configuration file
#' @param custom_file Path to the custom.R file to create. If NULL, will be created in the parent directory of output_dir.
#' @param progress Whether to display progress information
#'
#' @return A list containing the path to the custom.R file and the names of generated functions
#' @keywords internal
generate_scholar_functions <- function(
  authors = NULL,
  db_path = NULL,
  prompt_path = NULL,
  prompt_model = "anthropic/claude-sonnet-4",
  output_dir = NULL,
  config_path = "./scholarai_config.yml",
  custom_file = "scholar_functions.R",
  progress = TRUE
) {
  # Try to load configuration if parameters are NULL
  if (
    is.null(authors) ||
      is.null(db_path) ||
      is.null(prompt_path) ||
      is.null(output_dir)
  ) {
    if (progress)
      cli::cli_alert_info("Loading configuration from {.file {config_path}}")

    # Try to load config if file exists
    if (file.exists(config_path)) {
      # Check if yaml package is available
      if (!requireNamespace("yaml", quietly = TRUE)) {
        stop("Package 'yaml' is required but not installed.")
      }

      config <- yaml::read_yaml(config_path)

      # Use config values for NULL parameters
      if (is.null(authors) && !is.null(config$authors))
        authors <- config$authors
      if (is.null(db_path) && !is.null(config$db_path))
        db_path <- config$db_path
      if (is.null(prompt_path) && !is.null(config$prompt_path))
        prompt_path <- config$prompt_path
      if (is.null(output_dir) && !is.null(config$output_dir))
        output_dir <- config$output_dir
    }
  }

  # Validate required parameters
  if (is.null(authors)) stop("Authors must be provided or available in config")
  if (is.null(db_path))
    stop("Database path must be provided or available in config")
  if (is.null(output_dir))
    stop("Output directory must be provided or available in config")

  # If prompt_path is still NULL, try to infer it from output_dir
  if (is.null(prompt_path)) {
    default_prompt_path <- file.path(output_dir, "scholar_instructions.md")
    if (file.exists(default_prompt_path)) {
      prompt_path <- default_prompt_path
      if (progress)
        cli::cli_alert_info("Using inferred prompt path: {.path {prompt_path}}")
    } else {
      stop("Could not infer prompt path. Please provide it explicitly.")
    }
  }

  if (progress)
    cli::cli_alert_info("Creating custom.R file at {.path {custom_file}}")

  # Ensure directory exists
  dir.create(dirname(custom_file), showWarnings = FALSE, recursive = TRUE)

  # Start with header content
  custom_content <- c(
    "# This file contains custom functions for interacting with the scholarAI package",
    "# Generated by the scholarAI package on ",
    format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
    "",
    "# Load required packages",
    "library(scholarAI)",
    ""
  )

  # For each author, create a custom function
  scholar_functions <- list()
  for (author in authors) {
    # Extract the author's name from the URL-encoded slug
    author_name <- gsub("%20", " ", author)

    # Get first name for the function
    first_name <- strsplit(author_name, " ")[[1]][1]
    function_name <- paste0("ask", first_name)

    if (progress) {
      cli::cli_alert_info(
        "Creating function {.fn {function_name}} for {author_name}"
      )
    }

    # Generate the function
    scholar_fn <- generate_scholar_function(
      scholar_name = author_name,
      db_path = db_path,
      prompt_path = prompt_path,
      model = prompt_model
    )

    # Add function to global environment
    assign(function_name, scholar_fn, envir = .GlobalEnv)

    # Add function to the list for return value
    scholar_functions[[function_name]] <- scholar_fn

    # Add function definition to custom.R content
    custom_content <- c(
      custom_content,
      paste0(
        function_name,
        " <- function(query, limit = 5, temperature = 0.7, progress = TRUE) {"
      ),
      "  ask_scholar(",
      paste0("    query = query,"),
      paste0("    db_path = \"", db_path, "\","),
      paste0("    prompt_path = \"", prompt_path, "\","),
      paste0("    model = \"", prompt_model, "\","),
      "    limit = limit,",
      "    temperature = temperature,",
      "    progress = progress",
      "  )",
      "}",
      "",
      paste0(
        "# Example usage: ",
        function_name,
        "(\"What are your thoughts on housing policy?\")"
      )
    )

    if (progress) {
      cli::cli_alert_success(
        "Created function {.fn {function_name}} in global environment"
      )
    }
  }

  # Write the custom.R file
  writeLines(custom_content, custom_file)
  if (progress)
    cli::cli_alert_success("Created custom.R file with scholar functions")

  # Return the path to the custom.R file and the names of the generated functions
  result <- list(
    custom_file = custom_file,
    scholar_functions = names(scholar_functions)
  )

  # Update config with the latest values if they've changed
  if (file.exists(config_path)) {
    # Check if yaml package is available
    if (requireNamespace("yaml", quietly = TRUE)) {
      # Create updated config
      config <- list(
        output_dir = output_dir,
        authors = authors,
        db_path = db_path,
        prompt_path = prompt_path,
        last_updated = format(Sys.time(), "%Y-%m-%d %H:%M:%S")
      )

      # Write updated config
      yaml::write_yaml(config, config_path)

      if (progress) {
        cli::cli_alert_success("Configuration updated at {.file {config_path}}")
      }
    }
  }

  return(result)
}
