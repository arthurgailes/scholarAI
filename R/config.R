#' Save scholarAI configuration to a YAML file
#'
#' Saves key configuration settings like output directory, authors, and database path
#' to a YAML file in the project root directory. This allows other functions to use
#' these values as defaults.
#'
#' @param output_dir Path to the directory where corpus files will be stored
#' @param authors Character vector of author names
#' @param db_path Path to the DuckDB database (if already created)
#' @param config_path Path where the configuration file will be saved
#' @param progress Whether to display progress information
#'
#' @return Invisibly returns the path to the created configuration file
#' @export
save_scholar_config <- function(
  output_dir,
  authors = NULL,
  db_path = NULL,
  config_path = "./scholarai_config.yml",
  progress = TRUE
) {
  # Check for required packages
  if (!requireNamespace("yaml", quietly = TRUE)) {
    stop("Package 'yaml' is required but not installed.")
  }
  
  # Ensure output_dir is absolute path
  output_dir <- normalizePath(output_dir, mustWork = FALSE)
  
  # Create config list
  config <- list(
    output_dir = output_dir,
    authors = authors,
    db_path = db_path,
    last_updated = format(Sys.time(), "%Y-%m-%d %H:%M:%S")
  )
  
  # Ensure directory exists for config file
  config_dir <- dirname(config_path)
  if (!dir.exists(config_dir)) {
    dir.create(config_dir, recursive = TRUE, showWarnings = FALSE)
  }
  
  # Write config to YAML file
  yaml::write_yaml(config, config_path)
  
  if (progress) {
    cli::cli_alert_success("Configuration saved to {.file {config_path}}")
  }
  
  invisible(config_path)
}

#' Load scholarAI configuration from a YAML file
#'
#' Loads configuration settings from a YAML file. If the file doesn't exist,
#' returns NULL for each setting.
#'
#' @param config_path Path to the configuration file
#' @param progress Whether to display progress information
#'
#' @return A list containing configuration settings
#' @export
load_scholar_config <- function(
  config_path = "./scholarai_config.yml",
  progress = TRUE
) {
  # Check for required packages
  if (!requireNamespace("yaml", quietly = TRUE)) {
    stop("Package 'yaml' is required but not installed.")
  }
  
  # Check if config file exists
  if (!file.exists(config_path)) {
    if (progress) {
      cli::cli_alert_warning("Configuration file not found at {.file {config_path}}")
    }
    return(list(
      output_dir = NULL,
      authors = NULL,
      db_path = NULL
    ))
  }
  
  # Load config from YAML file
  config <- yaml::read_yaml(config_path)
  
  if (progress) {
    cli::cli_alert_info("Configuration loaded from {.file {config_path}}")
  }
  
  return(config)
}

#' Update scholarAI configuration
#'
#' Updates specific fields in the configuration file without changing others.
#'
#' @param ... Named parameters to update in the configuration
#' @param config_path Path to the configuration file
#' @param progress Whether to display progress information
#'
#' @return Invisibly returns the path to the updated configuration file
#' @export
update_scholar_config <- function(
  ...,
  config_path = "./scholarai_config.yml",
  progress = TRUE
) {
  # Load existing config
  config <- load_scholar_config(config_path, progress = FALSE)
  
  # Update with new values
  new_values <- list(...)
  for (name in names(new_values)) {
    config[[name]] <- new_values[[name]]
  }
  
  # Update timestamp
  config$last_updated <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")
  
  # Write updated config
  yaml::write_yaml(config, config_path)
  
  if (progress) {
    cli::cli_alert_success("Configuration updated at {.file {config_path}}")
  }
  
  invisible(config_path)
}
