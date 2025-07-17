#' Save scholarAI configuration to a YAML file
#'
#' Saves key configuration settings like output directory, authors, and database path
#' to a YAML file in the project root directory. This allows other functions to use
#' these values as defaults.
#'
#' @param output_dir Path to the directory where corpus files will be stored
#' @param authors Character vector of author names
#' @param db_path Path to the DuckDB database (if already created)

#' @param progress Whether to display progress information
#'
#' @return Invisibly returns the path to the created configuration file
#' @export
save_scholar_config <- function(
  output_dir,
  authors = NULL,
  db_path = NULL,
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
  
  # Always use default config file path
  config_path <- "./scholarai_config.yml"
  
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
  progress = TRUE
) {
  # Check for required packages
  if (!requireNamespace("yaml", quietly = TRUE)) {
    stop("Package 'yaml' is required but not installed.")
  }
  
    config_path <- "./scholarai_config.yml"
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
  progress = TRUE
) {
    config_path <- "./scholarai_config.yml"
  # Load existing config
  config <- load_scholar_config(progress = FALSE)
  
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
