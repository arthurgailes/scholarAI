#' Build a complete AI scholar model
#' 
#' This function ties together all the steps required to build an AI scholar
#' model, from scraping articles to generating the final prompt.
#'
#' @param authors Character vector of author slugs to scrape from AEI.
#'   Format should match the URL structure (e.g., "Tobias%20Peter").
#' @param output_dir Path to the directory where all outputs will be stored.
#'   Will be created if it doesn't exist.
#' @param max_pages Maximum number of pages to scrape per author. Default is 2000.
#' @param embedding_model The OpenAI embedding model to use. Default is "text-embedding-ada-002".
#' @param prompt_model The LLM model to use for generating the scholar instructions.
#'   Default is "google/gemini-2.0-flash-001".
#' @param progress Whether to display progress information. Default is TRUE.
#'
#' @return Invisibly returns a list with paths to all created artifacts.
#' @export
build_ai_scholar <- function(
  authors,
  output_dir,
  max_pages = 2000,
  embedding_model = "text-embedding-ada-002",
  prompt_model = "google/gemini-2.0-flash-001",
  progress = TRUE
) {
  # Input validation
  if (missing(authors) || length(authors) == 0) {
    stop("At least one author must be provided")
  }
  if (missing(output_dir)) {
    stop("Output directory must be provided")
  }
  
  # Ensure output directory exists
  if (!dir.exists(output_dir)) {
    dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
  }
  
  # Step 1: Scrape AEI articles
  if (progress) cli::cli_h1("Step 1: Scraping AEI articles")
  scrape_results <- scrape_aei(
    authors = authors,
    output_root = output_dir,
    max_pages = max_pages,
    progress = progress
  )
  
  # Step 2: Convert corpus to dataframe
  if (progress) cli::cli_h1("Step 2: Converting corpus to dataframe")
  corpus_df <- text_corpus_to_df(output_dir)
  
  # Step 3: Save corpus metadata
  if (progress) cli::cli_h1("Step 3: Saving corpus metadata")
  metadata_path <- save_corpus_metadata(output_dir)
  
  # Step 4: Convert corpus to DuckDB
  if (progress) cli::cli_h1("Step 4: Converting corpus to DuckDB")
  db_path <- corpus_to_duckdb(corpus_dir = output_dir)
  
  # Step 5: Generate corpus embeddings
  if (progress) cli::cli_h1("Step 5: Generating corpus embeddings")
  corpus_embeddings(
    db_path = db_path,
    model = embedding_model,
    progress = progress
  )
  
  # Step 6: Build scholar prompt
  if (progress) cli::cli_h1("Step 6: Building scholar prompt")
  prompt_path <- build_scholar_prompt(
    corpus_path = output_dir,
    model = prompt_model,
    output_path = file.path(output_dir, "scholar_instructions.md")
  )
  
  # Print summary if progress is enabled
  if (progress) {
    cli::cli_h1("AI Scholar build completed successfully")
    cli::cli_alert_success("Output directory: {.path {output_dir}}")
    cli::cli_alert_success("Database path: {.path {db_path}}")
    cli::cli_alert_success("Metadata path: {.path {metadata_path}}")
    cli::cli_alert_success("Scholar prompt: {.path {prompt_path}}")
    cli::cli_alert_success("Articles scraped: {nrow(scrape_results)}")
  }
  
  # Return paths to artifacts invisibly
  invisible(list(
    output_dir = output_dir,
    db_path = db_path,
    metadata_path = metadata_path,
    prompt_path = prompt_path,
    scrape_results = scrape_results
  ))
}
