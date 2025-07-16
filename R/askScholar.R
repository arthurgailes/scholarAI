#' Generic workhorse function for scholarAI query
#'
#' This function provides the core functionality for querying a scholarAI model.
#' It retrieves relevant documents from the corpus based on the query,
#' and uses them to generate a response that emulates the scholar's style and expertise.
#'
#' @param query Character string containing the user's query
#' @param db_path Path to the DuckDB database containing the corpus and embeddings
#' @param prompt_path Path to the scholar instructions file
#' @param model Character string specifying the LLM model to use for generating responses
#' @param limit Integer specifying the maximum number of documents to retrieve
#' @param temperature Numeric value controlling randomness in response generation
#' @param chat_function Character string specifying which ellmer chat function to use (e.g., "openrouter", "anthropic", "openai", "gemini")
#' @param progress Whether to display progress information
#'
#' @return Character string containing the AI scholar's response to the query
#' @export
ask_scholar <- function(
  query,
  db_path,
  prompt_path = NULL,
  model = "anthropic/claude-sonnet-4",
  limit = 5,
  temperature = 0.7,
  chat_function = "openrouter",
  progress = TRUE
) {
  # Input validation
  if (missing(query) || !is.character(query) || length(query) != 1) {
    stop("Query must be a single character string")
  }
  if (missing(db_path) || !file.exists(db_path)) {
    stop("Database path must be provided and exist")
  }
  
  # If prompt_path is NULL, try to infer it from the db_path
  if (is.null(prompt_path)) {
    corpus_dir <- dirname(db_path)
    default_prompt_path <- file.path(corpus_dir, "scholar_instructions.md")
    if (file.exists(default_prompt_path)) {
      prompt_path <- default_prompt_path
      if (progress) {
        cli::cli_alert_info("Using inferred prompt path: {.path {prompt_path}}")
      }
    } else {
      stop("Could not infer prompt path. Please provide it explicitly.")
    }
  } else if (!file.exists(prompt_path)) {
    stop("Provided prompt path does not exist")
  }
  
  # Connect to the database
  if (progress) cli::cli_alert_info("Connecting to database")
  con <- DBI::dbConnect(duckdb::duckdb(), db_path, array = "matrix")
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE), add = TRUE)
  
  # Find similar documents
  if (progress) cli::cli_alert_info("Finding relevant documents for query")
  similar_docs <- find_similar_documents(con, query, limit = limit)
  
  if (nrow(similar_docs) == 0) {
    stop("No relevant documents found for the query")
  }
  
  # Read the scholar instructions
  if (progress) cli::cli_alert_info("Reading scholar instructions")
  instructions <- readLines(prompt_path, warn = FALSE)
  instructions <- paste(instructions, collapse = "\n")
  
  # Prepare context from similar documents
  if (progress) cli::cli_alert_info("Preparing context from {nrow(similar_docs)} documents")
  context <- paste(
    sapply(seq_len(nrow(similar_docs)), function(i) {
      doc <- similar_docs[i, ]
      paste0(
        "Document ", i, ":\n",
        "Title: ", doc$title, "\n",
        "URL: ", doc$url, "\n",
        "Content:\n", doc$content, "\n\n"
      )
    }),
    collapse = "\n"
  )
  
  # Construct the prompt
  if (progress) cli::cli_alert_info("Constructing prompt")
  prompt <- paste0(
    instructions, "\n\n",
    "CONTEXT:\n", context, "\n\n",
    "USER QUERY: ", query, "\n\n",
    "Please respond to this query as the scholar, based on the provided context."
  )
  
  # Generate the response using ellmer package
  if (progress) cli::cli_alert_info("Generating response using {model}")
  
  # Set up chat arguments
  chat_args <- list(
    model = model,
    system_prompt = "You are an AI scholar responding to queries based on provided context.",
    api_args = list(temperature = temperature)
  )
  
  # Create chat instance and generate response
  tryCatch({
    # Use the specified chat function
    chat_fn_name <- paste0("ellmer::chat_", chat_function)
    
    # Create chat instance
    chat <- do.call(chat_fn_name, chat_args)
    
    # Generate response
    response <- chat$chat(prompt)
    
    if (progress) cli::cli_alert_success("Response generated successfully")
    return(response)
  }, error = function(e) {
    cli::cli_abort(c(
      "x" = "Error generating response.",
      "i" = paste0("Error message: ", e$message),
      "i" = "Check your API key and model name."
    ))
  })
}
