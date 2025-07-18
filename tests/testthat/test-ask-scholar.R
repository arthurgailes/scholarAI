test_that("ask_scholar validates inputs correctly", {
  # Test invalid query
  expect_error(
    ask_scholar(query = 1, db_path = "dummy.db"),
    "Query must be a single character string"
  )
  
  # Test non-existent database
  expect_error(
    ask_scholar(query = "test query", db_path = "nonexistent.db"),
    "Database path must be provided and exist"
  )
  
  # Create a temporary database file to test prompt path validation
  temp_db <- tempfile(fileext = ".db")
  file.create(temp_db)
  on.exit(unlink(temp_db))
  
  # Test non-existent prompt path
  expect_error(
    ask_scholar(
      query = "test query", 
      db_path = temp_db, 
      prompt_path = "nonexistent.md"
    ),
    "Provided prompt path does not exist"
  )
})

test_that("ask_scholar infers prompt path correctly", {
  # Create temporary database and prompt file
  db_dir <- tempdir()
  db_path <- file.path(db_dir, "test.db")
  prompt_path <- file.path(db_dir, "scholar_instructions.md")
  
  # Create the files
  file.create(db_path)
  writeLines("Test instructions", prompt_path)
  
  # Mock DBI::dbConnect to avoid actual connection
  mockery::stub(ask_scholar, "DBI::dbConnect", function(...) "mock_connection")
  
  # Mock find_similar_documents to return dummy data
  mock_docs <- data.frame(
    id = 1,
    title = "Test Document",
    url = "http://example.com",
    content = "Test content",
    similarity = 0.9
  )
  mockery::stub(ask_scholar, "find_similar_documents", mock_docs)
  
  # Mock chat function
  mock_chat <- list(chat = function(...) "Mock response")
  mockery::stub(ask_scholar, "do.call", mock_chat)
  
  # Mock dbDisconnect to avoid errors on exit
  mockery::stub(ask_scholar, "DBI::dbDisconnect", function(...) NULL)
  
  # Test with NULL prompt_path (should infer from db_path)
  expect_message(
    result <- ask_scholar(
      query = "test query",
      db_path = db_path,
      prompt_path = NULL,
      progress = TRUE
    ),
    "Using inferred prompt path"
  )
  
  expect_equal(result, "Mock response")
  
  # Clean up
  unlink(c(db_path, prompt_path))
})

test_that("ask_scholar handles document retrieval and response generation", {
  # Create temporary files
  db_path <- tempfile(fileext = ".db")
  prompt_path <- tempfile(fileext = ".md")
  
  # Create the files
  file.create(db_path)
  writeLines("Test scholar instructions", prompt_path)
  
  # Mock database connection
  mockery::stub(ask_scholar, "DBI::dbConnect", function(...) "mock_connection")
  
  # Mock document retrieval with multiple documents
  mock_docs <- data.frame(
    id = 1:3,
    title = paste("Document", 1:3),
    url = paste0("http://example.com/", 1:3),
    content = paste("Content for document", 1:3),
    similarity = c(0.9, 0.8, 0.7)
  )
  mockery::stub(ask_scholar, "find_similar_documents", mock_docs)
  
  # Mock chat function to capture the prompt
  prompt_capture <- NULL
  mock_chat <- list(
    chat = function(prompt) {
      prompt_capture <<- prompt
      return("Scholar response")
    }
  )
  mockery::stub(ask_scholar, "do.call", mock_chat)
  
  # Mock dbDisconnect
  mockery::stub(ask_scholar, "DBI::dbDisconnect", function(...) NULL)
  
  # Test with specific parameters
  result <- ask_scholar(
    query = "What about policy?",
    db_path = db_path,
    prompt_path = prompt_path,
    model = "test-model",
    limit = 3,
    temperature = 0.5,
    chat_function = "openai",
    progress = FALSE
  )
  
  # Verify result
  expect_equal(result, "Scholar response")
  
  # Verify prompt structure
  expect_true(grepl("Test scholar instructions", prompt_capture))
  expect_true(grepl("Document 1", prompt_capture))
  expect_true(grepl("Document 2", prompt_capture))
  expect_true(grepl("Document 3", prompt_capture))
  expect_true(grepl("USER QUERY: What about policy?", prompt_capture))
  
  # Clean up
  unlink(c(db_path, prompt_path))
})

test_that("ask_scholar uses the specified chat_function", {
  # Create temporary files
  db_path <- tempfile(fileext = ".db")
  prompt_path <- tempfile(fileext = ".md")
  file.create(db_path)
  writeLines("Test instructions", prompt_path)
  
  # Mock database connection
  mockery::stub(ask_scholar, "DBI::dbConnect", function(...) "mock_connection")
  mockery::stub(ask_scholar, "DBI::dbDisconnect", function(...) NULL)
  
  # Mock document retrieval
  mock_docs <- data.frame(
    id = 1,
    title = "Test Document",
    url = "http://example.com",
    content = "Test content",
    similarity = 0.9
  )
  mockery::stub(ask_scholar, "find_similar_documents", mock_docs)
  
  # Test different chat functions
  test_chat_function <- function(chat_function_name) {
    # Mock the get function to track which function is requested
    chat_fn_requested <- NULL
    mock_chat_fn <- function(...) {
      list(chat = function(...) paste("Response from", chat_function_name))
    }
    
    mockery::stub(
      ask_scholar, 
      "get", 
      function(x, envir, ...) {
        if (grepl("^chat_", x)) {
          chat_fn_requested <<- x
          return(mock_chat_fn)
        }
        # For any other get calls, use the real get function
        base::get(x, envir, ...)
      }
    )
    
    # Call ask_scholar with the specified chat function
    result <- ask_scholar(
      query = "test query",
      db_path = db_path,
      prompt_path = prompt_path,
      chat_function = chat_function_name,
      progress = FALSE
    )
    
    # Verify the correct chat function was requested
    expect_equal(chat_fn_requested, paste0("chat_", chat_function_name))
    
    # Verify the response
    expect_equal(result, paste("Response from", chat_function_name))
  }
  
  # Test each chat function
  chat_functions <- c("openai", "anthropic", "openrouter", "gemini")
  for (cf in chat_functions) {
    test_chat_function(cf)
  }
  
  # Clean up
  unlink(c(db_path, prompt_path))
})

test_that("ask_scholar handles API errors gracefully", {
  # Create temporary files
  db_path <- tempfile(fileext = ".db")
  prompt_path <- tempfile(fileext = ".md")
  
  # Create the files
  file.create(db_path)
  writeLines("Test instructions", prompt_path)
  
  # Mock database connection
  mockery::stub(ask_scholar, "DBI::dbConnect", function(...) "mock_connection")
  
  # Mock document retrieval
  mock_docs <- data.frame(
    id = 1,
    title = "Test Document",
    url = "http://example.com",
    content = "Test content",
    similarity = 0.9
  )
  mockery::stub(ask_scholar, "find_similar_documents", mock_docs)
  
  # Mock chat function to throw an error
  mock_chat <- list(
    chat = function(...) {
      stop("API error: Invalid API key")
    }
  )
  mockery::stub(ask_scholar, "do.call", mock_chat)
  
  # Mock dbDisconnect
  mockery::stub(ask_scholar, "DBI::dbDisconnect", function(...) NULL)
  
  # Test error handling
  expect_error(
    ask_scholar(
      query = "test query",
      db_path = db_path,
      prompt_path = prompt_path,
      progress = FALSE
    ),
    "Error generating response"
  )
  
  # Clean up
  unlink(c(db_path, prompt_path))
})

# Test integration with generate_scholar_functions
test_that("generate_scholar_function creates a working function", {
  # Create temporary files
  db_path <- tempfile(fileext = ".db")
  prompt_path <- tempfile(fileext = ".md")
  
  # Create the files
  file.create(db_path)
  writeLines("Test instructions", prompt_path)
  
  # Generate scholar function
  scholar_fn <- generate_scholar_function(
    scholar_name = "Jane Doe",
    db_path = db_path,
    prompt_path = prompt_path,
    model = "test-model"
  )
  
  # Verify function attributes
  expect_equal(attr(scholar_fn, "scholar_name"), "Jane Doe")
  expect_equal(attr(scholar_fn, "first_name"), "Jane")
  expect_equal(attr(scholar_fn, "db_path"), db_path)
  expect_equal(attr(scholar_fn, "prompt_path"), prompt_path)
  expect_equal(attr(scholar_fn, "model"), "test-model")
  
  # Mock ask_scholar function to verify it's called with correct parameters
  mockery::stub(scholar_fn, "ask_scholar", function(query, db_path, prompt_path, 
                                                  model, limit, temperature, progress) {
    list(
      query = query,
      db_path = db_path,
      prompt_path = prompt_path,
      model = model,
      limit = limit,
      temperature = temperature,
      progress = progress
    )
  })
  
  # Call the generated function
  result <- scholar_fn(
    query = "test query",
    limit = 10,
    temperature = 0.3,
    progress = FALSE
  )
  
  # Verify parameters passed to ask_scholar
  expect_equal(result$query, "test query")
  expect_equal(result$db_path, db_path)
  expect_equal(result$prompt_path, prompt_path)
  expect_equal(result$model, "test-model")
  expect_equal(result$limit, 10)
  expect_equal(result$temperature, 0.3)
  expect_equal(result$progress, FALSE)
  
  # Clean up
  unlink(c(db_path, prompt_path))
})

test_that("generate_scholar_functions creates multiple functions", {
  # Create temporary directory and files
  temp_dir <- tempdir()
  db_path <- file.path(temp_dir, "test.db")
  prompt_path <- file.path(temp_dir, "scholar_instructions.md")
  custom_file <- file.path(temp_dir, "scholar_functions.R")
  
  # Create the files
  file.create(db_path)
  writeLines("Test instructions", prompt_path)
  
  # Mock assign function to avoid actually creating functions in global env
  mockery::stub(generate_scholar_functions, "assign", function(...) NULL)
  
  # Mock generate_scholar_function to return a dummy function
  mockery::stub(generate_scholar_functions, "generate_scholar_function", function(...) {
    function(...) "mock response"
  })
  
  # Test with multiple authors
  result <- generate_scholar_functions(
    authors = c("John Smith", "Jane Doe"),
    db_path = db_path,
    prompt_path = prompt_path,
    prompt_model = "test-model",
    output_dir = temp_dir,
    custom_file = custom_file,
    progress = FALSE
  )
  
  # Verify result
  expect_equal(result$custom_file, custom_file)
  expect_equal(result$scholar_functions, c("askJohn", "askJane"))
  
  # Verify custom file was created
  expect_true(file.exists(custom_file))
  
  # Read the content of the custom file
  content <- readLines(custom_file)
  
  # Verify it contains the expected function definitions
  expect_true(any(grepl("askJohn <- function", content)))
  expect_true(any(grepl("askJane <- function", content)))
  
  # Clean up
  unlink(c(db_path, prompt_path, custom_file))
})
