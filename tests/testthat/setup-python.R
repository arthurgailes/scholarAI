# Check if Python is already initialized
if (!reticulate::py_available(initialize = FALSE)) {
  # Fallback to default local conda env
  conda_env_path <- normalizePath(here::here("conda_env"), mustWork = FALSE)
  message("Using default Conda environment at: ", conda_env_path)

  try(
    {
      reticulate::use_condaenv(
        conda_env_path,
        conda = "mamba",
        required = FALSE
      )
    },
    silent = TRUE
  )
} else {
  message("Python environment already initialized; skipping setup.")
}

# Optionally check for a required module
openai_available <- tryCatch(
  {
    reticulate::py_module_available("openai")
  },
  error = function(e) {
    message("Error checking for 'openai' module: ", e$message)
    FALSE
  }
)
