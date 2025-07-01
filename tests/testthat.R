#' Run testthat tests for scholarAI package
#' This automatically discovers tests in tests/testthat/
#' @keywords internal
#' @noRd

if (requireNamespace("testthat", quietly = TRUE)) {
  testthat::test_check("scholarAI")
}
