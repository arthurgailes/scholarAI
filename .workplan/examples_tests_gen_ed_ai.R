# Stand-alone examples and lightweight tests for the Edward Pinto AI helpers
source("W:/arthur/202404_heat_ai/code/function/gen_ed_ai.R")
library(testthat)
if (interactive()) {
  # Example 1 – generic answer -------------------------------------------------
  out_generic <- gen_ed_ai(
    "Give me a quick overview of US housing supply trends in the last decade.",
    geoid = "0",
    model = "google/gemini-2.5-flash",
    target_words = 150
  )
  print(out_generic$response)

  # Example 2 – op-ed (short; set a small target_words for demo) --------------
  out_oped <- gen_ed_oped(
    "How can cities tackle housing affordability?",
    geoid = "0",
    model = "google/gemini-2.5-flash",
    target_words = 250
  )
  cat(substr(out_oped$response, 1, 500), "…\n")
}
