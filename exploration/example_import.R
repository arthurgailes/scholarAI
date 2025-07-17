pacman::p_load(yyjsonr, collapse, fs, dplyr, glue, readr)

f <- "W:/arthur/202404_heat_ai/data/intermed/aei_search_results/op-eds/a-wasted-opportunity-to-improve-housing-outcomes-for-minorities"
text <- read_file(glue(f, "/article_text.txt"))
meta <- read_json_file(glue(f, "/output.json")) |> as.data.frame()

obj <- rename(meta, url = link, text = body) |>
  mutate(
    author = c("Edward Pinto", "Tobias Peter"),
    date = "2024-04-01",
    arbitrary = "anything i want"
  )

output <- list(text = text, metadata = obj)

write_json_file(output, glue("where_should_this_go", "/output.json"))

# usethis::use_package("reticulate")

meta <- read_json_file(
  "tests/testthat/test_data/aei_test_results/corpus_metadata.json"
)

sapply(meta, class)
