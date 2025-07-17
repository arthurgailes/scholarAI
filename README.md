# scholarAI

The use of this package is straightforward. Simply install and run in a local directory to build an A(E)I scholar.

```{r}
devtools::install_github("arthurgailes/scholarAI")

scholarAI::build_ai_scholar("NAME")
```

And you're done!This will download a scholar's writing from aei.org and build:
1. A duckdb corpus, which can be queried
2. An instruction template for reproducing the author's writing style

## Usage

The primary function of this is to chat with an AI scholar. `build_ai_scholar` will create a config files and a `scholar_functions.R` file, which contains a starter function for interacting with the scholar. For example, if the scholar is "Arthur Gailes", the function will be named `askArthur`.

```{r}
library(scholarAI)

source("scholar_functions.R")

askArthur("What is light-touch density?")
```


## Manual setup

`build_ai_scholar` is a convenience wrapper for the multiple peices of setup. If you want more control over the build process, you can run each step manually:

```{r}
config_path <- scholarAI::save_scholar_config(
    output_dir = output_dir,
    authors = authors
  )
scrape_results <- scholarAI::scrape_aei(authors = authors)
corpus_df <- scholarAI::text_corpus_to_df(output_dir)
metadata_path <- scholarAI::save_corpus_metadata(output_dir)
db_path <- scholarAI::corpus_to_duckdb(corpus_dir = output_dir)
scholarAI::corpus_embeddings(db_path = db_path)
prompt_path <- scholarAI::build_scholar_prompt(output_dir)
scholarAI::generate_scholar_functions(output_dir)
```