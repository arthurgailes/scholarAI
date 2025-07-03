# scholarAI Package Workplan

**Project:** Build an R-first scaffold—`aeiScholarAI`—that lets any intern generate a retrieval-augmented AI for a specific AEI scholar in under an hour.

**Outcome:**  
1. A core R package on GitHub (maintained by you) containing generic scraping, embedding, and chat functions that write to a portable DuckDB store.  
2. A single command (`create_scholar("<slug>")`) that generates a self-contained workspace (`~/aei_scholars/<slug>/`) with:  
   * `build_ai.R` – sequential script that scrapes AEI.org, embeds text, and produces a ready-to-query vector store.  
   * `custom.R` – pre-populated with helper stubs (`ask<Scholar>()`, `write<Scholar>Oped()`, etc.) that analysts can edit freely.  
   * `config.yml` – minimal scholar metadata (slug, name, AEI profile URL).  
3. Daily use requires only `source("custom.R")`; helpers are available in any R script, notebook, or Shiny app without additional setup.  
4. Sharing a built AI is as simple as zipping the scholar folder (data/, store.duckdb, custom.R) and unzipping on another machine, preserving full functionality.  
5. No **renv**, Docker, or environment variables—installing `duckdb` and the GitHub package is sufficient for all users.


## AEI Scholar AI – Simple Scaffold  

> **Goal:** Let any intern spin up a scholar-specific RAG in \< 1 hr and give analysts easy helper
> functions (`askEd()`, `writeEdOped()`, …) with minimal tooling.

---

## 1 Core package (`aeiScholarAI`)

```
aeiScholarAI/
  R/
    scrape_aei.R    # rvest scraper for AEI.org
    embed.R         # text → DuckDB embeddings (python?S)
    chat.R          # retrieval-augmented chat
    utils.R         # helper: create_scholar()
  inst/
    skeleton/       # template copied into every scholar workspace
      data/
        instructions/
        corpus/
        projects/
        store.duckdb
      custom.R      # user-defined helpers live here
      config.yml
DESCRIPTION
README.md
```

**Install once (per machine)**

```r
remotes::install_github("agailes1/aeiScholarAI")
```

---

## 2 Create a scholar workspace

```r
aeiScholarAI::create_scholar("Edward Pinto")
# → makes: ~/aei_scholars/edward_pinto/
```

**Files created**

```
~/aei_scholars/ed/
  build_ai.R
  custom.R          # contains askEd(), writeEdOped() stubs
  config.yml        # slug, name, AEI URL, etc.
```

---

## 3 Build the AI (≈ 30 min on first run)

```r
setwd("~/aei_scholars/edward_pinto")
source("build_ai.R")       # scrape → embed → ready to chat
```

`build_ai.R` is < 40 lines of plain sequential R—no *targets*, makefiles, or pipelines.

---

## 4 Daily use inside any project

```r
# load helper functions once per session
source("~/aei_scholars/edward_pinto/custom.R")   # defines askEd(), writeEdOped(), …

# now just call the helpers anywhere
askEd("What did you say about zoning in 2020?")
writeEdOped("How to fix housing supply")    # returns a draft op-ed
```

Because helpers live in `custom.R`, *any* script, notebook, or Shiny app can `source()` them—even if the app sits in a different folder.

---

## 5 How helpers are auto-generated (`custom.R` snippet)

```r
make_scholar_helpers <- function() {
  store <- duckdb::dbConnect(duckdb::duckdb(), "store.duckdb")

  ask <- function(prompt, ...) {
    source_docs <- scholarAI::query_duckdb_store(prompt)
    ellmer::chat(prompt, store = store, ...)
  }
  assign("askEd", ask, envir = .GlobalEnv)

  # example specialised helper
  writeOpEd <- function(topic) {
    prompt <- glue::glue(
      "Write a sharp, 800-word op-ed on '{topic}' in Ed's voice."
    )
    ask(prompt)
  }
  assign("writeEdOped", writeOpEd, envir = .GlobalEnv)
}

make_scholar_helpers()
```

Users edit or add functions in this file; no packages to build, nothing to install.

---

## 6 Optional Python (only if needed)
For embeddings, TBD.  
Add an **environment.yml** next to `build_ai.R`:

```yaml
name: aei-scholars
channels: conda-forge
dependencies:
  - python=3.12
  - chromadb
  - openai
```

If someone needs Python via **reticulate**, they run once:

```bash
mamba env create -f environment.yml
```

---

## 7 Sharing a finished AI

A single folder (`~/aei_scholars/ed`) contains:

```
data/instructions/          # system prompts, instructions, etc for context
data/corpus/                # raw HTML, cleaned text
projects/**/*               # Individual projects/chats undertaken by the AI.
store.duckdb                # 30–200 MB, portable
custom.R                    # user helpers
```

Zip it, pass it to a colleague, they unzip and `source("custom.R")`—done.

---


---

### Quick setup checklist

1. `remotes::install_github("agailes1/aeiScholarAI")`  
2. `aeiScholarAI::create_scholar("Edward J. Pinto")`  
3. `setwd("~/aei_scholars/edward_j_pinto"); source("build_ai.R")`  
4. `source("custom.R")` in any R session  
5. Use `askEd()`, `writeEdOped()`, etc.

> **Result:** Each intern’s workload reduces to a 30-minute “fill in YAML → run script” ritual.  
> Analysts get lightweight, portable helpers—and you keep ownership of the core code.
