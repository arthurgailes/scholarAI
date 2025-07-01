#' AEI Article & PDF Scraper
#'
#' This script mirrors the Python scraping pipeline located in
#' `.workplan/python_scrape` using tidyverse-friendly R code.  It can:
#'   1. Discover all article URLs for one or more AEI authors.
#'   2. Persist those links to `data/intermed/aei_search_results/scraped_links.csv`.
#'   3. For every discovered URL, download the article text, metadata (title, date, author),
#'      embedded/linked PDFs, and store everything in a structured folder tree under
#'      `data/intermed/aei_search_results/`.
#'   4. Duplicate all downloaded PDFs to a convenient backup location.
#'
#' All functions are exported so they can be reused from other package code or
#' interactive sessions.  The main entry point is [scrape_aei()], which ties the
#' pipeline together.
#'
#' @section Dependencies:
#' * rvest
#' * httr
#' * xml2
#' * stringr
#' * dplyr
#' * purrr
#'
#' Ensure these packages are installed before running.
#'
#' @name scrape_aei
NULL

# ---------------------------------------------------------------------------
# Helpers -------------------------------------------------------------------

utils::globalVariables(c(".", "links", "%>%"))


#' Retrieve all post links for a single AEI author
#'
#' @param author_slug Character. URL-encoded author string, e.g. "Edward%20J.%20Pinto".
#' @param max_pages Integer. Safety stop to avoid infinite loops (default 2000).
#' @param progress Logical. If TRUE, prints page numbers while scraping.
#'
#' @return Character vector of article URLs.
#' @export
get_author_links <- function(author_slug, max_pages = 2000, progress = TRUE) {
  base_url <- "https://www.aei.org/search-results/?wpsolr_fq%5B0%5D=author_str:"
  url_prefix <- paste0(base_url, author_slug, "&wpsolr_page=")

  page_num <- 1L
  out      <- character()

  repeat {
    if (page_num > max_pages) break
    if (progress) message("Scraping page ", page_num)

    page_url <- paste0(url_prefix, page_num)
    res      <- tryCatch(
      httr::GET(page_url, httr::config(ssl_verifypeer = FALSE), httr::timeout(20)),
      error = function(e) NULL
    )
    if (is.null(res) || httr::http_error(res)) break

    pg       <- xml2::read_html(res)
    links    <- pg %>%
      rvest::html_elements("div.post.post-search a[href]") %>%
      rvest::html_attr("href") %>%
      purrr::discard(is.na)

    if (length(links) == 0) break
    out <- c(out, links)
    page_num <- page_num + 1L
  }
  unique(out)
}

#' Convert a URL path into a safe folder name (one nested level allowed)
#'
#' @param url Character URL.
#' @return Character folder path relative to output directory.
create_folder_name <- function(url) {
  path <- url %>% urltools::url_parse() %>% purrr::pluck("path") %>% stringr::str_remove_all("^/|/$")
  parts <- stringr::str_split(path, "/", n = 2, simplify = TRUE)
  if (ncol(parts) == 0) return(path)
  if (ncol(parts) == 2 && nzchar(parts[2])) parts[2] <- stringr::str_replace_all(parts[2], "/", "-")
  paste(parts, collapse = "/")
}

# regex helpers --------------------------------------------------------------

.date_patterns <- paste0(
  "(" ,
  paste(c(
    "(?:Jan(?:uary)?|Feb(?:ruary)?|Mar(?:ch)?|Apr(?:il)?|May|Jun(?:e)?|Jul(?:y)?|Aug(?:ust)?|",
    "Sep(?:tember)?|Oct(?:ober)?|Nov(?:ember)?|Dec(?:ember)?)[[:space:]]+\\d{1,2},[[:space:]]+\\d{4}",
    "(?:Jan(?:uary)?|Feb(?:ruary)?|Mar(?:ch)?|Apr(?:il)?|May|Jun(?:e)?|Jul(?:y)?|Aug(?:ust)?|",
    "Sep(?:tember)?|Oct(?:ober)?|Nov(?:ember)?|Dec(?:ember)?)[[:space:]]+\\d{4}",
    "Week[[:space:]]+\\d{1,2}(?:[[:space:]]+&[[:space:]]+\\d{1,2})?,[[:space:]]+\\d{4}",
    "Q[1-4][[:space:]]+\\d{4}",
    "\\d{4}",
    "\\d{8}"
  ), collapse = "|"), ")")

#' Extract title, date & author from article html
extract_metadata <- function(article_html) {
  title <- article_html %>%
    rvest::html_elements(xpath = "//*[self::h1 or self::h2 or self::h3]") %>%
    rvest::html_text(trim = TRUE) %>%
    first_or("No Title Found")

  # try to find date
  possible_tags <- article_html %>% rvest::html_elements(xpath = "//time|//p|//span|//div")
  date <- possible_tags %>%
    rvest::html_text(trim = TRUE) %>%
    purrr::keep(~ stringr::str_detect(., .date_patterns)) %>%
    first_or("No Date Found")

  # search inside title as fallback
  if (identical(date, "No Date Found") && stringr::str_detect(title, .date_patterns))
    date <- stringr::str_extract(title, .date_patterns)

  # author
  author <- article_html %>%
    rvest::html_elements(xpath = "//*[contains(@class,'author')]") %>%
    rvest::html_text(trim = TRUE) %>%
    first_or("No Author Found") %>%
    stringr::str_remove("^(By|With) ") %>%
    stringr::str_replace_all("\\|", ", ")

  tibble::tibble(title, date, author)
}

first_or <- function(x, default) if (length(x) == 0) default else x[[1]]

#' Extract plain text from article html (<p>, <li>, headings)
extract_text <- function(article_html) {
  article_html %>%
    rvest::html_elements(xpath = "//p|//li|//h1|//h2|//h3|//h4|//h5|//h6") %>%
    rvest::html_text() %>%
    paste(collapse = "\n")
}

#' Download a single PDF given a URL
save_pdf <- function(pdf_url, dest_path) {
  res <- tryCatch(
    httr::GET(pdf_url, httr::config(ssl_verifypeer = FALSE), httr::timeout(60)),
    error = function(e) NULL
  )
  if (!is.null(res) && !httr::http_error(res)) writeBin(httr::content(res, "raw"), dest_path)
}

#' Discover & download PDFs on the page
handle_pdfs <- function(article_html, base_url, folder_path, greenlist) {
  n <- 1L
  # iframe-embedded
  article_html %>% rvest::html_elements("iframe") %>% rvest::html_attr("src") %>%
    purrr::keep(~ stringr::str_detect(., "\\.pdf$")) %>%
    purrr::walk(function(src) {
      pdf_url <- xml2::url_absolute(src, base_url)
      save_pdf(pdf_url, file.path(folder_path, paste0(n, ".pdf")))
      n <<- n + 1L
    })
  # extra anchors for greenlist urls
  if (base_url %in% greenlist) {
    article_html %>% rvest::html_elements("a") %>% rvest::html_attr("href") %>%
      purrr::keep(~ stringr::str_detect(., "\\.pdf$")) %>%
      purrr::walk(function(src) {
        pdf_url <- xml2::url_absolute(src, base_url)
        save_pdf(pdf_url, file.path(folder_path, paste0(n, ".pdf")))
        n <<- n + 1L
      })
  }
}

# core ----------------------------------------------------------------------

#' Extract article, metadata & PDFs for a given URL
#'
#' @param url Character.
#' @param output_root Root directory for `aei_search_results` (default inside project).
#' @param greenlist Character vector of URL prefixes that require extra PDF search.
#'
#' @return Named list with title, date, author.
extract_and_save <- function(url,
                             output_root = "data/intermed/aei_search_results",
                             greenlist = "https://www.aei.org/housing-supply-case-studies/") {
  page_name   <- create_folder_name(url)
  folder_path <- file.path(output_root, page_name)
  dir.create(folder_path, recursive = TRUE, showWarnings = FALSE)

  res <- tryCatch(
    httr::GET(url, httr::config(ssl_verifypeer = FALSE), httr::timeout(20)),
    error = function(e) NULL
  )
  if (is.null(res) || httr::http_error(res)) {
    warning("Failed to download ", url)
    return(list(title = NA_character_, date = NA_character_, author = NA_character_))
  }
  html <- xml2::read_html(res)

  main_article <- html %>% rvest::html_element("main") %>% rvest::html_element("article")
  if (is.na(main_article)) main_article <- html

  text  <- extract_text(main_article)
  writeLines(text, file.path(folder_path, "article_text.txt"), useBytes = TRUE)

  handle_pdfs(main_article, url, folder_path, greenlist)
  meta <- extract_metadata(main_article)
  as.list(meta)
}

#' Copy all PDFs in search_results tree to backup directory
#'
#' @param search_root Directory to search for PDFs.
#' @param backup_dir Destination directory.
copy_pdfs <- function(search_root = "data/intermed/aei_search_results",
                      backup_dir = "R:/archive/Published PDFs") {
  pdfs <- list.files(search_root, "[.]pdf$", recursive = TRUE, full.names = TRUE)
  dir.create(backup_dir, showWarnings = FALSE, recursive = TRUE)
  file.copy(pdfs, file.path(backup_dir, basename(pdfs)), overwrite = TRUE)
}

# ---------------------------------------------------------------------------
# Pipeline driver -----------------------------------------------------------

#' Run the full AEI scraping pipeline
#'
#' @param authors Character vector of URL-encoded author names.
#' @param output_root Directory where intermediate results live.
#' @export
scrape_aei <- function(authors = c("Edward%20J.%20Pinto", "Tobias%20Peter"),
                       output_root = "data/intermed/aei_search_results") {
  dir.create(output_root, recursive = TRUE, showWarnings = FALSE)

  greenlist <- "https://www.aei.org/housing-supply-case-studies/"

  # 1. discover links --------------------------------------------------------
  message("Discovering article links …")
  link_vec <- purrr::map_chr(authors, get_author_links) %>% unique()
  link_vec <- link_vec[!stringr::str_detect(link_vec, "profile/|uploads/")]  # filter unwanted
  link_df  <- tibble::tibble(links = link_vec,
                     folder_name = purrr::map_chr(link_vec, create_folder_name))
  readr::write_csv(link_df, file.path(output_root, "scraped_links.csv"))

  # 2. download content ------------------------------------------------------
  message("Downloading article content …")
  pb <- progress::progress_bar$new(total = nrow(link_df))
  meta_list <- purrr::pmap(link_df, function(links, folder_name) {
    pb$tick()
    extract_and_save(links, output_root, greenlist)
  })
  meta_df <- dplyr::bind_rows(meta_list)
  link_df <- dplyr::bind_cols(link_df, meta_df)

  # 3. Persist enriched csv --------------------------------------------------
  readr::write_csv(link_df, file.path(output_root, "updated_scraped_links.csv"))

  # 4. backup PDFs -----------------------------------------------------------
  copy_pdfs(output_root)

  invisible(link_df)
}
