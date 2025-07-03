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
#' @name scrape_aei
NULL

# ---------------------------------------------------------------------------
# Pipeline driver functions -------------------------------------------------

utils::globalVariables(c(".", "links"))


#' Retrieve all post links for a single AEI author
#'
#' @param author_slug Character. URL-encoded author string, e.g. "Edward%20J.%20Pinto".
#' @param max_pages Integer. Safety stop to avoid infinite loops (default 2000).
#' @param progress Logical. If TRUE, prints page numbers while scraping.
#'
#' @return Character vector of article URLs.
#' @export
get_author_links <- function(author_slug, max_pages = 2000, progress = TRUE) {
  base_url <- "https://www.aei.org/search-results/"
  url_prefix <- paste0(
    base_url,
    "?wpsolr_fq%5B0%5D=author_str:",
    author_slug,
    "&wpsolr_page="
  )

  page_num <- 1L
  out <- character()

  repeat {
    if (page_num > max_pages) break
    if (progress) message("Scraping page ", page_num)

    page_url <- paste0(url_prefix, page_num)
    res <- tryCatch(
      httr::GET(
        page_url,
        httr::config(ssl_verifypeer = FALSE),
        httr::timeout(20)
      ),
      error = function(e) NULL
    )
    if (is.null(res) || httr::http_error(res)) break

    pg <- xml2::read_html(res)
    # Replace pipe with nested function calls
    post_elements <- rvest::html_elements(pg, "div.post.post-search a[href]")
    links <- rvest::html_attr(post_elements, "href")
    links <- links[!is.na(links)]

    if (length(links) == 0) break
    out <- c(out, links)
    page_num <- page_num + 1L
  }
  unique(out)
}

#' Copy PDFs from article folders to a central location
#'
#' @param output_root Root directory containing article folders
#' @param pdf_dir Target directory for PDF copies (default: output_root/all_pdfs)
#' @return Invisibly returns paths to copied PDFs
#' @param url Character.
#' @param output_root Root directory for `aei_search_results` (default inside project).
#' @param greenlist Character vector of URL prefixes that require extra PDF search.
#'
#' @return Named list with title, date, author, and pdf_saved status.


#' Copy all PDFs in search_results tree to backup directory
#'
#' @param search_root Directory to search for PDFs.
#' @param backup_dir Destination directory.
#'
#' @return Integer count of successfully copied PDF files.
#' @export
copy_pdfs <- function(
  search_root = "data/intermed/aei_search_results",
  backup_dir = "R:/archive/Published PDFs"
) {
  pdfs <- list.files(
    search_root,
    "[.]pdf$",
    recursive = TRUE,
    full.names = TRUE
  )

  if (length(pdfs) == 0) {
    message("No PDF files found in ", search_root)
    return(0L)
  }

  dir.create(backup_dir, showWarnings = FALSE, recursive = TRUE)

  root_norm <- normalizePath(search_root, winslash = "/", mustWork = FALSE)
  parent_norm <- normalizePath(dirname(pdfs), winslash = "/", mustWork = FALSE)
  target_files <- file.path(
    backup_dir,
    ifelse(
      parent_norm == root_norm,
      basename(pdfs),
      paste0(basename(dirname(pdfs)), "_", basename(pdfs))
    )
  )
  num_copied <- sum(file.copy(pdfs, target_files, overwrite = TRUE))
  message(
    "Copied ",
    num_copied,
    " of ",
    length(pdfs),
    " PDF files to ",
    backup_dir
  )
  return(num_copied)
}

#' Run the full AEI scraping pipeline
#'
#' @param authors Character vector of author names.
#' @param output_root Directory where intermediate results live.
#' @param greenlist Character vector of URL prefixes that require extra PDF search.
#' @export
scrape_aei <- function(
  authors = c("Edward J. Pinto", "Tobias Peter"),
  output_root = "data/intermed/aei_search_results",
  greenlist = "https://www.aei.org/housing-supply-case-studies/"
) {
  dir.create(output_root, recursive = TRUE, showWarnings = FALSE)

  authors <- gsub(" ", "%20", authors)

  message("Discovering article links …")
  link_vec <- unlist(lapply(authors, scholarAI::get_author_links))
  link_vec <- unique(link_vec)

  link_vec <- link_vec[
    grepl("^https?://", link_vec) &
      !grepl("(profile/|uploads/|[.]pdf|[.]jpg|[.]png)", link_vec) &
      !grepl("aeideas$", link_vec)
  ]
  link_vec <- sub("^http://", "https://", link_vec)
  link_vec <- sub("([^/])$", "\\1/", link_vec)

  link_df <- data.frame(
    links = link_vec,
    folder_name = sapply(link_vec, scholarAI::create_folder_name, USE.NAMES = FALSE)
  )
  write.csv(
    link_df,
    file.path(output_root, "scraped_links.csv"),
    row.names = FALSE
  )

  message("Downloading article content …")
  pb <- progress::progress_bar$new(total = nrow(link_df))
  meta_list <- vector("list", nrow(link_df))
  for (i in seq_len(nrow(link_df))) {
    pb$tick()
    meta_list[[i]] <- scholarAI::extract_and_save(link_df$links[i], output_root, greenlist)
  }
  meta_df <- collapse::rowbind(meta_list)
  link_df <- cbind(link_df, meta_df)

  write.csv(
    link_df,
    file.path(output_root, "updated_scraped_links.csv"),
    row.names = FALSE
  )

  scholarAI::copy_pdfs(output_root)

  invisible(tibble::as_tibble(link_df))
}
