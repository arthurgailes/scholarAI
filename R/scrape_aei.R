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
#' * dplyr
#'
#' Ensure these packages are installed before running.
#'
#' @name scrape_aei
NULL

# ---------------------------------------------------------------------------
# Helpers -------------------------------------------------------------------

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

#' Convert a URL path into a safe folder name (one nested level allowed)
#'
#' @param url Character URL.
#' @return Character folder path relative to output directory.
create_folder_name <- function(url) {
  parsed_url <- urltools::url_parse(url)
  path <- parsed_url$path
  path <- gsub("^/|/$", "", path)
  parts <- strsplit(path, "/", fixed = TRUE)[[1]]
  if (length(parts) == 0) return(path)
  if (length(parts) >= 2) {
    first_part <- parts[1]
    rest_parts <- paste(parts[-1], collapse = "-")
    return(paste(first_part, rest_parts, sep = "/"))
  }
  path
}

# regex helpers --------------------------------------------------------------

.date_patterns <- paste0(
  "(",
  paste(
    c(
      "(?:Jan(?:uary)?|Feb(?:ruary)?|Mar(?:ch)?|Apr(?:il)?|May|Jun(?:e)?|Jul(?:y)?|Aug(?:ust)?|",
      "Sep(?:tember)?|Oct(?:ober)?|Nov(?:ember)?|Dec(?:ember)?)[[:space:]]+\\d{1,2},[[:space:]]+\\d{4}",
      "(?:Jan(?:uary)?|Feb(?:ruary)?|Mar(?:ch)?|Apr(?:il)?|May|Jun(?:e)?|Jul(?:y)?|Aug(?:ust)?|",
      "Sep(?:tember)?|Oct(?:ober)?|Nov(?:ember)?|Dec(?:ember)?)[[:space:]]+\\d{4}",
      "Week[[:space:]]+\\d{1,2}(?:[[:space:]]+&[[:space:]]+\\d{1,2})?,[[:space:]]+\\d{4}",
      "Q[1-4][[:space:]]+\\d{4}",
      "\\d{4}",
      "\\d{8}"
    ),
    collapse = "|"
  ),
  ")"
)

#' Extract title, date & author from article html
extract_metadata <- function(article_html) {
  # Extract title
  title_elements <- rvest::html_elements(
    article_html,
    xpath = "//*[self::h1 or self::h2 or self::h3]"
  )
  title_texts <- rvest::html_text(title_elements, trim = TRUE)
  title <- first_or(title_texts, "No Title Found")

  # Try to find date
  possible_tags <- rvest::html_elements(
    article_html,
    xpath = "//time|//p|//span|//div"
  )
  date_texts <- rvest::html_text(possible_tags, trim = TRUE)
  date_matches <- date_texts[sapply(
    date_texts,
    function(x) grepl(.date_patterns, x)
  )]
  date <- first_or(date_matches, "No Date Found")

  # Search inside title as fallback
  if (
    identical(date, "No Date Found") &&
      grepl(.date_patterns, title)
  )
    date <- regmatches(title, regexpr(.date_patterns, title))

  # Extract author
  author_elements <- rvest::html_elements(
    article_html,
    xpath = "//*[contains(@class,'author')]"
  )
  author_texts <- rvest::html_text(author_elements, trim = TRUE)
  author <- first_or(author_texts, "No Author Found")
  # Clean up author text
  author <- gsub("^(By|With) ", "", author)
  author <- gsub("\\|", ", ", author)

  tibble::tibble(title, date, author)
}

first_or <- function(x, default) if (length(x) == 0) default else x[[1]]

#' Extract plain text from article html (<p>, <li>, headings)
extract_text <- function(article_html) {
  # Get all paragraph, list item, and heading elements
  text_elements <- rvest::html_elements(
    article_html,
    xpath = "//p|//li|//h1|//h2|//h3|//h4|//h5|//h6"
  )
  # Extract text from elements
  text_content <- rvest::html_text(text_elements)
  # Combine all text with double newlines
  paste(text_content, collapse = "\n\n")
}

#' Download a single PDF given a URL
save_pdf <- function(pdf_url, dest_path) {
  res <- tryCatch(
    httr::GET(pdf_url, httr::config(ssl_verifypeer = FALSE), httr::timeout(60)),
    error = function(e) NULL
  )
  if (!is.null(res) && !httr::http_error(res))
    writeBin(httr::content(res, "raw"), dest_path)
}

#' Discover & download PDFs on the page
handle_pdfs <- function(article_html, base_url, folder_path, greenlist) {
  n <- 1L
  # iframe-embedded
  iframe_elements <- rvest::html_elements(article_html, "iframe")
  if (length(iframe_elements) > 0) {
    iframe_srcs <- rvest::html_attr(iframe_elements, "src")
    # Make sure we have a character vector, not NULL or list
    iframe_srcs <- as.character(iframe_srcs[!is.na(iframe_srcs)])

    if (length(iframe_srcs) > 0) {
      pdf_srcs <- iframe_srcs[sapply(
        iframe_srcs,
        function(x) grepl("\\.pdf$", x)
      )]
      for (src in pdf_srcs) {
        pdf_url <- xml2::url_absolute(src, base_url)
        save_pdf(pdf_url, file.path(folder_path, paste0(n, ".pdf")))
        n <- n + 1L
      }
    }
  }

  # extra anchors for greenlist urls
  if (base_url %in% greenlist) {
    a_elements <- rvest::html_elements(article_html, "a")
    if (length(a_elements) > 0) {
      a_hrefs <- rvest::html_attr(a_elements, "href")
      # Make sure we have a character vector, not NULL or list
      a_hrefs <- as.character(a_hrefs[!is.na(a_hrefs)])

      if (length(a_hrefs) > 0) {
        pdf_hrefs <- a_hrefs[sapply(a_hrefs, function(x) grepl("\\.pdf$", x))]
        for (src in pdf_hrefs) {
          pdf_url <- xml2::url_absolute(src, base_url)
          save_pdf(pdf_url, file.path(folder_path, paste0(n, ".pdf")))
          n <- n + 1L
        }
      }
    }
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
extract_and_save <- function(
  url,
  output_root = "data/intermed/aei_search_results",
  greenlist = "https://www.aei.org/housing-supply-case-studies/"
) {
  page_name <- create_folder_name(url)
  folder_path <- file.path(output_root, page_name)
  dir.create(folder_path, recursive = TRUE, showWarnings = FALSE)

  res <- tryCatch(
    httr::GET(url, httr::config(ssl_verifypeer = FALSE), httr::timeout(20)),
    error = function(e) NULL
  )
  if (is.null(res) || httr::http_error(res)) {
    warning("Failed to download ", url)
    return(list(
      title = NA_character_,
      date = NA_character_,
      author = NA_character_
    ))
  }
  html <- xml2::read_html(res)

  # Find main article element
  main_element <- rvest::html_element(html, "main")
  main_article <- rvest::html_element(main_element, "article")
  if (is.na(main_article)) main_article <- html

  text <- extract_text(main_article)
  writeLines(text, file.path(folder_path, "article_text.txt"), useBytes = TRUE)

  handle_pdfs(main_article, url, folder_path, greenlist)
  meta <- extract_metadata(main_article)

  # Create complete metadata object with all required fields
  # Split author string into a character vector for the authors array
  author_list <- if (!is.na(meta$author)) {
    strsplit(meta$author, ", ")[[1]]
  } else {
    character(0)
  }
  
  metadata <- list(
    title = meta$title,
    link = url,
    location = page_name,
    text = text,
    date = meta$date,
    authors = author_list
  )

  # Write metadata to JSON file
  yyjsonr::write_json_file(
    metadata,
    file.path(folder_path, "metadata.json"),
    pretty = TRUE,
    auto_unbox = TRUE
  )

  as.list(meta)
}

#' Copy all PDFs in search_results tree to backup directory
#'
#' @param search_root Directory to search for PDFs.
#' @param backup_dir Destination directory.
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
  dir.create(backup_dir, showWarnings = FALSE, recursive = TRUE)
  file.copy(pdfs, file.path(backup_dir, basename(pdfs)), overwrite = TRUE)
}

# ---------------------------------------------------------------------------
# Pipeline driver -----------------------------------------------------------

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

  # replace spaces in author names
  authors <- gsub(" ", "%20", authors)

  # 1. discover links --------------------------------------------------------
  message("Discovering article links …")
  # Combine all links from all authors into a single character vector
  link_vec <- unlist(lapply(authors, get_author_links))
  link_vec <- unique(link_vec)

  # Better filtering of unwanted URLs
  link_vec <- link_vec[
    # Keep only https URLs (or convert http to https)
    grepl("^https?://", link_vec) &
      # Filter out profile pages, uploads, PDFs, and other non-article pages
      !grepl("(profile/|uploads/|[.]pdf|[.]jpg|[.]png)", link_vec) &
      # Filter out certain sections that aren't articles
      !grepl("aeideas$", link_vec)
  ]

  # Convert any http URLs to https
  link_vec <- sub("^http://", "https://", link_vec)

  # Ensure URLs end with a trailing slash for consistency
  link_vec <- sub("([^/])$", "\\1/", link_vec)

  link_df <- tibble::tibble(
    links = link_vec,
    folder_name = sapply(link_vec, create_folder_name, USE.NAMES = FALSE)
  )
  write.csv(
    link_df,
    file.path(output_root, "scraped_links.csv"),
    row.names = FALSE
  )

  # 2. download content ------------------------------------------------------
  message("Downloading article content …")
  pb <- progress::progress_bar$new(total = nrow(link_df))
  meta_list <- vector("list", nrow(link_df))
  for (i in seq_len(nrow(link_df))) {
    pb$tick()
    meta_list[[i]] <- extract_and_save(link_df$links[i], output_root, greenlist)
  }
  meta_df <- dplyr::bind_rows(meta_list)
  link_df <- dplyr::bind_cols(link_df, meta_df)

  # 3. Persist enriched csv --------------------------------------------------
  write.csv(
    link_df,
    file.path(output_root, "updated_scraped_links.csv"),
    row.names = FALSE
  )

  # 4. backup PDFs -----------------------------------------------------------
  copy_pdfs(output_root)

  invisible(link_df)
}
