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

  tibble::tibble(title = title, date = date, author = author)
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
    error = function(e) FALSE
  )
  if (identical(res, FALSE)) return(FALSE)
  if (!is.null(res) && !httr::http_error(res)) {
    pdf_content <- tryCatch(httr::content(res, "raw"), error = function(e) NULL)
    if (is.null(pdf_content)) return(FALSE)
    tryCatch({
      writeBin(pdf_content, dest_path)
      TRUE
    }, error = function(e) FALSE)
  } else {
    FALSE
  }
}

#' Discover & download PDFs on the page
handle_pdfs <- function(article_html, base_url, folder_path, greenlist) {
  n <- 1L
  # Track downloaded PDFs to avoid duplicates
  downloaded_urls <- character(0)
  pdf_saved <- FALSE

  # 1. First check for direct PDF iframes - this is the most common case for AEI
  iframe_elements <- rvest::html_elements(article_html, "iframe")
  if (length(iframe_elements) > 0) {
    # Process all iframe sources
    for (i in seq_along(iframe_elements)) {
      # Get all attributes
      attrs <- rvest::html_attrs(iframe_elements[[i]])

      # Check for src attribute that points to PDF
      if ("src" %in% names(attrs)) {
        src <- attrs["src"]
        if (grepl("\\.pdf", src, ignore.case = TRUE)) {
          pdf_url <- xml2::url_absolute(src, base_url)
          if (!(pdf_url %in% downloaded_urls)) {
            pdf_path <- file.path(folder_path, paste0(n, ".pdf"))
            if (isTRUE(save_pdf(pdf_url, pdf_path))) {
              message("Saved PDF from iframe: ", basename(pdf_path))
              downloaded_urls <- c(downloaded_urls, pdf_url)
              n <- n + 1L
              pdf_saved <- TRUE
            }
          }
        }
      }

      # Check for data-src attribute that points to PDF
      if ("data-src" %in% names(attrs)) {
        data_src <- attrs["data-src"]
        if (!is.na(data_src) && grepl("\\.pdf", data_src, ignore.case = TRUE)) {
          pdf_url <- xml2::url_absolute(data_src, base_url)
          if (!(pdf_url %in% downloaded_urls)) {
            pdf_path <- file.path(folder_path, paste0(n, ".pdf"))
            if (isTRUE(save_pdf(pdf_url, pdf_path))) {
              message("Saved PDF from iframe data-src: ", basename(pdf_path))
              downloaded_urls <- c(downloaded_urls, pdf_url)
              n <- n + 1L
              pdf_saved <- TRUE
            }
          }
        }
      }
    }
  }

  # 2. Check for PDF viewers in iframes if we haven't found direct PDFs
  if (!pdf_saved && length(iframe_elements) > 0) {
    iframe_srcs <- rvest::html_attr(iframe_elements, "src")
    iframe_srcs <- as.character(iframe_srcs[!is.na(iframe_srcs)])

    for (src in iframe_srcs) {
      pdf_url <- xml2::url_absolute(src, base_url)
      # For PDF viewers, try to extract the PDF URL
      if (grepl("viewer|pdfjs|documentcloud", pdf_url, ignore.case = TRUE)) {
        # Extract PDF URL from viewer URL if possible
        pdf_param <- NULL
        if (grepl("[?&]file=", pdf_url)) {
          pdf_param <- sub(".*[?&]file=([^&]+).*", "\\1", pdf_url)
          pdf_param <- utils::URLdecode(pdf_param)
        } else if (grepl("[?&]pdf=", pdf_url)) {
          pdf_param <- sub(".*[?&]pdf=([^&]+).*", "\\1", pdf_url)
          pdf_param <- utils::URLdecode(pdf_param)
        }

        if (!is.null(pdf_param) && !(pdf_param %in% downloaded_urls)) {
          actual_pdf_url <- xml2::url_absolute(pdf_param, base_url)
          pdf_path <- file.path(folder_path, paste0(n, ".pdf"))
          if (save_pdf(actual_pdf_url, pdf_path)) {
            message("Saved PDF from viewer: ", basename(pdf_path))
            downloaded_urls <- c(downloaded_urls, actual_pdf_url)
            n <- n + 1L
            pdf_saved <- TRUE
          }
        }
      }
    }
  }

  # 3. Check for direct PDF links in anchor tags
  a_elements <- rvest::html_elements(article_html, "a[href]")
  if (length(a_elements) > 0) {
    a_hrefs <- rvest::html_attr(a_elements, "href")
    a_hrefs <- as.character(a_hrefs[!is.na(a_hrefs)])

    # Look for direct PDF links
    pdf_hrefs <- a_hrefs[grepl("\\.pdf", a_hrefs, ignore.case = TRUE)]
    for (href in pdf_hrefs) {
      pdf_url <- xml2::url_absolute(href, base_url)
      if (!(pdf_url %in% downloaded_urls)) {
        pdf_path <- file.path(folder_path, paste0(n, ".pdf"))
        if (isTRUE(save_pdf(pdf_url, pdf_path))) {
          message("Saved PDF from link: ", basename(pdf_path))
          downloaded_urls <- c(downloaded_urls, pdf_url)
          n <- n + 1L
          pdf_saved <- TRUE
        }
      }
    }
  }

  # Return TRUE if we saved any PDFs
  return(pdf_saved)
}

# core ----------------------------------------------------------------------

#' Extract article, metadata & PDFs for a given URL
#'
#' @param url Character.
#' @param output_root Root directory for `aei_search_results` (default inside project).
#' @param greenlist Character vector of URL prefixes that require extra PDF search.
#'
#' @return Named list with title, date, author, and pdf_saved status.
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
      author = NA_character_,
      pdf_saved = FALSE
    ))
  }
  html <- xml2::read_html(res)

  # Find main article element
  main_element <- rvest::html_element(html, "main")
  main_article <- rvest::html_element(main_element, "article")
  if (is.na(main_article)) main_article <- html

  text <- extract_text(main_article)
  writeLines(text, file.path(folder_path, "article_text.txt"), useBytes = TRUE)

  # Save PDFs and track if any were saved
  pdf_saved <- handle_pdfs(main_article, url, folder_path, greenlist)
  meta <- extract_metadata(main_article)

  # --- PDF text extraction ---
  pdf_files <- list.files(folder_path, pattern = "\\.pdf$", full.names = TRUE)
  pdf_text <- ""
  if (length(pdf_files) > 0) {
    if (!requireNamespace("pdftools", quietly = TRUE)) {
      stop("The pdftools package is required to extract PDF text.")
    }
    pdf_texts <- lapply(pdf_files, function(f) {
      txt <- tryCatch(
        pdftools::pdf_text(f),
        error = function(e) "[PDF extraction failed]"
      )
      paste0("[PDF: ", basename(f), "]\n", paste(txt, collapse = "\n"))
    })
    pdf_text <- paste(pdf_texts, collapse = "\n\n")
    # Append to article_text.txt
    cat(
      "\n\n--- PDF TEXT ---\n",
      pdf_text,
      file = file.path(folder_path, "article_text.txt"),
      append = TRUE
    )
  }

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
    pdf_text = pdf_text,
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

  # Return metadata with pdf_saved status
  result <- as.list(meta)
  result$pdf_saved <- pdf_saved
  result
}

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

  # Create unique filenames by prefixing with folder name
  root_norm <- normalizePath(search_root, winslash = "/", mustWork = FALSE)
  parent_norm <- normalizePath(dirname(pdfs), winslash = "/", mustWork = FALSE)
  target_files <- file.path(
    backup_dir,
    ifelse(parent_norm == root_norm, basename(pdfs), paste0(basename(dirname(pdfs)), "_", basename(pdfs)))
  )
  num_copied <- sum(file.copy(pdfs, target_files, overwrite = TRUE))
  message("Copied ", num_copied, " of ", length(pdfs), " PDF files to ", backup_dir)
  return(num_copied)
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

  link_df <- data.frame(
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
  meta_df <- collapse::rowbind(meta_list)
  link_df <- cbind(link_df, meta_df)

  # 3. Persist enriched csv --------------------------------------------------
  write.csv(
    link_df,
    file.path(output_root, "updated_scraped_links.csv"),
    row.names = FALSE
  )

  # 4. backup PDFs -----------------------------------------------------------
  copy_pdfs(output_root)

  invisible(tibble::as_tibble(link_df))
}
