# AEI extraction and download logic

#' Extract title, date & author from article html
#' @param article_html xml2 document
#' @return tibble with title, date, author
extract_metadata <- function(article_html) {
  # Extract title
  title_elements <- rvest::html_elements(
    article_html,
    xpath = "//*[self::h1 or self::h2 or self::h3]"
  )
  title_texts <- rvest::html_text(title_elements, trim = TRUE)
  title <- first_or(title_texts, "No Title Found")

  # Try to find date
  time_tags <- rvest::html_elements(article_html, xpath = "//time")
  date_texts <- as.character(rvest::html_text(time_tags, trim = TRUE))
  date_matches <- date_texts[
    !is.na(date_texts) &
      vapply(
        date_texts,
        function(x) grepl(date_patterns(), x),
        logical(1)
      )
  ]
  if (length(date_matches) == 0) {
    # Fallback: all tags, pick the shortest match
    possible_tags <- rvest::html_elements(
      article_html,
      xpath = "//p|//span|//div"
    )
    date_texts <- as.character(rvest::html_text(possible_tags, trim = TRUE))
    date_matches <- date_texts[
      !is.na(date_texts) &
        vapply(
          date_texts,
          function(x) grepl(date_patterns(), x),
          logical(1)
        )
    ]
    if (length(date_matches) > 0) {
      date <- date_matches[[which.min(nchar(date_matches))]]
    } else {
      date <- "No Date Found"
    }
  } else {
    date <- date_matches[[1]]
  }

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

#' Extract plain text from article html (<p>, <li>, headings)
#' @param article_html xml2 document
#' @return character
extract_text <- function(article_html) {
  text_elements <- rvest::html_elements(
    article_html,
    xpath = "//p|//li|//h1|//h2|//h3|//h4|//h5|//h6"
  )
  text_content <- rvest::html_text(text_elements)
  paste(text_content, collapse = "\n\n")
}

#' Download a single PDF given a URL
#' @keywords internal
save_pdf <- function(pdf_url, dest_path) {
  res <- tryCatch(
    httr::GET(pdf_url, httr::config(ssl_verifypeer = FALSE), httr::timeout(60)),
    error = function(e) FALSE
  )
  if (identical(res, FALSE)) return(FALSE)
  if (!is.null(res) && !httr::http_error(res)) {
    pdf_content <- tryCatch(httr::content(res, "raw"), error = function(e) NULL)
    if (is.null(pdf_content)) return(FALSE)
    tryCatch(
      {
        writeBin(pdf_content, dest_path)
        TRUE
      },
      error = function(e) FALSE
    )
  } else {
    FALSE
  }
}

#' Discover & download PDFs on the page
#' @keywords internal
handle_pdfs <- function(article_html, base_url, folder_path, greenlist) {
  n <- 1L
  downloaded_urls <- character(0)
  pdf_saved <- FALSE

  # 1. First check for direct PDF iframes
  iframe_elements <- rvest::html_elements(article_html, "iframe")
  if (length(iframe_elements) > 0) {
    for (i in seq_along(iframe_elements)) {
      attrs <- rvest::html_attrs(iframe_elements[[i]])
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
      if (grepl("viewer|pdfjs|documentcloud", pdf_url, ignore.case = TRUE)) {
        pdf_param <- NULL
        if (grepl("[?&]file=", pdf_url)) {
          pdf_param <- sub(".*[?&]file=([^&]+).*$", "\\1", pdf_url)
          pdf_param <- utils::URLdecode(pdf_param)
        } else if (grepl("[?&]pdf=", pdf_url)) {
          pdf_param <- sub(".*[?&]pdf=([^&]+).*$", "\\1", pdf_url)
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
  return(pdf_saved)
}

#' Extract article, metadata & PDFs for a given URL
#' @param url Character
#' @param output_root Root directory for results
#' @param greenlist Character vector of URL prefixes
#' @return Named list with title, date, author, and pdf_saved status
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
  main_element <- rvest::html_element(html, "main")
  main_article <- rvest::html_element(main_element, "article")
  if (is.na(main_article)) main_article <- html

  text <- extract_text(main_article)
  writeLines(text, file.path(folder_path, "article_text.txt"), useBytes = TRUE)
  pdf_saved <- handle_pdfs(main_article, url, folder_path, greenlist)
  meta <- extract_metadata(main_article)

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
    cat(
      "\n\n--- PDF TEXT ---\n",
      pdf_text,
      file = file.path(folder_path, "article_text.txt"),
      append = TRUE
    )
  }

  author_list <- if (!is.na(meta$author)) {
    strsplit(meta$author, ", ")[[1]]
  } else {
    character(0)
  }
  metadata <- list(
    title = meta$title,
    link = url,
    location = page_name,
    file_path = file.path(folder_path, "article_text.txt"),
    date = meta$date,
    authors = author_list
  )
  yyjsonr::write_json_file(
    metadata,
    file.path(folder_path, "metadata.json"),
    pretty = TRUE,
    auto_unbox = TRUE
  )
  result <- as.list(meta)
  result$pdf_saved <- pdf_saved
  result
}
