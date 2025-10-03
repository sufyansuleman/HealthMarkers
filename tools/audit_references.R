# Audit @references across R/*.R for CRAN-friendly formatting
# - Flags: bullets/headings, raw URLs, doi.org links, raw "doi:" tokens, PubMed links,
#          HTML/Markdown, validation/guideline keywords
# - Optional DOI HEAD check (requires httr)
# - Suggests where a known DOI should be added (dictionary-based)
# - Outputs a data.frame and prints a concise report; optional CSV export

audit_references <- function(root = ".", path = file.path(root, "R"),
                             check_doi = FALSE, timeout = 5,
                             write_csv = NULL, verbose = TRUE) {
  files <- list.files(path, pattern = "\\.[Rr]$", full.names = TRUE)
  if (!length(files)) stop("No R files found in: ", path)
  
  # Known primary refs and DOIs (safe additions)
  doi_map <- list(
    "homeostasis model assessment|\\bHOMA\\b|matthews.*1985"   = "10.1007/BF00280883",
    "quantitative insulin sensitivity check index|\\bQUICKI\\b|katz.*2000" = "10.1210/jcem.85.7.6661",
    "mcauley.*2001|index of insulin sensitivity.*triglyceride" = "10.2337/diacare.24.3.460",
    "\\bTyG\\b|product of fasting glucose and triglycerides|simental" = "10.1089/met.2007.0038",
    "\\bVAI\\b|visceral adiposity index|amato.*2010"          = "10.2337/dc09-1825",
    "\\bLAP\\b|lipid accumulation product|kahn.*2005"         = "10.2337/diacare.28.7.1757",
    "global lung function|gli equations|quanjer.*2012"        = "10.1183/09031936.00162412",
    "hankinson.*1999.*nhanes"                                 = "10.1164/ajrccm.159.1.9712108",
    "two formulas.*area under the curve|pruessner.*2003"      = "10.1016/S0306-4530(02)00108-7",
    "dill.*costill.*1974|j appl physiol.*37\\(2\\):247"       = "10.1152/jappl.1974.37.2.247",
    "qrisk3.*2017|hippisley-cox"                              = "10.1136/bmj.j2099"
  )
  
  drop_pat <- "(?i)\\b(review|guideline|consensus|statement|recommendation|position|validation|validated|meta[- ]?analysis|systematic review|update|overview)\\b"
  
  # Helpers
  find_ref_blocks <- function(txt) {
    idx <- grep("^#'\\s*@references\\b", txt)
    blocks <- list()
    for (i in idx) {
      j <- i + 1L
      while (j <= length(txt) && grepl("^#'", txt[j]) && !grepl("^#'\\s*@\\w+\\b", txt[j])) j <- j + 1L
      if (j - 1L > i) blocks[[length(blocks) + 1L]] <- list(start = i + 1L, end = j - 1L)
    }
    blocks
  }
  
  extract_dois <- function(lines) {
    L <- gsub("^#'\\s*", "", lines)
    dois <- character()
    m <- regmatches(L, gregexpr("\\\\doi\\{(10\\.[^}[:space:]]+)\\}", L, perl = TRUE))
    if (length(m)) dois <- c(dois, unlist(m, use.names = FALSE))
    m2 <- regmatches(L, gregexpr("https?://doi\\.org/(10\\.[^[:space:]]+)", L, perl = TRUE))
    if (length(m2)) dois <- c(dois, sub("^https?://doi\\.org/", "", unlist(m2, use.names = FALSE)))
    m3 <- regmatches(L, gregexpr("(?i)\\bdoi\\s*:\\s*(10\\.[^\\s]+)", L, perl = TRUE))
    if (length(m3)) dois <- c(dois, unlist(m3, use.names = FALSE))
    unique(dois)
  }
  
  line_has_known_but_missing_doi <- function(line) {
    low <- tolower(line)
    if (grepl("\\\\doi\\{", low)) return(NA_character_)
    for (pat in names(doi_map)) {
      if (grepl(pat, low, perl = TRUE)) return(doi_map[[pat]])
    }
    NA_character_
  }
  
  check_doi_head <- function(doi) {
    if (!requireNamespace("httr", quietly = TRUE)) return(NA_integer_)
    url <- paste0("https://doi.org/", doi)
    resp <- try(httr::RETRY("HEAD", url, times = 2, httr::timeout(timeout), quiet = TRUE), silent = TRUE)
    if (inherits(resp, "try-error")) return(NA_integer_)
    code <- httr::status_code(resp)
    # Fallback: some endpoints reject HEAD; try GET
    if (!is.na(code) && code == 405) {
      resp <- try(httr::RETRY("GET", url, times = 2, httr::timeout(timeout), quiet = TRUE), silent = TRUE)
      if (inherits(resp, "try-error")) return(NA_integer_)
      code <- httr::status_code(resp)
    }
    as.integer(code)
  }
  
  audit_rows <- list()
  
  for (f in files) {
    txt <- readLines(f, warn = FALSE, encoding = "UTF-8")
    blocks <- find_ref_blocks(txt)
    if (!length(blocks)) {
      audit_rows[[length(audit_rows) + 1L]] <- data.frame(
        file = f, has_refs = FALSE, blocks = 0L, n_lines = 0L,
        has_bullets = NA, has_headings = NA, has_urls = NA,
        has_pubmed = NA, has_raw_doi_url = NA, has_raw_doi_token = NA,
        has_html = NA, has_markdown = NA, has_validation_terms = NA,
        dois_found = 0L, dois_ok = NA, dois_fail = NA,
        needs_known_doi = 0L, stringsAsFactors = FALSE
      )
      next
    }
    
    # Aggregate flags per file
    flg <- list(
      has_bullets = FALSE, has_headings = FALSE, has_urls = FALSE,
      has_pubmed = FALSE, has_raw_doi_url = FALSE, has_raw_doi_token = FALSE,
      has_html = FALSE, has_markdown = FALSE, has_validation_terms = FALSE
    )
    n_lines <- 0L
    dois <- character()
    needs <- character()
    
    for (bi in blocks) {
      block <- txt[bi]
      n_lines <- n_lines + length(block)
      L <- gsub("^#'\\s*", "", block)
      
      flg$has_bullets <- flg$has_bullets || any(grepl("^[-*]\\s", L))
      flg$has_headings <- flg$has_headings || any(grepl("^#+\\s", L))
      flg$has_urls <- flg$has_urls || any(grepl("https?://", L))
      flg$has_pubmed <- flg$has_pubmed || any(grepl("ncbi\\.nlm\\.nih\\.gov|pubmed\\.", L, ignore.case = TRUE))
      flg$has_raw_doi_url <- flg$has_raw_doi_url || any(grepl("https?://doi\\.org/10\\.", L))
      flg$has_raw_doi_token <- flg$has_raw_doi_token || any(grepl("(?i)\\bdoi\\s*:", L, perl = TRUE))
      flg$has_html <- flg$has_html || any(grepl("<[A-Za-z]+>", L))
      flg$has_markdown <- flg$has_markdown || any(grepl("\\[[^\\]]+\\]\\([^\\)]+\\)", L))
      flg$has_validation_terms <- flg$has_validation_terms || any(grepl(drop_pat, L, perl = TRUE))
      
      dois <- c(dois, extract_dois(block))
      
      # Known DOI suggestions
      for (ln in L) {
        sug <- line_has_known_but_missing_doi(ln)
        if (!is.na(sug)) needs <- c(needs, sug)
      }
    }
    
    dois <- unique(dois)
    needs <- unique(needs)
    status_ok <- status_fail <- NA_integer_
    if (check_doi && length(dois)) {
      sc <- vapply(dois, check_doi_head, integer(1))
      status_ok <- sum(!is.na(sc) & sc >= 200 & sc < 400)
      status_fail <- sum(!is.na(sc) & sc >= 400)
    }
    
    audit_rows[[length(audit_rows) + 1L]] <- data.frame(
      file = f, has_refs = TRUE, blocks = length(blocks), n_lines = n_lines,
      has_bullets = flg$has_bullets, has_headings = flg$has_headings, has_urls = flg$has_urls,
      has_pubmed = flg$has_pubmed, has_raw_doi_url = flg$has_raw_doi_url,
      has_raw_doi_token = flg$has_raw_doi_token, has_html = flg$has_html,
      has_markdown = flg$has_markdown, has_validation_terms = flg$has_validation_terms,
      dois_found = length(dois), dois_ok = if (length(status_ok)) status_ok else NA,
      dois_fail = if (length(status_fail)) status_fail else NA,
      needs_known_doi = length(needs), stringsAsFactors = FALSE
    )
  }
  
  out <- do.call(rbind, audit_rows)
  
  if (!is.null(write_csv)) {
    utils::write.csv(out, write_csv, row.names = FALSE)
    if (verbose) message("Wrote audit CSV: ", write_csv)
  }
  
  if (verbose) {
    cat("\nReference audit summary (", path, ")\n", sep = "")
    cat("Files scanned:", nrow(out), "\n")
    cat("No @references:", sum(!out$has_refs), "\n")
    issues <- c(
      bullets = sum(out$has_bullets, na.rm = TRUE),
      headings = sum(out$has_headings, na.rm = TRUE),
      urls = sum(out$has_urls, na.rm = TRUE),
      pubmed = sum(out$has_pubmed, na.rm = TRUE),
      raw_doi_url = sum(out$has_raw_doi_url, na.rm = TRUE),
      raw_doi_token = sum(out$has_raw_doi_token, na.rm = TRUE),
      html = sum(out$has_html, na.rm = TRUE),
      markdown = sum(out$has_markdown, na.rm = TRUE),
      validation_terms = sum(out$has_validation_terms, na.rm = TRUE)
    )
    print(issues)
    
    bad <- subset(out, has_refs & (has_bullets | has_headings | has_urls | has_raw_doi_url |
                                     has_raw_doi_token | has_html | has_markdown | has_pubmed | has_validation_terms))
    if (nrow(bad)) {
      cat("\nFiles needing attention:\n")
      for (i in seq_len(nrow(bad))) {
        row <- bad[i, ]
        flags <- names(which(unlist(row[c("has_bullets","has_headings","has_urls","has_pubmed",
                                          "has_raw_doi_url","has_raw_doi_token","has_html",
                                          "has_markdown","has_validation_terms")]) == TRUE))
        cat("-", basename(row$file), ":", paste(flags, collapse = ", "), "\n")
      }
    }
    
    if (check_doi) {
      cat("\nDOI check: found total DOIs =", sum(out$dois_found), "; failures (HTTP >=400) sum =",
          sum(out$dois_fail, na.rm = TRUE), "\n")
    }
    
    need <- subset(out, needs_known_doi > 0)
    if (nrow(need)) {
      cat("\nFiles where a known DOI likely applies but is missing (based on text patterns):\n")
      cat(paste0("- ", basename(need$file), " (", need$needs_known_doi, ")\n"), sep = "")
      cat("Tip: run your cleanup to append \\doi{...} for these known primaries.\n")
    }
  }
  
  out
}

# Usage examples (Windows PowerShell):
# Rscript -e "source('tools/audit_references.R'); audit_references(path='R', check_doi=FALSE)"
# Rscript -e "source('tools/audit_references.R'); audit_references(path='R', check_doi=TRUE, write_csv='tools/reference_audit.csv')"

audit_references_details <- function(path = "R", include_clean = FALSE,
                                     write_csv = NULL, verbose = TRUE) {
  files <- list.files(path, pattern = "\\.[Rr]$", full.names = TRUE)
  if (!length(files)) stop("No R files found in: ", path)

  doi_map <- list(
    "homeostasis model assessment|\\bHOMA\\b|matthews.*1985"   = "10.1007/BF00280883",
    "quantitative insulin sensitivity check index|\\bQUICKI\\b|katz.*2000" = "10.1210/jcem.85.7.6661",
    "mcauley.*2001|index of insulin sensitivity.*triglyceride" = "10.2337/diacare.24.3.460",
    "\\bTyG\\b|product of fasting glucose and triglycerides|simental" = "10.1089/met.2007.0038",
    "\\bVAI\\b|visceral adiposity index|amato.*2010"          = "10.2337/dc09-1825",
    "\\bLAP\\b|lipid accumulation product|kahn.*2005"         = "10.2337/diacare.28.7.1757",
    "global lung function|gli equations|quanjer.*2012"        = "10.1183/09031936.00162412",
    "hankinson.*1999.*nhanes"                                 = "10.1164/ajrccm.159.1.9712108",
    "two formulas.*area under the curve|pruessner.*2003"      = "10.1016/S0306-4530(02)00108-7",
    "dill.*costill.*1974|j appl physiol.*37\\(2\\):247"       = "10.1152/jappl.1974.37.2.247",
    "qrisk3.*2017|hippisley-cox"                              = "10.1136/bmj.j2099"
  )

  drop_pat <- "(?i)\\b(review|guideline|consensus|statement|recommendation|position|validation|validated|meta[- ]?analysis|systematic review|update|overview)\\b"

  find_ref_blocks <- function(txt) {
    idx <- grep("^#'\\s*@references\\b", txt)
    blocks <- list()
    for (i in idx) {
      j <- i + 1L
      while (j <= length(txt) && grepl("^#'", txt[j]) && !grepl("^#'\\s*@\\w+\\b", txt[j])) j <- j + 1L
      if (j - 1L > i) blocks[[length(blocks) + 1L]] <- list(start = i + 1L, end = j - 1L)
    }
    blocks
  }

  needs_known_doi <- function(line) {
    low <- tolower(line)
    if (grepl("\\\\doi\\{10\\.", low) || grepl("https?://doi\\.org/10\\.", low)) return(NA_character_)
    for (pat in names(doi_map)) if (grepl(pat, low, perl = TRUE)) return(doi_map[[pat]])
    NA_character_
  }

  rows <- list()
  for (f in files) {
    txt <- readLines(f, warn = FALSE, encoding = "UTF-8")
    blocks <- find_ref_blocks(txt)
    if (!length(blocks)) next

    for (b in blocks) {
      for (ln in b$start:b$end) {
        raw <- txt[ln]
        rox <- sub("^#'\\s*", "", raw)
        flags <- list(
          bullet = grepl("^[-*]\\s", rox),
          heading = grepl("^#+\\s", rox),
          url = grepl("https?://", rox),
          pubmed = grepl("ncbi\\.nlm\\.nih\\.gov|pubmed\\.", rox, ignore.case = TRUE),
          raw_doi_url = grepl("https?://doi\\.org/10\\.", rox),
          raw_doi_token = grepl("(?i)\\bdoi\\s*[: ]", rox, perl = TRUE),
          html = grepl("<[A-Za-z]+>", rox),
          markdown = grepl("\\[[^\\]]+\\]\\([^\\)]+\\)", rox),
          validation = grepl(drop_pat, rox, perl = TRUE)
        )
        has_issue <- any(unlist(flags))
        if (include_clean || has_issue) {
          rows[[length(rows) + 1L]] <- data.frame(
            file = f, file_line = ln, text = rox,
            issue_bullet = flags$bullet,
            issue_heading = flags$heading,
            issue_url = flags$url,
            issue_pubmed = flags$pubmed,
            issue_raw_doi_url = flags$raw_doi_url,
            issue_raw_doi_token = flags$raw_doi_token,
            issue_html = flags$html,
            issue_markdown = flags$markdown,
            issue_validation = flags$validation,
            suggest_doi = needs_known_doi(rox),
            stringsAsFactors = FALSE
          )
        }
      }
    }
  }

  out <- if (length(rows)) do.call(rbind, rows) else
    data.frame(file = character(), file_line = integer(), text = character())

  if (!is.null(write_csv) && nrow(out)) {
    utils::write.csv(out, write_csv, row.names = FALSE)
    if (verbose) message("Wrote detailed audit CSV: ", write_csv)
  }
  if (verbose) cat("Detailed @references issues (per line). Rows:", nrow(out), "\n")
  out
}

print_reference_issues <- function(df, max_lines_per_file = 50) {
  if (!nrow(df)) { cat("No issues found.\n"); return(invisible()) }
  files <- split(df, df$file)
  for (f in names(files)) {
    cat("\n==>", basename(f), "\n")
    d <- files[[f]]
    if (nrow(d) > max_lines_per_file) {
      cat("(showing first", max_lines_per_file, "of", nrow(d), "lines)\n")
      d <- d[1:max_lines_per_file, ]
    }
    for (i in seq_len(nrow(d))) {
      row <- d[i, ]
      tags <- c(
        if (isTRUE(row$issue_bullet)) "bullet",
        if (isTRUE(row$issue_heading)) "heading",
        if (isTRUE(row$issue_url)) "url",
        if (isTRUE(row$issue_pubmed)) "pubmed",
        if (isTRUE(row$issue_raw_doi_url)) "raw_doi_url",
        if (isTRUE(row$issue_raw_doi_token)) "raw_doi_token",
        if (isTRUE(row$issue_html)) "html",
        if (isTRUE(row$issue_markdown)) "markdown",
        if (isTRUE(row$issue_validation)) "validation",
        if (!is.na(row$suggest_doi) && nzchar(row$suggest_doi)) paste0("suggest_doi:", row$suggest_doi)
      )
      cat(sprintf("  L%-5d [%s] %s\n", row$file_line, paste(tags, collapse = ","), row$text))
    }
  }
  invisible(df)
}