# tools/fix-nonascii.R
# Developer helpers to audit and fix non‑ASCII characters in source files.
# Keep this outside the package code (e.g., in tools/) and add ^tools$ to .Rbuildignore.

# ---- Audit: list non‑ASCII characters (with optional per‑line report) -------

#' List files containing non‑ASCII characters
#' @param paths Character vector of files/dirs to scan (default: "R").
#' @param pattern Regex of file extensions to include (default: .R/.Rmd).
#' @param recursive Recurse into directories.
#' @param report If TRUE, returns a data.frame with file/line/col/char/codepoint.
#'               If FALSE, returns a named list: file -> unique chars.
#' @return data.frame or list (invisibly).
list_non_ascii <- function(paths = "R",
                           pattern = "\\.(R|Rmd)$",
                           recursive = TRUE,
                           report = FALSE) {
  files <- unique(unlist(lapply(paths, function(p) {
    if (dir.exists(p)) {
      list.files(p, pattern, full.names = TRUE, recursive = recursive)
    } else if (file.exists(p)) {
      normalizePath(p)
    } else character(0)
  }), use.names = FALSE))
  
  if (!length(files)) {
    message("No files matched under: ", paste(paths, collapse = ", "))
    return(invisible(if (report) data.frame() else list()))
  }
  
  if (!report) {
    out <- lapply(files, function(f) {
      txt <- readLines(f, encoding = "UTF-8", warn = FALSE)
      bad <- unique(unlist(regmatches(txt, gregexpr("[^\\x00-\\x7F]", txt, perl = TRUE))))
      bad[bad == ""] <- NULL
      list(file = f, non_ascii = bad)
    })
    names(out) <- files
    out[sapply(out, function(x) length(x$non_ascii) > 0)]
  } else {
    rows <- list()
    for (f in files) {
      txt <- readLines(f, encoding = "UTF-8", warn = FALSE)
      for (i in seq_along(txt)) {
        line <- txt[[i]]
        m <- gregexpr("[^\\x00-\\x7F]", line, perl = TRUE)[[1]]
        if (m[1] != -1) {
          chars <- regmatches(line, list(m))[[1]]
          for (j in seq_along(chars)) {
            ch <- chars[[j]]
            cp <- utf8ToInt(ch)
            rows[[length(rows) + 1L]] <- data.frame(
              file = f,
              line = i,
              col  = m[j],
              char = ch,
              codepoint = sprintf("U+%04X", cp),
              stringsAsFactors = FALSE
            )
          }
        }
      }
    }
    if (!length(rows)) {
      message("No non‑ASCII characters found.")
      return(invisible(data.frame()))
    }
    df <- do.call(rbind, rows)
    rownames(df) <- NULL
    df
  }
}

# ---- Fix: replace non‑ASCII with ASCII or \uXXXX escapes + reporting --------

#' Fix non‑ASCII characters across files (with optional report)
#' @param paths Character vector of files/dirs to process (default: "R").
#' @param pattern Regex of file extensions to include (default: .R/.Rmd).
#' @param recursive Recurse into directories.
#' @param dry_run If TRUE, only report which files/positions would change.
#' @param backup If TRUE, write .bak files with original contents before changes.
#' @param use_unicode_escapes If TRUE, use \\uXXXX/\\UXXXXXXXX escapes instead of ASCII.
#' @param include_ext Optional extra extensions to include, e.g. c("qmd","Rd","Rnw").
#' @param report If TRUE, returns a data.frame of (file, line, col, char, replacement, codepoint).
#' @param write_report Optional file path to write the report as CSV.
#' @return Invisibly, character vector of files changed (or that would change).
fix_non_ascii <- function(paths = "R",
                          pattern = "\\.(R|Rmd)$",
                          recursive = TRUE,
                          dry_run = FALSE,
                          backup = TRUE,
                          use_unicode_escapes = FALSE,
                          include_ext = c(),
                          report = FALSE,
                          write_report = NULL) {
  
  # Base ASCII fallbacks
  repl_ascii <- c(
    "’"="'","‘"="'","“"="\"","”"="\"",
    "–"="-","—"="-","−"="-","‑"="-","—"="-",
    "…"="...","→"="->","×"="x","•"="-",
    "±"="+/-","≥"=">=","≤"="<=",
    "™"="(TM)","®"="(R)","©"="(C)",
    " "=" ",          # NBSP U+00A0
    "µ"="u",          # micro sign
    "°"=" deg"
  )
  # Superscripts/subscripts → ASCII
  sup_map <- c(
    "⁰"="0","¹"="1","²"="2","³"="3","⁴"="4","⁵"="5","⁶"="6","⁷"="7","⁸"="8","⁹"="9",
    "⁺"="+","⁻"="-","⁼"="=","ⁿ"="n"
  )
  sub_map <- c(
    "₀"="0","₁"="1","₂"="2","₃"="3","₄"="4","₅"="5","₆"="6","₇"="7","₈"="8","₉"="9",
    "₊"="+","₋"="-","₌"="=",
    "ₐ"="a","ₑ"="e","ₒ"="o","ₓ"="x","ₕ"="h","ₖ"="k","ₗ"="l","ₘ"="m","ₙ"="n","ₚ"="p","ₛ"="s","ₜ"="t"
  )
  # Domain‑specific
  domain <- c(
    "HCO₃"="HCO3","CO₂"="CO2",
    "m²"="m^2","cm²"="cm^2",
    "kg·m⁻²"="kg*m^-2","kg/m²"="kg/m^2"
  )
  repl <- c(repl_ascii, sup_map, sub_map, domain)
  # Extra symbols causing CRAN non-ASCII warnings
  extra <- c(
    "\u21EA" = "^",      # ⇪
    "\u2714" = "v",      # ✔︎
    "\u2190" = "<-",     # ←
    "\u21C2" = "v",      # ⇂
    "\u21AA" = "->",     # ↪
    "\u21D2" = "=>"      # ⇒
  )
  repl <- c(repl, extra)
  
  
  # Optionally use \uXXXX escapes (strings/comments OK)
  to_unicode_escape <- function(chars) {
    stats::setNames(
      nm = chars,
      object = vapply(chars, function(ch) {
        code <- utf8ToInt(ch)
        ifelse(code <= 0xFFFF, sprintf("\\u%04X", code), sprintf("\\U%08X", code))
      }, character(1))
    )
  }
  if (use_unicode_escapes) {
    keep_ascii <- c("≥","≤","±","→","…","kg·m⁻²","kg/m²")
    to_escape <- setdiff(names(repl), keep_ascii)
    repl[to_escape] <- to_unicode_escape(names(repl)[names(repl) %in% to_escape])
  }
  
  # Build file set
  patt <- if (length(include_ext)) {
    paste0("\\.(?:", paste0(include_ext, collapse = "|"), ")$")
  } else {
    pattern
  }
  files <- unique(unlist(lapply(paths, function(p) {
    if (dir.exists(p)) list.files(p, patt, full.names = TRUE, recursive = recursive)
    else if (file.exists(p)) normalizePath(p)
    else character(0)
  }), use.names = FALSE))
  
  if (!length(files)) {
    message("No files matched under: ", paste(paths, collapse = ", "))
    return(invisible(character()))
  }
  
  # Build a per‑occurrence report if requested
  report_df <- NULL
  if (report || !is.null(write_report)) {
    # Scan for non‑ASCII occurrences and attach intended replacement
    rows <- list()
    for (f in files) {
      txt <- readLines(f, encoding = "UTF-8", warn = FALSE)
      for (i in seq_along(txt)) {
        line <- txt[[i]]
        m <- gregexpr("[^\\x00-\\x7F]", line, perl = TRUE)[[1]]
        if (m[1] != -1) {
          chars <- regmatches(line, list(m))[[1]]
          for (j in seq_along(chars)) {
            ch <- chars[[j]]
            repl_val <- if (ch %in% names(repl)) repl[[ch]] else NA_character_
            rows[[length(rows) + 1L]] <- data.frame(
              file = f,
              line = i,
              col  = m[j],
              char = ch,
              codepoint = sprintf("U+%04X", utf8ToInt(ch)),
              replacement = repl_val,
              context = substr(line, max(1, m[j]-20), min(nchar(line), m[j]+20)),
              stringsAsFactors = FALSE
            )
          }
        }
      }
    }
    if (length(rows)) {
      report_df <- do.call(rbind, rows)
      rownames(report_df) <- NULL
      if (!is.null(write_report)) {
        utils::write.csv(report_df, file = write_report, row.names = FALSE)
        message("Wrote report to: ", write_report)
      }
    } else {
      message("No non‑ASCII characters found for reporting.")
      report_df <- data.frame()
    }
  }
  
  changed <- character(0)
  for (f in files) {
    txt <- readLines(f, encoding = "UTF-8", warn = FALSE)
    original <- txt
    for (ch in names(repl)) {
      if (any(grepl(ch, txt, fixed = TRUE, useBytes = TRUE))) {
        txt <- gsub(ch, repl[[ch]], txt, fixed = TRUE, useBytes = TRUE)
      }
    }
    # Normalize stray NBSPs
    txt <- gsub("\u00A0", " ", txt, useBytes = TRUE)
    
    if (!identical(txt, original)) {
      changed <- c(changed, f)
      if (dry_run) {
        message("[DRY RUN] Would fix non‑ASCII in: ", f)
      } else {
        if (backup) writeLines(original, paste0(f, ".bak"), useBytes = TRUE)
        writeLines(txt, f, useBytes = TRUE)
        message("Fixed non‑ASCII in: ", f)
      }
    }
  }
  
  if (dry_run) {
    message("Dry‑run complete. Files that would change: ", length(changed))
  } else {
    message("Fixed non‑ASCII in ", length(changed), " file(s).")
  }
  
  if (report || !is.null(write_report)) {
    return(report_df)
  }
  invisible(changed)
}


# source("tools/fix-nonascii.R")
# fix_non_ascii(paths = ".", recursive = TRUE, dry_run = TRUE, report = TRUE)
# or save a CSV:
# fix_non_ascii(paths = ".", recursive = TRUE, dry_run = TRUE, report = TRUE,
#              write_report = "nonascii-report.csv")


