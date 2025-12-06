# tools/fix-nonascii.R
# Comprehensive non-ASCII fixer for R packages

fix_non_ascii <- function(paths = "R",
                          pattern = "\\.(R|Rmd)$",
                          recursive = TRUE,
                          dry_run = FALSE,
                          backup = TRUE) {
  
  # Comprehensive replacement map - includes ALL common non-ASCII in scientific text
  repl <- c(
    # Quotes (use Unicode escapes to avoid parsing issues)
    "\u2018" = "'",  # left single quote '
    "\u2019" = "'",  # right single quote '
    "\u201C" = "\"", # left double quote "
    "\u201D" = "\"", # right double quote "
    "\u201A" = ",",  # single low quote ‚
    "\u201E" = "\"", # double low quote „
    
    # Dashes (MOST COMMON ISSUE)
    "\u2013" = "-",  # en-dash –
    "\u2014" = "-",  # em-dash —
    "\u2212" = "-",  # minus sign −
    "\u2011" = "-",  # non-breaking hyphen ‑
    
    # Math symbols
    "\u00B1" = "+/-", # plus-minus ±
    "\u00D7" = "*",   # multiplication ×
    "\u00F7" = "/",   # division ÷
    "\u2248" = "~=",  # approximately equal ≈
    "\u2260" = "!=",  # not equal ≠
    "\u2264" = "<=",  # less than or equal ≤
    "\u2265" = ">=",  # greater than or equal ≥
    "\u221E" = "Inf", # infinity ∞
    "\u2261" = "==",  # identical to ≡
    
    # Greek letters (COMMON IN STATS)
    "\u03B1" = "alpha", "\u03B2" = "beta", "\u03B3" = "gamma", "\u03B4" = "delta",
    "\u03B5" = "epsilon", "\u03B8" = "theta", "\u03BB" = "lambda", "\u03BC" = "mu",
    "\u00B5" = "mu",   # micro sign µ (NOT GREEK MU!)
    "\u03C0" = "pi", "\u03C1" = "rho", "\u03C3" = "sigma", "\u03C4" = "tau",
    "\u03C6" = "phi", "\u03C7" = "chi", "\u03C9" = "omega",
    
    # Arrows and bullets
    "\u2192" = "->", "\u2190" = "<-", "\u21D2" = "=>", "\u21D4" = "<=>",
    "\u2022" = "-", "\u00B7" = "*", "\u2026" = "...",
    
    # Spaces
    "\u00A0" = " ",  # non-breaking space
    "\u2009" = " ",  # thin space
    
    # Degree and other symbols
    "\u00B0" = " deg", "\u2122" = "(TM)", "\u00AE" = "(R)", "\u00A9" = "(C)",
    
    # Superscripts
    "\u2070" = "0", "\u00B9" = "1", "\u00B2" = "2", "\u00B3" = "3", "\u2074" = "4",
    "\u2075" = "5", "\u2076" = "6", "\u2077" = "7", "\u2078" = "8", "\u2079" = "9",
    "\u207A" = "+", "\u207B" = "-", "\u207C" = "=", "\u207F" = "n",
    
    # Subscripts
    "\u2080" = "0", "\u2081" = "1", "\u2082" = "2", "\u2083" = "3", "\u2084" = "4",
    "\u2085" = "5", "\u2086" = "6", "\u2087" = "7", "\u2088" = "8", "\u2089" = "9",
    "\u208A" = "+", "\u208B" = "-", "\u208C" = "=",
    
    # Accented characters (for names in references)
    "\u00E9" = "e", "\u00E8" = "e", "\u00EA" = "e", "\u00EB" = "e",
    "\u00E1" = "a", "\u00E0" = "a", "\u00E2" = "a", "\u00E4" = "a",
    "\u00ED" = "i", "\u00EC" = "i", "\u00EE" = "i", "\u00EF" = "i",
    "\u00F3" = "o", "\u00F2" = "o", "\u00F4" = "o", "\u00F6" = "o", "\u00F8" = "o",
    "\u00FA" = "u", "\u00F9" = "u", "\u00FB" = "u", "\u00FC" = "u",
    "\u00F1" = "n", "\u00E7" = "c", "\u00DF" = "ss",
    "\u00C9" = "E", "\u00D8" = "O"
  )
  
  # Build file list
  files <- character()
  for (p in paths) {
    if (dir.exists(p)) {
      files <- c(files, list.files(p, pattern, full.names = TRUE, recursive = recursive))
    } else if (file.exists(p)) {
      files <- c(files, normalizePath(p))
    }
  }
  files <- unique(files)
  
  if (!length(files)) {
    message("No files found matching pattern in: ", paste(paths, collapse = ", "))
    return(invisible(character()))
  }
  
  changed <- character()
  
  for (f in files) {
    # Read with UTF-8 encoding
    txt <- readLines(f, encoding = "UTF-8", warn = FALSE)
    original <- txt
    
    # Apply all replacements
    for (ch in names(repl)) {
      txt <- gsub(ch, repl[[ch]], txt, fixed = TRUE)
    }
    
    # Check if anything changed
    if (!identical(txt, original)) {
      changed <- c(changed, f)
      
      if (dry_run) {
        message("[DRY RUN] Would fix: ", basename(f))
        # Show first few changes
        n_show <- 0
        for (i in seq_along(txt)) {
          if (txt[i] != original[i] && n_show < 3) {
            message("  Line ", i, ":")
            message("    OLD: ", substr(original[i], 1, 80))
            message("    NEW: ", substr(txt[i], 1, 80))
            n_show <- n_show + 1
          }
        }
      } else {
        # Backup original
        if (backup) {
          writeLines(original, paste0(f, ".bak"), useBytes = TRUE)
        }
        # Write fixed version
        writeLines(txt, f, useBytes = TRUE)
        message("Fixed: ", basename(f))
      }
    }
  }
  
  if (!dry_run && length(changed) > 0) {
    message("\n✓ Fixed ", length(changed), " file(s)")
  } else if (dry_run && length(changed) > 0) {
    message("\n[DRY RUN] Would fix ", length(changed), " file(s)")
  } else {
    message("\n✓ No non-ASCII characters found")
  }
  
  invisible(changed)
}

# Helper: scan for any remaining non-ASCII
check_non_ascii <- function(paths = "R", pattern = "\\.(R|Rmd)$") {
  files <- list.files(paths, pattern, full.names = TRUE, recursive = TRUE)
  
  problems <- list()
  for (f in files) {
    txt <- readLines(f, encoding = "UTF-8", warn = FALSE)
    for (i in seq_along(txt)) {
      # Find all non-ASCII characters
      m <- gregexpr("[^\x01-\x7F]", txt[i], perl = TRUE)[[1]]
      if (m[1] != -1) {
        chars <- substring(txt[i], m, m)
        for (ch in unique(chars)) {
          problems[[length(problems) + 1]] <- list(
            file = basename(f),
            line = i,
            char = ch,
            code = sprintf("U+%04X", utf8ToInt(ch)),
            context = substr(txt[i], max(1, min(m)-15), min(nchar(txt[i]), max(m)+15))
          )
        }
      }
    }
  }
  
  if (length(problems) > 0) {
    message("Found ", length(problems), " non-ASCII characters:")
    for (p in problems) {
      message(sprintf("  %s:%d  '%s' (%s)  %s",
                      p$file, p$line, p$char, p$code, p$context))
    }
  } else {
    message("✓ No non-ASCII characters found!")
  }
  
  invisible(problems)
}



# Source the script
source("tools/fix-nonascii.R")

# Test with dry run first
fix_non_ascii(paths = "R", dry_run = TRUE, backup = FALSE)

# If looks good, run for real
fix_non_ascii(paths = "R", dry_run = FALSE, backup = FALSE)

# Verify all fixed
check_non_ascii(paths = "R")
