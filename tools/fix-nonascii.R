fix_non_ascii <- function(path = "R", recursive = FALSE) {
  replacements <- c(
    "’" = "'",    "‘" = "'",
    "“" = '"',    "”" = '"',
    "–" = "-",    "—" = "-",
    "→" = "->",   "²" = "^2"
  )
  
  # find all .R and .Rmd files under each path
  files <- unlist(lapply(path, function(p) {
    if (dir.exists(p)) {
      list.files(p, "\\.(R|Rmd)$", full.names = TRUE, recursive = recursive)
    } else if (file.exists(p)) {
      normalizePath(p)
    } else {
      warning("Path not found: ", p); character(0)
    }
  }), use.names = FALSE)
  
  files <- unique(files)
  if (!length(files)) {
    message("No .R or .Rmd files found in: ", paste(path, collapse = ", "))
    return(invisible(NULL))
  }
  
  for (f in files) {
    txt <- readLines(f, encoding = "UTF-8", warn = FALSE)
    for (bad in names(replacements)) {
      txt <- gsub(bad, replacements[bad], txt, fixed = TRUE)
    }
    writeLines(txt, f, useBytes = TRUE)
  }
  message("Fixed non-ASCII in ", length(files), " file(s).")
}
