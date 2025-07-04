# tools/fix-nonascii.R

fix_non_ascii <- function(path = "R") {
  replacements <- c(
    "’" = "'",    # curly apostrophe → straight
    "‘" = "'",    # left single quote
    "“" = '"',    # left double quote
    "”" = '"',    # right double quote
    "–" = "-",    # en-dash
    "—" = "-",    # em-dash
    "→" = "->",   # arrow → ->
    "²" = "^2"    # superscript two → ^2
  )
  
  files <- list.files(path, "\\.R$", full.names = TRUE)
  for (f in files) {
    txt <- readLines(f, encoding = "UTF-8", warn = FALSE)
    for (pat in names(replacements)) {
      txt <- gsub(pat, replacements[pat], txt, fixed = TRUE)
    }
    writeLines(txt, f, useBytes = TRUE)
  }
  message("Done sweeping ", length(files), " files in ‘", path, "’.")
}
