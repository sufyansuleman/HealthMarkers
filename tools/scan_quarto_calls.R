lib <- .libPaths()[1]
files <- list.files(lib, recursive = TRUE, full.names = TRUE)
files <- files[grepl("\\.R$|\\.r$", files)]
hits <- character()
for (f in files) {
  txt <- tryCatch(readLines(f, warn = FALSE), error = function(e) NULL)
  if (is.null(txt)) next
  if (any(grepl('system2\\(\\"quarto\\"', txt))) {
    hits <- c(hits, f)
  }
}
cat(paste(hits, collapse = "\n"))
