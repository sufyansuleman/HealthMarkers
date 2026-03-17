bib_path <- "c:/R_packages/HealthMarkers/inst/REFERENCES.bib"
bib <- readLines(bib_path, encoding = "UTF-8")
idx <- grep("3.*95-yr", bib)
cat("Line", idx, ":\n")
cat(bib[idx], "\n")
chars <- utf8ToInt(bib[idx])
cat("Unicode codepoints around '3':\n")
pos3 <- which(chars == 51L)  # '3' is 0x33
for (p in pos3) {
  window <- chars[max(1, p-1):min(length(chars), p+5)]
  cat(paste(sprintf("U+%04X", window), collapse=" "), "\n")
}
