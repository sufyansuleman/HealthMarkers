bib <- readLines("c:/R_packages/HealthMarkers/inst/REFERENCES.bib", encoding = "UTF-8")

show_entry <- function(key) {
  i <- grep(paste0("^@.*\\{", key, ","), bib)
  if (length(i) == 0) { cat("NOT FOUND:", key, "\n"); return() }
  j <- i + 1
  while (j <= length(bib) && !grepl("^\\}", bib[j])) j <- j + 1
  cat("\n=== ", key, " ===\n", sep = "")
  writeLines(bib[i:j])
}

show_entry("glutathione_redox_review")
show_entry("payne1973")
show_entry("lee2010hsi")
show_entry("kotronen2009nafldlfs")
show_entry("dobiasova2001atherogenic")
show_entry("quanjer2012")
show_entry("sayed2021iage")
show_entry("celli2004bode")
