test_dir <- "C:/R_packages/HealthMarkers/tests/testthat"
files <- list.files(test_dir, pattern = "^test-.*\\.R$", full.names = TRUE)

results <- data.frame(
  file         = character(),
  blocks_found = integer(),
  skips_added  = integer(),
  stringsAsFactors = FALSE
)

for (f in files) {
  lines <- readLines(f, warn = FALSE)

  # Lines where a test_that block starts (at column 1)
  block_lines <- which(grepl("^test_that\\(", lines))

  if (length(block_lines) <= 1L) {
    results <- rbind(results, data.frame(
      file = basename(f), blocks_found = length(block_lines), skips_added = 0L
    ))
    next
  }

  # All block-opener lines EXCEPT the first get skip_on_cran() inserted after them.
  # Process bottom-to-top so line numbers remain valid after each insertion.
  insert_after <- sort(block_lines[-1L], decreasing = TRUE)

  for (ln in insert_after) {
    lines <- c(lines[seq_len(ln)], "  skip_on_cran()", lines[seq(ln + 1L, length(lines))])
  }

  writeLines(lines, f)

  results <- rbind(results, data.frame(
    file = basename(f), blocks_found = length(block_lines), skips_added = length(insert_after)
  ))
}

print(results, row.names = FALSE)
cat("\nTotal files modified:", sum(results$skips_added > 0), "\n")
cat("Total skip_on_cran() inserted:", sum(results$skips_added), "\n")
