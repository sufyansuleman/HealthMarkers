sink("tools/_tmp_groups_out.txt", split=TRUE)
devtools::load_all(quiet=TRUE)
d <- readRDS("inst/extdata/simulated_hm_data.rds")

# Test each group individually
reg <- HealthMarkers:::.hm_marker_registry(verbose=FALSE)
all_groups <- names(reg)
cat("All registry groups:\n")
cat(paste(all_groups, collapse="\n"), "\n\n")

for (g in all_groups) {
  tryCatch({
    r <- suppressWarnings(all_health_markers(d, which=g, include_insulin=FALSE, verbose=FALSE))
    new_n <- ncol(r) - ncol(d)
    if (new_n > 0) {
      new_names <- setdiff(names(r), names(d))
      cat(sprintf("  OK  %-25s +%d: %s\n", g, new_n, paste(new_names, collapse=", ")))
    } else {
      cat(sprintf("  OK* %-25s (no new cols -- all NA or already present)\n", g))
    }
  }, error=function(e) {
    cat(sprintf(" ERR  %-25s %s\n", g, conditionMessage(e)))
  })
}

cat("\n=== DONE ===\n")
sink()
