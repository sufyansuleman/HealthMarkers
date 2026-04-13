devtools::load_all(quiet=TRUE)
d <- readRDS("inst/extdata/simulated_hm_data.rds")

reg <- HealthMarkers:::.hm_marker_registry(verbose=FALSE)
all_groups <- setdiff(names(reg), c("atherogenic_indices","cvd_risk"))

results <- data.frame(group=character(), status=character(), ncols=integer(), cols=character(), stringsAsFactors=FALSE)

for (g in all_groups) {
  tryCatch({
    r <- suppressWarnings(all_health_markers(d, which=g, include_insulin=FALSE, verbose=FALSE))
    new_n <- ncol(r) - ncol(d)
    new_names <- setdiff(names(r), names(d))
    results <- rbind(results, data.frame(
      group=g, status="OK", ncols=new_n,
      cols=paste(new_names, collapse="|"),
      stringsAsFactors=FALSE))
  }, error=function(e) {
    results <<- rbind(results, data.frame(
      group=g, status=paste("ERR:", conditionMessage(e)), ncols=0, cols="",
      stringsAsFactors=FALSE))
  })
}

write.csv(results, "tools/_tmp_results.csv", row.names=FALSE)
cat("Done. Results written to tools/_tmp_results.csv\n")
