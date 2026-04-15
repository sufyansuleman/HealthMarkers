devtools::load_all(quiet=TRUE)
df <- readRDS("inst/extdata/simulated_hm_data.rds")
df[["bmd_t"]] <- 0

# Test 1: do.call deparse fix
cat("Test 1: data_name with do.call...\n")
result1 <- do.call(lipid_markers, list(df, verbose=FALSE, na_action="keep"))
cat("OK - cols:", ncol(result1), "\n")

# Test 2: hm_inform with vector msg (small vector this time)
cat("Test 2: hm_inform with vector msg...\n")
long_vec <- c("test message line 1", "test message line 2", "test message line 3")
HealthMarkers:::hm_inform(long_vec, level="inform")
cat("OK - hm_inform handled small vector\n")

# Test 3: all_health_markers basic
cat("Test 3: all_health_markers(df, verbose=FALSE)...\n")
r <- tryCatch(
  all_health_markers(df, verbose=FALSE),
  error=function(e){ cat("ERROR:", conditionMessage(e), "\n"); NULL }
)
if (!is.null(r)) {
  cat("OK - result:", nrow(r), "x", ncol(r), "\n")
} else {
  cat("FAILED\n")
}
cat("DONE\n")
