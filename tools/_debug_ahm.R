library(HealthMarkers)
df <- readRDS("inst/extdata/simulated_hm_data.rds")
col_map164 <- hm_col_report(df, verbose=FALSE)

result <- list()
result$cm_len <- length(col_map164)

withCallingHandlers(
  fasting_is(df, col_map=col_map164, na_action="keep", verbose=FALSE),
  error = function(e) {
    result$error <<- conditionMessage(e)
    calls <- sys.calls()
    result$calls <<- vapply(seq_along(calls), function(i) sprintf("[%d] %s", i, deparse(calls[[i]])[1]), character(1))
  }
) |> tryCatch(error=function(e) result$caught <<- conditionMessage(e))

r2 <- tryCatch(
  fasting_is(df, col_map=NULL, na_action="keep", verbose=FALSE),
  error = function(e) NULL
)
result$null_cm_ok <- !is.null(r2)
if (!is.null(r2)) result$null_cm_cols <- ncol(r2)

saveRDS(result, "tools/dbg_result.rds")
message("DEBUG DONE")


# Reproduce exactly as all_health_markers does
col_map <- hm_col_report(df, verbose = FALSE)
cat("col_map entries:", length(col_map), "\n")

gp  <- HealthMarkers:::.hm_global_precompute(df, col_map, FALSE)
out <- gp$data
cat("out after precompute:", ncol(out), "\n")

# Run insulin panel (with col_map like all_health_markers does)
ins <- tryCatch(
  suppressWarnings(HealthMarkers:::all_insulin_indices(
    out, col_map = col_map, mode = "both", verbose = FALSE, na_action = "keep")),
  error = function(e) { cat("insulin error:", conditionMessage(e), "\n"); NULL }
)
cat("insulin result:", if (is.null(ins)) "NULL" else paste(ncol(ins), "cols"), "\n")
if (!is.null(ins) && is.data.frame(ins)) {
  keep <- setdiff(names(ins), names(out))
  if (length(keep)) out <- cbind(out, ins[keep])
}
cat("out after insulin:", ncol(out), "\n")

# Now try lipid exactly as .hm_safe_call would
data2 <- HealthMarkers:::.hm_prepare_for_group(out, "lipid", col_map)
cat("data2 cols:", ncol(data2), "\n")

tryCatch(
  { res <- lipid_markers(data2, verbose = TRUE, na_action = "keep"); cat("lipid OK:", ncol(res), "\n") },
  error = function(e) { cat("LIPID ERROR:", conditionMessage(e), "\n"); traceback() }
)

# Also try liver
data3 <- HealthMarkers:::.hm_prepare_for_group(out, "liver", col_map)
tryCatch(
  { res3 <- liver_markers(data3, verbose = TRUE, na_action = "keep"); cat("liver OK:", ncol(res3), "\n") },
  error = function(e) { cat("LIVER ERROR:", conditionMessage(e), "\n") }
)
