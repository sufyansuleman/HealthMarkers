devtools::load_all(quiet=TRUE)
df <- readRDS("inst/extdata/simulated_hm_data.rds")
df[["bmd_t"]] <- 0

col_map164 <- hm_col_report(df, verbose=FALSE, fuzzy=FALSE)
cat("col_map len:", length(col_map164), "\n")

gp <- HealthMarkers:::.hm_global_precompute(df, col_map164, FALSE)
out <- gp$data
cat("after global_precompute: ncol=", ncol(out), "\n")

ins <- HealthMarkers:::.hm_safe_call(
  all_insulin_indices, out, col_map164, TRUE, FALSE, "ip",
  list(normalize="none", mode="both", verbose=FALSE, na_action="keep")
)
if (is.data.frame(ins)) out <- HealthMarkers:::.hm_bind_new_cols(out, ins)
cat("after insulin_panel: ncol=", ncol(out), "\n")
cat("sum(nchar(names(out))):", sum(nchar(names(out))), "\n")

cat("Calling lipid_markers on augmented out...\n")
r <- withCallingHandlers(
  tryCatch(
    lipid_markers(out, verbose=FALSE, na_action="keep"),
    error = function(e) {
      cat("CAUGHT ERROR:", conditionMessage(e), "\n")
      NULL
    }
  ),
  error = function(e) {
    cat("CONDITION:", conditionMessage(e), "\n")
    calls <- sys.calls()
    cat("CALL STACK:\n")
    for (i in seq_along(calls)) cat(sprintf("  [%d] %s\n", i, deparse(calls[[i]])[1]))
  }
)
cat("Result:", if(is.null(r)) "NULL" else paste(ncol(r), "cols"), "\n")
