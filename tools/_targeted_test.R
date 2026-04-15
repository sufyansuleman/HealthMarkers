devtools::load_all(quiet=TRUE)
df <- readRDS("inst/extdata/simulated_hm_data.rds")
df[["bmd_t"]] <- 0

# Bug 1: all_insulin_indices() — was crashing without col_map default
cat("Bug 1: all_insulin_indices(df, mode='both', verbose=FALSE)...\n")
r1 <- tryCatch(all_insulin_indices(df, mode="both", verbose=FALSE),
  error=function(e) { cat("FAIL:", conditionMessage(e), "\n"); NULL })
cat(if(!is.null(r1)) paste("OK - cols:", ncol(r1)) else "FAILED", "\n\n")

# Bug 2: all_health_markers with only a few groups (to test deparse fix)
cat("Bug 2: all_health_markers(df, verbose=FALSE, which=c('lipid','liver','psych'))...\n")
r2 <- tryCatch(all_health_markers(df, verbose=FALSE, include_insulin=FALSE, which=c("lipid","liver","psych")),
  error=function(e) { cat("FAIL:", conditionMessage(e), "\n"); NULL })
cat(if(!is.null(r2)) paste("OK - cols:", ncol(r2)) else "FAILED", "\n\n")

# Bug 3: psych_markers alone
cat("Bug 3: psych_markers(df, verbose=FALSE) without bis/spq keys...\n")
r3 <- tryCatch(psych_markers(df, verbose=FALSE),
  error=function(e) { cat("FAIL:", conditionMessage(e), "\n"); NULL })
cat(if(!is.null(r3)) paste("OK - cols:", ncol(r3)) else "FAILED", "\n\n")

cat("ALL TARGETED TESTS DONE\n")
