## tools/add_missing_cols_to_sim_data.R
## Adds missing biomarker columns to simulated_hm_data.rds.
## Run once from the package root:  Rscript tools/add_missing_cols_to_sim_data.R

set.seed(2025L)

rds_path <- "inst/extdata/simulated_hm_data.rds"
d <- readRDS(rds_path)

n   <- nrow(d)
cat("Existing: ", nrow(d), "rows x", ncol(d), "cols\n")

## ---- helper: add column only if absent -------------------------------
add_col <- function(df, nm, vals) {
  if (!nm %in% names(df)) {
    df[[nm]] <- vals
    cat("  + added:", nm, "\n")
  } else {
    cat("  ~ already present:", nm, "\n")
  }
  df
}

## ---- Sex indicator (1=male, 2=female) for realistic ranges -----------
is_female <- if ("sex" %in% names(d)) d$sex == 2L else rep(c(TRUE, FALSE), length.out = n)

## ============================================================================
## Inflammatory  (monocytes missing)
## ============================================================================
d <- add_col(d, "monocytes",
             round(runif(n, 0.20, 0.80), 3))   # 10^9 / L

## ============================================================================
## Hormones  (glucagon, GH, progesterone, prolactin, FSH, LH, free_T3 missing)
## ============================================================================
d <- add_col(d, "glucagon",
             round(runif(n, 5, 20), 2))          # pg/mL

d <- add_col(d, "GH",
             round(runif(n, 0.05, 3.0), 3))       # ng/mL  (pulsatile; pop mean)

d <- add_col(d, "progesterone",
             round(ifelse(is_female,
                          runif(n, 0.3, 15.0),    # nmol/L – wide (follicular-luteal-cycle mix)
                          runif(n, 0.3,  1.0)),   # nmol/L – males
                   3))

d <- add_col(d, "prolactin",
             round(runif(n, 5.0, 30.0), 2))       # ng/mL / mIU/L (same numeric range here)

d <- add_col(d, "FSH",
             round(ifelse(is_female,
                          runif(n, 1.5, 20.0),    # IU/L – pre-menopausal range
                          runif(n, 1.5,  9.0)),   # IU/L – males
                   2))

d <- add_col(d, "LH",
             round(ifelse(is_female,
                          runif(n, 1.0, 15.0),    # IU/L – wide cycle-phase range
                          runif(n, 1.5,  8.5)),   # IU/L – males
                   2))

d <- add_col(d, "fT3",                            # free triiodothyronine, pmol/L
             round(runif(n, 3.5, 6.5), 2))

## ============================================================================
## Renal  (urea_serum missing – distinct from BUN)
## ============================================================================
d <- add_col(d, "urea_serum",                     # mmol/L (serum urea)
             round(runif(n, 3.5, 8.0), 2))

## ============================================================================
## Saliva cortisol names that autofill expects (aliases of cort1/2/3)
## - The pattern dict update makes cort1/2/3 resolve to saliva_cort1/2/3 keys,
##   but adding explicit names is belt-and-suspenders for users who pass
##   col_map = list(cort1 = "saliva_cort1", ...) directly.
## ============================================================================
if ("cort1" %in% names(d)) {
  d <- add_col(d, "saliva_cort1", d$cort1)
  d <- add_col(d, "saliva_cort2", if ("cort2" %in% names(d)) d$cort2 else d$cort1)
  d <- add_col(d, "saliva_cort3", if ("cort3" %in% names(d)) d$cort3 else d$cort1)
}

## ============================================================================
## Hormone-markers cortisol time-points (aliases for cort1/cort2)
## The updated pattern dict maps cortisol_0 -> cort1, cortisol_30 -> cort2
## automatically, but keep explicit columns for clarity.
## ============================================================================
if ("cort1" %in% names(d)) {
  d <- add_col(d, "cortisol_0",
               if ("cort1" %in% names(d)) d$cort1 else runif(n, 8, 25))
  d <- add_col(d, "cortisol_30",
               if ("cort2" %in% names(d)) d$cort2 else d$cortisol_0 * 1.5)
}

## ============================================================================
## Summary
## ============================================================================
cat("\nFinal: ", nrow(d), "rows x", ncol(d), "cols\n")
cat("New col count delta:", ncol(d) - nrow(readRDS(rds_path) |> as.data.frame() |> (function(x) x[1,])()), "\n")

saveRDS(d, rds_path)
cat("Saved to", rds_path, "\n")
