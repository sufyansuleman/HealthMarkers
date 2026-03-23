## ==========================================================================
## HealthMarkers v0.1.1 — user-level integration test
##
## HOW TO INSTALL AND RUN AS A USER:
##
##   # Option A — from the built tarball:
##   install.packages("C:/R_packages/HealthMarkers_0.1.1.tar.gz",
##                    repos = NULL, type = "source")
##
##   # Option B — from inside the project (devtools):
##   devtools::install(quick = TRUE, upgrade = "never")
##
##   # Then run this file in a fresh R session:
##   source("tools/user_test.R")
## ==========================================================================

library(HealthMarkers)

cat("\n", strrep("=", 60), "\n")
cat("HealthMarkers", as.character(packageVersion("HealthMarkers")), "-- user test\n")
cat(strrep("=", 60), "\n")

## --------------------------------------------------------------------------
## Synthetic 5-patient dataset
## --------------------------------------------------------------------------
pts <- data.frame(
  ## Demographics
  age    = c(52, 67, 41, 73, 58),
  sex    = c(1, 0, 1, 0, 1),         # 1=M, 0=F for ogtt numeric model
  sex_c  = c("M","F","M","F","M"),    # character form for other functions
  ## Fasting glucose + insulin
  G0     = c(5.8, 7.2, 4.9, 6.5, 5.1),
  I0     = c(14.2, 22.1, 9.8, 18.4, 11.0),
  ## OGTT samples
  G30    = c(8.1, 10.5, 7.0, 9.8, 7.5),
  G60    = c(9.4, 12.1, 7.8, 11.0, 8.2),
  G120   = c(7.6,  9.3, 6.2,  8.7, 6.8),
  I30    = c(55,   88,  40,   72,  48),
  I60    = c(70,  110,  52,   95,  60),
  I120   = c(45,   65,  30,   58,  38),
  ## Anthropometry
  weight = c(95, 78, 88, 70, 102),
  height = c(178, 162, 182, 158, 183),
  BMI    = c(30.0, 29.7, 26.6, 28.1, 30.5),
  bmi    = c(30.0, 29.7, 26.6, 28.1, 30.5),  # lowercase key for adipo_is/ogtt_is
  WC     = c(98, 88, 92, 86, 105),
  hip    = c(104, 98, 100, 96, 110),
  ## FFA (required by adipo_is)
  FFA    = c(0.55, 0.48, 0.42, 0.60, 0.70),
  ## Lipids
  TC     = c(5.6, 6.2, 4.8, 5.9, 5.3),
  LDL_c  = c(3.4, 3.8, 2.9, 3.6, 3.1),
  HDL_c  = c(1.1, 1.3, 1.5, 1.2, 1.4),
  TG     = c(2.1, 2.8, 1.4, 2.5, 1.8),
  ApoB   = c(1.0, 1.2, 0.85, 1.1, 0.95),
  ## Liver
  ALT    = c(38, 52, 25, 44, 60),
  AST    = c(30, 40, 20, 35, 48),
  GGT    = c(42, 60, 22, 50, 75),
  ## Renal
  creatinine       = c(88, 72, 95, 65, 102),
  eGFR             = c(74, 80, 68, 72, 62),
  UACR             = c(18, 35, 12, 48, 22),
  urine_albumin    = c(3.2, 6.3, 2.1, 8.6, 4.0),
  urine_creatinine = c(9.8, 10.2, 8.4, 10.5, 9.1),
  ## Corrected calcium: keys are "calcium" and "albumin" (not Ca/Alb)
  calcium = c(2.38, 2.51, 2.29, 2.45, 2.33),
  albumin = c(42, 38, 44, 36, 41),
  ## Blood count (inflammatory): keys are neutrophils/lymphocytes/monocytes/platelets
  NEUT  = c(4.8, 6.2, 3.9, 5.5, 7.0),
  LYMPH = c(2.1, 1.6, 2.4, 1.8, 1.5),
  MONO  = c(0.58, 0.72, 0.50, 0.65, 0.80),
  PLT   = c(230, 198, 260, 215, 185),
  ## Vitamin D
  VitD  = c(48, 22, 61, 18, 35),
  ## Neurofilament light (NfL)
  NfL   = c(8.2, 15.4, 6.9, 22.3, 11.1),
  ## Blood pressure
  SBP   = c(138, 145, 122, 152, 130),
  DBP   = c(88,  90,  78,  92,  82)
)

## --------------------------------------------------------------------------
## 1. Column inference (infer_cols)
## --------------------------------------------------------------------------
cat("\n--- 1. infer_cols() ---\n")
spec <- list(G0=NULL, I0=NULL, G30=NULL, G120=NULL,
             TC=NULL, LDL_c=NULL, HDL_c=NULL, TG=NULL,
             BMI=NULL, WC=NULL, weight=NULL,
             ALT=NULL, AST=NULL, GGT=NULL,
             eGFR=NULL, UACR=NULL, creatinine=NULL,
             urine_albumin=NULL, urine_creatinine=NULL,
             age=NULL, sex=NULL)
col_map <- infer_cols(pts, map=spec, strategy="first",
                      strict=FALSE, verbose=TRUE)
resolved <- sum(!sapply(col_map, is.null))
cat("Resolved", resolved, "out of", length(spec), "keys.\n")

## --------------------------------------------------------------------------
## 2. all_health_markers() dispatcher
## --------------------------------------------------------------------------
cat("\n--- 2. all_health_markers() dispatcher (verbose = TRUE) ---\n")
options(healthmarkers.verbose = TRUE)

res <- all_health_markers(
  data  = pts,
  which = c("insulin_fasting", "glycemic", "lipid", "atherogenic",
            "liver", "liver_fat", "renal", "ckd_stage", "urine",
            "inflammatory", "vitamin_d_status", "calcium_corrected",
            "obesity_metrics"),
  col_map = list(
    G0="G0", I0="I0",
    TC="TC", LDL_c="LDL_c", HDL_c="HDL_c", TG="TG",
    BMI="BMI", WC="WC", weight="weight", height="height",
    ALT="ALT", AST="AST", GGT="GGT",
    age="age", sex="sex_c",
    eGFR="eGFR", UACR="UACR", creatinine="creatinine",
    urine_albumin="urine_albumin", urine_creatinine="urine_creatinine",
    calcium="calcium", albumin="albumin",
    neutrophils="NEUT", lymphocytes="LYMPH",
    monocytes="MONO", platelets="PLT",
    vitamin_d="VitD"
  ),
  verbose = TRUE
)
new_cols <- setdiff(names(res), names(pts))
cat("\nNew marker columns:", length(new_cols),
    "| Total columns:", ncol(res), "| Rows:", nrow(res), "\n")
cat("New columns:\n")
cat(paste(" ", sort(new_cols), collapse = "\n"), "\n")
cat("\nResults (new columns only):\n")
print(res[, new_cols, drop = FALSE])

## --------------------------------------------------------------------------
## 3. fasting_is() — output columns: HOMA_IR_inv, QUICKI, FIRI, ...
## --------------------------------------------------------------------------
cat("\n--- 3. fasting_is() ---\n")
fi <- fasting_is(pts, col_map=list(G0="G0", I0="I0"), verbose=TRUE)
new_fi <- setdiff(names(fi), names(pts))
cat("  Columns added (", length(new_fi), "):", paste(new_fi, collapse = ", "), "\n")
print(fi[, new_fi, drop = FALSE])

## --------------------------------------------------------------------------
## 4. ogtt_is() — needs weight, bmi (lowercase), age, sex; Matsuda_ISI
## --------------------------------------------------------------------------
cat("\n--- 4. ogtt_is() ---\n")
oi <- ogtt_is(
  pts,
  col_map = list(
    G0="G0", I0="I0", G30="G30", G60="G60", G120="G120",
    I30="I30", I60="I60", I120="I120",
    weight="weight", bmi="bmi", age="age", sex="sex"
  ),
  verbose = TRUE
)
new_oi <- setdiff(names(oi), names(pts))
cat("  Columns added (", length(new_oi), "):", paste(new_oi, collapse = ", "), "\n")
print(oi[, new_oi, drop = FALSE])

## --------------------------------------------------------------------------
## 5. adipo_is() — keys: waist (not WC), bmi (lowercase), FFA required
## --------------------------------------------------------------------------
cat("\n--- 5. adipo_is() ---\n")
ai <- adipo_is(
  pts,
  col_map = list(
    G0="G0", I0="I0", TG="TG", HDL_c="HDL_c",
    FFA="FFA", waist="WC", bmi="bmi"
  ),
  verbose = TRUE
)
new_ai <- setdiff(names(ai), names(pts))
cat("  Columns added (", length(new_ai), "):", paste(new_ai, collapse = ", "), "\n")
print(ai[, new_ai, drop = FALSE])

## --------------------------------------------------------------------------
## 6. atherogenic_indices()
## --------------------------------------------------------------------------
cat("\n--- 6. atherogenic_indices() ---\n")
ath <- atherogenic_indices(
  pts,
  col_map = list(TC="TC", LDL_c="LDL_c", HDL_c="HDL_c", TG="TG"),
  verbose = TRUE
)
new_ath <- setdiff(names(ath), names(pts))
cat("  Columns added (", length(new_ath), "):", paste(new_ath, collapse = ", "), "\n")
print(ath[, new_ath, drop = FALSE])

## --------------------------------------------------------------------------
## 7. cvd_marker_aip() and cvd_marker_ldl_particle_number()
## --------------------------------------------------------------------------
cat("\n--- 7. cvd_marker_aip() ---\n")
aip <- cvd_marker_aip(pts, col_map=list(TG="TG", HDL_c="HDL_c"), verbose=TRUE)
new_aip <- setdiff(names(aip), names(pts))
cat("  Columns added (", length(new_aip), "):", paste(new_aip, collapse = ", "), "\n")
print(aip[, new_aip, drop = FALSE])

cat("\n--- 7b. cvd_marker_ldl_particle_number() ---\n")
ldlp <- cvd_marker_ldl_particle_number(pts, col_map=list(ApoB="ApoB"), verbose=TRUE)
new_ldlp <- setdiff(names(ldlp), names(pts))
cat("  Columns added (", length(new_ldlp), "):", paste(new_ldlp, collapse = ", "), "\n")
print(ldlp[, new_ldlp, drop = FALSE])

## --------------------------------------------------------------------------
## 8. liver_fat_markers() (new function; output: HSI, NAFLD_LFS)
## --------------------------------------------------------------------------
cat("\n--- 8. liver_fat_markers() ---\n")
lf <- liver_fat_markers(
  pts,
  col_map = list(BMI="BMI", WC="WC", TG="TG", GGT="GGT",
                 ALT="ALT", AST="AST", age="age", sex="sex_c",
                 I0="I0"),  # I0 needed to derive insulin for NAFLD_LFS
  verbose = TRUE
)
new_lf <- setdiff(names(lf), names(pts))
cat("  Columns added (", length(new_lf), "):", paste(new_lf, collapse = ", "), "\n")
print(lf[, new_lf, drop = FALSE])

## --------------------------------------------------------------------------
## 9. ckd_stage()
## --------------------------------------------------------------------------
cat("\n--- 9. ckd_stage() ---\n")
ckd <- ckd_stage(pts, col_map=list(eGFR="eGFR", UACR="UACR"), verbose=TRUE)
new_ckd <- setdiff(names(ckd), names(pts))
cat("  Columns added (", length(new_ckd), "):", paste(new_ckd, collapse = ", "), "\n")
print(ckd[, new_ckd, drop = FALSE])

## --------------------------------------------------------------------------
## 10. corrected_calcium() — keys: calcium / albumin (lowercase)
## --------------------------------------------------------------------------
cat("\n--- 10. corrected_calcium() ---\n")
cc <- corrected_calcium(
  pts,
  col_map = list(calcium="calcium", albumin="albumin"),
  verbose = TRUE
)
new_cc <- setdiff(names(cc), names(pts))
cat("  Columns added (", length(new_cc), "):", paste(new_cc, collapse = ", "), "\n")
print(cc[, new_cc, drop = FALSE])

## --------------------------------------------------------------------------
## 11. inflammatory_markers() — keys: neutrophils/lymphocytes/monocytes/platelets
## --------------------------------------------------------------------------
cat("\n--- 11. inflammatory_markers() ---\n")
infl <- inflammatory_markers(
  pts,
  col_map = list(neutrophils="NEUT", lymphocytes="LYMPH",
                 monocytes="MONO", platelets="PLT"),
  verbose = TRUE
)
new_infl <- setdiff(names(infl), names(pts))
cat("  Columns added (", length(new_infl), "):", paste(new_infl, collapse = ", "), "\n")
print(infl[, new_infl, drop = FALSE])

## --------------------------------------------------------------------------
## 12. vitamin_d_status()
## --------------------------------------------------------------------------
cat("\n--- 12. vitamin_d_status() ---\n")
vd <- vitamin_d_status(pts, col_map=list(vitamin_d="VitD"), verbose=TRUE)
new_vd <- setdiff(names(vd), names(pts))
cat("  Columns added (", length(new_vd), "):", paste(new_vd, collapse = ", "), "\n")
print(vd[, new_vd, drop = FALSE])

## --------------------------------------------------------------------------
## 13. nfl_marker() (new function; key: nfl; output: nfl_value)
## --------------------------------------------------------------------------
cat("\n--- 13. nfl_marker() ---\n")
nfl <- nfl_marker(pts, col_map=list(nfl="NfL"), verbose=TRUE)
new_nfl <- setdiff(names(nfl), names(pts))
cat("  Columns added (", length(new_nfl), "):", paste(new_nfl, collapse = ", "), "\n")
print(nfl[, new_nfl, drop = FALSE])

## --------------------------------------------------------------------------
## 14. impute_missing() (new function)
## --------------------------------------------------------------------------
cat("\n--- 14. impute_missing() ---\n")
pts_na <- pts[1:4, c("G0","I0","TG","HDL_c","BMI","WC")]
pts_na[2, "TG"]    <- NA
pts_na[4, "HDL_c"] <- NA
cat("Before — NA count:", sum(is.na(pts_na)), "\n")
imp <- impute_missing(pts_na, method="median", verbose=TRUE)
cat("After  — NA count:", sum(is.na(imp)), "\n")
cat("Imputed TG  (row 2):", round(imp$TG[2], 2), "\n")
cat("Imputed HDL (row 4):", round(imp$HDL_c[4], 2), "\n")
cat("  Results (imputed data):\n")
print(imp)

## --------------------------------------------------------------------------
## 15. Global verbose option
## --------------------------------------------------------------------------
cat("\n--- 15. Global verbose option ---\n")
options(healthmarkers.verbose = TRUE)
cat("options(healthmarkers.verbose = TRUE) -- all debug messages now visible:\n")
tmp <- fasting_is(pts[1, ], col_map=list(G0="G0", I0="I0"), verbose=FALSE)
options(healthmarkers.verbose = FALSE)
cat("HOMA-IR (global option restored):", round(tmp$HOMA_IR_inv, 3), "\n")

## --------------------------------------------------------------------------
## 16. calc_sds()
## --------------------------------------------------------------------------
cat("\n--- 16. calc_sds() ---\n")
## calc_sds takes data, vars (column names), and ref (data.frame with variable/mean/sd)
bmi_ref <- data.frame(variable="BMI", mean=25, sd=4)
z <- calc_sds(pts, vars="BMI", ref=bmi_ref, verbose=TRUE)
new_z <- setdiff(names(z), names(pts))
cat("  Columns added (", length(new_z), "):", paste(new_z, collapse = ", "), "\n")
print(z[, new_z, drop = FALSE])

## --------------------------------------------------------------------------
## 17. marker_summary() on dispatcher result
## --------------------------------------------------------------------------
cat("\n--- 17. marker_summary() ---\n")
ms <- marker_summary(res, verbose=TRUE)
cat("Summary rows (numeric markers):", nrow(ms), "\n")
print(ms)

## --------------------------------------------------------------------------
## 18. NA audit on all dispatcher output columns
## --------------------------------------------------------------------------
cat("\n--- 18. NA audit ---\n")
new_cols <- setdiff(names(res), names(pts))
na_counts <- sapply(res[new_cols], function(x) sum(is.na(x)))
cols_with_na <- na_counts[na_counts > 0]
if (length(cols_with_na) == 0) {
  cat("All computed columns: ZERO unexpected NAs. \n")
} else {
  cat("Columns with NAs (expected if inputs were out-of-range or missing):\n")
  print(sort(cols_with_na, decreasing=TRUE))
}

cat("\n", strrep("=", 60), "\n")
cat("ALL USER TESTS COMPLETE\n")
cat(strrep("=", 60), "\n\n")
