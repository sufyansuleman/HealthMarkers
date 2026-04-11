## =============================================================================
## HealthMarkers – Comprehensive User-Perspective Test Script
## =============================================================================
## Covers every exported function group.
## Run block-by-block in an interactive R session (or source() the whole file).
## Lines that require optional packages (PooledCohort, QRISK3, di, rspiro)
## are guarded with requireNamespace() and will skip gracefully if absent.
## =============================================================================

# ── INSTALLATION ──────────────────────────────────────────────────────────────
# Run once, then restart R before continuing.

install.packages("C:/R_packages/HealthMarkers", repos = NULL, type = "source")

library(HealthMarkers)

# ── 0. GLOBAL OPTIONS ────────────────────────────────────────────────────────
# verbose = TRUE on any function call is now sufficient for full output.
# The global option only controls *debug*-level internal messages (off by default).
# Uncomment to enable deep debug logging:
# options(healthmarkers.verbose = "debug")


# ── 1. LOAD & INSPECT REAL DATA ──────────────────────────────────────────────
# Read the real phenotype file (tab-delimited .txt)
dat <- read.table(
  "O:/HE_Sufyan/PhD_work/01_Projects/01_PhD_project/01_Phenotypic_correlations/data-raw/MergedPhenotypes_11mar2021.txt",
  header    = TRUE,
  sep       = "\t",
  stringsAsFactors = FALSE,
  na.strings = c("", "NA", ".", "N/A")
)

cat("Dimensions:", nrow(dat), "rows x", ncol(dat), "cols\n")
cat("Column names:\n"); print(names(dat))
str(dat[, 1:20])                        # first 20 columns
# sex is coded 1/2 in this dataset -- all functions accept 1/2 directly.




# ── 2. MAIN DISPATCHER ───────────────────────────────────────────────────────
# 2a. Run selected groups with explicit col_map
result_full <- all_health_markers(
  dat,
  verbose = TRUE
)


# 2c. Summary of full result
marker_summary(result_full, verbose = TRUE)

# 2d. metabolic_markers() aggregator
met_result <- metabolic_markers(
  dat,
  col_map = col_map,
  which   = c("lipid", "liver", "glycemic"),
  verbose = TRUE
)
dim(met_result)


# ── 3. INSULIN SENSITIVITY INDICES ───────────────────────────────────────────
# 3a. Fasting IS
fasting_result <- fasting_is(
  data    = dat,
  col_map = list(G0 = "pglu0", I0 = "insu0", TG = "trig", HDL_c = "hdlc",
                 BMI = "bmi", waist = "waist"),
  normalize = "none",
  verbose   = TRUE
)
head(fasting_result)

# 3b. Fasting IS with z-score normalisation
fasting_z <- fasting_is(
  data      = dat,
  col_map   = list(G0 = "pglu0", I0 = "insu0"),
  normalize = "z",
  verbose   = FALSE
)
head(fasting_z)


# 3c. OGTT IS
ogtt_result <- ogtt_is(
  data    = dat,
  col_map = list(G0 = "pglu0", G30 = "pglu30", G120 = "pglu120",
                 I0 = "insu0", I30 = "insu30", I120 = "insu120",
                 weight = "weight", bmi = "bmi",
                 age = "age", sex = "sex"),
  normalize = "none",
  verbose   = TRUE
)
head(ogtt_result[, 1:6])

# 3d. Adipose IS
adipo_result <- adipo_is(
  data    = dat,
  col_map = list(G0 = "pglu0", I0 = "insu0", TG = "trig", HDL_c = "hdlc",
                 waist = "waist", bmi = "bmi"),
  verbose = TRUE
)
head(adipo_result)

# 3e. All insulin indices combined (fasting + OGTT + adipose; tracer/DXA not in dataset)
all_ins <- all_insulin_indices(
  data    = dat,
  col_map = list(G0 = "pglu0", I0 = "insu0", G30 = "pglu30", I30 = "insu30",
                 G120 = "pglu120", I120 = "insu120",
                 TG = "trig", HDL_c = "hdlc",
                 waist = "waist", weight = "weight", bmi = "bmi",
                 age = "age", sex = "sex",
                 fat_mass = "fatmass"),
  normalize = "none",
  mode      = "both",
  verbose   = TRUE
)
dim(all_ins)


# ── 4. GLYCEMIC MARKERS ───────────────────────────────────────────────────────
glyc_result <- glycemic_markers(
  data    = dat,
  col_map = list(G0 = "pglu0", I0 = "insu0", HbA1c = "hba1c",
                 G120 = "pglu120", I120 = "insu120",
                 HDL_c = "hdlc", TG = "trig", BMI = "bmi"),
  verbose = TRUE
)
head(glyc_result)


# ── 5. LIPID MARKERS & ATHEROGENIC INDICES ───────────────────────────────────
# 5a. lipid_markers
lipid_result <- lipid_markers(
  data    = dat,
  col_map = list(TC = "chol", HDL_c = "hdlc", TG = "trig",
                 LDL_c = "ldl", ApoB = "APOB", ApoA1 = "APOA1",
                 waist = "waist", BMI = "bmi"),
  verbose = TRUE
)
head(lipid_result)

# 5b. atherogenic_indices
ath_result <- atherogenic_indices(
  data    = dat,
  col_map = list(TG = "trig", HDL_c = "hdlc", TC = "chol", LDL_c = "ldl"),
  verbose = TRUE
)
head(ath_result)

# 5c. AIP marker via cvd_marker_aip
aip_result <- cvd_marker_aip(
  data    = dat,
  col_map = list(TG = "trig", HDL_c = "hdlc"),
  verbose = TRUE
)
head(aip_result)

# 5d. LDL particle number estimate
ldl_pn <- cvd_marker_ldl_particle_number(
  data    = dat,
  col_map = list(ApoB = "APOB"),
  verbose = TRUE
)
head(ldl_pn)


# ── 6. LIVER MARKERS ─────────────────────────────────────────────────────────
# Note: no GGT, AST, or bilirubin in this dataset; those index components will be NA.
liver_result <- liver_markers(
  data    = dat,
  col_map = list(BMI = "bmi", waist = "waist", TG = "trig",
                 age = "age", ALT = "alat",
                 albumin = "alb", creatinine = "crea"),
  verbose = TRUE
)
head(liver_result)

# liver_fat_markers
liverfat_result <- liver_fat_markers(
  data    = dat,
  col_map = list(BMI = "bmi", waist = "waist", TG = "trig",
                 ALT = "alat", albumin = "alb"),
  verbose = TRUE
)
head(liverfat_result)


# ── 7. RENAL MARKERS ─────────────────────────────────────────────────────────
# 7a. renal_markers
# Note: race, BUN, cystatin_C not in this dataset; eGFR will be estimated from creatinine.
renal_result <- renal_markers(
  data    = dat,
  col_map = list(creatinine = "crea", age = "age", sex = "sex"),
  verbose = TRUE
)
head(renal_result)

# 7b. urine_markers
urine_result <- urine_markers(
  data    = dat,
  col_map = list(urine_creat = "ucrea", urine_albumin = "ualb",
                 urine_na = "uNa", urine_k = "uK"),
  verbose = TRUE
)
head(urine_result)

# 7c. ckd_stage – requires eGFR; compute it first from creatinine
# (skip if renal_markers above already produced an eGFR column)
if ("eGFR" %in% names(renal_result)) {
  ckd_result <- ckd_stage(
    data    = renal_result,
    col_map = list(eGFR = "eGFR", UACR = "ualbcrea"),
    verbose = TRUE
  )
  head(ckd_result)
}

# 7d. Kidney Failure Risk (also needs eGFR)
if ("eGFR" %in% names(renal_result)) {
  kfre_result <- kidney_failure_risk(
    data    = renal_result,
    col_map = list(age = "age", sex = "sex",
                   eGFR = "eGFR", UACR = "ualbcrea"),
    verbose = TRUE
  )
  head(kfre_result)
}


# ── 8. INFLAMMATORY MARKERS & iAGE ───────────────────────────────────────────
# 8a. inflammatory_markers
# Note: CBC (neutrophils, lymphocytes, platelets) not in this dataset;
# only CRP and albumin are available -- ratio-based indices will be NA.
inflam_result <- inflammatory_markers(
  data    = dat,
  col_map = list(CRP = "CRP_tethys", albumin = "alb"),
  panel   = "both",
  verbose = TRUE
)
head(inflam_result)

# 8b. iAge – simplified inflammatory age clock (CRP_tethys and IL6 available)
iage_result <- iAge(
  data    = dat,
  col_map = list(CRP = "CRP_tethys", IL6 = "IL6"),
  verbose = TRUE
)
head(iage_result)


# ── 9. OXIDATIVE MARKERS ─────────────────────────────────────────────────────
ox_result <- oxidative_markers(
  data    = dat,
  col_map = list(GSH = "GSH", GSSG = "GSSG"),
  verbose = TRUE
)
head(ox_result)


# ── 10. BONE MARKERS ─────────────────────────────────────────────────────────
bone_result <- bone_markers(
  data    = dat,
  col_map = list(age           = "age",
                 weight        = "weight",
                 height        = "height_m",
                 ALM           = "ALM",
                 FM            = "FM",
                 BMD           = "BMD",
                 BMD_ref_mean  = "BMD_ref_mean",
                 BMD_ref_sd    = "BMD_ref_sd",
                 TBS           = "TBS",
                 PINP          = "PINP",
                 CTX           = "CTX",
                 BSAP          = "BSAP",
                 Osteocalcin   = "Osteocalcin"),
  verbose = TRUE
)
head(bone_result)

# ALM/BMI index
alm_result <- alm_bmi_index(
  data    = dat,
  col_map = list(alm = "ALM", bmi = "BMI", sex = "sex"),
  verbose = TRUE
)
head(alm_result)

# FRAX score
frax_result <- frax_score(
  data    = dat,
  col_map = list(age = "age", sex = "sex",
                 prior_fracture = "prior_fracture",
                 parent_fracture = "parent_fracture",
                 steroids = "steroids",
                 rheumatoid = "rheumatoid",
                 secondary_op = "secondary_op",
                 smoker = "smoker",
                 alcohol = "alcohol",
                 bmd = "bmd"),
  verbose = TRUE
)
head(frax_result)


# ── 11. HORMONE MARKERS ───────────────────────────────────────────────────────
# Only map columns present in the simulated data
# testo, shbg, tsh, ft4, IGF1, SHBG, DHEAS available in this dataset
hormone_result <- hormone_markers(
  data    = dat,
  col_map = list(
    total_testosterone = "testo",
    SHBG               = "shbg",
    TSH                = "tsh",
    free_T4            = "ft4",
    IGF1               = "IGF1",
    DHEAS              = "dhaes"
  ),
  verbose = TRUE
)
head(hormone_result)


# ── 12. VITAMIN MARKERS & VITAMIN D STATUS ───────────────────────────────────
# 12a. vitamin_markers – only columns available in this dataset
vit_result <- vitamin_markers(
  data    = dat,
  col_map = list(
    VitD     = "vitd25",
    B12      = "vitb12",
    Folate   = "folate",
    Ferritin = "ferri",
    TSH      = "tsh",
    free_T4  = "ft4"
  ),
  verbose = TRUE
)
head(vit_result)

# 12b. vitamin_d_status
vitd_status <- vitamin_d_status(
  data    = dat,
  col_map = list(vitd = "vitd25"),
  verbose = TRUE
)
table(vitd_status$vitamin_d_status)

# 12c. nutrient_markers
nutr_result <- nutrient_markers(
  data    = dat,
  col_map = list(
    ferritin        = "ferritin",
    transferrin_sat = "transferrin_sat",
    albumin         = "albumin",
    total_protein   = "total_protein",
    EPA             = "EPA",
    DHA             = "DHA",
    Mg              = "magnesium",
    creatinine      = "creatinine",
    glycated_albumin = "glycated_albumin",
    uric_acid       = "uric_acid",
    BUN             = "BUN",
    phosphate       = "phosphate",
    calcium         = "calcium",
    Na              = "Na",
    K               = "K",
    Cl              = "Cl",
    HCO3            = "HCO3",
    Tyr             = "Tyr",
    Phe             = "Phe"
  ),
  verbose = TRUE
)
head(nutr_result)


# ── 13. CORRECTED CALCIUM ────────────────────────────────────────────────────
ca_result <- corrected_calcium(
  data    = dat,
  col_map = list(calcium = "calcium", albumin = "alb"),
  units   = "auto",
  verbose = TRUE
)
head(ca_result)


# ── 14. PULMONARY & SPIROMETRY MARKERS ───────────────────────────────────────
# 14a. pulmo_markers (requires rspiro)
# sim data has fev1/fvc (lowercase) and sex "M"/"F" (accepted by .pm_map_sex)
if (requireNamespace("rspiro", quietly = TRUE)) {
  pulmo_result <- pulmo_markers(
    data     = dat,
    equation = "GLI",
    verbose  = TRUE
  )
  head(pulmo_result)
} else {
  message("rspiro not installed – skipping pulmo_markers()")
}

# 14b. BODE index (uses FEV1%, 6-min walk, mMRC, BMI)
# FEV1pct, sixmwd, mmrc not pre-computed in this dataset; will return NAs
bode_result <- bode_index(
  data    = dat,
  col_map = list(fev1_pct = "FEV1pct", sixmwd = "sixmwd",
                 mmrc = "mmrc", bmi = "bmi"),
  verbose = TRUE
)
head(bode_result)

# 14c. spirometry_markers passthrough
spiro_result <- spirometry_markers(
  data    = dat,
  col_map = list(
    fev1       = "FEV1",
    fvc        = "FVC",
    fev1_pct   = "FEV1pct",
    age        = "age",
    height     = "height",
    sex        = "sex",
    ethnicity  = "ethnicity"
  )
)
head(spiro_result)


# ── 15. OBESITY INDICES ───────────────────────────────────────────────────────
obsesity_result <- obesity_indices(
  data         = dat,
  weight       = weight,
  height       = height_m,
  waist        = waist,
  hip          = hip,
  sex          = sex,
  weight_unit  = "kg",
  height_unit  = "m",
  include_RFM  = TRUE,
  adjust_WHR   = TRUE,
  verbose      = TRUE
)
head(obsesity_result)


# ── 16. METABOLIC RISK FEATURES (pediatric-style flags) ──────────────────────
# All needed columns now pre-computed in sim data
mrf_result <- metabolic_risk_features(
  data    = dat,
  col_map = list(
    chol_total    = "total_chol",
    chol_ldl      = "LDL_c",
    chol_hdl      = "HDL_c",
    triglycerides = "TG",
    age_year      = "age",
    z_HOMA        = "z_HOMA",
    glucose       = "glucose",
    HbA1c         = "HbA1c_ifcc",
    bp_sys_z      = "bp_sys_z",
    bp_dia_z      = "bp_dia_z"
  ),
  verbose = TRUE
)
head(mrf_result)


# ── 17. METABOLIC SYNDROME SEVERITY SCORE ────────────────────────────────────
# sim data has glucose, bp_sys, bp_dia; race "white"/"black"/"hispanic" maps
# to NHW/NHB/HW automatically via toupper() in metss
mets_result <- metss(
  data    = dat,
  verbose = TRUE
)
head(mets_result)


# ── 18. ALLOSTATIC LOAD ───────────────────────────────────────────────────────
al_result <- allostatic_load(
  data       = dat,
  thresholds = list(sbp = 130, dbp = 85, CRP = 3.0, BMI = 30,
                    waist = 102, TG = 1.7, HDL_c = 1.0),
  na_action  = "keep",
  verbose    = TRUE
)
head(al_result)


# ── 19. FRAILTY INDEX ─────────────────────────────────────────────────────────
if (requireNamespace("di", quietly = TRUE)) {
  deficit_cols <- c("hypertension", "diabetes", "stroke", "dementia",
                    "copd", "cancer", "mi", "chf", "rheum", "hiv",
                    "mild_liver", "sev_liver", "renal")
  fi_result <- frailty_index(
    data     = dat,
    cols     = deficit_cols,
    age      = "age",
    rescale  = TRUE,
    na_action = "keep",
    return   = "data",
    verbose  = TRUE
  )
  head(fi_result)
  plot_frailty_age(dat, cols = deficit_cols, age = "age", bins = 5)
} else {
  message("di not installed – skipping frailty_index()")
}


# ── 20. SARC-F SCORE ─────────────────────────────────────────────────────────
sarc_result <- sarc_f_score(
  data    = dat,
  col_map = list(strength = "Strength", walking = "Walking",
                 chair    = "Chair",    stairs   = "Stairs",
                 falls    = "Falls"),
  verbose     = TRUE,
  check_extreme = TRUE,
  extreme_action = "cap"
)
head(sarc_result)
table(sarc_result$sarc_f_high_risk)


# ── 21. CHARLSON COMORBIDITY INDEX ───────────────────────────────────────────
charlson_result <- charlson_index(
  data    = dat,
  col_map = list(
    mi = "mi", chf = "chf", pvd = "pvd", stroke = "stroke",
    dementia = "dementia", copd = "copd", rheum = "rheum",
    ulcer = "ulcer", mild_liver = "mild_liver", diabetes = "diabetes",
    diab_comp = "diab_comp", hemiplegia = "hemiplegia",
    renal = "renal", cancer = "cancer", leukemia = "leukemia",
    lymphoma = "lymphoma", sev_liver = "sev_liver",
    metastatic_cancer = "metastatic_cancer", hiv = "hiv"
  ),
  verbose = TRUE
)
head(charlson_result)
summary(charlson_result$charlson_index)


# ── 22. CARDIOVASCULAR RISK ───────────────────────────────────────────────────
# 22a. ASCVD (requires PooledCohort)
if (requireNamespace("PooledCohort", quietly = TRUE)) {
  ascvd_result <- cvd_risk_ascvd(dat, year = 10, verbose = TRUE)
  head(ascvd_result)
  stroke_result <- cvd_risk_stroke(dat, verbose = TRUE)
  head(stroke_result)
} else {
  message("PooledCohort not installed – skipping cvd_risk_ascvd(), cvd_risk_stroke()")
}

# 22b. cvd_risk dispatcher
cvd_result <- cvd_risk(dat, model = "AIP",
                       col_map = list(TG = "TG", HDL_c = "HDL_c"))
head(cvd_result)


# ── 23. KYN / TRP RATIO ───────────────────────────────────────────────────────
ktr_result <- kyn_trp_ratio(
  data    = dat,
  col_map = list(kynurenine = "kynurenine", tryptophan = "tryptophan"),
  verbose = TRUE
)
head(ktr_result)


# ── 24. NFL MARKER ────────────────────────────────────────────────────────────
nfl_result <- nfl_marker(
  data    = dat,
  col_map = list(nfl = "nfl"),
  verbose = TRUE,
  check_extreme = TRUE
)
head(nfl_result)


# ── 25. SWEAT MARKERS ────────────────────────────────────────────────────────
sweat_result <- sweat_markers(
  data    = dat,
  col_map = list(
    sweat_chloride    = "sweat_chloride",
    sweat_Na          = "sweat_Na",
    sweat_K           = "sweat_K",
    sweat_lactate     = "sweat_lactate",
    weight_before     = "weight_before",
    weight_after      = "weight_after",
    duration          = "duration",
    body_surface_area = "body_surface_area"
  ),
  verbose = TRUE
)
head(sweat_result)


# ── 26. SALIVA MARKERS ───────────────────────────────────────────────────────
saliva_result <- saliva_markers(
  data    = dat,
  col_map = list(
    amylase  = "amylase",
    glucose  = "saliva_glucose",
    cort1    = "cort1",
    cort2    = "cort2",
    cort3    = "cort3"
  ),
  verbose = TRUE
)
head(saliva_result)


# ── 27. PSYCH SCORES ─────────────────────────────────────────────────────────
# phq/gad/who/k6 item columns are now included in the sim data
dat_psych <- dat

# PHQ-9
phq9_result <- phq9_score(
  data    = dat_psych,
  col_map = list(items = stats::setNames(paste0("phq", 1:9), sprintf("phq9_%02d", 1:9))),
  verbose = TRUE
)
head(phq9_result); table(phq9_result$phq9_category)

# GAD-7
gad7_result <- gad7_score(
  data    = dat_psych,
  col_map = list(items = stats::setNames(paste0("gad", 1:7), sprintf("gad7_%02d", 1:7))),
  verbose = TRUE
)
head(gad7_result)

# WHO-5
who5_result <- who5_score(
  data    = dat_psych,
  col_map = list(items = stats::setNames(paste0("who", 1:5), sprintf("who5_%02d", 1:5))),
  verbose = TRUE
)
head(who5_result)

# K-6
k6_result <- k6_score(
  data    = dat_psych,
  col_map = list(items = stats::setNames(paste0("k6_", 1:6), sprintf("k6_%02d", 1:6))),
  verbose = TRUE
)
head(k6_result)

# psych_markers aggregator (uses the psych item columns we just added)
psych_result <- psych_markers(
  data    = dat_psych,
  which   = c("phq9", "gad7", "who5", "k6"),
  col_map = list(
    phq9 = list(items = stats::setNames(paste0("phq", 1:9), sprintf("phq9_%02d", 1:9))),
    gad7 = list(items = stats::setNames(paste0("gad", 1:7), sprintf("gad7_%02d", 1:7))),
    who5 = list(items = stats::setNames(paste0("who", 1:5), sprintf("who5_%02d", 1:5))),
    k6   = list(items = stats::setNames(paste0("k6_", 1:6), sprintf("k6_%02d",   1:6)))
  ),
  verbose = TRUE
)
dim(psych_result)


# ── 28. MISSING VALUE HANDLING ────────────────────────────────────────────────
# 28a. na_action = "warn" (fasting_is – just fixed)
dat_missing        <- dat
dat_missing$G0[1:5] <- NA
fasting_warn <- fasting_is(
  data    = dat_missing,
  col_map = list(G0 = "G0", I0 = "I0"),
  na_action = "warn",
  verbose = TRUE
)
cat("NA rows in output:", sum(is.na(fasting_warn[[1]])), "\n")

# 28b. na_action = "omit"
fasting_omit <- fasting_is(
  data    = dat_missing,
  col_map = list(G0 = "G0", I0 = "I0"),
  na_action = "omit"
)
cat("Rows returned (omit):", nrow(fasting_omit), "of", nrow(dat), "\n")

# 28c. na_action = "error" – expect an error
tryCatch(
  fasting_is(dat_missing, col_map = list(G0 = "G0", I0 = "I0"),
             na_action = "error"),
  error = function(e) cat("Expected error caught:", conditionMessage(e), "\n")
)


# ── 29. NORMALIZATION ────────────────────────────────────────────────────────
x <- c(1, 5, 3, NA, 2, 8, 4)
cat("z-score:     ", normalize_vec(x, "z"), "\n")
cat("range [0,1]: ", normalize_vec(x, "range", feature_range = c(0, 1)), "\n")
cat("robust:      ", normalize_vec(x, "robust"), "\n")
cat("inverse-norm:", normalize_vec(x, "inverse"), "\n")
cat("all-NA guard:", normalize_vec(rep(NA_real_, 5), "z"), "\n")  # should be all NA, no warning


# ── 30. COLUMN INFERENCE ────────────────────────────────────────────────────
# infer_cols() – guess mappings from data column names
inferred <- infer_cols(
  dat,
  map      = list(G0 = NULL, I0 = NULL, TG = NULL, HDL_c = NULL,
                  BMI = NULL, waist = NULL),
  prefer   = list(TG = "TG", HDL_c = "HDL_c", BMI = "BMI"),
  strategy = "prefer",
  verbose  = TRUE,
  return   = "map"
)
print(inferred)

# hm_infer_cols() – pattern-based inference
patterns <- .hm_default_col_patterns_exact()
auto_map <- hm_infer_cols(dat, patterns = patterns,
                          required_keys = c("G0","I0","TG","HDL_c"),
                          verbose = TRUE)
print(auto_map[c("G0","I0","TG","HDL_c")])


# ── 31. IMPUTATION ────────────────────────────────────────────────────────────
dat_with_na            <- dat
dat_with_na$G0[1:10]   <- NA
dat_with_na$BMI[3:7]   <- NA
dat_with_na$CRP[15:20] <- NA

# 31a. Mean imputation
imp_mean <- impute_missing(dat_with_na, method = "mean", verbose = TRUE)
cat("NAs before:", sum(is.na(dat_with_na[, c("G0","BMI","CRP")])),
    "  after:", sum(is.na(imp_mean[,  c("G0","BMI","CRP")])), "\n")

# 31b. Median imputation (specific columns)
imp_med <- impute_missing(dat_with_na, method = "median",
                          cols = c("G0","BMI"), verbose = TRUE)

# 31c. Constant imputation
imp_const <- impute_missing(dat_with_na, method = "constant", constant = -99,
                             cols = "CRP", verbose = TRUE)

# 31d. MICE (requires mice)
if (requireNamespace("mice", quietly = TRUE)) {
  imp_mice <- impute_mice(dat_with_na[, c("G0","BMI","CRP","age")],
                          m = 3, verbose = TRUE)
  dim(imp_mice)
} else {
  message("mice not installed – skipping impute_mice()")
}

# 31e. missForest (requires missForest)
if (requireNamespace("missForest", quietly = TRUE)) {
  imp_mf <- impute_missforest(dat_with_na[, c("G0","BMI","CRP","age")],
                               verbose = TRUE)
  dim(imp_mf)
} else {
  message("missForest not installed – skipping impute_missforest()")
}


# ── 32. EXTREME VALUE HANDLING ───────────────────────────────────────────────
dat_extremes        <- dat
dat_extremes$G0[1]  <- 999   # absurd glucose
dat_extremes$BMI[2] <- 0.5   # impossible BMI

fasting_cap <- fasting_is(
  data    = dat_extremes,
  col_map = list(G0 = "G0", I0 = "I0"),
  check_extreme  = TRUE,
  extreme_action = "cap",
  verbose = TRUE
)
# check_extreme acts on the *output* indices, not on the input G0 column;
# G0=999 -> HOMA_IR_inv row 1 would be extreme -> capped to extreme_limit
cat("HOMA_IR_inv row 1 after cap:", fasting_cap[["HOMA_IR_inv"]][1], "\n")

# "NA" action
fasting_na_action <- fasting_is(
  data    = dat_extremes,
  col_map = list(G0 = "G0", I0 = "I0"),
  check_extreme  = TRUE,
  extreme_action = "NA",
  verbose = FALSE
)


# ── 33. ADIPOSITY SDS ─────────────────────────────────────────────────────────
# adiposity_sds() computes age/sex-referenced SDS (needs reference list)
# Minimal usage with built-in WHO-style identity ref (demo only)
# adiposity_sds() and adiposity_sds_strat() are best demonstrated from vignettes;
# shown here with minimal example
demo_bmi_ref <- list(
  mean = stats::setNames(rep(22, 40), as.character(seq_len(40))),
  sd   = stats::setNames(rep(3,  40), as.character(seq_len(40)))
)
# adiposity_sds(dat, ref = demo_bmi_ref, ...)  # illustrative; proper ref needed


# ── 34. SUMMARY UTILITIES ────────────────────────────────────────────────────
# marker_summary on a result tibble
sum_tbl <- marker_summary(lipid_result, verbose = TRUE)
print(sum_tbl)

# health_summary on the full dispatcher result
hs <- health_summary(result_full)
print(hs)


# ── 35. VERBOSITY CONTROL ────────────────────────────────────────────────────
# verbose = TRUE on a function call always shows inform-level messages.
# verbose = FALSE suppresses them (debug messages require the global option).

fasting_quiet <- fasting_is(
  dat, col_map = list(G0 = "G0", I0 = "I0"), verbose = FALSE
)

# Enable deep debug logging globally (internal step messages):
options(healthmarkers.verbose = "debug")
fasting_debug <- fasting_is(
  dat, col_map = list(G0 = "G0", I0 = "I0"), verbose = TRUE
)
options(healthmarkers.verbose = "none")  # restore


# ── 36. ERROR HANDLING – BAD INPUTS ──────────────────────────────────────────
# Missing required column
tryCatch(
  fasting_is(dat, col_map = list(G0 = "NoSuchColumn", I0 = "I0")),
  error = function(e) cat("Expected error:", conditionMessage(e), "\n")
)

# Wrong data type
tryCatch(
  lipid_markers("not a dataframe", col_map = list(TC = "TC", HDL_c = "HDL_c", TG = "TG")),
  error = function(e) cat("Expected error:", conditionMessage(e), "\n")
)

# Bad normalize value
tryCatch(
  fasting_is(dat, col_map = list(G0 = "G0", I0 = "I0"), normalize = "bogus"),
  error = function(e) cat("Expected error:", conditionMessage(e), "\n")
)

# Unknown marker group in dispatcher
tryCatch(
  all_health_markers(dat, which = "not_a_group"),
  error = function(e) cat("Expected error:", conditionMessage(e), "\n")
)

cat("\n=== All test blocks complete ===\n")



