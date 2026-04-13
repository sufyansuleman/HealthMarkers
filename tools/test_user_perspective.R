## =============================================================================
## HealthMarkers – Comprehensive User-Perspective Test Script v2
## =============================================================================
## Tests EVERY exported function in the package.
## Real data is used where columns are available; missing variables are filled
## from simulated data (sim_extra) so every function can execute.
##
## Usage: Run section-by-section in an interactive R session, OR:
##   source("tools/test_user_perspective.R")
##
## Optional packages:  di  rspiro  PooledCohort  mice  missForest
## Those sections skip gracefully when the package is absent.
## =============================================================================


## -- 0. INSTALL & LOAD --------------------------------------------------------
## Step 1 (once): uncomment the line below, run it, then RESTART R.
## Step 2 (every session): run from library() onward (install line stays commented).

 install.packages("C:/R_packages/HealthMarkers", repos = NULL, type = "source")

library(HealthMarkers)

## Simple status tracker -------------------------------------------------------
.test_results <- list()
.hm_try <- function(name, expr) {
  res <- tryCatch(
    withCallingHandlers(expr, message = function(m) invokeRestart("muffleMessage")),
    error = function(e) {
      cat(sprintf("  FAIL  %-45s %s\n", name, conditionMessage(e)))
      NULL
    }
  )
  .test_results[[name]] <<- if (!is.null(res)) "PASS" else "FAIL"
  invisible(res)
}

cat("\n========= HealthMarkers user-perspective tests =========\n\n")


## -- 1. LOAD REAL DATA --------------------------------------------------------

dat <- read.table(
  "O:/HE_Sufyan/PhD_work/01_Projects/01_PhD_project/01_Phenotypic_correlations/data-raw/MergedPhenotypes_11mar2021.txt",
  header           = TRUE,
  sep              = "\t",
  stringsAsFactors = FALSE,
  na.strings       = c("", "NA", ".", "N/A")
)
cat("Real data dimensions:", nrow(dat), "rows x", ncol(dat), "cols\n")

n <- nrow(dat)
set.seed(42)

cat("First 30 column names:\n")
print(head(names(dat), 30))


## -- 2. BUILD SIMULATED SUPPLEMENT (sim_extra) --------------------------------
## Values use physiologically plausible ranges.
## Only columns absent from real data need simulating.

sim_extra <- data.frame(

  ## Bone / body composition markers
  ALM             = pmax(0, rnorm(n, 20.5, 4.0)),
  BMD             = pmax(0, rnorm(n, 0.95, 0.12)),
  BMD_ref_mean    = 0.95,
  BMD_ref_sd      = 0.12,
  TBS             = pmax(0, rnorm(n, 1.32, 0.12)),
  PINP            = pmax(0, rnorm(n, 45, 18)),
  CTX             = pmax(0, rnorm(n, 0.30, 0.12)),
  BSAP            = pmax(0, rnorm(n, 28, 9)),
  Osteocalcin     = pmax(0, rnorm(n, 21, 8)),

  ## Inflammatory / iAge
  CRP             = pmax(0, exp(rnorm(n, 0.5, 0.8))),
  IL6             = pmax(0, exp(rnorm(n, 0.4, 0.9))),
  TNFa            = pmax(0, exp(rnorm(n, 0.3, 0.7))),
  neutrophils     = pmax(0, rnorm(n, 3.8, 1.2)),
  lymphocytes     = pmax(0, rnorm(n, 2.0, 0.6)),
  monocytes       = pmax(0, rnorm(n, 0.45, 0.15)),
  eosinophils     = pmax(0, rnorm(n, 0.18, 0.10)),
  WBC             = pmax(0, rnorm(n, 6.2, 1.8)),
  platelets       = pmax(0, rnorm(n, 245, 65)),

  ## Oxidative stress
  GSH             = pmax(0, rnorm(n, 8.2, 2.1)),
  GSSG            = pmax(0, rnorm(n, 1.1, 0.4)),

  ## Kynurenine pathway
  kynurenine      = pmax(0, rnorm(n, 2.5, 0.8)),
  tryptophan      = pmax(0, rnorm(n, 65,  15)),

  ## Neurofilament light chain
  nfl             = pmax(0, exp(rnorm(n, 2.0, 0.7))),

  ## Saliva markers
  cort1           = pmax(0, rnorm(n, 15, 6)),
  cort2           = pmax(0, rnorm(n,  9, 4)),
  cort3           = pmax(0, rnorm(n,  5, 3)),
  saliva_cort1    = pmax(0, rnorm(n, 15, 6)),
  saliva_cort2    = pmax(0, rnorm(n,  9, 4)),
  saliva_cort3    = pmax(0, rnorm(n,  5, 3)),
  amylase         = pmax(0, rnorm(n, 80, 40)),
  saliva_glucose  = pmax(0, rnorm(n, 0.15, 0.05)),

  ## Sweat markers
  sweat_chloride    = pmax(0, rnorm(n, 35, 15)),
  sweat_Na          = pmax(0, rnorm(n, 40, 12)),
  sweat_K           = pmax(0, rnorm(n,  8,  3)),
  sweat_lactate     = pmax(0, rnorm(n,  5,  2)),
  weight_before     = pmax(50, rnorm(n, 78, 14)),
  weight_after      = pmax(50, rnorm(n, 77, 14)),
  duration          = pmax(30, rnorm(n, 60, 10)),
  body_surface_area = pmax(1.2, rnorm(n, 1.9, 0.2)),

  ## Hormones (in case not in real data)
  testo           = pmax(0, rnorm(n, 15,  6)),
  shbg            = pmax(0, rnorm(n, 40, 18)),
  tsh             = pmax(0, rnorm(n,  2,  1.2)),
  ft4             = pmax(0, rnorm(n, 16,  4)),
  IGF1            = pmax(0, rnorm(n, 180, 60)),
  dheas           = pmax(0, rnorm(n, 3.5, 1.5)),
  glucagon        = pmax(0, rnorm(n, 75,  20)),
  GH              = pmax(0, rnorm(n,  1.0, 0.8)),
  progesterone    = pmax(0, rnorm(n,  5,  6)),
  prolactin       = pmax(0, rnorm(n, 12,  6)),
  FSH             = pmax(0, rnorm(n,  6,  4)),
  LH              = pmax(0, rnorm(n,  5,  3)),
  fT3             = pmax(0, rnorm(n,  5.0, 0.8)),
  estradiol       = pmax(0, rnorm(n, 80,  40)),
  renin           = pmax(0, rnorm(n, 12,   8)),
  aldosterone     = pmax(0, rnorm(n, 200, 80)),

  ## Vitamins (in case not in real data)
  vitb12          = pmax(0, rnorm(n, 380, 120)),
  folate          = pmax(0, rnorm(n,  18,   8)),
  ferri           = pmax(0, exp(rnorm(n, 4.0, 0.6))),

  ## Nutrients
  ferritin        = pmax(0, exp(rnorm(n, 4.0, 0.6))),
  transferrin_sat = pmin(1, pmax(0, rnorm(n, 0.28, 0.10))),
  total_protein   = pmax(0, rnorm(n, 72, 6)),
  EPA             = pmax(0, rnorm(n,  2, 1)),
  DHA             = pmax(0, rnorm(n,  4, 1.5)),
  magnesium       = pmax(0, rnorm(n, 0.85, 0.10)),
  uric_acid       = pmax(0, rnorm(n, 310, 80)),
  BUN             = pmax(0, rnorm(n, 5.5, 1.8)),
  urea_serum      = pmax(0, rnorm(n, 5.5, 1.5)),
  phosphate       = pmax(0, rnorm(n, 1.1, 0.2)),
  calcium         = pmax(0, rnorm(n, 2.35, 0.15)),
  Na              = pmax(0, rnorm(n, 140, 3)),
  K               = pmax(0, rnorm(n, 4.1, 0.4)),
  Cl              = pmax(0, rnorm(n, 103, 3)),
  HCO3            = pmax(0, rnorm(n, 24, 2.5)),
  Tyr             = pmax(0, rnorm(n, 65, 20)),
  Phe             = pmax(0, rnorm(n, 55, 15)),

  ## Liver extras
  GGT             = pmax(0, exp(rnorm(n, 3.0, 0.6))),
  bilirubin       = pmax(0, rnorm(n, 11, 5)),
  AST_sim         = pmax(0, exp(rnorm(n, 3.0, 0.4))),

  ## Metabolic risk features extras
  z_HOMA          = rnorm(n),
  bp_sys_z        = rnorm(n),
  bp_dia_z        = rnorm(n),

  ## Race/ethnicity for metss
  race            = sample(c("white","black","hispanic"), n, replace = TRUE),

  ## Simulated eGFR (for ckd_stage / kidney_failure_risk when renal not pre-run)
  eGFR_sim        = pmax(15, rnorm(n, 80, 20)),
  UACR            = pmax(0, exp(rnorm(n, 1.5, 1.0))),

  ## Pulmonary / spirometry
  FEV1            = pmax(0, rnorm(n, 3.2, 0.8)),
  FVC             = pmax(0, rnorm(n, 4.1, 0.9)),
  FEV1pct         = pmin(100, pmax(20, rnorm(n, 78, 15))),
  height_cm       = pmax(140, rnorm(n, 170, 10)),
  sixmwd          = pmax(100, rnorm(n, 480, 90)),
  mmrc            = sample(0:4, n, replace = TRUE),

  ## Tracer / DXA insulin sensitivity
  rate_palmitate  = pmax(0, rnorm(n, 105, 30)),
  rate_glycerol   = pmax(0, rnorm(n, 140, 40)),
  fat_mass        = pmax(5, rnorm(n, 22, 8)),

  ## Anthropometry (in case not in real data)
  hip             = pmax(60, rnorm(n, 96, 12)),
  height_m_sim    = pmax(1.40, rnorm(n, 1.70, 0.10)),

  ## Urine (in case not in real data)
  ucrea_sim       = pmax(0, rnorm(n, 9.0, 3.5)),
  ualb_sim        = pmax(0, exp(rnorm(n, 1.5, 0.8))),
  uNa_sim         = pmax(0, rnorm(n, 100, 30)),
  uK_sim          = pmax(0, rnorm(n,  40, 15)),

  ## FRAX
  prior_fracture  = rbinom(n, 1, 0.08),
  parent_fracture = rbinom(n, 1, 0.10),
  steroids        = rbinom(n, 1, 0.05),
  smoker          = rbinom(n, 1, 0.20),
  alcohol         = rbinom(n, 1, 0.15),
  rheumatoid      = rbinom(n, 1, 0.03),
  secondary_op    = rbinom(n, 1, 0.04),
  bmd_t           = rnorm(n, -0.5, 1.0),

  ## SARC-F
  Strength   = sample(0:2, n, replace = TRUE),
  Walking    = sample(0:2, n, replace = TRUE),
  Chair      = sample(0:2, n, replace = TRUE),
  Stairs     = sample(0:2, n, replace = TRUE),
  Falls      = sample(0:2, n, replace = TRUE),

  ## Charlson comorbidity
  mi                = rbinom(n, 1, 0.06),
  chf               = rbinom(n, 1, 0.04),
  pvd               = rbinom(n, 1, 0.03),
  stroke            = rbinom(n, 1, 0.04),
  dementia          = rbinom(n, 1, 0.02),
  copd              = rbinom(n, 1, 0.05),
  rheum             = rbinom(n, 1, 0.02),
  ulcer             = rbinom(n, 1, 0.03),
  mild_liver        = rbinom(n, 1, 0.02),
  diabetes          = rbinom(n, 1, 0.08),
  diab_comp         = rbinom(n, 1, 0.02),
  hemiplegia        = rbinom(n, 1, 0.01),
  renal_dis         = rbinom(n, 1, 0.03),
  cancer            = rbinom(n, 1, 0.04),
  leukemia          = rbinom(n, 1, 0.01),
  lymphoma          = rbinom(n, 1, 0.01),
  sev_liver         = rbinom(n, 1, 0.01),
  metastatic_cancer = rbinom(n, 1, 0.01),
  hiv               = rbinom(n, 1, 0.005),

  ## Psych diagnosis and medication flags
  dx_mdd            = rbinom(n, 1, 0.12),
  dx_anxiety        = rbinom(n, 1, 0.10),
  dx_adhd           = rbinom(n, 1, 0.05),
  dx_bipolar        = rbinom(n, 1, 0.02),
  dx_scz            = rbinom(n, 1, 0.01),
  dx_sud            = rbinom(n, 1, 0.05),
  med_ssri          = rbinom(n, 1, 0.10),
  med_snri          = rbinom(n, 1, 0.05),
  med_antipsychotic = rbinom(n, 1, 0.03),
  med_mood_stab     = rbinom(n, 1, 0.02),
  med_anxiolytic    = rbinom(n, 1, 0.04),

  ## PHQ-9 items
  phq9_01 = sample(0:3, n, replace = TRUE), phq9_02 = sample(0:3, n, replace = TRUE),
  phq9_03 = sample(0:3, n, replace = TRUE), phq9_04 = sample(0:3, n, replace = TRUE),
  phq9_05 = sample(0:3, n, replace = TRUE), phq9_06 = sample(0:3, n, replace = TRUE),
  phq9_07 = sample(0:3, n, replace = TRUE), phq9_08 = sample(0:3, n, replace = TRUE),
  phq9_09 = sample(0:3, n, replace = TRUE),

  ## GAD-7 items
  gad7_01 = sample(0:3, n, replace = TRUE), gad7_02 = sample(0:3, n, replace = TRUE),
  gad7_03 = sample(0:3, n, replace = TRUE), gad7_04 = sample(0:3, n, replace = TRUE),
  gad7_05 = sample(0:3, n, replace = TRUE), gad7_06 = sample(0:3, n, replace = TRUE),
  gad7_07 = sample(0:3, n, replace = TRUE),

  ## WHO-5 items
  who5_01 = sample(0:5, n, replace = TRUE), who5_02 = sample(0:5, n, replace = TRUE),
  who5_03 = sample(0:5, n, replace = TRUE), who5_04 = sample(0:5, n, replace = TRUE),
  who5_05 = sample(0:5, n, replace = TRUE),

  ## K-6 items
  k6_01 = sample(0:4, n, replace = TRUE), k6_02 = sample(0:4, n, replace = TRUE),
  k6_03 = sample(0:4, n, replace = TRUE), k6_04 = sample(0:4, n, replace = TRUE),
  k6_05 = sample(0:4, n, replace = TRUE), k6_06 = sample(0:4, n, replace = TRUE),

  ## K-10 items
  k10_01 = sample(0:4, n, replace = TRUE), k10_02 = sample(0:4, n, replace = TRUE),
  k10_03 = sample(0:4, n, replace = TRUE), k10_04 = sample(0:4, n, replace = TRUE),
  k10_05 = sample(0:4, n, replace = TRUE), k10_06 = sample(0:4, n, replace = TRUE),
  k10_07 = sample(0:4, n, replace = TRUE), k10_08 = sample(0:4, n, replace = TRUE),
  k10_09 = sample(0:4, n, replace = TRUE), k10_10 = sample(0:4, n, replace = TRUE),

  ## GHQ-12 items
  ghq12_01 = sample(0:3, n, replace = TRUE), ghq12_02 = sample(0:3, n, replace = TRUE),
  ghq12_03 = sample(0:3, n, replace = TRUE), ghq12_04 = sample(0:3, n, replace = TRUE),
  ghq12_05 = sample(0:3, n, replace = TRUE), ghq12_06 = sample(0:3, n, replace = TRUE),
  ghq12_07 = sample(0:3, n, replace = TRUE), ghq12_08 = sample(0:3, n, replace = TRUE),
  ghq12_09 = sample(0:3, n, replace = TRUE), ghq12_10 = sample(0:3, n, replace = TRUE),
  ghq12_11 = sample(0:3, n, replace = TRUE), ghq12_12 = sample(0:3, n, replace = TRUE),

  ## ISI items
  isi_01 = sample(0:4, n, replace = TRUE), isi_02 = sample(0:4, n, replace = TRUE),
  isi_03 = sample(0:4, n, replace = TRUE), isi_04 = sample(0:4, n, replace = TRUE),
  isi_05 = sample(0:4, n, replace = TRUE), isi_06 = sample(0:4, n, replace = TRUE),
  isi_07 = sample(0:4, n, replace = TRUE),

  ## MDQ items
  mdq_01 = sample(0:1, n, replace = TRUE), mdq_02 = sample(0:1, n, replace = TRUE),
  mdq_03 = sample(0:1, n, replace = TRUE), mdq_04 = sample(0:1, n, replace = TRUE),
  mdq_05 = sample(0:1, n, replace = TRUE), mdq_06 = sample(0:1, n, replace = TRUE),
  mdq_07 = sample(0:1, n, replace = TRUE), mdq_08 = sample(0:1, n, replace = TRUE),
  mdq_09 = sample(0:1, n, replace = TRUE), mdq_10 = sample(0:1, n, replace = TRUE),
  mdq_11 = sample(0:1, n, replace = TRUE), mdq_12 = sample(0:1, n, replace = TRUE),
  mdq_13 = sample(0:1, n, replace = TRUE),
  mdq_cluster = sample(0:1, n, replace = TRUE),
  mdq_impair  = sample(0:1, n, replace = TRUE),

  ## ASRS items
  asrs_01 = sample(0:3, n, replace = TRUE), asrs_02 = sample(0:3, n, replace = TRUE),
  asrs_03 = sample(0:3, n, replace = TRUE), asrs_04 = sample(0:3, n, replace = TRUE),
  asrs_05 = sample(0:3, n, replace = TRUE), asrs_06 = sample(0:3, n, replace = TRUE),
  asrs_07 = sample(0:3, n, replace = TRUE), asrs_08 = sample(0:3, n, replace = TRUE),
  asrs_09 = sample(0:3, n, replace = TRUE), asrs_10 = sample(0:3, n, replace = TRUE),
  asrs_11 = sample(0:3, n, replace = TRUE), asrs_12 = sample(0:3, n, replace = TRUE),
  asrs_13 = sample(0:3, n, replace = TRUE), asrs_14 = sample(0:3, n, replace = TRUE),
  asrs_15 = sample(0:3, n, replace = TRUE), asrs_16 = sample(0:3, n, replace = TRUE),
  asrs_17 = sample(0:3, n, replace = TRUE), asrs_18 = sample(0:3, n, replace = TRUE),

  ## Cognitive tasks
  task_memory    = rnorm(n, 50, 10),
  task_attention = rnorm(n, 48, 12),
  task_executive = rnorm(n, 52, 11),

  stringsAsFactors = FALSE
)


## -- 3. BUILD DAT_FULL --------------------------------------------------------
## Merge real data with simulated columns (sim columns not overwriting real ones)

new_cols  <- setdiff(names(sim_extra), names(dat))
dat_full  <- cbind(dat, sim_extra[, new_cols, drop = FALSE])
cat("dat_full:", nrow(dat_full), "rows x", ncol(dat_full), "cols\n\n")

## Convenience aliases so downstream calls find standard column names
## regardless of how real data spells them.

# AST: real data uses "asat"; add "AST" alias if needed
if ("asat" %in% names(dat_full) && !"AST" %in% names(dat_full))
  dat_full$AST <- dat_full$asat

# Height: prefer "height_m"; fall back to simulated "height_m_sim"
if (!"height_m" %in% names(dat_full)) {
  if ("height" %in% names(dat_full)) {
    dat_full$height_m <- dat_full$height
  } else {
    dat_full$height_m <- dat_full$height_m_sim
  }
}
if (!"height" %in% names(dat_full))
  dat_full$height <- dat_full$height_m

# Urine columns: prefer real; fall back to sim
if (!"ucrea" %in% names(dat_full))  dat_full$ucrea  <- dat_full$ucrea_sim
if (!"ualb"  %in% names(dat_full))  dat_full$ualb   <- dat_full$ualb_sim
if (!"uNa"   %in% names(dat_full))  dat_full$uNa    <- dat_full$uNa_sim
if (!"uK"    %in% names(dat_full))  dat_full$uK     <- dat_full$uK_sim

# Standard urine column names required by urine_markers() (no col_map accepted)
if (!"urine_albumin"    %in% names(dat_full)) dat_full$urine_albumin    <- dat_full$ualb
if (!"urine_creatinine" %in% names(dat_full)) dat_full$urine_creatinine <- dat_full$ucrea
if (!"urine_Na"         %in% names(dat_full)) dat_full$urine_Na         <- dat_full$uNa
if (!"urine_K"          %in% names(dat_full)) dat_full$urine_K          <- dat_full$uK

# Hip: use simulated if not in real data
if (!"hip" %in% names(dat_full))
  dat_full$hip <- dat_full$hip  # already added via sim_extra

# eGFR placeholder (real eGFR computed by renal_markers later)
dat_full$eGFR_base <- dat_full$eGFR_sim


## =============================================================================
## SECTION A: FULL DISPATCHER
## =============================================================================

cat("\n--- A. all_health_markers() ---\n")

.hm_try("all_health_markers", {
  res_all <- all_health_markers(dat_full, verbose = TRUE)
  cat("  Dispatcher:", ncol(res_all), "result cols,", nrow(res_all), "rows\n")
  res_all
})

.hm_try("health_summary", {
  r <- .test_results[["all_health_markers"]]
  if (!is.null(r) && is.data.frame(r)) health_summary(r) else stop("no dispatcher result")
})

.hm_try("marker_summary_full", {
  r <- .test_results[["all_health_markers"]]
  if (!is.null(r) && is.data.frame(r)) marker_summary(r, verbose = FALSE) else stop("no result")
})


## =============================================================================
## SECTION B: INSULIN SENSITIVITY
## =============================================================================

cat("\n--- B1. fasting_is() ---\n")
.hm_try("fasting_is_auto",     fasting_is(dat_full, verbose = FALSE))
.hm_try("fasting_is_explicit", fasting_is(dat_full,
  col_map = list(G0 = "pglu0", I0 = "insu0", TG = "trig",
                 HDL_c = "hdlc", BMI = "bmi", waist = "waist"),
  normalize = "none", verbose = FALSE))
.hm_try("fasting_is_zscore",   fasting_is(dat_full,
  col_map = list(G0 = "pglu0", I0 = "insu0"),
  normalize = "z", verbose = FALSE))

cat("\n--- B2. ogtt_is() ---\n")
.hm_try("ogtt_is_explicit", ogtt_is(dat_full,
  col_map = list(G0 = "pglu0", G30 = "pglu30", G120 = "pglu120",
                 I0 = "insu0", I30 = "insu30", I120 = "insu120",
                 weight = "weight", bmi = "bmi", age = "age", sex = "sex"),
  normalize = "none", verbose = FALSE))

cat("\n--- B3. adipo_is() ---\n")
.hm_try("adipo_is_explicit", adipo_is(dat_full,
  col_map = list(G0 = "pglu0", I0 = "insu0", TG = "trig",
                 HDL_c = "hdlc", waist = "waist", bmi = "bmi"),
  verbose = FALSE))

cat("\n--- B4. tracer_dxa_is() ---\n")
.hm_try("tracer_dxa_is_explicit", tracer_dxa_is(dat_full,
  col_map = list(I0 = "insu0", rate_palmitate = "rate_palmitate",
                 rate_glycerol = "rate_glycerol", fat_mass = "fat_mass",
                 weight = "weight", HDL_c = "hdlc", bmi = "bmi"),
  verbose = FALSE))

cat("\n--- B5. all_insulin_indices() ---\n")
.hm_try("all_insulin_indices", all_insulin_indices(dat_full,
  col_map = list(G0 = "pglu0", I0 = "insu0", G30 = "pglu30", I30 = "insu30",
                 G120 = "pglu120", I120 = "insu120",
                 TG = "trig", HDL_c = "hdlc",
                 waist = "waist", weight = "weight", bmi = "bmi",
                 age = "age", sex = "sex", fat_mass = "fat_mass"),
  mode = "both", verbose = FALSE))


## =============================================================================
## SECTION C: GLYCEMIC & METABOLIC
## =============================================================================

cat("\n--- C1. glycemic_markers() ---\n")
.hm_try("glycemic_markers", glycemic_markers(dat_full,
  col_map = list(G0 = "pglu0", I0 = "insu0", HbA1c = "hba1c",
                 G120 = "pglu120", I120 = "insu120",
                 HDL_c = "hdlc", TG = "trig", BMI = "bmi"),
  verbose = FALSE))

cat("\n--- C2. metabolic_markers() aggregator ---\n")
.hm_try("metabolic_markers", metabolic_markers(dat_full,
  which = c("lipid", "glycemic"), verbose = FALSE))

cat("\n--- C3. metabolic_risk_features() ---\n")
.hm_try("metabolic_risk_features_auto",     metabolic_risk_features(dat_full, verbose = FALSE))
.hm_try("metabolic_risk_features_explicit", metabolic_risk_features(dat_full,
  col_map = list(chol_total = "chol", chol_ldl = "ldl", chol_hdl = "hdlc",
                 triglycerides = "trig", age_year = "age",
                 z_HOMA = "z_HOMA", glucose = "pglu0",
                 bp_sys_z = "bp_sys_z", bp_dia_z = "bp_dia_z"),
  verbose = FALSE))

cat("\n--- C4. metss() ---\n")
.hm_try("metss_auto",     metss(dat_full, verbose = FALSE))
.hm_try("metss_explicit", metss(dat_full,
  col_map = list(sbp = "sysbp", dbp = "diabp", hdl = "hdlc",
                 tg = "trig", glucose = "pglu0",
                 waist = "waist", bmi = "bmi",
                 sex = "sex", race = "race"),
  verbose = FALSE))


## =============================================================================
## SECTION D: LIPID & CARDIOVASCULAR
## =============================================================================

cat("\n--- D1. lipid_markers() ---\n")
.hm_try("lipid_markers", lipid_markers(dat_full,
  col_map = list(TC = "chol", HDL_c = "hdlc", TG = "trig", LDL_c = "ldl",
                 ApoB = "APOB", waist = "waist", BMI = "bmi"),
  verbose = FALSE))

cat("\n--- D2. atherogenic_indices() ---\n")
.hm_try("atherogenic_indices", atherogenic_indices(dat_full,
  col_map = list(TG = "trig", HDL_c = "hdlc", TC = "chol", LDL_c = "ldl"),
  verbose = FALSE))

cat("\n--- D3. cvd_marker_aip() ---\n")
.hm_try("cvd_marker_aip", cvd_marker_aip(dat_full,
  col_map = list(TG = "trig", HDL_c = "hdlc"), verbose = FALSE))

cat("\n--- D4. cvd_marker_ldl_particle_number() ---\n")
.hm_try("cvd_marker_ldl_particle_number", cvd_marker_ldl_particle_number(dat_full,
  col_map = list(ApoB = "APOB"), verbose = FALSE))

cat("\n--- D5. cvd_risk() / cvd_risk_scorescvd() ---\n")
.hm_try("cvd_risk_aip", cvd_risk(dat_full, model = "AIP",
  col_map = list(TG = "trig", HDL_c = "hdlc")))

.hm_try("cvd_risk_scorescvd", cvd_risk_scorescvd(dat_full,
  col_map = list(age = "age", sex = "sex", TC = "chol",
                 HDL_c = "hdlc", SBP = "sysbp", smoker = "smoker"),
  verbose = FALSE))

if (requireNamespace("PooledCohort", quietly = TRUE)) {
  .hm_try("cvd_risk_ascvd",  cvd_risk_ascvd(dat_full, year = 10, verbose = FALSE))
  .hm_try("cvd_risk_stroke", cvd_risk_stroke(dat_full, verbose = FALSE))
} else {
  message("PooledCohort not installed -- skipping cvd_risk_ascvd(), cvd_risk_stroke()")
}

if (requireNamespace("QRISK3", quietly = TRUE) || requireNamespace("qrisk3", quietly = TRUE)) {
  .hm_try("cvd_risk_qrisk3", cvd_risk_qrisk3(dat_full, verbose = FALSE))
} else {
  message("qrisk3 not installed -- skipping cvd_risk_qrisk3()")
}

cat("\n--- D6. allostatic_load() ---\n")
.hm_try("allostatic_load", allostatic_load(dat_full,
  thresholds = list(sbp = 130, dbp = 85, CRP = 3.0, BMI = 30,
                    waist = 102, TG = 1.7, HDL_c = 1.0),
  na_action = "keep", verbose = FALSE))


## =============================================================================
## SECTION E: LIVER & RENAL
## =============================================================================

cat("\n--- E1. liver_markers() ---\n")
.hm_try("liver_markers_auto",     liver_markers(dat_full, verbose = FALSE))
.hm_try("liver_markers_explicit", liver_markers(dat_full,
  col_map = list(BMI = "bmi", waist = "waist", TG = "trig", age = "age",
                 ALT = "alat", AST = "AST", GGT = "GGT",
                 albumin = "alb", bilirubin = "bilirubin", creatinine = "crea"),
  verbose = FALSE))

cat("\n--- E2. liver_fat_markers() ---\n")
.hm_try("liver_fat_markers_auto",     liver_fat_markers(dat_full, verbose = FALSE))
.hm_try("liver_fat_markers_explicit", liver_fat_markers(dat_full,
  col_map = list(BMI = "bmi", waist = "waist", TG = "trig",
                 ALT = "alat", AST = "AST", GGT = "GGT",
                 albumin = "alb", bilirubin = "bilirubin"),
  verbose = FALSE))

cat("\n--- E3. renal_markers() ---\n")
.hm_try("renal_markers", renal_markers(dat_full,
  col_map = list(creatinine = "crea", age = "age", sex = "sex",
                 BUN = "BUN", race = "race"),
  verbose = FALSE))

# Append eGFR for downstream use
{
  tmp_renal <- renal_markers(dat_full,
    col_map = list(creatinine = "crea", age = "age", sex = "sex",
                   BUN = "BUN", race = "race"),
    verbose = FALSE)
  new_renal <- setdiff(names(tmp_renal), names(dat_full))
  if (length(new_renal)) dat_full <- cbind(dat_full, tmp_renal[, new_renal, drop = FALSE])
  if ("eGFR" %in% names(dat_full)) dat_full$eGFR_base <- dat_full$eGFR
}

cat("\n--- E4. urine_markers() ---\n")
.hm_try("urine_markers_auto",     urine_markers(dat_full, verbose = FALSE))
# urine_markers() uses fixed column names (urine_albumin, urine_creatinine, etc.)
# and does not accept a col_map argument; test na_action variant instead
.hm_try("urine_markers_omit_na",  urine_markers(dat_full, na_action = "omit", verbose = FALSE))

cat("\n--- E5. ckd_stage() ---\n")
.hm_try("ckd_stage_auto",     ckd_stage(dat_full, verbose = FALSE))
.hm_try("ckd_stage_explicit", {
  tmp <- dat_full
  tmp$eGFR <- tmp$eGFR_base
  ckd_stage(tmp, col_map = list(eGFR = "eGFR", UACR = "UACR"), verbose = FALSE)
})

cat("\n--- E6. kidney_failure_risk() ---\n")
.hm_try("kidney_failure_risk", {
  tmp <- dat_full
  tmp$eGFR <- tmp$eGFR_base
  kidney_failure_risk(tmp,
    col_map = list(age = "age", sex = "sex", eGFR = "eGFR", UACR = "UACR"),
    verbose = FALSE)
})


## =============================================================================
## SECTION F: INFLAMMATORY, OXIDATIVE & SPECIALTY BIOMARKERS
## =============================================================================

cat("\n--- F1. inflammatory_markers() ---\n")
.hm_try("inflammatory_markers_auto",     inflammatory_markers(dat_full, panel = "both", verbose = FALSE))
.hm_try("inflammatory_markers_explicit", inflammatory_markers(dat_full,
  col_map = list(CRP = "CRP", albumin = "alb",
                 neutrophils = "neutrophils", lymphocytes = "lymphocytes",
                 monocytes = "monocytes", platelets = "platelets"),
  panel = "both", verbose = FALSE))

cat("\n--- F2. iAge() ---\n")
.hm_try("iAge_auto",     iAge(dat_full, verbose = FALSE))
.hm_try("iAge_explicit", iAge(dat_full,
  col_map = list(CRP = "CRP", IL6 = "IL6", TNFa = "TNFa"),
  verbose = FALSE))

cat("\n--- F3. oxidative_markers() ---\n")
.hm_try("oxidative_markers_auto",     oxidative_markers(dat_full, verbose = FALSE))
.hm_try("oxidative_markers_explicit", oxidative_markers(dat_full,
  col_map = list(GSH = "GSH", GSSG = "GSSG"), verbose = FALSE))

cat("\n--- F4. kyn_trp_ratio() ---\n")
.hm_try("kyn_trp_ratio_auto",     kyn_trp_ratio(dat_full, verbose = FALSE))
.hm_try("kyn_trp_ratio_explicit", kyn_trp_ratio(dat_full,
  col_map = list(kynurenine = "kynurenine", tryptophan = "tryptophan"),
  verbose = FALSE))

cat("\n--- F5. nfl_marker() ---\n")
.hm_try("nfl_marker_auto",     nfl_marker(dat_full, verbose = FALSE))
.hm_try("nfl_marker_explicit", nfl_marker(dat_full,
  col_map = list(nfl = "nfl"), verbose = FALSE))


## =============================================================================
## SECTION G: HORMONES, VITAMINS & NUTRIENTS
## =============================================================================

cat("\n--- G1. hormone_markers() ---\n")
.hm_try("hormone_markers_auto",     hormone_markers(dat_full, verbose = FALSE))
.hm_try("hormone_markers_explicit", hormone_markers(dat_full,
  col_map = list(total_testosterone = "testo", SHBG = "shbg",
                 TSH = "tsh", free_T4 = "ft4",
                 IGF1 = "IGF1", prolactin = "prolactin",
                 LH = "LH", FSH = "FSH",
                 estradiol = "estradiol", progesterone = "progesterone",
                 aldosterone = "aldosterone", renin = "renin"),
  verbose = FALSE))

cat("\n--- G2. vitamin_markers() ---\n")
.hm_try("vitamin_markers_auto",     vitamin_markers(dat_full, verbose = FALSE))
.hm_try("vitamin_markers_explicit", vitamin_markers(dat_full,
  col_map = list(VitD = "vitd25", B12 = "vitb12",
                 Folate = "folate", Ferritin = "ferri",
                 TSH = "tsh", free_T4 = "ft4"),
  verbose = FALSE))

cat("\n--- G3. vitamin_d_status() ---\n")
.hm_try("vitamin_d_status_auto",     vitamin_d_status(dat_full, verbose = FALSE))
.hm_try("vitamin_d_status_explicit", vitamin_d_status(dat_full,
  col_map = list(vitd = "vitd25"), verbose = FALSE))

cat("\n--- G4. nutrient_markers() ---\n")
.hm_try("nutrient_markers_auto",     nutrient_markers(dat_full, verbose = FALSE))
.hm_try("nutrient_markers_explicit", nutrient_markers(dat_full,
  col_map = list(ferritin = "ferritin", albumin = "alb",
                 uric_acid = "uric_acid", BUN = "BUN",
                 phosphate = "phosphate", calcium = "calcium",
                 Na = "Na", K = "K"),
  verbose = FALSE))

cat("\n--- G5. corrected_calcium() ---\n")
.hm_try("corrected_calcium_auto",     corrected_calcium(dat_full, verbose = FALSE))
.hm_try("corrected_calcium_explicit", corrected_calcium(dat_full,
  col_map = list(calcium = "calcium", albumin = "alb"),
  units = "auto", verbose = FALSE))


## =============================================================================
## SECTION H: BONE & BODY COMPOSITION
## =============================================================================

cat("\n--- H1. bone_markers() ---\n")
.hm_try("bone_markers_explicit", bone_markers(dat_full,
  col_map = list(age = "age", weight = "weight", height = "height_m",
                 ALM = "ALM", BMD = "BMD",
                 BMD_ref_mean = "BMD_ref_mean", BMD_ref_sd = "BMD_ref_sd",
                 TBS = "TBS", PINP = "PINP", CTX = "CTX",
                 BSAP = "BSAP", Osteocalcin = "Osteocalcin"),
  verbose = FALSE))

cat("\n--- H2. alm_bmi_index() ---\n")
.hm_try("alm_bmi_index_auto",     alm_bmi_index(dat_full, verbose = FALSE))
.hm_try("alm_bmi_index_explicit", alm_bmi_index(dat_full,
  col_map = list(alm = "ALM", bmi = "bmi", sex = "sex"), verbose = FALSE))

cat("\n--- H3. frax_score() ---\n")
.hm_try("frax_score_auto",     frax_score(dat_full, verbose = FALSE))
.hm_try("frax_score_explicit", frax_score(dat_full,
  col_map = list(age = "age", sex = "sex",
                 prior_fracture  = "prior_fracture",
                 parent_fracture = "parent_fracture",
                 steroids        = "steroids",
                 smoker          = "smoker",
                 alcohol         = "alcohol",
                 bmd_t           = "bmd_t"),
  verbose = FALSE))


## =============================================================================
## SECTION I: OBESITY & ADIPOSITY
## =============================================================================

cat("\n--- I1. obesity_indices() ---\n")
.hm_try("obesity_indices", obesity_indices(dat_full,
  weight = weight, height = height_m, waist = waist, hip = hip, sex = sex,
  weight_unit = "kg", height_unit = "m",
  include_RFM = TRUE, adjust_WHR = TRUE, verbose = FALSE))

cat("\n--- I2. adiposity_sds() ---\n")
bmi_ref <- lapply(setNames(as.list(seq(18, 80)), as.character(18:80)),
                  function(x) c(mean = 25.0, sd = 4.5))
.hm_try("adiposity_sds_explicit", adiposity_sds(dat_full,
  col_map = list(value = "bmi", age = "age"),
  ref = bmi_ref, na_action = "keep", verbose = FALSE))

cat("\n--- I3. adiposity_sds_strat() ---\n")
bmi_ref_strat <- list(
  M = list(bmi = c(mean = 25.5, sd = 4.2)),
  F = list(bmi = c(mean = 24.5, sd = 4.8))
)
.hm_try("adiposity_sds_strat_explicit", adiposity_sds_strat(dat_full,
  col_map = list(sex = "sex", vars = list(bmi = "bmi")),
  ref = bmi_ref_strat, na_action = "keep", verbose = FALSE))

cat("\n--- I4. calc_sds() ---\n")
sds_ref <- list(bmi = c(mean = 25.0, sd = 4.5), waist = c(mean = 88.0, sd = 12.0))
.hm_try("calc_sds_explicit", calc_sds(dat_full,
  vars = c("bmi", "waist"), ref = sds_ref,
  na_strategy = "keep", extreme_strategy = "cap",
  return = "data", verbose = FALSE))


## =============================================================================
## SECTION J: PULMONARY
## =============================================================================

cat("\n--- J1. spirometry_markers() ---\n")
.hm_try("spirometry_markers_auto",     spirometry_markers(dat_full, verbose = FALSE))
.hm_try("spirometry_markers_explicit", spirometry_markers(dat_full,
  col_map = list(fev1 = "FEV1", fvc = "FVC",
                 fev1_pct = "FEV1pct", age = "age",
                 height = "height_m", sex = "sex")))

if (requireNamespace("rspiro", quietly = TRUE)) {
  cat("\n--- J2. pulmo_markers() ---\n")
  .hm_try("pulmo_markers", pulmo_markers(dat_full, equation = "GLI", verbose = FALSE))
} else {
  message("rspiro not installed -- skipping pulmo_markers()")
}

cat("\n--- J3. bode_index() ---\n")
.hm_try("bode_index_auto",     bode_index(dat_full, verbose = FALSE))
.hm_try("bode_index_explicit", bode_index(dat_full,
  col_map = list(fev1_pct = "FEV1pct", sixmwd = "sixmwd",
                 mmrc = "mmrc", bmi = "bmi"),
  verbose = FALSE))


## =============================================================================
## SECTION K: BIOFLUIDS (SALIVA & SWEAT)
## =============================================================================

cat("\n--- K1. saliva_markers() ---\n")
.hm_try("saliva_markers_auto",     saliva_markers(dat_full, verbose = FALSE))
.hm_try("saliva_markers_explicit", saliva_markers(dat_full,
  col_map = list(cort1 = "cort1", cort2 = "cort2", cort3 = "cort3",
                 amylase = "amylase", glucose = "saliva_glucose"),
  times = c(0, 30, 60), verbose = FALSE))

cat("\n--- K2. sweat_markers() ---\n")
.hm_try("sweat_markers_explicit", sweat_markers(dat_full,
  col_map = list(sweat_chloride = "sweat_chloride", sweat_Na = "sweat_Na",
                 sweat_K = "sweat_K", sweat_lactate = "sweat_lactate",
                 weight_before = "weight_before", weight_after = "weight_after",
                 duration = "duration", body_surface_area = "body_surface_area"),
  verbose = FALSE))


## =============================================================================
## SECTION L: FRAILTY, SARCOPENIA & COMORBIDITY
## =============================================================================

cat("\n--- L1. frailty_index() ---\n")
deficit_cols <- c("mi","chf","pvd","stroke","dementia","copd","rheum",
                  "ulcer","mild_liver","diabetes","diab_comp","hemiplegia",
                  "renal_dis","cancer")
if (requireNamespace("di", quietly = TRUE)) {
  .hm_try("frailty_index", frailty_index(dat_full, cols = deficit_cols,
    age = "age", rescale = TRUE, na_action = "keep",
    return = "data", verbose = FALSE))
  .hm_try("plot_frailty_age", {
    p <- plot_frailty_age(dat_full, cols = deficit_cols, age = "age", bins = 5)
    invisible(p)
  })
} else {
  message("di not installed -- skipping frailty_index(), plot_frailty_age()")
}

cat("\n--- L2. sarc_f_score() ---\n")
.hm_try("sarc_f_score_explicit", sarc_f_score(dat_full,
  col_map = list(strength = "Strength", walking = "Walking",
                 chair = "Chair", stairs = "Stairs", falls = "Falls"),
  verbose = FALSE))

cat("\n--- L3. charlson_index() ---\n")
.hm_try("charlson_index_explicit", charlson_index(dat_full,
  col_map = list(
    mi = "mi", chf = "chf", pvd = "pvd", stroke = "stroke",
    dementia = "dementia", copd = "copd", rheum = "rheum",
    ulcer = "ulcer", mild_liver = "mild_liver", diabetes = "diabetes",
    diab_comp = "diab_comp", hemiplegia = "hemiplegia",
    renal = "renal_dis", cancer = "cancer",
    leukemia = "leukemia", lymphoma = "lymphoma",
    sev_liver = "sev_liver", metastatic_cancer = "metastatic_cancer",
    hiv = "hiv"),
  verbose = FALSE))


## =============================================================================
## SECTION M: PSYCHIATRIC SCORES
## =============================================================================

cat("\n--- M1. phq9_score() ---\n")
.hm_try("phq9_score_auto", phq9_score(dat_full, verbose = FALSE))

cat("\n--- M2. gad7_score() ---\n")
.hm_try("gad7_score_auto", gad7_score(dat_full, verbose = FALSE))

cat("\n--- M3. who5_score() ---\n")
.hm_try("who5_score_auto", who5_score(dat_full, verbose = FALSE))

cat("\n--- M4. k6_score() ---\n")
.hm_try("k6_score_auto", k6_score(dat_full, verbose = FALSE))

cat("\n--- M5. k10_score() ---\n")
.hm_try("k10_score_auto", k10_score(dat_full, verbose = FALSE))

cat("\n--- M6. ghq12_score() ---\n")
.hm_try("ghq12_score_likert", ghq12_score(dat_full, method = "likert", verbose = FALSE))
.hm_try("ghq12_score_binary", ghq12_score(dat_full, method = "binary", verbose = FALSE))

cat("\n--- M7. isi_score() ---\n")
.hm_try("isi_score_auto", isi_score(dat_full, verbose = FALSE))

cat("\n--- M8. mdq_score() ---\n")
.hm_try("mdq_score_auto", mdq_score(dat_full, verbose = FALSE))

cat("\n--- M9. asrs_score() ---\n")
.hm_try("asrs_score_auto", asrs_score(dat_full, verbose = FALSE))

cat("\n--- M10. bis_score() ---\n")
bis_key_ex <- list(items = sprintf("asrs_%02d", 1:6), min_val = 0, max_val = 3,
                   subscales = list(motor = 1:2, attentional = 3:4, nonplanning = 5:6))
.hm_try("bis_score_explicit", bis_score(dat_full, key = bis_key_ex, verbose = FALSE))

cat("\n--- M11. spq_score() ---\n")
spq_key_ex <- list(items = sprintf("asrs_%02d", 7:18), min_val = 0, max_val = 3)
.hm_try("spq_score_explicit", spq_score(dat_full, key = spq_key_ex, verbose = FALSE))

cat("\n--- M12. cognitive_score() ---\n")
.hm_try("cognitive_score_explicit", cognitive_score(dat_full,
  col_map = list(tasks = list(
    memory    = "task_memory",
    attention = "task_attention",
    executive = "task_executive"
  )), method = "z_mean", verbose = FALSE))

cat("\n--- M13. psych_dx_flags() ---\n")
.hm_try("psych_dx_flags_explicit", psych_dx_flags(dat_full,
  col_map = list(dx = list(
    mdd     = "dx_mdd", anxiety = "dx_anxiety",
    adhd    = "dx_adhd", bipolar = "dx_bipolar"
  )), verbose = FALSE))

cat("\n--- M14. psych_med_flags() ---\n")
.hm_try("psych_med_flags_explicit", psych_med_flags(dat_full,
  col_map = list(med = list(
    ssri          = "med_ssri",
    snri          = "med_snri",
    antipsychotic = "med_antipsychotic",
    mood_stabilizer = "med_mood_stab",
    anxiolytic    = "med_anxiolytic"
  )), verbose = FALSE))

cat("\n--- M15. psych_markers() aggregator ---\n")
.hm_try("psych_markers_full", psych_markers(dat_full,
  which = c("phq9","gad7","who5","k6","k10","ghq12_likert","isi","mdq","asrs"),
  verbose = FALSE))


## =============================================================================
## SECTION N: UTILITIES
## =============================================================================

cat("\n--- N1. hm_col_report() ---\n")
.hm_try("hm_col_report", {
  rpt <- hm_col_report(dat_full, verbose = FALSE)
  invisible(rpt)
})

cat("\n--- N2. infer_cols() ---\n")
.hm_try("infer_cols", {
  infer_cols(dat_full,
    map      = list(G0 = NULL, I0 = NULL, TG = NULL, HDL_c = NULL, BMI = NULL, waist = NULL),
    prefer   = list(TG = "trig", HDL_c = "hdlc", BMI = "bmi"),
    strategy = "prefer", verbose = FALSE, return = "map")
})

cat("\n--- N3. normalize_vec() ---\n")
x_vec <- c(1, 5, 3, NA, 2, 8, 4)
.hm_try("normalize_vec_z",       normalize_vec(x_vec, "z"))
.hm_try("normalize_vec_range",   normalize_vec(x_vec, "range", feature_range = c(0, 1)))
.hm_try("normalize_vec_robust",  normalize_vec(x_vec, "robust"))
.hm_try("normalize_vec_inverse", normalize_vec(x_vec, "inverse"))

cat("\n--- N4. validate_inputs() ---\n")
.hm_try("validate_inputs", validate_inputs(dat_full,
  col_map = list(G0 = "pglu0", I0 = "insu0"),
  fun_name = "test_call", required_keys = c("G0","I0")))

cat("\n--- N5. marker_summary() ---\n")
.hm_try("marker_summary", {
  tmp <- lipid_markers(dat_full,
    col_map = list(TC = "chol", HDL_c = "hdlc", TG = "trig", LDL_c = "ldl"),
    verbose = FALSE)
  marker_summary(tmp, verbose = FALSE)
})

cat("\n--- N6. impute_missing() ---\n")
dat_na <- dat_full
dat_na$pglu0[sample(n, 50)] <- NA
dat_na$bmi[sample(n, 30)]   <- NA

.hm_try("impute_missing_mean",   impute_missing(dat_na, method = "mean",   cols = c("pglu0","bmi"), verbose = FALSE))
.hm_try("impute_missing_median", impute_missing(dat_na, method = "median", cols = c("pglu0","bmi"), verbose = FALSE))
.hm_try("impute_missing_const",  impute_missing(dat_na, method = "constant", constant = 0, cols = "pglu0", verbose = FALSE))

if (requireNamespace("mice", quietly = TRUE)) {
  .hm_try("impute_mice", impute_mice(dat_na[, c("pglu0","bmi","age")], m = 3, verbose = FALSE))
} else {
  message("mice not installed -- skipping impute_mice()")
}

if (requireNamespace("missForest", quietly = TRUE)) {
  .hm_try("impute_missforest", impute_missforest(dat_na[, c("pglu0","bmi","age")], verbose = FALSE))
} else {
  message("missForest not installed -- skipping impute_missforest()")
}


## =============================================================================
## SECTION O: NA & ERROR HANDLING
## =============================================================================

cat("\n--- O1. NA action modes ---\n")
dat_miss <- dat_full
dat_miss$pglu0[1:5] <- NA

.hm_try("na_action_warn", fasting_is(dat_miss,
  col_map = list(G0 = "pglu0", I0 = "insu0"), na_action = "warn", verbose = FALSE))
.hm_try("na_action_omit", fasting_is(dat_miss,
  col_map = list(G0 = "pglu0", I0 = "insu0"), na_action = "omit", verbose = FALSE))
.hm_try("na_action_error_caught", tryCatch(
  fasting_is(dat_miss, col_map = list(G0 = "pglu0", I0 = "insu0"), na_action = "error"),
  error = function(e) TRUE))

cat("\n--- O2. Expected errors ---\n")
.hm_try("error_missing_col", tryCatch(
  fasting_is(dat_full, col_map = list(G0 = "NoSuchCol", I0 = "insu0")),
  error = function(e) TRUE))
.hm_try("error_bad_data_type", tryCatch(
  lipid_markers("not_a_dataframe", col_map = list(TC = "chol", HDL_c = "hdlc", TG = "trig")),
  error = function(e) TRUE))
.hm_try("error_bad_normalize", tryCatch(
  fasting_is(dat_full, col_map = list(G0 = "pglu0", I0 = "insu0"), normalize = "bogus"),
  error = function(e) TRUE))


## =============================================================================
## FINAL STATUS SUMMARY
## =============================================================================

cat("\n\n========= TEST RESULTS =========\n")

status_df <- data.frame(
  Function = names(.test_results),
  Status   = unlist(.test_results),
  stringsAsFactors = FALSE
)
rownames(status_df) <- NULL

n_pass <- sum(status_df$Status == "PASS")
n_fail <- sum(status_df$Status == "FAIL")

cat(sprintf("PASS: %d   FAIL: %d   TOTAL: %d\n\n", n_pass, n_fail, nrow(status_df)))

if (n_fail > 0) {
  cat("Failed tests:\n")
  print(status_df[status_df$Status == "FAIL", , drop = FALSE])
} else {
  cat("All tests passed!\n")
}

cat("\nFull results:\n")
print(status_df)
cat("\n========= DONE =========\n")
