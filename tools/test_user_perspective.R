## =============================================================================
## HealthMarkers ? User Perspective Test Script
## =============================================================================
## Run every exported function with the built-in simulated dataset.
## No real data or internet connection required.
##
## Usage (pick one):
  install.packages(".", repos = NULL, type = "source")  # from package root
##   devtools::load_all()                                   # developer shortcut
## =============================================================================

 devtools::load_all(quiet = TRUE)

## -- LOAD DATA ----------------------------------------------------------------

df <- readRDS(system.file("extdata", "simulated_hm_data.rds", package = "HealthMarkers"))
## During development (from package root):
## df <- readRDS("inst/extdata/simulated_hm_data.rds")

cat("Simulated data:", nrow(df), "rows x", ncol(df), "cols\n\n")

## Derived column used by frax_score
df$bmd_t <- round((df$BMD - df$BMD_ref_mean) / df$BMD_ref_sd, 2)


## =============================================================================
## A.  FULL DISPATCHER
## =============================================================================

res_all <- all_health_markers(df, verbose = TRUE)
health_summary(res_all)
marker_summary(res_all, verbose = TRUE)


## =============================================================================
## B.  INSULIN SENSITIVITY
## =============================================================================

fasting_is(df, verbose = TRUE)
fasting_is(df, col_map = list(G0 = "G0", I0 = "I0"), normalize = "z", verbose = TRUE)

ogtt_is(df,
  col_map = list(G0 = "G0", G30 = "G30", G120 = "G120",
                 I0 = "I0", I30 = "I30", I120 = "I120",
                 weight = "weight", bmi = "bmi", age = "age", sex = "sex_c"),
  verbose = TRUE)

adipo_is(df, verbose = TRUE)

tracer_dxa_is(df,
  col_map = list(I0 = "I0", rate_palmitate = "rate_palmitate",
                 rate_glycerol = "rate_glycerol", fat_mass = "fat_mass",
                 weight = "weight", HDL_c = "HDL_c", bmi = "bmi"),
  verbose = TRUE)

all_insulin_indices(df, mode = "both", verbose = TRUE)


## =============================================================================
## C.  GLYCEMIC & METABOLIC
## =============================================================================

glycemic_markers(df, verbose = TRUE)

metabolic_markers(df, verbose = TRUE)

metabolic_risk_features(df, verbose = TRUE)

metss(df, verbose = TRUE)


## =============================================================================
## D.  LIPID & CARDIOVASCULAR
## =============================================================================

lipid_markers(df,
  col_map = list(TC = "TC", HDL_c = "HDL_c", TG = "TG",
                 LDL_c = "LDL_c", ApoB = "ApoB"),
  verbose = TRUE)

atherogenic_indices(df,
  col_map = list(TG = "TG", HDL_c = "HDL_c", TC = "TC", LDL_c = "LDL_c"),
  verbose = TRUE)

cvd_marker_aip(df,
  col_map = list(TG = "TG", HDL_c = "HDL_c"), verbose = TRUE)

cvd_marker_ldl_particle_number(df,
  col_map = list(ApoB = "ApoB"), verbose = TRUE)

cvd_risk(df, model = "AIP", col_map = list(TG = "TG", HDL_c = "HDL_c"))

cvd_risk_scorescvd(df,
  col_map = list(age = "age", sex = "sex_c", TC = "TC",
                 HDL_c = "HDL_c", SBP = "sbp", smoker = "smoker"),
  verbose = TRUE)

if (requireNamespace("PooledCohort", quietly = TRUE)) {
  cvd_risk_ascvd(df, year = 10, verbose = TRUE)
  cvd_risk_stroke(df, verbose = TRUE)
} else message("PooledCohort not installed -- skipping cvd_risk_ascvd(), cvd_risk_stroke()")

if (requireNamespace("QRISK3", quietly = TRUE) || requireNamespace("qrisk3", quietly = TRUE)) {
  cvd_risk_qrisk3(df, verbose = TRUE)
} else message("qrisk3 not installed -- skipping cvd_risk_qrisk3()")

allostatic_load(df,
  thresholds = list(sbp = 130, dbp = 85, CRP = 3.0, BMI = 30,
                    waist = 102, TG = 1.7, HDL_c = 1.0),
  na_action = "keep", verbose = TRUE)


## =============================================================================
## E.  LIVER & RENAL
## =============================================================================

liver_markers(df, verbose = TRUE)

liver_fat_markers(df, verbose = TRUE)

renal_markers(df,
  col_map = list(creatinine = "creatinine", age = "age",
                 sex = "sex_c", BUN = "BUN", race = "race"),
  verbose = TRUE)

urine_markers(df, verbose = TRUE)
urine_markers(df, na_action = "omit", verbose = TRUE)

ckd_stage(df, verbose = TRUE)

kidney_failure_risk(df,
  col_map = list(age = "age", sex = "sex_c", eGFR = "eGFR", UACR = "UACR"),
  verbose = TRUE)


## =============================================================================
## F.  INFLAMMATORY, OXIDATIVE & SPECIALTY BIOMARKERS
## =============================================================================

inflammatory_markers(df, panel = "both", verbose = TRUE)

iAge(df, verbose = TRUE)

oxidative_markers(df, verbose = TRUE)

kyn_trp_ratio(df, verbose = TRUE)

nfl_marker(df, verbose = TRUE)


## =============================================================================
## G.  HORMONES, VITAMINS & NUTRIENTS
## =============================================================================

hormone_markers(df, verbose = TRUE)

vitamin_markers(df, verbose = TRUE)

vitamin_d_status(df, verbose = TRUE)

nutrient_markers(df, verbose = TRUE)

corrected_calcium(df, units = "auto", verbose = TRUE)


## =============================================================================
## H.  BONE & BODY COMPOSITION
## =============================================================================

bone_markers(df,
  col_map = list(age = "age", weight = "weight", height = "height_m",
                 ALM = "ALM", BMD = "BMD",
                 BMD_ref_mean = "BMD_ref_mean", BMD_ref_sd = "BMD_ref_sd",
                 TBS = "TBS", PINP = "PINP", CTX = "CTX",
                 BSAP = "BSAP", Osteocalcin = "Osteocalcin"),
  verbose = TRUE)

alm_bmi_index(df, verbose = TRUE)

frax_score(df, verbose = TRUE)


## =============================================================================
## I.  OBESITY & ADIPOSITY
## =============================================================================

obesity_indices(df,
  weight = weight, height = height_m, waist = waist, hip = hip, sex = sex_c,
  weight_unit = "kg", height_unit = "m",
  include_RFM = TRUE, adjust_WHR = TRUE, verbose = TRUE)

bmi_ref <- lapply(setNames(as.list(seq(18, 80)), as.character(18:80)),
                  function(x) c(mean = 25.0, sd = 4.5))
adiposity_sds(df,
  col_map = list(value = "bmi", age = "age"),
  ref = bmi_ref, na_action = "keep", verbose = TRUE)

bmi_ref_strat <- list(
  M = list(bmi = c(mean = 25.5, sd = 4.2)),
  F = list(bmi = c(mean = 24.5, sd = 4.8))
)
adiposity_sds_strat(df,
  col_map = list(sex = "sex_c", vars = list(bmi = "bmi")),
  ref = bmi_ref_strat, na_action = "keep", verbose = TRUE)

sds_ref <- list(bmi = c(mean = 25.0, sd = 4.5), waist = c(mean = 88.0, sd = 12.0))
calc_sds(df,
  vars = c("bmi", "waist"), ref = sds_ref,
  na_strategy = "keep", extreme_strategy = "cap",
  return = "data", verbose = TRUE)


## =============================================================================
## J.  PULMONARY
## =============================================================================

spirometry_markers(df, verbose = TRUE)

if (requireNamespace("rspiro", quietly = TRUE)) {
  pulmo_markers(df, equation = "GLI", verbose = TRUE)
} else message("rspiro not installed -- skipping pulmo_markers()")

bode_index(df, verbose = TRUE)


## =============================================================================
## K.  BIOFLUIDS (SALIVA & SWEAT)
## =============================================================================

saliva_markers(df, verbose = TRUE)

sweat_markers(df,
  col_map = list(sweat_chloride = "sweat_chloride", sweat_Na = "sweat_Na",
                 sweat_K = "sweat_K", sweat_lactate = "sweat_lactate",
                 weight_before = "weight_before", weight_after = "weight_after",
                 duration = "duration", body_surface_area = "body_surface_area"),
  verbose = TRUE)


## =============================================================================
## L.  FRAILTY, SARCOPENIA & COMORBIDITY
## =============================================================================

if (requireNamespace("di", quietly = TRUE)) {
  deficit_cols <- sprintf("d%02d", 1:20)
  frailty_index(df, cols = deficit_cols, age = "age",
    rescale = TRUE, na_action = "keep", return = "data", verbose = TRUE)
  plot_frailty_age(df, cols = deficit_cols, age = "age", bins = 5)
} else message("di not installed -- skipping frailty_index(), plot_frailty_age()")

sarc_f_score(df,
  col_map = list(strength = "strength", walking = "walking",
                 chair = "chair", stairs = "stairs", falls = "falls"),
  verbose = TRUE)

charlson_index(df,
  col_map = list(
    mi = "mi",    chf = "chf",    pvd = "pvd",    stroke = "stroke",
    dementia = "dementia",        copd = "copd",  rheum = "rheum",
    ulcer = "ulcer",              mild_liver = "mild_liver",
    diabetes = "diabetes",        diab_comp = "diab_comp",
    hemiplegia = "hemiplegia",    renal = "renal",
    cancer = "cancer",            leukemia = "leukemia",
    lymphoma = "lymphoma",        sev_liver = "sev_liver",
    metastatic_cancer = "metastatic_cancer", hiv = "hiv"),
  verbose = TRUE)


## =============================================================================
## M.  PSYCHIATRIC SCORES
## =============================================================================

phq9_score(df, verbose = TRUE)
gad7_score(df, verbose = TRUE)
who5_score(df, verbose = TRUE)
k6_score(df, verbose = TRUE)
k10_score(df, verbose = TRUE)
ghq12_score(df, method = "likert", verbose = TRUE)
ghq12_score(df, method = "binary", verbose = TRUE)
isi_score(df, verbose = TRUE)
mdq_score(df, verbose = TRUE)
asrs_score(df, verbose = TRUE)

bis_score(df,
  key = list(items = sprintf("bis_%02d", 1:30), min_val = 1, max_val = 4,
             subscales = list(attentional = 1:8, motor = 9:19, nonplanning = 20:30)),
  verbose = TRUE)

spq_score(df,
  key = list(items = sprintf("spq_%02d", 1:74), min_val = 0, max_val = 1),
  verbose = TRUE)

cognitive_score(df,
  col_map = list(tasks = list(
    memory    = "cog_memory",
    attention = "cog_attention",
    executive = "cog_executive")),
  method = "z_mean", verbose = TRUE)

psych_dx_flags(df,
  col_map = list(dx = list(
    mdd = "dx_mdd", anxiety = "dx_anxiety",
    adhd = "dx_adhd", bipolar = "dx_bipolar")),
  verbose = TRUE)

psych_med_flags(df,
  col_map = list(med = list(
    ssri            = "med_ssri",
    snri            = "med_snri",
    antipsychotic   = "med_antipsychotic",
    mood_stabilizer = "med_mood_stabilizer",
    anxiolytic      = "med_anxiolytic")),
  verbose = TRUE)

psych_markers(df,
  which = c("phq9", "gad7", "who5", "k6", "k10",
            "ghq12_likert", "isi", "mdq", "asrs"),
  verbose = TRUE)


## =============================================================================
## N.  UTILITIES
## =============================================================================

hm_col_report(df, verbose = TRUE)

infer_cols(df,
  map      = list(G0 = NULL, I0 = NULL, TG = NULL,
                  HDL_c = NULL, BMI = NULL, waist = NULL),
  prefer   = list(TG = "TG", HDL_c = "HDL_c", BMI = "bmi"),
  strategy = "prefer", verbose = TRUE, return = "map")

x <- c(1, 5, 3, NA, 2, 8, 4)
normalize_vec(x, "z")
normalize_vec(x, "range", feature_range = c(0, 1))
normalize_vec(x, "robust")
normalize_vec(x, "inverse")

validate_inputs(df,
  col_map = list(G0 = "G0", I0 = "I0"),
  fun_name = "test_call", required_keys = c("G0", "I0"))

marker_summary(
  lipid_markers(df, col_map = list(TC = "TC", HDL_c = "HDL_c",
                                   TG = "TG", LDL_c = "LDL_c"),
                verbose = TRUE),
  verbose = TRUE)

## Imputation
df_na <- df
df_na$G0[sample(nrow(df), 50)]  <- NA
df_na$bmi[sample(nrow(df), 30)] <- NA

impute_missing(df_na, method = "mean",     cols = c("G0", "bmi"), verbose = TRUE)
impute_missing(df_na, method = "median",   cols = c("G0", "bmi"), verbose = TRUE)
impute_missing(df_na, method = "constant", cols = "G0", constant = 0, verbose = TRUE)

if (requireNamespace("mice", quietly = TRUE)) {
  impute_mice(df_na[, c("G0", "bmi", "age")], m = 3, verbose = TRUE)
} else message("mice not installed -- skipping impute_mice()")

if (requireNamespace("missForest", quietly = TRUE)) {
  impute_missforest(df_na[, c("G0", "bmi", "age")], verbose = TRUE)
} else message("missForest not installed -- skipping impute_missforest()")


## =============================================================================
## O.  NA & ERROR HANDLING
## =============================================================================

df_miss <- df
df_miss$G0[1:5] <- NA

fasting_is(df_miss, col_map = list(G0 = "G0", I0 = "I0"), na_action = "warn",  verbose = TRUE)
fasting_is(df_miss, col_map = list(G0 = "G0", I0 = "I0"), na_action = "omit",  verbose = TRUE)
tryCatch(
  fasting_is(df_miss, col_map = list(G0 = "G0", I0 = "I0"), na_action = "error"),
  error = function(e) message("Expected error: ", conditionMessage(e)))

tryCatch(
  fasting_is(df, col_map = list(G0 = "NoSuchCol", I0 = "I0")),
  error = function(e) message("Expected error: ", conditionMessage(e)))

tryCatch(
  lipid_markers("not_a_dataframe",
                col_map = list(TC = "TC", HDL_c = "HDL_c", TG = "TG")),
  error = function(e) message("Expected error: ", conditionMessage(e)))


cat("\n========= DONE =========\n")
