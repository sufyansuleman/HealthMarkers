# Package index

## Dispatcher functions

Compute many marker groups at once and return a single wide tibble.

- [`all_health_markers()`](https://sufyansuleman.github.io/HealthMarkers/reference/all_health_markers.md)
  : Compute all available HealthMarkers categories
- [`all_insulin_indices()`](https://sufyansuleman.github.io/HealthMarkers/reference/all_insulin_indices.md)
  : Compute insulin sensitivity/resistance panels (fasting, OGTT,
  adipose, tracer/DXA)
- [`metabolic_markers()`](https://sufyansuleman.github.io/HealthMarkers/reference/metabolic_markers.md)
  : Aggregate selected metabolic marker groups

## Insulin sensitivity

Fasting, OGTT, adipose-tissue, tracer and DXA-based indices.

- [`fasting_is()`](https://sufyansuleman.github.io/HealthMarkers/reference/fasting_is.md)
  : Calculate fasting-based insulin sensitivity indices
- [`ogtt_is()`](https://sufyansuleman.github.io/HealthMarkers/reference/ogtt_is.md)
  : Calculate OGTT-based insulin sensitivity indices
- [`adipo_is()`](https://sufyansuleman.github.io/HealthMarkers/reference/adipo_is.md)
  : Adipose insulin sensitivity indices (QUICKI, VAI, LAP, TyG, TG/HDL,
  Belfiore)
- [`tracer_dxa_is()`](https://sufyansuleman.github.io/HealthMarkers/reference/tracer_dxa_is.md)
  : Compute tracer/DXA-based insulin sensitivity indices

## Glycaemic & lipid markers

- [`glycemic_markers()`](https://sufyansuleman.github.io/HealthMarkers/reference/glycemic_markers.md)
  : Calculate glycemic-, C-peptide-, and additional metabolic markers
- [`lipid_markers()`](https://sufyansuleman.github.io/HealthMarkers/reference/lipid_markers.md)
  : Calculate lipid-panel markers, Visceral Adiposity Index (VAI), Lipid
  Accumulation Product (LAP), and TyG-BMI index
- [`atherogenic_indices()`](https://sufyansuleman.github.io/HealthMarkers/reference/atherogenic_indices.md)
  : Compute atherogenic indices
- [`cvd_marker_aip()`](https://sufyansuleman.github.io/HealthMarkers/reference/cvd_marker_aip.md)
  : Atherogenic Index of Plasma (AIP)
- [`cvd_marker_ldl_particle_number()`](https://sufyansuleman.github.io/HealthMarkers/reference/cvd_marker_ldl_particle_number.md)
  : LDL Particle Number Estimate (via ApoB)

## Cardiovascular risk

- [`cvd_risk()`](https://sufyansuleman.github.io/HealthMarkers/reference/cvd_risk.md)
  : Compute cardiovascular risk or marker by selected model
- [`cvd_risk_ascvd()`](https://sufyansuleman.github.io/HealthMarkers/reference/cvd_risk_ascvd.md)
  : ASCVD risk (ACC/AHA Pooled Cohort Equations)
- [`cvd_risk_qrisk3()`](https://sufyansuleman.github.io/HealthMarkers/reference/cvd_risk_qrisk3.md)
  : QRISK3 10-year risk (UK QRISK3-2017)
- [`cvd_risk_scorescvd()`](https://sufyansuleman.github.io/HealthMarkers/reference/cvd_risk_scorescvd.md)
  : RiskScorescvd calculator
- [`cvd_risk_stroke()`](https://sufyansuleman.github.io/HealthMarkers/reference/cvd_risk_stroke.md)
  : Stroke 10-year risk

## Liver function

- [`liver_markers()`](https://sufyansuleman.github.io/HealthMarkers/reference/liver_markers.md)
  : Compute liver-related indices (FLI, NFS, APRI, FIB-4, BARD, ALBI,
  MELD-XI) with validation and diagnostics
- [`liver_fat_markers()`](https://sufyansuleman.github.io/HealthMarkers/reference/liver_fat_markers.md)
  : Liver fat surrogates: HSI and NAFLD Liver Fat Score

## Metabolic syndrome & allostatic load

- [`metss()`](https://sufyansuleman.github.io/HealthMarkers/reference/metss.md)
  : Metabolic Syndrome Severity Score (MetSSS)
- [`metabolic_risk_features()`](https://sufyansuleman.github.io/HealthMarkers/reference/metabolic_risk_features.md)
  : Calculate metabolic risk feature flags (pediatric-friendly
  thresholds)
- [`allostatic_load()`](https://sufyansuleman.github.io/HealthMarkers/reference/allostatic_load.md)
  : Allostatic Load Index

## Renal / CKD

- [`kidney_failure_risk()`](https://sufyansuleman.github.io/HealthMarkers/reference/kidney_failure_risk.md)
  : Kidney Failure Risk Equation (KFRE, 2- and 5-year risk)
- [`renal_markers()`](https://sufyansuleman.github.io/HealthMarkers/reference/renal_markers.md)
  : Calculate a Suite of Renal Function, Injury, and Excretion Markers
- [`ckd_stage()`](https://sufyansuleman.github.io/HealthMarkers/reference/ckd_stage.md)
  : CKD staging (GFR and albuminuria) and KDIGO risk
- [`urine_markers()`](https://sufyansuleman.github.io/HealthMarkers/reference/urine_markers.md)
  : Calculate urine-only renal and tubular markers (research-ready)

## Pulmonary function

- [`pulmo_markers()`](https://sufyansuleman.github.io/HealthMarkers/reference/pulmo_markers.md)
  : Calculate pulmonary function markers (FEV1/FVC, z-scores, percent
  predicted, LLN, etc.)
- [`spirometry_markers()`](https://sufyansuleman.github.io/HealthMarkers/reference/spirometry_markers.md)
  : Spirometry markers: FEV1/FVC, LLN-based obstruction, GOLD grade,
  bronchodilator response
- [`bode_index()`](https://sufyansuleman.github.io/HealthMarkers/reference/bode_index.md)
  : BODE Index (BMI, Obstruction, Dyspnea, Exercise capacity)

## Inflammatory & aging markers

- [`inflammatory_markers()`](https://sufyansuleman.github.io/HealthMarkers/reference/inflammatory_markers.md)
  : Compute inflammatory indices (classic, eosinophil, or both)
- [`iAge()`](https://sufyansuleman.github.io/HealthMarkers/reference/iAge.md)
  : Compute a simplified Inflammatory Age Index (iAge) with QA and
  verbose summaries
- [`oxidative_markers()`](https://sufyansuleman.github.io/HealthMarkers/reference/oxidative_markers.md)
  : Oxidative stress markers
- [`kyn_trp_ratio()`](https://sufyansuleman.github.io/HealthMarkers/reference/kyn_trp_ratio.md)
  : Kynurenine/Tryptophan Ratio (KTR)

## Hormonal markers

- [`hormone_markers()`](https://sufyansuleman.github.io/HealthMarkers/reference/hormone_markers.md)
  : Compute a suite of hormone ratio markers with QA and verbose
  summaries

## Body composition & anthropometry

- [`obesity_indices()`](https://sufyansuleman.github.io/HealthMarkers/reference/obesity_indices.md)
  : Compute anthropometric obesity & adiposity indices
- [`adiposity_sds()`](https://sufyansuleman.github.io/HealthMarkers/reference/adiposity_sds.md)
  : Calculate standardized scores (SDS) for adiposity measures
- [`adiposity_sds_strat()`](https://sufyansuleman.github.io/HealthMarkers/reference/adiposity_sds_strat.md)
  : Compute sex-stratified standardized scores (SDS) for adiposity
  measures
- [`alm_bmi_index()`](https://sufyansuleman.github.io/HealthMarkers/reference/alm_bmi_index.md)
  : Appendicular Lean Mass to BMI Index
- [`calc_sds()`](https://sufyansuleman.github.io/HealthMarkers/reference/calc_sds.md)
  : Calculate Standard Deviation Scores (SDS; z-scores) for health
  markers

## Bone & fracture risk

- [`bone_markers()`](https://sufyansuleman.github.io/HealthMarkers/reference/bone_markers.md)
  : Compute Bone Health & Body-Composition Markers (HM-CS v3)
- [`frax_score()`](https://sufyansuleman.github.io/HealthMarkers/reference/frax_score.md)
  : FRAX 10-Year Fracture Risk Score (simplified placeholder)

## Frailty & comorbidity

- [`frailty_index()`](https://sufyansuleman.github.io/HealthMarkers/reference/frailty_index.md)
  : Compute Frailty (Deficit) Index using di::di with QA and verbose
  summaries
- [`charlson_index()`](https://sufyansuleman.github.io/HealthMarkers/reference/charlson_index.md)
  : Charlson Comorbidity Index (CCI)
- [`sarc_f_score()`](https://sufyansuleman.github.io/HealthMarkers/reference/sarc_f_score.md)
  : SARC-F Sarcopenia Screening Score

## Vitamins & nutrients

- [`vitamin_markers()`](https://sufyansuleman.github.io/HealthMarkers/reference/vitamin_markers.md)
  : Compute composite vitamin and endocrine marker ratios and z-scores
- [`vitamin_d_status()`](https://sufyansuleman.github.io/HealthMarkers/reference/vitamin_d_status.md)
  : Vitamin D Status Category
- [`nutrient_markers()`](https://sufyansuleman.github.io/HealthMarkers/reference/nutrient_markers.md)
  : Compute a Suite of Nutrient-Based Health Markers

## Alternate biofluids

- [`saliva_markers()`](https://sufyansuleman.github.io/HealthMarkers/reference/saliva_markers.md)
  : Calculate saliva-based stress & glycemic markers
- [`sweat_markers()`](https://sufyansuleman.github.io/HealthMarkers/reference/sweat_markers.md)
  : Calculate sweat-based ionic & metabolic markers

## Neurological & calcium

- [`nfl_marker()`](https://sufyansuleman.github.io/HealthMarkers/reference/nfl_marker.md)
  : Neurofilament Light Chain (NfL) Biomarker
- [`corrected_calcium()`](https://sufyansuleman.github.io/HealthMarkers/reference/corrected_calcium.md)
  : Albumin-Corrected Calcium

## Psychiatric rating scales

- [`psych_markers()`](https://sufyansuleman.github.io/HealthMarkers/reference/psych_markers.md)
  : Psychometric markers dispatcher
- [`phq9_score()`](https://sufyansuleman.github.io/HealthMarkers/reference/phq9_score.md)
  : PHQ-9 / PHQ-8 scoring
- [`gad7_score()`](https://sufyansuleman.github.io/HealthMarkers/reference/gad7_score.md)
  : GAD-7 scoring
- [`k6_score()`](https://sufyansuleman.github.io/HealthMarkers/reference/k6_score.md)
  : K6 scoring
- [`k10_score()`](https://sufyansuleman.github.io/HealthMarkers/reference/k10_score.md)
  : K10 scoring
- [`ghq12_score()`](https://sufyansuleman.github.io/HealthMarkers/reference/ghq12_score.md)
  : GHQ-12 scoring (Likert or binary)
- [`who5_score()`](https://sufyansuleman.github.io/HealthMarkers/reference/who5_score.md)
  : WHO-5 scoring
- [`isi_score()`](https://sufyansuleman.github.io/HealthMarkers/reference/isi_score.md)
  : Insomnia Severity Index (ISI) scoring
- [`mdq_score()`](https://sufyansuleman.github.io/HealthMarkers/reference/mdq_score.md)
  : Mood Disorder Questionnaire scoring
- [`asrs_score()`](https://sufyansuleman.github.io/HealthMarkers/reference/asrs_score.md)
  : Adult ADHD Self-Report Scale scoring
- [`bis_score()`](https://sufyansuleman.github.io/HealthMarkers/reference/bis_score.md)
  : Barratt Impulsiveness Scale (key-driven)
- [`spq_score()`](https://sufyansuleman.github.io/HealthMarkers/reference/spq_score.md)
  : Schizotypal Personality Questionnaire (key-driven)
- [`cognitive_score()`](https://sufyansuleman.github.io/HealthMarkers/reference/cognitive_score.md)
  : Cognitive composite (z-mean or PCA1)
- [`psych_dx_flags()`](https://sufyansuleman.github.io/HealthMarkers/reference/psych_dx_flags.md)
  : Psychiatric diagnosis flags aggregator
- [`psych_med_flags()`](https://sufyansuleman.github.io/HealthMarkers/reference/psych_med_flags.md)
  : Psychiatric medication flags aggregator

## Utilities

- [`hm_col_report()`](https://sufyansuleman.github.io/HealthMarkers/reference/hm_col_report.md)
  : Diagnose and report column name mapping for your data
- [`infer_cols()`](https://sufyansuleman.github.io/HealthMarkers/reference/infer_cols.md)
  [`hm_infer_cols()`](https://sufyansuleman.github.io/HealthMarkers/reference/infer_cols.md)
  : Infer column names from user data based on flexible patterns, with
  logging
- [`impute_missing()`](https://sufyansuleman.github.io/HealthMarkers/reference/impute_missing.md)
  : Impute missing values in a data.frame or tibble (simple,
  column-wise)
- [`impute_mice()`](https://sufyansuleman.github.io/HealthMarkers/reference/impute_mice.md)
  : Impute missing values via Multiple Imputation (mice)
- [`impute_missforest()`](https://sufyansuleman.github.io/HealthMarkers/reference/impute_missforest.md)
  : Impute missing values via random forest (missForest)
- [`normalize_vec()`](https://sufyansuleman.github.io/HealthMarkers/reference/normalize_vec.md)
  : Normalize a numeric vector
- [`validate_inputs()`](https://sufyansuleman.github.io/HealthMarkers/reference/validate_inputs.md)
  : Validate required inputs for a calling function
- [`marker_summary()`](https://sufyansuleman.github.io/HealthMarkers/reference/marker_summary.md)
  : Summarize marker outputs
- [`health_summary()`](https://sufyansuleman.github.io/HealthMarkers/reference/health_summary.md)
  : Summarize selected numeric marker columns
- [`plot_frailty_age()`](https://sufyansuleman.github.io/HealthMarkers/reference/plot_frailty_age.md)
  : Plot FI vs age (convenience wrapper)
