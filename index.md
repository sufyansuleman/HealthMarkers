# HealthMarkers

- [HealthMarkers](#healthmarkers)
  - [Installation](#installation)
  - [Package overview](#package-overview)
  - [How to use HealthMarkers](#how-to-use-healthmarkers)
  - [Function-by-function guide](#function-by-function-guide)
  - [Column mapping](#column-mapping)
  - [Handle missing data before
    computing](#handle-missing-data-before-computing)
  - [Verbose diagnostics](#verbose-diagnostics)
  - [Further information](#further-information)
  - [Vignettes](#vignettes)
  - [Contributing](#contributing)
  - [Citation](#citation)
  - [License](#license)

# HealthMarkers

**HealthMarkers** is a comprehensive R toolkit for computing,
standardising, and summarising clinical and research biomarkers from
routine laboratory and phenotypic data. It provides over 50 specialist
functions covering insulin sensitivity indices, cardiovascular risk
scores, inflammatory aging clocks, frailty indices, psychiatric rating
scales, alternate-biofluid panels, and much more. All accessible through
a unified dispatcher,
[`all_health_markers()`](https://sufyansuleman.github.io/HealthMarkers/reference/all_health_markers.md).

> **Full documentation, function reference, and vignettes** are
> available at the package website:  
> <https://sufyansuleman.github.io/HealthMarkers/>

- **One interface, many markers.** A single
  [`all_health_markers()`](https://sufyansuleman.github.io/HealthMarkers/reference/all_health_markers.md)
  call returns glycaemic, lipid, liver, renal, pulmonary, inflammatory,
  hormonal, bone, psychiatric, and nutritional markers as one wide
  tibble.
- **Safe by default.** NA handling, input validation, column-name
  inference, and range-capping are built in. Failed marker groups are
  skipped with a warning, never crashing your pipeline.
- **Reproducible.** Explicit `col_map` arguments map *your* column names
  to expected internal keys, no silent renaming.
- **Reference-backed.** Every function cites the primary paper. Full
  bibliography in `inst/REFERENCES.bib`. 46 vignettes with worked
  clinical examples are included.

------------------------------------------------------------------------

## Installation

``` r
# From CRAN
install.packages("HealthMarkers")

# Development version from GitHub
remotes::install_github("sufyansuleman/HealthMarkers")
```

Optional packages unlock additional marker groups:

``` r
install.packages(c("CVrisk", "rspiro", "PooledCohort", "QRISK3",
                   "RiskScorescvd", "di", "mice", "missForest"))
```

When optional packages are absent, their dependent groups are skipped
safely; running with `verbose = TRUE` shows which groups were computed
and which were skipped (and why) in the summary message.

------------------------------------------------------------------------

## Package overview

| Domain                    | Functions                                                                                                                                                                                                                                                                                                                                                                                                                                                                                           | Outputs                                                            |
|---------------------------|-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------|--------------------------------------------------------------------|
| **Insulin sensitivity**   | [`fasting_is()`](https://sufyansuleman.github.io/HealthMarkers/reference/fasting_is.md), [`ogtt_is()`](https://sufyansuleman.github.io/HealthMarkers/reference/ogtt_is.md), [`adipo_is()`](https://sufyansuleman.github.io/HealthMarkers/reference/adipo_is.md), [`tracer_dxa_is()`](https://sufyansuleman.github.io/HealthMarkers/reference/tracer_dxa_is.md), [`all_insulin_indices()`](https://sufyansuleman.github.io/HealthMarkers/reference/all_insulin_indices.md)                           | HOMA-IR, QUICKI, Matsuda, Stumvoll, Gutt, SPISE, LIRI, 40+ indices |
| **Glycaemic**             | [`glycemic_markers()`](https://sufyansuleman.github.io/HealthMarkers/reference/glycemic_markers.md)                                                                                                                                                                                                                                                                                                                                                                                                 | TyG index, METS-IR, LAR, ASI, HOMA-CP, diabetes risk flags         |
| **Lipid & atherogenic**   | [`lipid_markers()`](https://sufyansuleman.github.io/HealthMarkers/reference/lipid_markers.md), [`atherogenic_indices()`](https://sufyansuleman.github.io/HealthMarkers/reference/atherogenic_indices.md), [`cvd_marker_aip()`](https://sufyansuleman.github.io/HealthMarkers/reference/cvd_marker_aip.md), [`cvd_marker_ldl_particle_number()`](https://sufyansuleman.github.io/HealthMarkers/reference/cvd_marker_ldl_particle_number.md)                                                          | TC/HDL, AIP, CRI-I/II, Castelli, LDL particle number               |
| **Liver**                 | [`liver_markers()`](https://sufyansuleman.github.io/HealthMarkers/reference/liver_markers.md), [`liver_fat_markers()`](https://sufyansuleman.github.io/HealthMarkers/reference/liver_fat_markers.md)                                                                                                                                                                                                                                                                                                | FLI, NFS, FIB-4, APRI, BARD, ALBI, MELD-XI, HSI, LAP               |
| **Metabolic syndrome**    | [`metss()`](https://sufyansuleman.github.io/HealthMarkers/reference/metss.md), [`metabolic_risk_features()`](https://sufyansuleman.github.io/HealthMarkers/reference/metabolic_risk_features.md), [`allostatic_load()`](https://sufyansuleman.github.io/HealthMarkers/reference/allostatic_load.md)                                                                                                                                                                                                 | MetS severity, component flags, allostatic load index              |
| **Cardiovascular risk**   | [`cvd_risk()`](https://sufyansuleman.github.io/HealthMarkers/reference/cvd_risk.md), [`cvd_risk_ascvd()`](https://sufyansuleman.github.io/HealthMarkers/reference/cvd_risk_ascvd.md), [`cvd_risk_qrisk3()`](https://sufyansuleman.github.io/HealthMarkers/reference/cvd_risk_qrisk3.md), [`cvd_risk_scorescvd()`](https://sufyansuleman.github.io/HealthMarkers/reference/cvd_risk_scorescvd.md), [`cvd_risk_stroke()`](https://sufyansuleman.github.io/HealthMarkers/reference/cvd_risk_stroke.md) | ASCVD (PCE), QRISK3, SCORE2/SCORE2-OP, 10-yr stroke risk           |
| **Renal / CKD**           | [`kidney_failure_risk()`](https://sufyansuleman.github.io/HealthMarkers/reference/kidney_failure_risk.md), [`renal_markers()`](https://sufyansuleman.github.io/HealthMarkers/reference/renal_markers.md), [`ckd_stage()`](https://sufyansuleman.github.io/HealthMarkers/reference/ckd_stage.md), [`urine_markers()`](https://sufyansuleman.github.io/HealthMarkers/reference/urine_markers.md)                                                                                                      | KFRE 2-yr/5-yr, eGFR (CKD-EPI), CKD stage, UACR, FE-Urea           |
| **Pulmonary**             | [`pulmo_markers()`](https://sufyansuleman.github.io/HealthMarkers/reference/pulmo_markers.md), [`spirometry_markers()`](https://sufyansuleman.github.io/HealthMarkers/reference/spirometry_markers.md), [`bode_index()`](https://sufyansuleman.github.io/HealthMarkers/reference/bode_index.md)                                                                                                                                                                                                     | FEV1/FVC z-scores, GLI 2012 % predicted, BODE index                |
| **Inflammatory**          | [`inflammatory_markers()`](https://sufyansuleman.github.io/HealthMarkers/reference/inflammatory_markers.md), [`iAge()`](https://sufyansuleman.github.io/HealthMarkers/reference/iAge.md)                                                                                                                                                                                                                                                                                                            | NLR, PLR, SII, LMR, iAge inflammatory clock                        |
| **Hormonal**              | [`hormone_markers()`](https://sufyansuleman.github.io/HealthMarkers/reference/hormone_markers.md)                                                                                                                                                                                                                                                                                                                                                                                                   | T/E2 ratio, TSH/fT4, cortisol/DHEA, LH/FSH, HOMA-B, FAI            |
| **Body composition**      | [`obesity_indices()`](https://sufyansuleman.github.io/HealthMarkers/reference/obesity_indices.md), [`adiposity_sds()`](https://sufyansuleman.github.io/HealthMarkers/reference/adiposity_sds.md), [`adiposity_sds_strat()`](https://sufyansuleman.github.io/HealthMarkers/reference/adiposity_sds_strat.md), [`alm_bmi_index()`](https://sufyansuleman.github.io/HealthMarkers/reference/alm_bmi_index.md)                                                                                          | BMI, WHR, ABSI, BRI, BAI, sex/age-stratified SDS, ALM/BMI          |
| **Bone**                  | [`bone_markers()`](https://sufyansuleman.github.io/HealthMarkers/reference/bone_markers.md), [`frax_score()`](https://sufyansuleman.github.io/HealthMarkers/reference/frax_score.md)                                                                                                                                                                                                                                                                                                                | P1NP, osteocalcin, CTX, NTX, FRAX 10-yr fracture probability       |
| **Frailty / comorbidity** | [`frailty_index()`](https://sufyansuleman.github.io/HealthMarkers/reference/frailty_index.md), [`charlson_index()`](https://sufyansuleman.github.io/HealthMarkers/reference/charlson_index.md), [`sarc_f_score()`](https://sufyansuleman.github.io/HealthMarkers/reference/sarc_f_score.md)                                                                                                                                                                                                         | Rockwood deficit index, Charlson CCI, SARC-F                       |
| **Vitamins & nutrients**  | [`vitamin_markers()`](https://sufyansuleman.github.io/HealthMarkers/reference/vitamin_markers.md), [`vitamin_d_status()`](https://sufyansuleman.github.io/HealthMarkers/reference/vitamin_d_status.md), [`nutrient_markers()`](https://sufyansuleman.github.io/HealthMarkers/reference/nutrient_markers.md)                                                                                                                                                                                         | Vitamin D status category, B12/folate ratio, ferritin saturation   |
| **Alternate biofluids**   | [`saliva_markers()`](https://sufyansuleman.github.io/HealthMarkers/reference/saliva_markers.md), [`sweat_markers()`](https://sufyansuleman.github.io/HealthMarkers/reference/sweat_markers.md), [`urine_markers()`](https://sufyansuleman.github.io/HealthMarkers/reference/urine_markers.md)                                                                                                                                                                                                       | Cortisol awakening response, sweat chloride, urinary ratios        |
| **Neurological**          | [`nfl_marker()`](https://sufyansuleman.github.io/HealthMarkers/reference/nfl_marker.md), [`kyn_trp_ratio()`](https://sufyansuleman.github.io/HealthMarkers/reference/kyn_trp_ratio.md), [`corrected_calcium()`](https://sufyansuleman.github.io/HealthMarkers/reference/corrected_calcium.md)                                                                                                                                                                                                       | Age-adjusted NfL, kynurenine/tryptophan ratio, corrected calcium   |
| **Psychiatric**           | [`psych_markers()`](https://sufyansuleman.github.io/HealthMarkers/reference/psych_markers.md)                                                                                                                                                                                                                                                                                                                                                                                                       | PHQ-9, GAD-7, ISI, GHQ-12, K10, K6, WHO-5, ASRS, BIS-11, SPQ       |
| **Anthropometric SDS**    | [`calc_sds()`](https://sufyansuleman.github.io/HealthMarkers/reference/calc_sds.md)                                                                                                                                                                                                                                                                                                                                                                                                                 | Generic SDS z-score from any reference mean and SD                 |

------------------------------------------------------------------------

## How to use HealthMarkers

### all_health_markers()\`: the dispatcher

**Use this when** you want to compute many marker groups in one call and
receive everything back as a single wide tibble appended to your
original data.

``` r
library(HealthMarkers)

labs <- data.frame(
  age  = 52,  sex  = "M",
  G0   = 5.8, I0   = 14.2,
  TC   = 5.6, LDL_c = 3.4, HDL_c = 1.1, TG = 2.1,
  ALT  = 38,  AST  = 30,   BMI   = 30.1,
  SBP  = 138, DBP  = 88,   WC    = 98,
  eGFR = 74,  UACR = 18
)

results <- all_health_markers(
  data    = labs,
  which   = c("glycemic", "lipid", "liver", "renal", "kidney_kfre",
              "mets", "obesity_metrics"),
  verbose = TRUE
)

# results is the original data + all computed columns as one wide tibble
names(results)
```

The `which` argument accepts any of the following group keys:

``` R
insulin_fasting     insulin_ogtt        insulin_adipose     insulin_tracer_dxa
glycemic            lipid               atherogenic         cvd_aip
cvd_risk            cvd_ldl_particles   cvd_ascvd           cvd_qrisk3
cvd_scorescvd       cvd_stroke          liver               liver_fat
mets
metabolic_risk      pulmo               spirometry          bode
saliva              sweat               urine               renal
kidney_kfre         ckd_stage           nutrient            vitamin
vitamin_d_status    hormone             inflammatory        iAge
bone                frax                allostatic_load     oxidative
frailty_index       charlson            sarc_f              psych
nfl                 inflammatory_age    calcium_corrected   kyn_trp
adiposity_sds       adiposity_sds_strat obesity_metrics     alm_bmi
```

Pass `which = "all"` to run every group (groups requiring unavailable
optional packages will be silently skipped).

### Individual functions: targeted computation

**Use this when** you need fine-grained control, are working with
specialist data (e.g. OGTT time-series, DXA outputs, spirometry), or
want to inspect one marker family in detail.

------------------------------------------------------------------------

## Function-by-function guide

### Insulin sensitivity

**When to use:** fasting glucose + insulin data are available; OGTT data
with multiple time points; DXA body-composition data; tracer clamp data.

``` r
# Fasting indices (HOMA-IR, QUICKI, Bennett, FIRI, ...)
# Needs: G0 (fasting glucose mmol/L), I0 (fasting insulin mU/L)
fasting_is(data, col_map = list(G0 = "glucose", I0 = "insulin"))

# OGTT indices (Matsuda, Stumvoll, Gutt, Avignon, ...)
# Needs: G0/G30/G60/G120 and I0/I30/I60/I120 (mmol/L and mU/L)
ogtt_is(data, col_map = list(G0="G0", G30="G30", G60="G60", G120="G120",
                              I0="I0", I30="I30", I60="I60", I120="I120"))

# Adipose-tissue indices (LIRI, SPISE, VAI, LAP, ...)
# Needs: BMI, WC, TG, HDL_c
adipo_is(data, col_map = list(BMI="BMI", WC="WC", TG="TG", HDL_c="HDL_c"))

# DXA / tracer-based indices
# Needs: fat mass, lean mass, Ra (palmitate/glycerol rates)
tracer_dxa_is(data, col_map = list(fat_mass="FM_kg", lean_mass="LM_kg"))

# All insulin indices at once (fasting + OGTT + adipose + DXA)
all_insulin_indices(data, col_map = list(...), normalize = "none",
                    mode = "both",  # "IS" = sensitivity only, "IR" = resistance only
                    na_action = "keep")
```

### Cardiovascular risk

**When to use:** primary prevention cohorts; assessing 10-year MACE
risk; comparing risk algorithms side-by-side.

``` r
# ASCVD Pooled Cohort Equations (10-yr or 30-yr)
# Needs: age, sex, race, total_chol, HDL_c, sbp, bp_treated, smoker, diabetes, bmi
# Requires: PooledCohort package
cvd_risk_ascvd(data, year = 10)

# QRISK3 (UK population)
# Requires: QRISK3 package
cvd_risk_qrisk3(data)

# SCORE2 / SCORE2-OP (European)
# Requires: RiskScorescvd package
cvd_risk_scorescvd(data)

# 10-year stroke risk (Pooled Cohort)
# Requires: PooledCohort package
cvd_risk_stroke(data)

# Atherogenic index of plasma (log TG/HDL)
cvd_marker_aip(data, col_map = list(TG = "TG", HDL_c = "HDL_c"))

# LDL particle number from ApoB
cvd_marker_ldl_particle_number(data, col_map = list(ApoB = "ApoB"))

# Run all CVD algorithms at once and pick one model
cvd_risk(data, model = "ALL")   # or "ASCVD", "QRISK3", "Stroke", etc.
```

### Renal function

**When to use:** nephrology studies; CKD cohorts; monitoring
progression.

``` r
# Kidney Failure Risk Equation (KFRE) 2-year and 5-year probability
# Needs: age, sex, eGFR (CKD-EPI, mL/min/1.73m²), UACR (mg/g)
kidney_failure_risk(data, col_map = list(age="age", sex="sex",
                                          eGFR="eGFR", UACR="UACR"))

# eGFR, creatinine ratios, BUN/creatinine, FE-Urea, etc.
renal_markers(data, col_map = list(creatinine="Creat", age="age", sex="sex"))

# KDIGO CKD staging (G1–G5 × A1–A3)
ckd_stage(data, col_map = list(eGFR="eGFR", UACR="UACR"))

# Urine panel: protein/creatinine ratio, microalbumin, osmolality
urine_markers(data, col_map = list(urine_creat="UCr", urine_protein="UPr"))
```

### Pulmonary function

**When to use:** respiratory epidemiology; COPD staging; lung-function
studies.

``` r
# Individual spirometry z-scores and % predicted (GLI 2012)
# Needs: FEV1, FVC; optionally age, height, sex, ethnicity
# Requires: rspiro package
spirometry_markers(data, col_map = list(fev1="FEV1", fvc="FVC",
                                         age="age", height="ht_cm", sex="sex"))

# Simpler pulmonary ratios (FEV1/FVC, FEF25-75, etc.) no extra packages needed
pulmo_markers(data)

# BODE index for COPD prognosis
# Needs: FEV1% predicted, 6-minute walk distance, mMRC dyspnoea score, BMI
bode_index(data, col_map = list(fev1_pct="FEV1pct", sixmwd="Walk6m",
                                  mmrc="mMRC", bmi="BMI"))
```

### Frailty and comorbidity

**When to use:** geriatric studies; surgical risk; comorbidity
adjustment.

``` r
# Rockwood deficit-accumulation frailty index
# Needs: a data frame of binary deficit columns (0/1)
# Requires: di package
frailty_index(data, deficit_cols = c("hypertension","diabetes","mobility_loss",...))

# Charlson Comorbidity Index (predicts 1-year mortality)
# Needs: binary columns for each condition
charlson_index(data, col_map = list(mi="MI", chf="CHF", diabetes="DM",...))

# SARC-F muscle function screening (5-item questionnaire)
# Needs: 5 SARC-F item columns (strength, assistance walking, rise from chair,
#         climb stairs, falls)
sarc_f_score(data, col_map = list(strength="Q1", walking="Q2",...))
```

### Psychiatric scores

**When to use:** mental health research; epidemiological surveys with
standardised questionnaires.

``` r
# Score one or many scales from item columns
# Supported: PHQ-9, GAD-7, K6, K10, GHQ-12, WHO-5, ISI, MDQ,
#            ASRS, BIS-11, SPQ, cognitive composite
#
# col_map is a nested list keyed by instrument name.
# Internal item keys use zero-padded names (phq9_01 ... phq9_09).
psych_markers(
  data,
  col_map = list(
    phq9 = list(items = list(phq9_01 = "Q1", phq9_02 = "Q2", ...)),
    gad7 = list(items = list(gad7_01 = "G1", gad7_02 = "G2", ...))
  ),
  which   = c("phq9", "gad7", "k10")  # choose scales to score
)

# If your columns are already named phq9_01 ... phq9_09 etc., no col_map needed:
phq9_score(data)
gad7_score(data)
k10_score(data)
```

### Body composition and anthropometric SDS

**When to use:** paediatric cohorts (SDS); obesity epidemiology;
sarcopenia assessment.

``` r
# Common obesity and adiposity indices
# Needs: height, weight, WC, hip circumference; optionally age, sex
obesity_indices(data)

# SDS z-scores from user-supplied reference mean and SD
calc_sds(x = data$BMI, mean_ref = 22.5, sd_ref = 3.8)

# Sex-stratified SDS for multiple adiposity variables simultaneously
adiposity_sds_strat(data, col_map = list(sex = "sex"),
                    var_cols = c("BMI","WC","WHR"),
                    ref_male = list(BMI = c(mean=25, sd=4)),
                    ref_female = list(BMI = c(mean=24, sd=3.8)))

# Appendicular lean mass / BMI index (sarcopenia screening)
alm_bmi_index(data, col_map = list(alm="ALM_kg", bmi="BMI", sex="Sex"))
```

### Bone markers and FRAX

**When to use:** osteoporosis research; fracture risk assessment; bone
turnover monitoring.

``` r
# Bone turnover markers: P1NP, osteocalcin, CTX, NTX ratios
bone_markers(data, col_map = list(P1NP="p1np", CTX="ctx_s"))

# FRAX 10-year fracture probability (hip and major osteoporotic fractures)
# Needs: age, sex, BMI; optionally BMD T-score, prior fracture, steroid use, etc.
frax_score(data, col_map = list(age="Age", sex="Sex", bmd_t="TScore"),
           country = "UK")
```

### Inflammatory and aging markers

**When to use:** immunology studies; biological age estimation; chronic
disease research.

``` r
# Blood count-derived inflammatory ratios
# Needs: neutrophils, lymphocytes, monocytes, platelets
inflammatory_markers(data, col_map = list(neut="NEUT", lymph="LYMPH",
                                           mono="MONO", plt="PLT"))

# iAge inflammatory aging clock
# Needs: a panel of inflammatory proteins (IL-6, CXCL9, etc.)
iAge(data, col_map = list(IL6="IL6", CXCL9="CXCL9"))
```

### Alternate biofluids

**When to use:** stress research (saliva); cystic fibrosis / sweat
testing; nephrology urine panels.

``` r
saliva_markers(data, col_map = list(cortisol_wake="C_wake",
                                     cortisol_30="C_30min"))
sweat_markers(data,  col_map = list(sweat_chloride="Cl_mmol"))
urine_markers(data,  col_map = list(urine_creat="UCr", urine_na="UNa"))
```

------------------------------------------------------------------------

## Column mapping

Every function accepts a `col_map` argument a named list mapping
internal keys (what the function expects) to your actual column names
(what you have).

``` r
# Internal key = "G0", your column is called "fasting_glucose_mmol"
fasting_is(
  data    = my_data,
  col_map = list(G0 = "fasting_glucose_mmol", I0 = "insulin_uU_mL")
)
```

For multi-domain calls, use
[`infer_cols()`](https://sufyansuleman.github.io/HealthMarkers/reference/infer_cols.md)
to let the package auto-map by pattern matching:

``` r
# See what gets mapped and what's missing
inferred <- infer_cols(my_data)
str(inferred)

# Pass directly the dispatcher will use it
all_health_markers(data = my_data, col_map = inferred)
```

------------------------------------------------------------------------

## Handle missing data before computing

Impute missing values before passing data to any marker function:

``` r
# Multiple imputation (mice) recommended for inference
completed <- impute_mice(my_data, m = 5, seed = 42)

# Random-forest imputation (missForest) recommended for prediction
completed <- impute_missforest(my_data)

# Simple mean / median / mode quick exploratory use
completed <- impute_missing(my_data, method = "median")
```

------------------------------------------------------------------------

## Verbose diagnostics

Set `verbose = TRUE` on any function to see progress, NA counts, and
elapsed time:

``` r
results <- all_health_markers(data = labs, which = c("lipid","liver"),
                               verbose = TRUE)
#> Column mapping summary: TC->TC (user), HDL_c->HDL_c (user) ...
#> -> lipid
#> -> liver
#> all_health_markers(): summary - computed: lipid, liver | skipped/failed: none
```

Enable globally for an entire session:

``` r
# levels: "none" (default), "inform" (progress only), "debug" (all internal steps)
options(healthmarkers.verbose = "inform")
```

------------------------------------------------------------------------

## Further information

The **package website** contains the full function reference, rendered
vignettes, and a searchable article index:

- **Website:** <https://sufyansuleman.github.io/HealthMarkers/>
- **Function reference:**
  <https://sufyansuleman.github.io/HealthMarkers/reference/>
- **All articles / vignettes:**
  <https://sufyansuleman.github.io/HealthMarkers/articles/>

------------------------------------------------------------------------

## Vignettes

There are **46 vignettes** covering every marker domain. The 12 core
vignettes below are bundled with the package; the remaining 34 are
available exclusively on the package website (they are not built by CRAN
to keep installation fast).

**Bundled with the package** accessible via
[`browseVignettes()`](https://rdrr.io/r/utils/browseVignettes.html) or
[`vignette()`](https://rdrr.io/r/utils/vignette.html):

``` r
browseVignettes("HealthMarkers")

vignette("getting-started",     package = "HealthMarkers")
vignette("fasting_is",          package = "HealthMarkers")
vignette("ogtt_is",             package = "HealthMarkers")
vignette("glycemic_markers",    package = "HealthMarkers")
vignette("lipid_markers",       package = "HealthMarkers")
vignette("cvd_risk",            package = "HealthMarkers")
vignette("liver_markers",       package = "HealthMarkers")
vignette("frailty_index",       package = "HealthMarkers")
vignette("inflammatory_markers",package = "HealthMarkers")
vignette("obesity_indices",     package = "HealthMarkers")
vignette("impute_missing",      package = "HealthMarkers")
vignette("health_markers",      package = "HealthMarkers")
```

**All 46 vignettes** (including adipo_is, tracer_dxa_is,
allostatic_load, bone_markers, psych_markers, and 29 more) are rendered
and searchable on the package website:

> <https://sufyansuleman.github.io/HealthMarkers/articles/>

------------------------------------------------------------------------

## Contributing

Issues and pull requests are welcome at
<https://github.com/sufyansuleman/HealthMarkers/issues>.

When contributing a new marker function please:

1.  Add a unit test in `tests/testthat/` with at least one numeric
    check.
2.  Add a `@references` entry in the roxygen block and cite the primary
    paper in `inst/REFERENCES.bib`.
3.  Register the function in the
    [`all_health_markers()`](https://sufyansuleman.github.io/HealthMarkers/reference/all_health_markers.md)
    dispatcher if it fits an existing domain.
4.  Add or update the relevant vignette in `vignettes/`.

------------------------------------------------------------------------

## Citation

``` r
citation("HealthMarkers")
```

------------------------------------------------------------------------

## License

MIT Sufyan Suleman ([ORCID
0000-0001-6612-6915](https://orcid.org/0000-0001-6612-6915))
