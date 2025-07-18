HealthMarkers
================

- [🔍 Overview](#-overview)
  - [📦 Installation](#-installation)
  - [📑 Overview of functions](#-overview-of-functions)
  - [1. Fasting Insulin Sensitivity
    Indices](#1-fasting-insulin-sensitivity-indices)
  - [2. OGTT Insulin Sensitivity
    Indices](#2-ogtt-insulin-sensitivity-indices)
  - [3. Adipose-based Insulin Sensitivity
    Indices](#3-adipose-based-insulin-sensitivity-indices)
  - [4. Tracer‐DXA Insulin Sensitivity
    Indices](#4-tracerdxa-insulin-sensitivity-indices)
  - [5. Glycemic and C‑peptide Insulin Sensitivity
    Markers](#5-glycemic-and-cpeptide-insulin-sensitivity-markers)
  - [6. Metabolic Risk Feature Flags](#6-metabolic-risk-feature-flags)
  - [7. Metabolic Syndrome Severity
    Score](#7-metabolic-syndrome-severity-score)
  - [8. Standardized‑Score (SDS) for Adiposity
    Measures](#8-standardizedscore-sds-for-adiposity-measures)
  - [9. Stratified Adiposity Standard Deviation
    Scores](#9-stratified-adiposity-standard-deviation-scores)
  - [10. Lipid Panel Markers](#10-lipid-panel-markers)
  - [11. Liver Panel Markers](#11-liver-panel-markers)
  - [12. Pulmonary Function Markers](#12-pulmonary-function-markers)
  - [13. Cardiovascular Risk Features](#13-cardiovascular-risk-features)
  - [14. Saliva-Based Stress & Glycemic
    Markers](#14-saliva-based-stress--glycemic-markers)
  - [15. Sweat-Based Ionic & Metabolic
    Markers](#15-sweat-based-ionic--metabolic-markers)
  - [16. Urine-Based Renal & Protein
    Markers](#16-urine-based-renal--protein-markers)
  - [🌐 Online Documentation](#-online-documentation)
  - [📚 References](#-references)
  - [🤝 Contributing](#-contributing)
  - [📜 License](#-license)

<!-- badges: start -->
<!-- badges: end -->

# 🔍 Overview

**HealthMarkers** is a powerful, end-to-end R toolkit for deriving and
standardizing a comprehensive array of physiological biomarkers. From
core metabolic and cardiometabolic, cardivascular indices to liver,
kidney, lung, and novel biofluid based markers, it brings best-in-class
algorithms into one consistent, user-friendly pipeline.

------------------------------------------------------------------------

## 📦 Installation

``` r
# CRAN release
install.packages("HealthMarkers")

# Development version
# remotes::install_github("sufyansuleman/HealthMarkers")
```

------------------------------------------------------------------------

## 📑 Overview of functions

------------------------------------------------------------------------

## 1. Fasting Insulin Sensitivity Indices

**Function:**
`fasting_is(data, col_map, normalize = "none", verbose = FALSE)`

**Purpose:** Compute ten fasting‐based insulin sensitivity and
resistance indices from glucose and insulin measurements. Internally,
`validate_inputs()` checks for `G0` and `I0`, unit conversions
(G0-\>mg/dL, I0-\>µU/mL), then applies `normalize_vec()` per
`normalize`.

### Input Data

A tibble or data.frame with at least two columns mapped via `col_map`:

- `G0`: fasting glucose (mmol/L)
- `I0`: fasting insulin (pmol/L)

``` r
col_map <- list(
  G0 = "fasting_glucose_col",
  I0 = "fasting_insulin_col"
)
```

### Usage

``` r
res_fast <- fasting_is(
  data      = df_fast,
  col_map   = list(G0 = "G0", I0 = "I0"),
  normalize = "none", # "none", "z", "inverse", "range", "robust"
  verbose   = TRUE     # print progress
)
print(res_fast)
```

### Output Columns

- `Fasting_inv`
- `Raynaud`  
- `HOMA_IR_inv`
- `FIRI`
- `QUICKI`
- `Belfiore_basal`
- `Ig_ratio_basal`
- `Isi_basal`
- `Bennett`
- `HOMA_IR_rev_inv`

### Note: For IS indices **formulas** look [here](https://academic.oup.com/jcem/article/109/11/2754/7650976?login=true#487293469)

------------------------------------------------------------------------

## 2. OGTT Insulin Sensitivity Indices

**Function:**
`ogtt_is(data, col_map, normalize = "none", verbose = FALSE)`

**Purpose:** Calculate OGTT-based insulin sensitivity indices. Uses
`validate_inputs()`, converts glucose to mg/dL and insulin to µU/mL,
computes AUCs and means, index formulas, then optional normalization.

### Input Data

A tibble or data.frame with columns mapped via `col_map`:

- `G0`, `G30`, `G120`: glucose at 0, 30, 120 min (mmol/L)
- `I0`, `I30`, `I120`: insulin at 0, 30, 120 min (pmol/L)
- `weight`: body weight (kg)
- `bmi`: body-mass index (kg/m^2)
- `age`: years
- `sex`: 1 = male, 2 = female

``` r
df_ogtt <- tibble::tibble(
  G0=5.5, I0=60, G30=7.8, I30=90,
  G120=6.2, I120=50, weight=70, bmi=24,
  age=30, sex=1
)
```

### Usage

``` r
res_ogtt <- ogtt_is(
  data      = df_ogtt,
  col_map   = list(
    G0   = "G0",   I0   = "I0",
    G30  = "G30",  I30  = "I30",
    G120 = "G120", I120 = "I120",
    weight = "weight", bmi = "bmi",
    age    = "age",    sex = "sex"
  ),
  normalize = "none",
  verbose   = TRUE
)
print(res_ogtt)
```

### Output Columns

- `Isi_120`
- `Cederholm_index`
- `Gutt_index`
- `Avignon_Si0`
- `Avignon_Si120`
- `Avignon_Sim`
- `Modified_stumvoll`
- `Stumvoll_Demographics`
- `Matsuda_AUC`
- `Matsuda_ISI`
- `BigttSi`
- `Ifc_inv`
- `HIRI_inv`
- `Belfiore_isi_gly` *Note: glucose×18 -\> mg/dL; insulin/6 -\> µU/mL*

### For IS indices **formulas** look [here](https://academic.oup.com/jcem/article/109/11/2754/7650976?login=true#487293469)

------------------------------------------------------------------------

## 3. Adipose-based Insulin Sensitivity Indices

**Function:**
`adipo_is(data, col_map, normalize = "none", verbose = FALSE)`

**Purpose:** Compute ten adipose-based insulin sensitivity and
resistance indices. `validate_inputs()`, unit conversions (G0-\>mg/dL,
I0-\>µU/mL, TG-\>mg/dL, HDL_c-\>mg/dL), then normalization.

### Input Data

A tibble or data.frame with columns mapped via `col_map`:

- `G0`, `I0`: fasting glucose & insulin
- `TG`, `HDL_c`: lipids (mmol/L)
- `FFA`, `waist`, `bmi`

``` r
df_adipo <- tibble(
  G0=5.5, I0=60, TG=1.2, HDL_c=1.0,
  FFA=0.45, waist=80, bmi=24
)
```

### Usage

``` r
res_adipo <- adipo_is(
  data      = df_adipo,
  col_map   = list(
    G0="G0", I0="I0", TG="TG",
    HDL_c="HDL_c", FFA="FFA",
    waist="waist", bmi="bmi"
  ),
  normalize = "none",
  verbose   = TRUE
)
print(res_adipo)
```

### Output Columns

- `Revised_QUICKI`
- `VAI_Men_inv`, `VAI_Women_inv`
- `TG_HDL_C_inv`
- `TyG_inv`
- `LAP_Men_inv`, `LAP_Women_inv`
- `McAuley_index`
- `Adipo_inv`
- `Belfiore_inv_FFA`

*Notes: unit conversions as above.*

------------------------------------------------------------------------

## 4. Tracer‐DXA Insulin Sensitivity Indices

**Function:** `tracer_dxa_is(data, normalize = "none", verbose = FALSE)`

**Purpose:** Estimate insulin sensitivity using tracer kinetics and DXA
lean mass.

### Input Data

A tibble with:

- `Si_tracer`: tracer-derived sensitivity
- `lean_mass`: DXA lean mass (kg)

Optional:

- `age`, `sex`, `bmi`

``` r
df_tracer <- tibble(
  Si_tracer=2.5, lean_mass=50,
  age=30, sex=1, bmi=24
)
```

### Usage

``` r
res_tracer <- tracer_dxa_is(
  data      = df_tracer,
  normalize = "none",
  verbose   = TRUE
)
print(res_tracer)
```

### Output Columns

- `Si_tracer` - raw tracer-based sensitivity
- `Si_leanscale` - scaled by lean mass
- `Si_ageadj` - age-adjusted

------------------------------------------------------------------------

## 5. Glycemic and C‑peptide Insulin Sensitivity Markers

**Function:**

``` r
glycemic_markers(data, verbose = FALSE)
```

**Purpose:** Compute glycemic‑ and C‑peptide‑based insulin sensitivity
and resistance markers from routine fasting labs and anthropometry. The
function returns:

- **SPISE** (Single‑Point Insulin Sensitivity Estimator)
- **METS_IR** (Metabolic Score for Insulin Resistance)
- **prediabetes** flag (`0`/`1`) if HbA1c ≥ 42 mmol/mol
- **diabetes** flag (`0`/`1`) if HbA1c ≥ 48 mmol/mol
- **HOMA_CP** (C‑peptide‑based HOMA‑IR)

### Input Data

A `data.frame` or `tibble` containing at minimum:

- `HDL_c` (HDL cholesterol, mmol/L)
- `TG` (triglycerides, mmol/L)
- `BMI` (body mass index, kg/m^2)

**Optional** for additional markers:

- `glucose` (fasting glucose, mmol/L) - required for `METS_IR`
- `HbA1c` (mmol/mol) - required for `prediabetes`/`diabetes`
- `C_peptide` (pmol/L) - with `G0` for `HOMA_CP`
- `G0` (glucose, mmol/L)

**Example input:**

``` r
library(tibble)
df <- tibble(
  HDL_c     = 1.0,
  TG        = 1.3,
  BMI       = 24,
  glucose   = 5.6,
  HbA1c     = 44,
  C_peptide = 300,
  G0        = 5.5,
  I0        = 60
)
```

### Usage

``` r
res <- glycemic_markers(
  data    = df,
  verbose = TRUE  # prints a progress message
)
print(res)
```

### Output Columns

A tibble with columns:

- `SPISE` — Single‑Point Insulin Sensitivity Estimator
- `METS_IR` — Metabolic Score for Insulin Resistance
- `prediabetes` — `0/1` flag if `HbA1c ≥ 42`
- `diabetes` — `0/1` flag if `HbA1c ≥ 48`
- `HOMA_CP` — C‑peptide–based HOMA‑IR (or `NA` if inputs missing)

*For full details, see `?glycemic_markers` in R.*

                                                            |

------------------------------------------------------------------------

## 6. Metabolic Risk Feature Flags

**Function:**

``` r
metabolic_risk_features(data)
```

**Purpose:** Compute binary flags for four key metabolic risk features
using routine labs and z‑scores:

- **dyslipidemia** - based on cholesterol and triglyceride thresholds
  (age‑specific)
- **insulin_resistance** - HOMA‑IR z-score \> 1.28
- **hyperglycemia** - fasting glucose 5.6-6.9 mmol/L or HbA1c 39-47
  mmol/mol
- **hypertension** - systolic or diastolic BP z-score \> 1.64

### Input Data

A `data.frame` or `tibble` containing at least:

| Column          | Type    | Description                    |
|-----------------|---------|--------------------------------|
| `chol_total`    | numeric | Total cholesterol (mmol/L)     |
| `chol_ldl`      | numeric | LDL cholesterol (mmol/L)       |
| `chol_hdl`      | numeric | HDL cholesterol (mmol/L)       |
| `triglycerides` | numeric | Triglycerides (mmol/L)         |
| `age_year`      | numeric | Age in years                   |
| `z_HOMA`        | numeric | Standardized HOMA‑IR score     |
| `glucose`       | numeric | Fasting glucose (mmol/L)       |
| `HbA1c`         | numeric | Glycated hemoglobin (mmol/mol) |
| `bp_sys_z`      | numeric | Systolic BP z-score            |
| `bp_dia_z`      | numeric | Diastolic BP z-score           |

**Example input:**

``` r
library(tibble)

df <- tibble(
  chol_total    = 6.0,
  chol_ldl      = 3.5,
  chol_hdl      = 1.0,
  triglycerides = 1.2,
  age_year      = 25,
  z_HOMA        = 1.5,
  glucose       = 5.8,
  HbA1c         = 40,
  bp_sys_z      = 1.7,
  bp_dia_z      = 1.0
)
```

### Usage

``` r
res_flags <- metabolic_risk_features(
  data = df
)
print(res_flags)
```

### Output Columns

The returned `tibble` has one **factor** column (`levels = c("0","1")`)
per feature:

| Column | Criteria |
|----|----|
| `dyslipidemia` | `1` if any: total cholesterol \> 5.2, LDL \> 3.4, HDL \< 1.0, or age‑adjusted triglycerides thresholds, else `0` |
| `insulin_resistance` | `1` if `z_HOMA` \> 1.28, else `0` |
| `hyperglycemia` | `1` if (5.6 \< glucose \< 6.9) or (39 \< HbA1c \< 47), else `0` |
| `hypertension` | `1` if `bp_sys_z` \> 1.64 or `bp_dia_z` \> 1.64, else `0` |

------------------------------------------------------------------------

## 7. Metabolic Syndrome Severity Score

**Function:**

``` r
metss(data, params = <default>, verbose = FALSE)
```

**Purpose:** Compute a continuous Metabolic Syndrome Severity Score
(MetSSS), a z‑score representing syndrome severity, based on Wiley &
Carrington (2016) equations. The score integrates waist circumference,
blood pressure, triglycerides, HDL cholesterol, fasting glucose, sex,
and race.

### Input Data

A `data.frame` or `tibble` with the following columns:

| Column    | Type             | Description                     |
|-----------|------------------|---------------------------------|
| `waist`   | numeric          | Waist circumference (cm)        |
| `bp_sys`  | numeric          | Systolic blood pressure (mmHg)  |
| `bp_dia`  | numeric          | Diastolic blood pressure (mmHg) |
| `TG`      | numeric          | Triglycerides (mmol/L)          |
| `HDL_c`   | numeric          | HDL cholesterol (mmol/L)        |
| `glucose` | numeric          | Fasting glucose (mmol/L)        |
| `sex`     | integer          | Sex code: 1 = male, 2 = female  |
| `race`    | character/factor | One of “NHW”, “NHB”, “HW”, “HA” |

**Optional Parameters** (via `params` list):

- A named list mapping each sex‑race group (e.g. “NHW_M”, “NHB_F”) to a
  parameter list containing:

  - `intercept` (numeric)
  - `waist`, `TG`, `HDL`, `glucose`, `MAP` each as named numeric vectors
    with `mean`, `sd`, and `coef`

Defaults to a single entry `NHW_M` from Wiley & Carrington (2016).

**Example input:**

``` r
library(tibble)
df <- tibble(
  waist   = 94,
  bp_sys  = 120,
  bp_dia  = 80,
  TG      = 1.5,
  HDL_c   = 1.1,
  glucose = 5.3,
  sex     = 1,
  race    = "NHW"
)
```

### Usage

``` r
# Compute MetSSS with default parameters
res <- metss(
  data    = df,
  verbose = TRUE  # prints startup message
)
print(res)
```

To supply custom parameter sets:

``` r
custom_params <- list(
  NHW_M = list(
    intercept = -2.344,
    waist     = c(mean=94, sd=12.4, coef=0.846),
    TG        = c(mean=1.5,sd=0.6,coef=0.701),
    HDL       = c(mean=1.1,sd=0.3,coef=-0.663),
    glucose   = c(mean=5.3,sd=0.6,coef=0.658),
    MAP       = c(mean=97, sd=11, coef=0.466)
  ),
  NHW_F = ...
)
res2 <- metss(data = df, params = custom_params)
```

### Output Columns

The function returns a `tibble` with a single column:

| Column   | Description                                    |
|----------|------------------------------------------------|
| `MetSSS` | Continuous metabolic syndrome severity z-score |

*For detailed methodology, see Wiley & Carrington (2016) and the
`?metss` help page.*

------------------------------------------------------------------------

## 8. Standardized‑Score (SDS) for Adiposity Measures

**Function:**

``` r
adiposity_sds(
  data,    # data.frame or tibble of raw anthropometry
  ref,     # named list of reference means & sds
  verbose = FALSE  # print progress
)
```

**Purpose:**

- Validate and compute z‑scores (standard deviation scores) for each
  specified anthropometric variable.
- Allows you to compare individual measures (e.g., BMI, waist
  circumference) to an external reference distribution.
- Returns a tibble with one `<variable>_SDS` column per reference entry.

### Input Data and References

1.  **`data`** (`data.frame` or `tibble`):

    - Must contain numeric columns for each anthropometric variable to
      be standardized (names matching `names(ref)`).

2.  **`ref`** (`named list`):

    - Each element corresponds to a variable name in `data`.

    - Each element is a numeric vector of length 2, with names:

      - `mean` — the reference population mean
      - `sd` — the reference population standard deviation (must be \>
        0)

    - **Example:**

      ``` r
      ref <- list(
        BMI  = c(mean = 18.5, sd = 4.2),
        waist = c(mean = 75,   sd = 10)
      )
      ```

### Validation Steps

- Checks that `ref` is a named list with at least one element.
- Verifies each `ref[[var]]` is numeric, length 2, and contains names
  `'mean'` and `'sd'`.
- Ensures `sd` value is strictly positive; throws an error otherwise.
- Confirms that `data` includes all variables specified in `names(ref)`;
  lists missing names if any.

### Computation Details

- For each variable `var` in `names(ref)`, computes:

  ``` r
  z_var = (data[[var]] - ref[[var]]["mean"]) / ref[[var]]["sd"]
  ```

- Results are automatically named `<var>_SDS` in the output tibble.

- Handles single-row inputs gracefully, returning a tibble of length
  one.

### Example Usage

``` r
library(tibble)
# Sample data
df <- tibble(
  BMI   = c(24, 18),
  waist = c(80, 70)
)
# Reference distributions
ref <- list(
  BMI   = c(mean = 18.5, sd = 4.2),
  waist = c(mean = 75,   sd = 10)
)
# Compute SDS
out <- adiposity_sds(df, ref, verbose = TRUE)
print(out)
#> # A tibble: 2 × 2
#>   BMI_SDS waist_SDS
#>     <dbl>     <dbl>
#> 1   1.31       0.5 
#> 2  -0.119     -0.5
```

### Output

A tibble with columns:

| Column Name | Definition                     |
|-------------|--------------------------------|
| `<var>_SDS` | Z-score for `var`:             |
|             | `(observed_value - mean) / sd` |

*For full details, see `?adiposity_sds` in R.*

------------------------------------------------------------------------

## 9. Stratified Adiposity Standard Deviation Scores

**Function:**

``` r
adiposity_sds_strat(data, ref, sex_col = "sex", verbose = FALSE)
```

**Purpose:** Compute sex‑stratified standard deviation scores (SDS) for
adiposity measures, using sex‑specific reference means and standard
deviations.

### Input Data

A `data.frame` or `tibble` containing:

| Column | Type | Description |
|----|----|----|
| `<var>` | numeric | Adiposity measure (e.g., BMI, waist) matched to `ref` variable names |
| `sex_col` | character | Sex code: values must be “M” or “F” (default column name is `sex`) |

Additionally, provide `ref`, a named list with elements “M” and “F”,
each a named list of reference vectors for each variable:

``` r
ref <- list(
  M = list(
    BMI   = c(mean = 23, sd = 3.5),
    waist = c(mean = 85, sd = 10)
  ),
  F = list(
    BMI   = c(mean = 21, sd = 3),
    waist = c(mean = 75, sd = 9)
  )
)
```

**Example input:**

``` r
library(tibble)
df <- tibble(
  BMI   = c(22, 25),
  waist = c(88, 70),
  sex   = c("M", "F")
)
```

### Usage

``` r
res_strat <- adiposity_sds_strat(
  data    = df,
  ref     = ref,
  sex_col = "sex",  # or your column name
  verbose = TRUE      # prints progress message
)
print(res_strat)
```

### Output Columns

The returned `tibble` contains one SDS column per reference variable,
named `<var>_SDS`:

| Column      | Description                                   |
|-------------|-----------------------------------------------|
| `BMI_SDS`   | (BMI - mean) / sd, using sex‑specific stats   |
| `waist_SDS` | (waist - mean) / sd, using sex‑specific stats |

*For detailed usage, see the `?adiposity_sds_strat` help page.*

------------------------------------------------------------------------

## 10. Lipid Panel Markers

**Function:**

``` r
lipid_markers(
  data,
  col_map = list(
    TC    = "TC",
    HDL_c = "HDL_c",
    TG    = "TG",
    LDL_c = "LDL_c",
    ApoB  = "ApoB",
    ApoA1 = "ApoA1"
  ),
  verbose = FALSE
)
```

**Purpose:** Derive key lipid-panel metrics from total cholesterol, HDL,
and triglycerides, with optional LDL and apolipoproteins:

- `non_HDL_c` - non-HDL cholesterol
- `remnant_c` - cholesterol in remnant lipoproteins
- `ratio_TC_HDL` - total cholesterol to HDL ratio
- `ratio_TG_HDL` - triglyceride to HDL ratio
- `ratio_LDL_HDL`- LDL to HDL ratio
- `ApoB_ApoA1` - ApoB to ApoA1 ratio (if both provided)

### Input Data

A `data.frame` or `tibble` containing your lipid measurements and column
mappings via `col_map`:

| Key | Column | Required? | Description |
|----|----|----|----|
| `TC` | Total Cholesterol | Yes | Total cholesterol (mmol/L) |
| `HDL_c` | HDL Cholesterol | Yes | HDL cholesterol (mmol/L) |
| `TG` | Triglycerides | Yes | Triglycerides (mmol/L) |
| `LDL_c` | LDL Cholesterol | No | LDL cholesterol (mmol/L); estimated if not provided |
| `ApoB` | Apolipoprotein B | No | Apolipoprotein B (mg/dL or specified units) |
| `ApoA1` | Apolipoprotein A1 | No | Apolipoprotein A1 (mg/dL or specified units) |

**Note:** If `LDL_c` is missing, it is estimated by Friedewald formula:
LDL = TC − HDL_c − TG/5.

**Example input:**

``` r
library(tibble)
df <- tibble(
  TC    = 5.0,
  HDL_c = 1.0,
  TG    = 1.3,
  LDL_c = 3.0,
  ApoB  = 1.1,
  ApoA1 = 1.5
)
```

### Usage

``` r
res_lip <- lipid_markers(
  data    = df,
  col_map = list(
    TC    = "TC",
    HDL_c = "HDL_c",
    TG    = "TG",
    LDL_c = "LDL_c",
    ApoB  = "ApoB",
    ApoA1 = "ApoA1"
  ),
  verbose = TRUE  # prints a message on computation
)
print(res_lip)
```

### Output Columns

The returned `tibble` contains:

| Column          | Description                                             |
|-----------------|---------------------------------------------------------|
| `non_HDL_c`     | `TC - HDL_c`                                            |
| `remnant_c`     | `TC - (HDL_c + LDL_c)`                                  |
| `ratio_TC_HDL`  | `TC / HDL_c`                                            |
| `ratio_TG_HDL`  | `TG / HDL_c`                                            |
| `ratio_LDL_HDL` | `LDL_c / HDL_c`                                         |
| `ApoB_ApoA1`    | `ApoB / ApoA1` or `NA` if either apolipoprotein missing |

*For more details, see `?lipid_markers` in your R session.*

------------------------------------------------------------------------

## 11. Liver Panel Markers

**Function:**

``` r
liver_markers(
  data,
  col_map = list(
    BMI           = "BMI",
    waist         = "waist",
    triglycerides = "triglycerides",
    GGT           = "GGT",
    age           = "age",
    AST           = "AST",
    ALT           = "ALT",
    platelets     = "platelets",
    albumin       = "albumin",
    diabetes      = "diabetes",
    bilirubin     = "bilirubin",
    creatinine    = "creatinine"
  ),
  verbose = FALSE
)
```

**Purpose:** Compute a panel of blood-based liver function and fibrosis
markers using routine labs and anthropometry, including:

- **FLI** - Fatty Liver Index
- **NFS** - NAFLD Fibrosis Score
- **APRI** - AST-to-Platelet Ratio Index
- **FIB4** - Fibrosis-4 Index
- **BARD** - BMI-AST/ALT-Diabetes score
- **ALBI** - Albumin-Bilirubin score
- **MELD_XI** - MELD Excluding INR score

### Input Data

A `data.frame` or `tibble` with liver and anthropometry data. Map your
column names via `col_map`:

| Key             | Column        | Required? | Description                         |
|-----------------|---------------|-----------|-------------------------------------|
| `BMI`           | BMI           | Yes       | Body mass index (kg/m^2)            |
| `waist`         | Waist         | Yes       | Waist circumference (cm)            |
| `triglycerides` | Triglycerides | Yes       | Fasting triglycerides (mg/dL)       |
| `GGT`           | GGT           | Yes       | Gamma‐glutamyl transferase (U/L)    |
| `age`           | Age           | Yes       | Age in years                        |
| `AST`           | AST           | Yes       | Aspartate aminotransferase (U/L)    |
| `ALT`           | ALT           | Yes       | Alanine aminotransferase (U/L)      |
| `platelets`     | Platelets     | Yes       | Platelet count (10⁹/L)              |
| `albumin`       | Albumin       | Yes       | Serum albumin (g/L)                 |
| `diabetes`      | Diabetes      | Yes       | Diabetes status (0/1 or TRUE/FALSE) |
| `bilirubin`     | Bilirubin     | Yes       | Total bilirubin (mg/dL)             |
| `creatinine`    | Creatinine    | Yes       | Serum creatinine (mg/dL)            |

**Example input:**

``` r
library(tibble)
df <- tibble(
  BMI           = 24,
  waist         = 80,
  triglycerides = 150,
  GGT           = 30,
  age           = 30,
  AST           = 25,
  ALT           = 20,
  platelets     = 250,
  albumin       = 45,
  diabetes      = FALSE,
  bilirubin     = 1.0,
  creatinine    = 0.8
)
```

### Usage

``` r
res_liver <- liver_markers(
  data    = df,
  col_map = list(
    BMI           = "BMI",
    waist         = "waist",
    triglycerides = "triglycerides",
    GGT           = "GGT",
    age           = "age",
    AST           = "AST",
    ALT           = "ALT",
    platelets     = "platelets",
    albumin       = "albumin",
    diabetes      = "diabetes",
    bilirubin     = "bilirubin",
    creatinine    = "creatinine"
  ),
  verbose = TRUE  # prints progress
)
print(res_liver)
```

### Output Columns

The returned `tibble` includes:

| Column    | Description                                 |
|-----------|---------------------------------------------|
| `FLI`     | Fatty Liver Index (0-100 scale)             |
| `NFS`     | NAFLD Fibrosis Score                        |
| `APRI`    | AST-to-Platelet Ratio Index                 |
| `FIB4`    | Fibrosis-4 Index                            |
| `BARD`    | BARD score (BMI + AST/ALT ratio + diabetes) |
| `ALBI`    | Albumin-Bilirubin score                     |
| `MELD_XI` | MELD Excluding INR score                    |

*For detailed formulas and thresholds, see `?liver_markers` in R.*

------------------------------------------------------------------------

## 12. Pulmonary Function Markers

The `pulmo_markers()` function generates key pulmonary function
outputs-predicted normals, z-scores, percent predicted, and lower limits
of normal (LLN)-for FEV₁, FVC, and their ratio using reference equations
from the **rspiro** package.

### Installation of Dependencies

Install **rspiro** from CRAN or GitHub (if not on CRAN):

``` r
# From CRAN (if available)
install.packages("rspiro")

# Or directly from GitHub
if (!requireNamespace("remotes", quietly = TRUE)) {
  install.packages("remotes")
}
remotes::install_github("MRCIEU/rspiro")
```

### Usage Example

``` r
library(dplyr)
library(rspiro)
library(HealthMarkers)

# Example data: one individual
df <- tibble(
  age       = 50,             # years
  sex       = "female",      # "male"/"female"
  height    = 165,            # cm or m (auto-detected)
  ethnicity = "Caucasian",   # e.g. "Caucasian", "African-American"
  fev1      = 2.5,            # observed FEV1 in L
  fvc       = 3.0             # observed FVC in L
)

# Compute pulmonary markers using GLI equations
pulmo_results <- pulmo_markers(
  data     = df,
  equation = "GLI",    # or "GLIgl", "NHANES3"
  verbose  = TRUE
)

print(pulmo_results)
```

### Output Columns

The resulting tibble contains:

| Column             | Description                          |
|--------------------|--------------------------------------|
| `fev1_pred`        | Predicted FEV₁ (L)                   |
| `fev1_z`           | FEV₁ z-score                         |
| `fev1_pctpred`     | FEV₁ percent predicted (%)           |
| `fev1_LLN`         | FEV₁ lower limit of normal (L)       |
| `fvc_pred`         | Predicted FVC (L)                    |
| `fvc_z`            | FVC z-score                          |
| `fvc_pctpred`      | FVC percent predicted (%)            |
| `fvc_LLN`          | FVC lower limit of normal (L)        |
| `fev1_fvc_ratio`   | Observed FEV₁/FVC ratio              |
| `fev1_fvc_pred`    | Predicted FEV₁/FVC ratio             |
| `fev1_fvc_z`       | FEV₁/FVC ratio z-score               |
| `fev1_fvc_pctpred` | FEV₁/FVC percent predicted (%)       |
| `fev1_fvc_LLN`     | FEV₁/FVC ratio lower limit of normal |

### Equations Supported

- **GLI**: Global Lung Function Initiative standard equations
- **GLIgl**: GLI generalized least squares fit
- **NHANES3**: NHANES III reference set

------------------------------------------------------------------------

## 13. Cardiovascular Risk Features

This section covers the installation of the required CVD‑risk packages
and demonstrates how to compute risk estimates using the suite of
`cvd_risk_*()` wrapper functions and the unified `cvd_risk()`
dispatcher.

### Installation of Dependencies

Install the CRAN-hosted CVD‑risk packages:

``` r
install.packages(c(
  "PooledCohort",    # AHA/ACC Pooled Cohort Equations
  "QRISK3",         # UK QRISK3 algorithm
  "CVrisk",         # Framingham and related 10-year risk models
  "RiskScorescvd"   # ED-based HEART, TIMI, SCORE2, etc.
))
```

Install the two GitHub-only packages:

``` r
# Ensure 'remotes' is available
if (!requireNamespace("remotes", quietly = TRUE)) {
  install.packages("remotes")
}

# Framingham Risk Score implementation
remotes::install_github("PHP2560-Statistical-Programming-R/r-framingham")

# WHO/ISH CVD risk charts
remotes::install_github("DylanRJCollins/whoishRisk")
```

### Loading Libraries

``` r
library(PooledCohort)
library(QRISK3)
library(CVrisk)
library(RiskScorescvd)
library(rframingham)
library(whoishRisk)
```

### Usage Examples

Below are examples for each model-specific wrapper, followed by a
unified dispatch via `cvd_risk()`.

### 1. ASCVD (AHA/ACC Pooled Cohort Equations)

``` r
# Prepare data
df <- tibble::tibble(
  age          = 55,
  sex          = 1,        # 1 = male, 0 = female
  race         = "white",
  smoker       = FALSE,
  total_chol   = 200,      # mg/dL
  HDL_c        = 50,       # mg/dL
  sbp          = 140,      # systolic BP
  bp_treated   = FALSE,
  diabetes     = FALSE,
  bmi          = 27
)

# 10-year risk
cvd_risk_ascvd(df, year = 10)
# 30-year risk
cvd_risk_ascvd(df, year = 30)
```

### 2. QRISK3 (UK 2017)

``` r
cvd_risk_qrisk3(df)
```

### 3. MESA 10‑year CHD risk

``` r
# Data must include: race, gender (or sex), age, total_chol, HDL_c, sbp, bp_treated, smoker, diabetes
cvd_risk_mesa(df)
```

### 4. Stroke 10‑year risk (Pooled Cohort)

``` r
cvd_risk_stroke(df)
```

### 5. WHO/ISH 10‑year CVD risk

``` r
# Region argument defaults to SEAR_D; adjust via `region = "..."`
cvd_risk_who(df)
```

### 6. Unified dispatcher

Use `cvd_risk()` to call any model by name:

``` r
# e.g. compute QRISK3 via dispatcher
cvd_risk(df, model = "QRISK3")
```

### Available Models

- **ASCVD** (year = 10 or 30)
- **QRISK3** (year = 10)
- **MESA** (year = 10)
- **Stroke** (year = 10)
- **WHO** (year = 10)

Each function returns a tibble with columns:

- `model` - model name
- `year` - risk horizon in years
- `risk` - numeric percentage estimate

### Low-level Wrapper Functions

In addition to the high-level `cvd_risk_*()` and `cvd_risk()`
dispatcher, HealthMarkers provides thin, auto-generated wrappers for
each underlying package function. These allow direct access to specific
risk calculations if you need more fine-grained control:

- **Pooled Cohort** wrappers:

  - `pooledcohort_predict_10yr_ascvd_risk(...)`
  - `pooledcohort_predict_10yr_cvd_risk(...)`
  - `pooledcohort_predict_10yr_chd_risk(...)`
  - `pooledcohort_predict_10yr_stroke_risk(...)`
  - `pooledcohort_predict_10yr_hf_risk(...)`
  - `pooledcohort_predict_30yr_ascvd_risk(...)`
  - `pooledcohort_predict_30yr_cvd_risk(...)`
  - `pooledcohort_predict_30yr_chd_risk(...)`
  - `pooledcohort_predict_30yr_stroke_risk(...)`
  - `pooledcohort_predict_30yr_hf_risk(...)`
  - `pooledcohort_predict_5yr_ascvd_risk(...)`

- **QRISK3** wrapper:

  - `qrisk3_QRISK3_2017(...)`

- **CVrisk / MESA** wrappers:

  - `cvrisk_chd_10y_mesa(...)`
  - `cvrisk_ascvd_10y_accaha(...)`
  - `cvrisk_compute_CVrisk(...)`
  - `cvrisk_ascvd_10y_frs_simple(...)`
  - `cvrisk_ascvd_10y_frs(...)`
  - `cvrisk_chd_10y_mesa_cac(...)`

- **WHO/ISH** wrapper:

  - `whoishrisk_WHO_ISH_Risk(...)`

- **RiskScorescvd** wrappers (ED‐based and SCORE2 family):

  - `riskscorescvd_round_to_nearest_digit(...)`
  - `riskscorescvd_HEART_scores(...)`
  - `riskscorescvd_HEART(...)`
  - `riskscorescvd_TIMI_scores(...)`
  - `riskscorescvd_TIMI(...)`
  - `riskscorescvd_EDACS_scores(...)`
  - `riskscorescvd_EDACS(...)`
  - `riskscorescvd_GRACE_scores(...)`
  - `riskscorescvd_GRACE(...)`
  - `riskscorescvd_SCORE2_scores(...)`
  - `riskscorescvd_SCORE2(...)`
  - `riskscorescvd_SCORE2_CKD_scores(...)`
  - `riskscorescvd_SCORE2_CKD(...)`
  - `riskscorescvd_SCORE2_Diabetes(...)`

------------------------------------------------------------------------

## 14. Saliva-Based Stress & Glycemic Markers

**Function:**

``` r
saliva_markers(data, verbose = FALSE)
```

**Purpose:** Compute salivary biomarkers reflecting stress response and
glycemic status using waking cortisol, cortisol awakening response
(CAR), salivary α‑amylase, and salivary glucose.

### Input Data

A `data.frame` or `tibble` containing at least the following columns:

| Column           | Type    | Description                                   |
|------------------|---------|-----------------------------------------------|
| `saliva_cort1`   | numeric | Waking saliva cortisol (nmol/L)               |
| `saliva_cort2`   | numeric | Saliva cortisol ~30 min after waking (nmol/L) |
| `saliva_cort3`   | numeric | Saliva cortisol ~60 min after waking (nmol/L) |
| `saliva_amylase` | numeric | Salivary α‑amylase activity (U/mL)            |
| `saliva_glucose` | numeric | Salivary glucose concentration (mg/dL)        |

**Example input:**

``` r
library(tibble)
df <- tibble(
  saliva_cort1   = 12.5,
  saliva_cort2   = 18.0,
  saliva_cort3   = 16.2,
  saliva_amylase = 85,
  saliva_glucose = 4.2
)
```

### Usage

``` r
res_saliva <- saliva_markers(
  data    = df,
  verbose = TRUE  # prints progress message
)
print(res_saliva)
```

### Output Columns

The function returns a `tibble` with the following columns:

| Column | Description |
|----|----|
| `log_cortisol_wake` | Natural log of waking saliva cortisol (log(saliva_cort1)) |
| `CAR_AUC` | Cortisol Awakening Response AUC (trapezoidal over 0-60 min) |
| `log_amylase` | Natural log of salivary α‑amylase (log(saliva_amylase)) |
| `saliva_glucose` | Raw salivary glucose concentration (mg/dL) |

*For more information, see the `?saliva_markers` help page in R.*

------------------------------------------------------------------------

## 15. Sweat-Based Ionic & Metabolic Markers

**Function:**

``` r
sweat_markers(data, verbose = FALSE)
```

**Purpose:** Compute sweat-derived markers of electrolyte balance,
metabolism, and sweat rate:

- **sweat_chloride** - sweat chloride concentration (mmol/L)
- **Na_K_ratio** - ratio of sweat sodium to potassium
- **sweat_lactate** - sweat lactate concentration (mmol/L)
- **sweat_rate** - sweat rate normalized by body surface area (L/m^2/h)

### Input Data

A `data.frame` or `tibble` containing at least the following columns:

| Column              | Type    | Description                              |
|---------------------|---------|------------------------------------------|
| `sweat_chloride`    | numeric | Sweat chloride (mmol/L)                  |
| `sweat_Na`          | numeric | Sweat sodium (mmol/L)                    |
| `sweat_K`           | numeric | Sweat potassium (mmol/L)                 |
| `sweat_lactate`     | numeric | Sweat lactate (mmol/L)                   |
| `weight_before`     | numeric | Body weight before sweat collection (kg) |
| `weight_after`      | numeric | Body weight after sweat collection (kg)  |
| `duration`          | numeric | Collection duration (h)                  |
| `body_surface_area` | numeric | Body surface area (m^2)                  |

**Example input:**

``` r
library(tibble)
df <- tibble(
  sweat_chloride    = 45,
  sweat_Na          = 55,
  sweat_K           = 5,
  sweat_lactate     = 4.8,
  weight_before     = 70.0,
  weight_after      = 69.5,
  duration          = 1.0,
  body_surface_area = 1.9
)
```

### Usage

``` r
res_sweat <- sweat_markers(
  data    = df,
  verbose = TRUE  # prints progress message
)
print(res_sweat)
```

### Output Columns

The function returns a `tibble` with:

| Column | Description |
|----|----|
| `sweat_chloride` | Input sweat chloride (mmol/L) |
| `Na_K_ratio` | Sodium-to-potassium ratio (`sweat_Na / sweat_K`) |
| `sweat_lactate` | Input sweat lactate (mmol/L) |
| `sweat_rate` | Calculated sweat rate: (weight_before − weight_after) / duration / body_surface_area |

------------------------------------------------------------------------

*For more details, see the `?sweat_markers` help page in R.*

------------------------------------------------------------------------

## 16. Urine-Based Renal & Protein Markers

**Function:**

``` r
urine_markers(data, verbose = FALSE)
```

**Purpose:** Compute urine-derived markers of kidney function and
proteinuria, including:

- **UACR** - Albumin-to-Creatinine Ratio (mg/g)
- **microalbuminuria** - categorical flag (`"normal"` vs `"micro"`)
- **eGFR_CKD_EPI** - estimated glomerular filtration rate (mL/min/1.73
  m^2) using race-free CKD-EPI 2021 equation
- **FENa** - Fractional Excretion of Sodium (%)
- **UPCR** - Urine Protein-to-Creatinine Ratio (mg/g), if urine_protein
  provided

### Input Data

A `data.frame` or `tibble` containing at least the following columns:

| Column             | Type    | Description                                    |
|--------------------|---------|------------------------------------------------|
| `urine_albumin`    | numeric | Urine albumin concentration (mg/L)             |
| `urine_creatinine` | numeric | Urine creatinine concentration (mg/dL)         |
| `serum_creatinine` | numeric | Serum creatinine concentration (mg/dL)         |
| `plasma_Na`        | numeric | Plasma sodium concentration (mmol/L)           |
| `urine_Na`         | numeric | Urine sodium concentration (mmol/L)            |
| `age`              | numeric | Age in years                                   |
| `sex`              | integer | Sex code: 1 = male, 2 = female                 |
| `urine_protein`    | numeric | (optional) Urine total protein (mg/L) for UPCR |

**Example input:**

``` r
library(tibble)
df <- tibble(
  urine_albumin    = 30,
  urine_creatinine = 1.2,
  serum_creatinine = 0.9,
  plasma_Na        = 140,
  urine_Na         = 100,
  age              = 55,
  sex              = 2,
  urine_protein    = 150
)
```

### Usage

``` r
res_urine <- urine_markers(
  data    = df,
  verbose = TRUE  # prints progress messages
)
print(res_urine)
```

### Output Columns

The returned `tibble` contains:

| Column | Description |
|----|----|
| `UACR` | Albumin (mg/L) / Creatinine (mg/dL) × 1000 -\> mg/g |
| `microalbuminuria` | Factor with levels `"normal"`, `"micro"` based on UACR (30-300 mg/g) |
| `eGFR_CKD_EPI` | eGFR using CKD-EPI 2021 race-free equation (mL/min/1.73 m^2) |
| `FENa` | (urine_Na × serum_creatinine) / (plasma_Na × urine_creatinine) × 100 (%) |
| `UPCR` | Urine protein (mg/L) / (urine_creatinine (mg/dL) × 0.01) -\> mg/g (or `NA` if missing) |

*For full details, see the `?urine_markers` help page in R.*

------------------------------------------------------------------------

## 🌐 Online Documentation

- **Reference manual**:
  <https://sufyansuleman.github.io/HealthMarkers/reference/index.html>
- **pkgdown site**: <https://sufyansuleman.github.io/HealthMarkers/>

------------------------------------------------------------------------

## 📚 References

Full citation list [here](inst/REFERENCES.md)

------------------------------------------------------------------------

## 🤝 Contributing

Please follow the established style and add tests/examples. Submit via
GitHub PR.

------------------------------------------------------------------------

## 📜 License

MIT © [Sufyan Suleman](https://github.com/sufyansuleman)
