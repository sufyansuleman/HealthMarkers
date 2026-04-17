# Multi-Biobank Compatibility: Automatic Column Recognition

## Overview

HealthMarkers is designed to work with data from a wide range of
international cohort studies and biobanks **without any manual column
renaming**. The internal synonym dictionary covers naming conventions
from 15+ major studies. This article explains how the matching works,
which biobanks are supported, and how to diagnose and fix cases where a
column is not recognised.

------------------------------------------------------------------------

## How column inference works

Every HealthMarkers function looks up your column names through a
five-layer matching pipeline before any computation:

| Layer | Method                                               | Example                               |
|-------|------------------------------------------------------|---------------------------------------|
| 1     | Exact match in synonym dictionary                    | `LBXGLU` → `fasting_glucose`          |
| 2     | Case-insensitive exact match                         | `lbxglu` → `fasting_glucose`          |
| 3     | Data column name contains a synonym (≥ 4 chars)      | `plasma_glukoosi` → `fasting_glucose` |
| 4     | Synonym contains the data column name (≥ 4 chars)    | `glukos` → `fasting_glucose`          |
| 5     | Fuzzy (Jaro–Winkler) match, only when `fuzzy = TRUE` | `glucos` → `fasting_glucose`          |

The first four layers are active by default. Layer 5 is opt-in and is
most useful for catching typos in column names.

------------------------------------------------------------------------

## Supported biobanks and naming systems

### UK Biobank (UKB)

UK Biobank fields follow the pattern `analyte_0_0` (assessment visit 0,
instance 0) and `analyte_1_0` (reassessment). HealthMarkers recognises
these for all major analytes:

| Internal key        | UKB field                              | UKB field code |
|---------------------|----------------------------------------|----------------|
| `fasting_glucose`   | `glucose_0_0`                          | 30740          |
| `total_cholesterol` | `cholesterol_0_0`                      | 30690          |
| `HDL_c`             | `hdl_cholesterol_0_0`                  | 30760          |
| `LDL_c`             | `ldl_direct_0_0`                       | 30780          |
| `TG`                | `triglycerides_0_0`                    | 30870          |
| `ALT`               | `alanine_aminotransferase_0_0`         | 30620          |
| `AST`               | `aspartate_aminotransferase_0_0`       | 30650          |
| `creatinine`        | `creatinine_0_0`                       | 30700          |
| `albumin`           | `albumin_0_0`                          | 30600          |
| `HbA1c`             | `glycated_haemoglobin_hba1c_0_0`       | 30750          |
| `vitaminD`          | `vitamin_d_0_0`                        | 30890          |
| `Hgb`               | `haemoglobin_concentration_0_0`        | 30020          |
| `WBC`               | `white_blood_cell_leucocyte_count_0_0` | 30000          |
| `platelets`         | `platelet_count_0_0`                   | 30080          |
| `SBP`               | `systolic_blood_pressure_0_0`          | 4080           |
| `DBP`               | `diastolic_blood_pressure_0_0`         | 4079           |
| `height`            | `standing_height_0_0`                  | 50             |
| `weight`            | `body_weight_0_0`                      | 21002          |
| `BMI`               | `body_mass_index_bmi_0_0`              | 21001          |
| `waist`             | `waist_circumference_0_0`              | 48             |
| `testosterone`      | `testosterone_0_0`                     | 30850          |
| `TSH`               | `thyroid_stimulating_hormone_tsh_0_0`  | 30830          |
| `urea_serum`        | `urea_0_0`                             | 30670          |
| `uric_acid`         | `urate_0_0`                            | 30880          |
| `sodium`            | `sodium_0_0`                           | 30530          |
| `potassium`         | `potassium_0_0`                        | 30520          |
| `calcium`           | `calcium_0_0`                          | 30680          |
| `phosphate`         | `phosphate_0_0`                        | 30810          |

### NHANES (National Health and Nutrition Examination Survey, USA)

NHANES uses uppercase prefix codes. HealthMarkers recognises both the
`LBX` (examination) and `LBD` (derived) prefixes:

| Internal key        | NHANES variable | Questionnaire            |
|---------------------|-----------------|--------------------------|
| `fasting_glucose`   | `LBXGLU`        | Biochemistry Profile     |
| `total_cholesterol` | `LBXSCH`        | Biochemistry Profile     |
| `HDL_c`             | `LBDHDD`        | HDL-Cholesterol          |
| `LDL_c`             | `LBDLDL`        | Cholesterol-LDL          |
| `TG`                | `LBXSTR`        | Biochemistry Profile     |
| `ALT`               | `LBXSATSI`      | Biochemistry Profile     |
| `AST`               | `LBXSASSI`      | Biochemistry Profile     |
| `creatinine`        | `LBXSCR`        | Biochemistry Profile     |
| `albumin`           | `LBXSAL`        | Biochemistry Profile     |
| `HbA1c`             | `LBXGH`         | Glycohemoglobin          |
| `vitaminD`          | `LBXVD2`        | Vitamin D                |
| `Hgb`               | `LBXHGB`        | CBC                      |
| `WBC`               | `LBXWBCSI`      | CBC                      |
| `platelets`         | `LBXPLTSI`      | CBC                      |
| `ferritin`          | `LBXFER`        | Ferritin                 |
| `CRP`               | `LBXHSCRP`      | hs-CRP                   |
| `SBP`               | `BPXSY1`        | Blood Pressure           |
| `DBP`               | `BPXDI1`        | Blood Pressure           |
| `u_albumin`         | `URXUMA`        | Albumin-Creatinine Urine |
| `u_creatinine`      | `URXUCR`        | Albumin-Creatinine Urine |
| `Homocysteine`      | `LBXHCY`        | Homocysteine             |

### Danish national registers / EHR (LABKA, OPEN)

The Danish laboratory information system uses **NPU codes**
(Nordic-Baltic/IFCC nomenclature). All primary NPU codes are recognised:

| Internal key        | NPU code   | Danish clinical label  |
|---------------------|------------|------------------------|
| `creatinine`        | `NPU01994` | P-Kreatinin            |
| `total_cholesterol` | `NPU01567` | P-Kolesterol           |
| `HDL_c`             | `NPU01568` | P-HDL-Kolesterol       |
| `LDL_c`             | `NPU01569` | P-LDL-Kolesterol       |
| `TG`                | `NPU01592` | P-Triglycerider        |
| `fasting_glucose`   | `NPU27300` | P-Glukose (faste)      |
| `HbA1c`             | `NPU27300` | P-HbA1c                |
| `ALT`               | `NPU03429` | P-ALAT                 |
| `AST`               | `NPU03631` | P-ASAT                 |
| `GGT`               | `NPU01817` | P-GGT                  |
| `albumin`           | `NPU04998` | P-Albumin              |
| `Hgb`               | `NPU03609` | B-Hæmoglobin           |
| `WBC`               | `NPU02593` | B-Leukocytter          |
| `platelets`         | `NPU03568` | B-Trombocytter         |
| `sodium`            | `NPU01960` | P-Natrium              |
| `potassium`         | `NPU01961` | P-Kalium               |
| `urea_serum`        | `NPU01927` | P-Carbamid             |
| `uric_acid`         | `NPU01937` | P-Urinsyre             |
| `vitaminD`          | `NPU10501` | P-25-Hydroxy-Vitamin D |
| `ferritin`          | `NPU04698` | P-Ferritin             |
| `TSH`               | `NPU01407` | P-TSH                  |
| `Homocysteine`      | `NPU19404` | P-Homocystein          |
| `CRP`               | `NPU10438` | P-hs-CRP               |
| `cystatin_C`        | `NPU18454` | P-Cystatin C           |
| `u_albumin`         | `NPU17550` | U-Albumin              |
| `u_creatinine`      | `NPU17997` | U-Kreatinin            |

### HUNT Study and Tromsø Study (Norway)

Norwegian cohorts use a mix of Norwegian-language labels and
international codes:

| Internal key      | Norwegian label                           | Notes                       |
|-------------------|-------------------------------------------|-----------------------------|
| `age`             | `alder`                                   |                             |
| `sex`             | `kjonn`, `kjønn`                          |                             |
| `height`          | `hoyde`, `hoyden`                         |                             |
| `weight`          | `vekt`                                    |                             |
| `waist`           | `midjeomkrets`, `midjemaal`, `livvidde`   |                             |
| `SBP`             | `systolisk_blodtrykk`                     | Double k                    |
| `DBP`             | `diastolisk_blodtrykk`                    |                             |
| `fasting_glucose` | `fastende_blodsukker`, `fastende_glukose` |                             |
| `TG`              | `triglyserider`                           | Note -er ending             |
| `urea_serum`      | `karbamid`, `p_karbamid`                  | Note k (not c as in Danish) |

### FinnGen / THL Biobank (Finland)

Finnish-language labels have characteristic `-i` and `-iini` endings:

| Internal key        | Finnish label                              |
|---------------------|--------------------------------------------|
| `age`               | `ika`, `ikä`, `syntymavuosi`               |
| `sex`               | `sukupuoli`, `sp`                          |
| `height`            | `pituus`                                   |
| `weight`            | `paino`, `kehonpaino`                      |
| `fasting_glucose`   | `glukoosi`, `p_glukoosi`, `paastoglukoosi` |
| `HbA1c`             | `hemoglobiini_a1c`                         |
| `total_cholesterol` | `kolesteroli`, `kokonaiskolesteroli`       |
| `TG`                | `triglyseridit`                            |
| `ALT`               | `alaniiniaminotransferaasi`                |
| `creatinine`        | `kreatiniini`, `p_kreatiniini`             |
| `uric_acid`         | `virtsahappo`, `uraatti`                   |
| `Hgb`               | `hemoglobiini`                             |
| `WBC`               | `leukosyytit`                              |
| `platelets`         | `trombosyytit`                             |
| `ferritin`          | `ferritiini`                               |
| `vitaminD`          | `D_vitamiini`                              |
| `TSH`               | `tyreotropiini`                            |
| `testosterone`      | `testosteroni`                             |

### Estonian Biobank (EstBB)

Estonian has some unique spellings (double vowels, different roots):

| Internal key        | Estonian label                   | Notable difference             |
|---------------------|----------------------------------|--------------------------------|
| `age`               | `vanus`                          |                                |
| `sex`               | `sugu`                           |                                |
| `height`            | `pikkus`                         |                                |
| `weight`            | `kaal`                           |                                |
| `fasting_glucose`   | `glukoos`, `p_glukoos`           | Single o                       |
| `total_cholesterol` | `kolesterool`, `kogukolesterool` | Double o                       |
| `creatinine`        | `kreatiniin`                     | Single i (vs Finnish double i) |
| `Hgb`               | `hemoglobiin`                    |                                |
| `sodium`            | `naatrium`                       | Extra ‘a’ vs Latin natrium!    |
| `potassium`         | `kaalium`                        | Different spelling             |
| `uric_acid`         | `kusihape`, `uraat`              |                                |
| `ferritin`          | `ferritiin`                      |                                |

### LifeLines Cohort / Rotterdam Study (Netherlands)

Dutch uses distinct roots for several analytes:

| Internal key        | Dutch label                           | Notable difference       |
|---------------------|---------------------------------------|--------------------------|
| `age`               | `leeftijd`                            |                          |
| `sex`               | `geslacht`                            |                          |
| `height`            | `lengte`                              |                          |
| `weight`            | `gewicht`                             |                          |
| `waist`             | `tailleomtrek`, `buikomvang`          |                          |
| `fasting_glucose`   | `nuchtere_glucose`, `glucose_nuchter` |                          |
| `total_cholesterol` | `totaal_cholesterol`                  |                          |
| `TG`                | `triglyceriden`                       |                          |
| `urea_serum`        | `ureum`, `serum_ureum`                | ureum ≠ urea in English! |
| `uric_acid`         | `urinezuur`, `serum_urinezuur`        | Very different root      |
| `Hgb`               | `hemoglobine`                         | With final -e            |
| `WBC`               | `leukocyten`                          | Single t                 |
| `platelets`         | `trombocyten`, `bloedplaatjes`        |                          |
| `CRP`               | `c_reactief_proteine`                 |                          |
| `vitaminD`          | `vitamine_D`                          |                          |
| `u_albumin`         | `albumine_urine`, `micro_albumine`    |                          |

### Generation Scotland (GS:SFHS)

| Internal key | GS field name  |
|--------------|----------------|
| `sex`        | `genetic_sex`  |
| `ethnicity`  | `ethnic_group` |
| `SBP`        | `SBP_mean`     |
| `DBP`        | `DBP_mean`     |

### All of Us / OMOP CDM (LOINC codes)

For EHR-derived data modelled on the OMOP Common Data Model, concept
codes in `LOINC_XXXX_X` format are recognised for all major analytes
(hyphen in the LOINC code is replaced by underscore):

| Internal key        | LOINC code      | Analyte                                     |
|---------------------|-----------------|---------------------------------------------|
| `fasting_glucose`   | `LOINC_2345_7`  | Glucose \[Moles/volume\] in Serum/Plasma    |
| `total_cholesterol` | `LOINC_2093_3`  | Cholesterol \[Moles/volume\]                |
| `HDL_c`             | `LOINC_2085_9`  | Cholesterol in HDL                          |
| `LDL_c`             | `LOINC_13457_7` | Cholesterol in LDL (calculated)             |
| `TG`                | `LOINC_2571_8`  | Triglyceride                                |
| `HbA1c`             | `LOINC_4548_4`  | Hemoglobin A1c/Hemoglobin.total             |
| `creatinine`        | `LOINC_2160_0`  | Creatinine \[Moles/volume\] in Serum/Plasma |
| `eGFR`              | `LOINC_62238_1` | GFR/1.73 sq M.predicted                     |
| `ALT`               | `LOINC_1742_6`  | Alanine aminotransferase                    |
| `AST`               | `LOINC_1920_8`  | Aspartate aminotransferase                  |
| `albumin`           | `LOINC_1751_7`  | Albumin \[Mass/volume\] in Serum or Plasma  |
| `Hgb`               | `LOINC_718_7`   | Hemoglobin \[Mass/volume\] in Blood         |
| `WBC`               | `LOINC_6690_2`  | Leukocytes \[#/volume\] in Blood            |
| `platelets`         | `LOINC_777_3`   | Platelets \[#/volume\] in Blood             |
| `SBP`               | `LOINC_8480_6`  | Systolic blood pressure                     |
| `DBP`               | `LOINC_8462_4`  | Diastolic blood pressure                    |
| `vitaminD`          | `LOINC_62292_8` | 25-hydroxyvitamin D3                        |
| `TSH`               | `LOINC_3016_3`  | Thyrotropin                                 |
| `CRP`               | `LOINC_30522_7` | C reactive protein \[Mass/volume\] (hs)     |
| `ferritin`          | `LOINC_2276_4`  | Ferritin                                    |
| `uric_acid`         | `LOINC_3084_1`  | Urate                                       |
| `sodium`            | `LOINC_2951_2`  | Sodium                                      |
| `potassium`         | `LOINC_2823_3`  | Potassium                                   |
| `u_albumin`         | `LOINC_1754_1`  | Albumin \[Mass/volume\] in Urine            |
| `u_creatinine`      | `LOINC_2161_8`  | Creatinine \[Mass/volume\] in Urine         |
| `Homocysteine`      | `LOINC_13965_9` | Homocysteine                                |
| `testosterone`      | `LOINC_2986_3`  | Testosterone \[Moles/volume\]               |
| `BMI`               | `LOINC_39156_5` | Body mass index (BMI)                       |
| `height`            | `LOINC_8302_2`  | Body height                                 |
| `weight`            | `LOINC_29463_7` | Body weight                                 |

------------------------------------------------------------------------

## Diagnosing column matching with `hm_col_report()`

Before running any computation on a new dataset, call
[`hm_col_report()`](https://sufyansuleman.github.io/HealthMarkers/reference/hm_col_report.md)
to see exactly which columns are matched and how:

``` r
library(HealthMarkers)

# Load your biobank extract (example: HUNT data with Norwegian column names)
hunt_data <- read.csv("hunt_extract.csv")

# Run the column report
hm_col_report(hunt_data)
```

The output looks like:

    ── HealthMarkers column report ────────────────────────────────────────────
     Data: 56301 rows × 187 columns   |   Keys in dictionary: 258

     key                  data_column              how matched
     -------------------- ------------------------ ------------------
     age                  alder                    col contains synonym ✔
     sex                  kjonn                    exact ✔
     height               hoyde                    exact ✔
     fasting_glucose      fastende_blodsukker      col contains synonym ✔
     TG                   triglyserider            col contains synonym ✔
     creatinine           kreatinin                exact ✔
     SBP                  systolisk_blodtrykk      col contains synonym ✔
     eGFR                 ─                        NOT FOUND ✘

     ✔ 142 keys matched   ✘ 116 keys not found

    ── col_map template for missing keys ──────────────────────────────────────
     col_map <- list(
       eGFR = "from_your_data",  # fill in your column name
     )

For the `NOT FOUND` keys, either: 1. Add the column to your data (or
compute it from raw inputs), 2. Provide an explicit `col_map` entry, or
3. Ignore it — functions will produce `NA` for any marker that requires
that key.

``` r
# Capture the auto-detected map and add manual overrides
cm <- hm_col_report(hunt_data, verbose = FALSE)
cm$eGFR <- "ckd_epi_gfr_ml_min"   # fill in the actual column name

results <- all_health_markers(hunt_data, col_map = cm,
                              which = c("glycemic", "lipid", "renal",
                                        "inflammatory", "liver"))
```

------------------------------------------------------------------------

## Auto-derived columns

Even when raw computed inputs are not in your data, HealthMarkers can
derive them automatically from more basic measurements before marker
computation begins. The following secondary variables are auto-derived:

| Derived key        | Derived from                 | Formula/method      |
|--------------------|------------------------------|---------------------|
| `eGFR`             | `creatinine`, `age`, `sex`   | CKD-EPI 2021        |
| `UACR`             | `u_albumin`, `u_creatinine`  | ratio (mg/g)        |
| `LDL_c`            | `TC`, `HDL_c`, `TG`          | Friedewald equation |
| `WHR`              | `waist`, `hip`               | ratio               |
| `BMI`              | `height`, `weight`           | kg/m²               |
| `MAP`              | `SBP`, `DBP`                 | (SBP + 2×DBP) / 3   |
| `PP`               | `SBP`, `DBP`                 | SBP − DBP           |
| `non_HDL_c`        | `TC`, `HDL_c`                | TC − HDL_c          |
| `TC_HDL_ratio`     | `TC`, `HDL_c`                | ratio               |
| `TG_HDL_ratio`     | `TG`, `HDL_c`                | ratio               |
| `AIP`              | `TG`, `HDL_c`                | log₁₀(TG/HDL_c)     |
| `creatinine_ratio` | `creatinine`, `u_creatinine` | ratio               |

This means, for example, that
[`kidney_failure_risk()`](https://sufyansuleman.github.io/HealthMarkers/reference/kidney_failure_risk.md)
will work even if `eGFR` is absent from your data, as long as
`creatinine`, `age`, and `sex` are present.

------------------------------------------------------------------------

## Example: running on UK Biobank data

``` r
library(HealthMarkers)

# Load UKB extract (columns follow _0_0 naming)
ukb <- readRDS("ukb_pheno.rds")

# Check column matching — most UKB columns are auto-detected
hm_col_report(ukb)

# Run a broad panel — no col_map needed for UKB standard names
results <- all_health_markers(
  data    = ukb,
  which   = c("glycemic", "lipid", "liver", "renal",
              "inflammatory", "obesity_metrics", "vitamin"),
  verbose = TRUE
)
```

## Example: running on NHANES data

``` r
# NHANES lab data (LBXGLU, LBXSCH, LBXWBCSI, etc.)
nhanes_lab <- read.csv("nhanes_lab.csv")
nhanes_bp  <- read.csv("nhanes_bp.csv")
nhanes_dem <- read.csv("nhanes_demo.csv")

nhanes <- nhanes_lab |>
  dplyr::left_join(nhanes_bp,  by = "SEQN") |>
  dplyr::left_join(nhanes_dem, by = "SEQN")

results <- all_health_markers(nhanes, which = c("glycemic", "lipid",
                                                 "renal", "inflammatory"),
                               verbose = FALSE)
```

## Example: running on OMOP / All of Us data (LOINC codes)

``` r
# Columns named LOINC_2345_7, LOINC_2160_0, LOINC_718_7, etc.
omop_labs <- dplyr::collect(tbl(con, "measurement_wide"))

# All LOINC_XXXX_X columns are recognised automatically
hm_col_report(omop_labs)

results <- all_health_markers(omop_labs,
                              which = c("glycemic", "lipid", "renal",
                                        "inflammatory", "vitamin"))
```

------------------------------------------------------------------------

## Adding a custom biobank

If your biobank uses names not already in the dictionary, you have two
options:

**Option A — explicit `col_map` (immediate, no code changes):**

``` r
my_col_map <- list(
  fasting_glucose  = "p_glu_0",
  total_cholesterol = "tot_kol",
  creatinine       = "s_krea",
  age              = "alder_bij_opname",
  sex              = "geslacht_f"
)

results <- all_health_markers(data, col_map = my_col_map,
                              which = c("glycemic", "lipid", "renal"))
```

**Option B — open a GitHub issue / pull request** to add permanent
recognition of the naming system to the dictionary:

> <https://github.com/sufyansuleman/HealthMarkers/issues>

Please include: biobank name, the analyte/variable, and the column
name(s) used.

------------------------------------------------------------------------

## Summary

| Biobank / system      | Language           | Key feature                                       |
|-----------------------|--------------------|---------------------------------------------------|
| UK Biobank            | English            | `_0_0` field suffix notation                      |
| NHANES                | English            | `LBX`/`LBD`/`BPX`/`URX` prefixes                  |
| Danish registers      | Danish + NPU codes | `NPU01994` etc.; `kreatinin`, `kolesterol`        |
| HUNT / Tromsø         | Norwegian          | `triglyserider`, `karbamid`, double-k `blodtrykk` |
| FinnGen / THL         | Finnish            | `-iini` endings; `glukoosi`, `kreatiniini`        |
| Estonian Biobank      | Estonian           | `kolesterool` (double-o), `naatrium` (≠ natrium)  |
| LifeLines / Rotterdam | Dutch              | `ureum` (= urea), `urinezuur` (uric acid)         |
| Generation Scotland   | English            | `SBP_mean`, `genetic_sex`                         |
| All of Us / OMOP      | LOINC              | `LOINC_XXXX_X` format                             |
| NAKO / KORA           | German             | `Cholesterin`, `Triglyzeride`, `Harnsäure`        |
