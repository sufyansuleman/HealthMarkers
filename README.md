HealthMarkers
================

- [🔍 Overview](#-overview)
  - [📦 Installation](#-installation)
  - [📑 Overview of Bundles](#-overview-of-bundles)
  - [⚙️ Usage Examples](#️-usage-examples)
  - [🌐 Online Documentation](#-online-documentation)
  - [📚 References](#-references)
  - [🤝 Contributing](#-contributing)
  - [📜 License](#-license)

<!-- badges: start -->

[![CRAN_Status_Badge](https://www.r-pkg.org/badges/version/HealthMarkers)](https://cran.r-project.org/package=HealthMarkers)
[![Travis_build_status](https://travis-ci.com/sufyansuleman/HealthMarkers.svg?branch=main)](https://travis-ci.com/sufyansuleman/HealthMarkers)

<!-- badges: end -->

# 🔍 Overview

**HealthMarkers** is a unified R package that computes a **comprehensive
suite** of physiological and clinical biomarkers covering metabolic,
cardiometabolic, hepatic, renal, pulmonary, and novel biofluid derived
bimarkers.

This README explains in detail how each bundle of calculations works,
what inputs they require, and which functions implement them.

------------------------------------------------------------------------

## 📦 Installation

``` r
# CRAN release
install.packages("HealthMarkers")

# Development version
# remotes::install_github("sufyansuleman/HealthMarkers")
```

------------------------------------------------------------------------

## 📑 Overview of Bundles

### 1. Insulin Sensitivity & Resistance

| Category | Function | Description |
|:---|:---|:---|
| Fasting | `fasting_indices()` | Computes 10 indices from fasting glucose (G0) and insulin (I0): HOMA-IR (and inverse), QUICKI, Raynaud, Bennett, FIRI, Belfiore_basal, IG_ratio_basal, ISI_basal, HOMA_rev_inv. Each uses unit conversions: G0×18→mg/dL, I0÷6→µU/mL. |
| OGTT | `ogtt_indices()` | Derives dynamic IS estimates at 0,30,120 min: Matsuda (AUC & row-mean), Gutt, Cederholm, Avignon_Si0/120, BIGTT-SI, Belfiore_ISI, HIRI_inv, Ifc_inv. Uses AUC formulas and log transforms. |
| Adipose | `adipose_indices()` | Integrates TG, HDL_c, FFA with anthropometry: Revised_QUICKI, VAI_Men/Women_inv, LAP_Men/Women_inv, TyG_inv, TG_HDL_C_inv, McAuley, Adipo_inv, Belfiore_inv_FFA (FFA AUC). Conversions: TG×88.57, HDL×38.67. |
| Tracer/DXA | `tracer_dxa_indices()` | Uses tracer rates (`rate_palmitate`, `rate_glycerol`) and DXA fat_mass to compute LIRI_inv, Lipo_inv, ATIRI_inv. Each index inverts a published regression-based formula (e.g. LIRI via Bellissimo equation). |
| All | `all_insulin()` | Wrapper to run any combination of the four above; `mode = "IS"` or `"IR"` toggles inversion. |

### 2. Anthropometry & Metabolic Syndrome

| Metric | Function | Details and References |
|:---|:---|:---|
| Adiposity SDS | `adiposity_sds()` | Standardized z-scores for BMI, waist, body-fat%, WHR, WHtR. Sex-stratified using Danish reference growth curves (Klein et al. 2020). Option `reference = "Danish"`. |
| MetS Severity | `metss_score()` | Continuous MetS Severity Score by Wiley & Carrington (2016). Accounts for sex and race with published coefficients. |

### 3. Lipids & Cardiovascular Ratios

| Measure | Function | Formula / Note |
|:---|:---|:---|
| Non-HDL | `non_hdl()` | `TC - HDL_c`. |
| Remnant Cholesterol | `remnant_chol()` | `TC - LDL_c - HDL_c`. |
| Ratios | `lipid_ratios()` | TC/HDL, TG/HDL, LDL/HDL, ApoB/ApoA1. Atherogenic Index of Plasma (AIP) = log(TG/HDL). |

### 4. Liver Markers

| Score | Function | Formula & Reference |
|:---|:---|:---|
| Fatty Liver Index | `fli()` | Bedogni equation: uses BMI, waist, TG, GGT (Bedogni et al. 2006). |
| NAFLD Fibrosis Score | `nfs()` | Uses age, BMI, AST/ALT, platelets, albumin (Angulo et al. 2007). |
| APRI | `apri()` | `(AST/upper_limit_AST) / platelets ×100`. |
| FIB-4 | `fib4()` | `(age × AST)/(platelets × sqrt(ALT))`. |
| BARD, ALBI, MELD-XI | `bard()`, `albi()`, `meld_xi()` | Published formulas combining bilirubin, albumin, INR, creatinine. |

### 5. Cardiometabolic Flags

| Flag | Function | Rule |
|:---|:---|:---|
| Dyslipidemia | `flag_dyslip()` | TG≥1.7 mmol/L or HDL_c \<1.0 (♂)/\<1.3 (♀). |
| IR | `flag_ir()` | HOMA-IR \>2.5 (literature threshold), QUICKI \<0.33. |
| Hyperglycemia | `flag_hypergly()` | Fasting glucose ≥5.6 mmol/L or HbA1c ≥5.7%. |
| Hypertension | `flag_hypert()` | SBP≥130 or DBP≥80 mmHg. |

### 6. Glycemic & Non-Insulin Indices

| Index | Function | Note |
|:---|:---|:---|
| SPISE | `spise()` | `600 × HDL_c^0.185/(TG^0.2 × BMI^1.338)`. |
| METS-IR | `mets_ir()` | `(ln(2×G0) + ln(TG)) × BMI)/(ln(HDL_c))`. |
| C-peptide HOMA | `homa_cp()` | Uses fasting C-peptide instead of insulin for HOMA. |

### 7. Novel Biofluid Panels

**Saliva** (functions `saliva_*`):

- `saliva_cortisol_auc()`: AUC trapezoidal for CAR.
- `saliva_amylase_log()`: log-transform amylase.
- `saliva_glucose()`.

**Sweat** (functions `sweat_*`):

- `sweat_ions()`: computes Na/K ratio, identifies Cl outliers.
- `sweat_rate()`: calculates sweat volume per area per hour.

**Urine** (functions `urine_*`):

- `uacr()`, `eGFR_ckdepi()`, `fena()`, `upcr()`.

### 8. Pulmonary Function

| Metric | Function | Source & Details |
|:---|:---|:---|
| FEV1 %pred | `spirometry_pred()` | Uses `rspiro::predict_spiro()` with Dinit equations for Danish adult reference (Quanjer et al. 2012). |
| FVC %pred | same |  |
| FEV1/FVC ratio | same |  |

------------------------------------------------------------------------

## ⚙️ Usage Examples

See the [Getting Started
vignette](vignettes/Getting-Started-with-HealthMarkers.html) for full
code walkthroughs.

------------------------------------------------------------------------

## 🌐 Online Documentation

- **Reference manual**:
  <https://sufyansuleman.github.io/HealthMarkers/reference/index.html>
- **pkgdown site**: <https://sufyansuleman.github.io/HealthMarkers/>

------------------------------------------------------------------------

## 📚 References

Full citation list in `NEWS.md` and the vignette. Update as needed.

------------------------------------------------------------------------

## 🤝 Contributing

Please follow the established style and add tests/examples. Submit via
GitHub PR.

------------------------------------------------------------------------

## 📜 License

MIT © [Sufyan Suleman](https://github.com/sufyansuleman)
