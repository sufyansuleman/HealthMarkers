# Changelog

## HealthMarkers 0.1.4

CRAN release: 2026-06-17

### Formula & unit audit

This release follows a systematic, script-by-script audit of every
marker-calculation function in the package, checking each formula and
its unit handling against the original publication. The
supporting/utility scripts were also confirmed to apply no hidden unit
conversions (column inference maps names only; the global
pre-computation helper keeps variables in their canonical units). The
fixes below correct genuine formula/unit bugs; the great majority of
functions were verified correct and unchanged.

#### Bug fixes — insulin sensitivity indices

- **[`fasting_is()`](https://sufyansuleman.github.io/HealthMarkers/reference/fasting_is.md)
  — `HOMA_IR_inv`.** HOMA-IR (Matthews et al. 1985) is defined with
  glucose in mmol/L (divisor 22.5). The previous code applied the mg/dL
  conversion (`*18`) while keeping the 22.5 divisor, yielding values 18x
  too large. Now uses raw mmol/L glucose. `HOMA_IR_rev_inv` (mg/dL
  convention, `/405`) already produced the correct value and is
  unchanged; the two columns are now equal, as intended.

- **[`fasting_is()`](https://sufyansuleman.github.io/HealthMarkers/reference/fasting_is.md)
  — `FIRI`.** The Fasting Insulin Resistance Index (Duncan et al. 1995)
  is defined with glucose in mmol/L (divisor 25). Now uses raw mmol/L
  glucose instead of the mg/dL-converted value (previously 18x too
  large).

- **[`fasting_is()`](https://sufyansuleman.github.io/HealthMarkers/reference/fasting_is.md)
  — `QUICKI`.** Now uses `log10` (as defined by Katz et al.

  2000. instead of natural log. Glucose (mg/dL) and insulin (muU/mL)
        units were already correct.

- **[`ogtt_is()`](https://sufyansuleman.github.io/HealthMarkers/reference/ogtt_is.md)
  — `Cederholm_index`.** Cederholm & Wibell (1990) is defined with
  glucose in mmol/L; the formula’s `*180` term is itself the mmol/L-\>mg
  conversion. The previous code pre-converted glucose to mg/dL (`*18`),
  causing a double conversion and a non-linear distortion of the index.
  Now uses raw mmol/L glucose. `Gutt_index` (defined in mg/dL, no `*180`
  term) is unchanged and correct.

#### Bug fixes — other markers

- **[`glycemic_markers()`](https://sufyansuleman.github.io/HealthMarkers/reference/glycemic_markers.md)
  — `SPISE` and `METS_IR`.** Both indices are defined for inputs in
  **mg/dL** (SPISE: Paulmichl 2016; METS-IR: Bello-Gaytan 2018) and
  carry validated cut-offs in those units. The previous code fed raw
  mmol/L values, giving systematically wrong (non-constant) results. Now
  converts HDL-c (`*38.67`), TG (`*88.57`) and glucose (`*18`) to mg/dL
  internally. `TyG_index` was already correct.

- **[`liver_fat_markers()`](https://sufyansuleman.github.io/HealthMarkers/reference/liver_fat_markers.md)
  — `NAFLD_LFS`.** The NAFLD Liver Fat Score (Kotronen et al. 2009)
  codes type-2 diabetes as **yes = 2 / no = 0**, so the diabetic
  contribution is `0.45 * 2 = 0.90`. The previous code used a 0/1
  indicator, giving half the intended contribution for people with
  diabetes. Fixed.

- **[`renal_markers()`](https://sufyansuleman.github.io/HealthMarkers/reference/renal_markers.md)
  — `eGFR_combined`.** The combined creatinine–cystatin C CKD-EPI
  equation (Inker et al. 2012) uses its own creatinine `alpha` (-0.207
  male / -0.248 female) and its own female (`x0.969`) and Black
  (`x1.08`) multipliers. The previous code reused the creatinine-only
  `alpha` (-0.411 / -0.329) and incorrect multipliers (`1.008`,
  `1.145`). All three corrected. `eGFR_cr` (2009) and `eGFR_cys` (2012)
  were already correct.

- **[`ckd_stage()`](https://sufyansuleman.github.io/HealthMarkers/reference/ckd_stage.md)
  — `KDIGO_risk`.** Two cells of the KDIGO 2012 risk heatmap were
  mis-mapped: G1/G2 + A3 now returns **High** (was “Moderate”), and
  G3a + A3 now returns **Very High** (was “High”). GFR and albuminuria
  stage cut-offs were already correct.

- **[`kidney_failure_risk()`](https://sufyansuleman.github.io/HealthMarkers/reference/kidney_failure_risk.md)
  — KFRE.** The 4-variable Kidney Failure Risk Equation (Tangri et
  al. 2011) was reimplemented correctly. The previous linear predictor
  used log-transformed age and eGFR (the equation uses `age/10` and
  `eGFR/5` linearly), the wrong sign on age, no covariate centring, an
  incorrect sex coefficient, and baseline-survival constants matching no
  published calibration. Now uses the non-North-American calibration:
  `PI = -0.2201*(age/10 - 7.036) + 0.2467*(male - 0.5642) - 0.5567*(eGFR/5 - 7.222) + 0.4510*(ln(ACR_mmol) - 5.137)`,
  `S0(2y) = 0.9832`, `S0(5y) = 0.9365`. UACR continues to be supplied in
  mg/g and is converted internally to mg/mmol (`/8.84`).

- **[`obesity_indices()`](https://sufyansuleman.github.io/HealthMarkers/reference/obesity_indices.md)
  — `BRI`.** The Body Roundness Index (Thomas et al. 2013) eccentricity
  ratio is `WC / (pi * height)`; the previous code used
  `WC / (2*pi * height)` (half the correct value), which produced
  near-zero or negative BRI for normal adults. Fixed.

#### Documentation clarifications

- **[`atherogenic_indices()`](https://sufyansuleman.github.io/HealthMarkers/reference/atherogenic_indices.md)
  and
  [`cvd_marker_aip()`](https://sufyansuleman.github.io/HealthMarkers/reference/cvd_marker_aip.md)
  — AIP units.** The Atherogenic Index of Plasma `log10(TG/HDL)` is not
  scale-invariant (TG and HDL-c convert to mg/dL with different
  factors), and its published risk strata assume mmol/L. Documentation
  and the
  [`atherogenic_indices()`](https://sufyansuleman.github.io/HealthMarkers/reference/atherogenic_indices.md)
  example were corrected to mmol/L, with an explicit note that mg/dL
  inputs shift AIP by about +0.36. Formulas are unchanged.

#### Internal

- **[`obesity_indices()`](https://sufyansuleman.github.io/HealthMarkers/reference/obesity_indices.md)
  — dplyr deprecation.** Replaced the unit-normalization
  [`dplyr::case_when()`](https://dplyr.tidyverse.org/reference/case-and-replace-when.html)
  calls with scalar conversion factors. `weight_unit` and `height_unit`
  are scalar arguments, so the previous size-1 LHS / vector RHS pattern
  triggered a deprecation warning under dplyr \>= 1.2.0. Behaviour is
  unchanged.

#### Metadata

- Bumped `Version` to 0.1.4.

## HealthMarkers 0.1.3

### Bug fixes and CRAN checks

- **CRAN / Fedora `r-devel-linux-x86_64-fedora-gcc` ERROR fix.**  
  `test-frailty_index.R` test 1 (“frailty_index errors without di
  installed”) failed on Fedora because the `di` namespace is already
  resident in memory for the whole test session, so
  [`requireNamespace("di")`](https://rdrr.io/r/base/ns-load.html) always
  returns `TRUE` regardless of
  [`withr::with_libpaths()`](https://withr.r-lib.org/reference/with_libpaths.html)
  or mocking
  [`base::requireNamespace`](https://rdrr.io/r/base/ns-load.html). Fixed
  by mocking `.need_pkg_di()` directly in the `HealthMarkers` namespace
  via `testthat::local_mocked_bindings(.package = "HealthMarkers")`.
  This is reliable on all platforms regardless of whether `di` is
  installed or loaded.

- **Metadata** — Bumped `Version` to 0.1.3 and updated `CRAN-SUBMISSION`
  and `cran-comments.md` to reflect the resubmission.

## HealthMarkers 0.1.2

CRAN release: 2026-05-19

### New features

- **[`hm_normalize()`](https://sufyansuleman.github.io/HealthMarkers/reference/hm_normalize.md)
  — post-computation normalisation helper (new function).** Apply
  z-score, rank-based inverse-normal transform (Rankit), min-max, or
  robust median/MAD scaling to any marker output data frame. Use `cols`
  to target specific columns and `skip_cols` to protect covariates
  (e.g. age, BMI) from being rescaled. This covers domain functions
  whose internal `normalize` argument currently has no effect
  ([`glycemic_markers()`](https://sufyansuleman.github.io/HealthMarkers/reference/glycemic_markers.md),
  [`lipid_markers()`](https://sufyansuleman.github.io/HealthMarkers/reference/lipid_markers.md),
  [`renal_markers()`](https://sufyansuleman.github.io/HealthMarkers/reference/renal_markers.md),
  etc.). See
  [`?hm_normalize`](https://sufyansuleman.github.io/HealthMarkers/reference/hm_normalize.md).

- **Multi-biobank column inference (major).** The internal synonym
  dictionary
  ([`.hm_default_col_patterns_exact()`](https://sufyansuleman.github.io/HealthMarkers/reference/dot-hm_default_col_patterns_exact.md))
  now recognises column-naming conventions from 15+ major cohort studies
  and biobanks for all primary analytes:

  - **UK Biobank** — `_0_0` / `_1_0` field naming (e.g. `glucose_0_0`,
    `creatinine_0_0`, `vitamin_d_0_0`); `standing_height`, `25OHD`.
  - **NHANES** — LBXS, LBDS, BPX, URX prefix variables (e.g. `LBXGLU`,
    `LBXSCH`, `BPXSY1`, `URXUMA`).
  - **Danish national registers / EHR (LABKA/OPEN)** — NPU codes
    (`NPU01994` for creatinine, `NPU01567` for cholesterol, `NPU03609`
    for haemoglobin, etc.) and Danish clinical labels (`kreatinin`,
    `kolesterol`, `blodsukker`, `leukocytter`, `trombocytter`,
    `d_vitamin`).
  - **HUNT Study and Tromsø Study (Norway)** — Norwegian-language terms
    (`blodsukkerfasting`, `systolisk_blodtrykk`, `triglyserider`,
    `karbamid`, `kjonn`, `midjeomkrets`).
  - **SCAPIS / TwinGene (Sweden)** — Swedish-language terms (`glukos`,
    `urinsyra`, `leukocyter`, `trombocyter`, `kön`, `längd`, `vikt`).
  - **FinnGen / THL Biobank (Finland)** — Finnish-language terms
    (`glukoosi`, `kolesteroli`, `kreatiniini`, `hemoglobiini`,
    `sukupuoli`, `virtsahappo`, `leukosyytit`, `ferritiini`,
    `D_vitamiini`).
  - **Estonian Biobank (EstBB)** — Estonian terms (`glukoos`,
    `kolesterool`, `kreatiniin`, `hemoglobiin`, `naatrium`, `kaalium`,
    `vanus`, `sugu`).
  - **LifeLines Cohort / Rotterdam Study (Netherlands)** — Dutch terms
    (`nuchtere_glucose`, `totaal_cholesterol`, `urinezuur`, `ureum`,
    `leukocyten`, `hemoglobine`, `vitamine_D`, `tailleomtrek`).
  - **Generation Scotland (GS:SFHS)** — `SBP_mean`, `DBP_mean`,
    `genetic_sex`, `ethnic_group`.
  - **All of Us / OMOP CDM** — LOINC codes in `LOINC_XXXX_X` format for
    all major analytes (e.g. `LOINC_2345_7` for fasting glucose,
    `LOINC_2160_0` for creatinine, `LOINC_718_7` for haemoglobin).
  - **NAKO / KORA (Germany)** — German-language terms (`Cholesterin`,
    `Triglyzeride`, `Harnsäure`, `Harnstoff`, `Leukozyten`,
    `Thrombozyten`).

- Added
  [`hm_col_report()`](https://sufyansuleman.github.io/HealthMarkers/reference/hm_col_report.md)
  — an interactive column-mapping diagnostic. Call
  `hm_col_report(your_data)` **before** running any computation to see a
  formatted table of which internal keys were matched to columns in your
  data and how (exact synonym, case-insensitive, substring, or fuzzy),
  plus a ready-to-paste `col_map` template for any unmatched keys.  
  The function uses a five-layer matching pipeline and returns the
  matched mappings invisibly so the result can be passed directly as
  `col_map` to any HealthMarkers function.

- **Auto-derivation of computed inputs.**
  [`.hm_global_precompute()`](https://sufyansuleman.github.io/HealthMarkers/reference/dot-hm_global_precompute.md)
  now automatically derives 18+ secondary columns before marker
  computation begins, so functions that require (e.g.) `eGFR`, `UACR`,
  `WHR`, `LDL_c`, or `waist` no longer fail silently when only the raw
  inputs are present. Affected keys include `eGFR` (from
  creatinine/age/sex via CKD-EPI), `WHR` (from `waist` and `hip`),
  `UACR` (from `u_albumin` / `u_creatinine`), `LDL_c` (Friedewald from
  TC/HDL_c/TG), `MAP`, `PP`, `BMI` (from height/weight), and more.

- Expanded the internal synonym dictionary (`R/utils_infer-cols.R`) for
  all variable groups, additionally incorporating:

  - Inter99 / ADDITION real phenotype dataset labels
  - Common literature spellings (`TryG`, `TAG`, `TRIG`,
    `triacylglycerol`; `SGPT`/`GPT`/`ALAT`; `hsCRP`/`hs_CRP`)
  - Longitudinal follow-up suffixes (`_0`, `_1`, `_3`, `_5`)
  - NMR metabolomics (urine) panel, ECG markers, lifestyle covariates,
    and cytokine multiplex proteins.

### Bug fixes

- [`adiposity_sds()`](https://sufyansuleman.github.io/HealthMarkers/reference/adiposity_sds.md):
  fixed `fn_name` lookup bug that caused incorrect error messages when
  validation failed on non-standard column names.

### Internal

- Renamed the internal verbose-message helper from
  [`hm_col_report()`](https://sufyansuleman.github.io/HealthMarkers/reference/hm_col_report.md)
  to
  [`hm_fmt_col_map()`](https://sufyansuleman.github.io/HealthMarkers/reference/hm_fmt_col_map.md)
  to avoid a name collision with the new exported function.

## HealthMarkers 0.1.1

### New features

- Added
  [`liver_fat_markers()`](https://sufyansuleman.github.io/HealthMarkers/reference/liver_fat_markers.md)
  for hepatic steatosis and fibrosis index calculation.
- Added
  [`nfl_marker()`](https://sufyansuleman.github.io/HealthMarkers/reference/nfl_marker.md)
  for plasma neurofilament light chain (NfL) z-score computation.
- Added
  [`impute_missing()`](https://sufyansuleman.github.io/HealthMarkers/reference/impute_missing.md)
  for within-row imputation of missing biomarker values.

### Bug fixes and improvements

- [`pulmo_markers()`](https://sufyansuleman.github.io/HealthMarkers/reference/pulmo_markers.md):
  fixed column-inference logic for spirometry z-score inputs.
- All marker functions: standardised verbose progress messages to emit
  at the “debug” level when `verbose = FALSE`, so that
  `getOption("healthmarkers.verbose")` controls visibility consistently
  across every function.
- Computing-phase messages are now unconditionally emitted at “debug”
  level (independent of the per-call `verbose` argument) in
  [`bone_markers()`](https://sufyansuleman.github.io/HealthMarkers/reference/bone_markers.md),
  [`ckd_stage()`](https://sufyansuleman.github.io/HealthMarkers/reference/ckd_stage.md),
  and
  [`corrected_calcium()`](https://sufyansuleman.github.io/HealthMarkers/reference/corrected_calcium.md).
- [`all_health_markers()`](https://sufyansuleman.github.io/HealthMarkers/reference/all_health_markers.md):
  column inference is now keyed to the requested groups, falls back to
  regex-based
  [`infer_cols()`](https://sufyansuleman.github.io/HealthMarkers/reference/infer_cols.md)
  when exact matches fail, and reports per-group status (including
  missing optional packages) in verbose summaries.
- Cardiovascular risk wrappers:
  [`cvd_risk_qrisk3()`](https://sufyansuleman.github.io/HealthMarkers/reference/cvd_risk_qrisk3.md)
  now prefers the correctly named `ethnicity` column (typo tolerated for
  backward compatibility), and optional dependency errors now report the
  missing package name consistently.
- Shared validation: lipid marker helpers now route through
  [`hm_validate_inputs()`](https://sufyansuleman.github.io/HealthMarkers/reference/hm_validate_inputs.md)
  so duplicate/empty mappings are caught uniformly.

### Internal

- Package gains a `_pkgdown.yml` that lists all 46 vignettes in the
  articles index for the pkgdown site.
