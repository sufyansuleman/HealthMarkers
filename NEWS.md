# HealthMarkers 0.1.2 (development)

## New features

* Added `hm_col_report()` — an interactive column-mapping diagnostic. Call
  `hm_col_report(your_data)` **before** running any computation to see a
  formatted table of which internal keys were matched to columns in your data
  and how (exact synonym, case-insensitive, substring, or fuzzy), plus a
  ready-to-paste `col_map` template for any unmatched keys.  
  The function uses a five-layer matching pipeline and returns the matched
  mappings invisibly so the result can be passed directly as `col_map` to any
  HealthMarkers function.

* Expanded the internal synonym dictionary (`.hm_default_col_patterns_exact()`)
  for all variable groups, incorporating column naming conventions from:
  - The Inter99 / ADDITION real phenotype dataset (e.g. `pglu0`, `insu0`,
    `trig`, `hdlc`, `alat`, `alb`, `crea`, `ualbcrea`, `vitd25`, `dhaes`)
  - UK Biobank-style names (e.g. `sbp`, `standing_height`, `25OHD`)
  - Common literature spellings (e.g. `TryG`, `TAG`, `TRIG`, `triacylglycerol`
    for triglycerides; `SGPT`/`GPT`/`ALAT` for ALT; `hsCRP`/`hs_CRP` for CRP)
  - Longitudinal follow-up suffixes (`_0`, `_1`, `_3`, `_5`)
  - Added new variable groups: urine electrolytes/metabolomics (NMR panel),
    ECG markers, lifestyle covariates, and cytokine multiplex panel proteins.

## Internal

* Renamed the internal verbose-message helper from `hm_col_report()` to
  `hm_fmt_col_map()` to avoid a name collision with the new exported function.

# HealthMarkers 0.1.1

## New features

* Added `liver_fat_markers()` for hepatic steatosis and fibrosis index calculation.
* Added `nfl_marker()` for plasma neurofilament light chain (NfL) z-score computation.
* Added `impute_missing()` for within-row imputation of missing biomarker values.

## Bug fixes and improvements

* `pulmo_markers()`: fixed column-inference logic for spirometry z-score inputs.
* All marker functions: standardised verbose progress messages to emit at the
  `"debug"` level when `verbose = FALSE`, so that `getOption("healthmarkers.verbose")`
  controls visibility consistently across every function.
* Computing-phase messages are now unconditionally emitted at `"debug"` level
  (independent of the per-call `verbose` argument) in `bone_markers()`,
  `ckd_stage()`, and `corrected_calcium()`.
* `all_health_markers()`: column inference is now keyed to the requested
  groups, falls back to regex-based `infer_cols()` when exact matches fail, and
  reports per-group status (including missing optional packages) in verbose
  summaries.
* Cardiovascular risk wrappers: `cvd_risk_qrisk3()` now prefers the correctly
  named `ethnicity` column (typo tolerated for backward compatibility), and
  optional dependency errors now report the missing package name consistently.
* Shared validation: lipid marker helpers now route through `hm_validate_inputs()`
  so duplicate/empty mappings are caught uniformly.

## Internal

* Package gains a `_pkgdown.yml` that lists all 46 vignettes in the articles
  index for the pkgdown site.
