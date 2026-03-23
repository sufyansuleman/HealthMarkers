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

## Internal

* Package gains a `_pkgdown.yml` that lists all 46 vignettes in the articles
  index for the pkgdown site.
