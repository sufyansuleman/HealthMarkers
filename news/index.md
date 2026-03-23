# Changelog

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
  at the `"debug"` level when `verbose = FALSE`, so that
  `getOption("healthmarkers.verbose")` controls visibility consistently
  across every function.
- Computing-phase messages are now unconditionally emitted at `"debug"`
  level (independent of the per-call `verbose` argument) in
  [`bone_markers()`](https://sufyansuleman.github.io/HealthMarkers/reference/bone_markers.md),
  [`ckd_stage()`](https://sufyansuleman.github.io/HealthMarkers/reference/ckd_stage.md),
  and
  [`corrected_calcium()`](https://sufyansuleman.github.io/HealthMarkers/reference/corrected_calcium.md).

### Internal

- Package gains a `_pkgdown.yml` that lists all 46 vignettes in the
  articles index for the pkgdown site.
