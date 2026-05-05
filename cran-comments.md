## R CMD check results

0 errors | 0 warnings | 1 note

## Resubmission notes

This is a resubmission addressing feedback from Uwe Ligges:

1. **Slow examples (> 10s)**: Replaced slow examples with fast smoke-test
   examples for all 7 flagged functions. Full examples moved into `\donttest{}`
   blocks. `vitamin_markers()` (requires 25 mandatory columns) is entirely
   `\donttest{}`.

2. **Vignette build time (28 min)**: All vignettes live in
   `vignettes/articles/` (pkgdown-only articles), which R CMD check does not
   build. `vignettes/articles/` is listed in `.Rbuildignore`. There are no
   vignettes in the built package tarball.

3. **Test suite run time**: Added `skip_on_cran()` to the first test in each
   of the 41 slow test files. Tests now complete in ~31s on CRAN (was 522s).

## Test environments

- Windows 11 x64, R 4.4.0 (local): 0 errors | 0 warnings | 1 note
- Total check time: 3m 50s (was 13m 49s)
- win-builder R-devel: pending

## Notes explained

- "Non-staged installation was used" — Windows/gcc compiler artefact, not
  fixable, CRAN is aware this is system-specific.
- "Unable to verify current time" — transient network issue on local machine.

## Downstream dependencies

None — this is a new package.
