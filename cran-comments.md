## R CMD check results (Fedora Docker)

0 errors | 0 warnings | 0 notes

## Test environments

- Windows 11 x64, R 4.4.0 (local): 0 errors | 0 warnings | 1 note
- win-builder R-devel (2026-05-12 r90049): 0 errors | 0 warnings | 2 notes
- Fedora Docker (local): 0 errors | 0 warnings | 0 notes

## Notes explained

**Note 1 — CRAN incoming feasibility (new submission + possibly misspelled words + DOIs)**

- "New submission" — this is a first CRAN submission.
- "Possibly misspelled words" — all flagged words are correct technical or
  medical terms in common use: ASCVD, CKD, DXA, FRAX, KFRE, QRISK, eGFR,
  iAge, Charlson, Rockwood, Framingham, Dyspnea, absorptiometry, atherogenic,
  biofluids, glycaemic, spirometry, steatosis, tibble, anthropometric, pre.
  None are misspellings.
- "Possibly invalid DOIs" — two DOIs in man/normalize_vec.Rd returned 404 on
  win-builder:
    - doi:10.1007/s10519-009-9241-9 (Beasley et al. 2009, Behavior Genetics)
    - doi:10.1136/bmj.313.7047.41 (Bland & Altman 1996, BMJ)
  Both are valid and resolve correctly in a browser. The 404s are due to
  Springer and BMJ blocking automated DOI checks from the win-builder server.
  The DOIs have been verified manually.

**Note 2 — Examples with CPU time > 10s**

`metabolic_markers()` was flagged at 10.03s on win-builder. The example has
been moved into a `\donttest{}` block in this submission.

## Resubmission notes

This is a resubmission for version 0.1.3 addressing feedback from Uwe Ligges:

1. **Slow examples (> 10s)**: Replaced slow examples with fast smoke-test
   examples for all 7 flagged functions. Full examples moved into `\donttest{}`
   blocks. `vitamin_markers()` (requires 25 mandatory columns) and
   `metabolic_markers()` are entirely `\donttest{}`.

2. **Vignette build time (28 min)**: All vignettes live in
   `vignettes/articles/` (pkgdown-only articles), which R CMD check does not
   build. `vignettes/articles/` is listed in `.Rbuildignore`. There are no
   vignettes in the built package tarball.

3. **Test suite run time**: Added `skip_on_cran()` to the first test in each
   of the 41 slow test files. Tests now complete in ~31s on CRAN (was 522s).

4. **CRAN-like Suggests check**: Fixed `tests/testthat/test-frailty_index.R` so
   it reliably simulates a missing `di` package during `R_CHECK_SUGGESTS_ONLY=false`.

5. **Metadata and documentation updates**: Updated `DESCRIPTION` title and
   description for a more realistic CRAN package summary, and tightened the
   README installation and quick-start sections.

6. **No API changes**: This submission contains only documentation, metadata,
   and test infrastructure updates; no exported function behavior has been
   changed.

7. **Fedora build / CRAN check (`r-devel-linux-x86_64-fedora-gcc` ERROR)**:  
   Root cause identified and fixed. The test `test-frailty_index.R:16`
   ("frailty_index errors without di installed") failed because on Fedora the
   `di` namespace is already resident in the session, so `requireNamespace("di")`
   always returns `TRUE` — the old `withr::with_libpaths()` / `base::requireNamespace`
   mock could not intercept it. Fixed by replacing that approach with
   `testthat::local_mocked_bindings(.need_pkg_di = ..., .package = "HealthMarkers")`
   which mocks the internal gatekeeper function directly and is reliable on all
   platforms. Confirmed FAIL 0 locally.

## Downstream dependencies

None — this is a new package.
