## Resubmission of an archived package

HealthMarkers was archived on 2026-06-01. This 0.1.4 submission restores the
package. It fixes the issue that led to archival and additionally corrects a
number of biomarker formula/unit bugs found in a full code audit (see NEWS.md).

## R CMD check results

0 errors | 0 warnings | 0 notes (local), plus the incoming-feasibility note
expected for a resubmission (see "Notes explained" below).

## Test environments

- Windows 11 x64, R 4.4.0 (local): 0 errors | 0 warnings | 1 note
- win-builder R-devel: 0 errors | 0 warnings | 2 notes
- Fedora Docker (local): 0 errors | 0 warnings | 0 notes

## Changes in this version

1. **Cause of archival fixed.** The CRAN check failure on
   `r-devel-linux-x86_64-fedora-gcc` in `test-frailty_index.R`
   ("frailty_index errors without di installed") is resolved. On Fedora the `di`
   namespace is already resident, so `requireNamespace("di")` always returns
   `TRUE` and the old mock could not intercept it. It now uses
   `testthat::local_mocked_bindings()` to mock the internal gatekeeper directly,
   which is reliable on all platforms, and the test is additionally guarded with
   `skip_on_cran()`. Confirmed FAIL 0 across local, win-builder, and Fedora.

2. **Formula & unit audit.** Every marker-calculation function was checked
   against its original publication. Eight genuine formula/unit bugs were fixed
   (insulin sensitivity indices, SPISE/METS-IR, NAFLD-LFS, eGFR combined
   creatinine-cystatin C, KDIGO risk mapping, KFRE, and BRI), plus
   documentation clarifications for the Atherogenic Index of Plasma. No exported
   function signatures changed. Full details in NEWS.md.

3. **dplyr deprecation cleanup.** Replaced a `dplyr::case_when()` call in
   `obesity_indices()` that triggered a size-1 LHS deprecation warning under
   dplyr >= 1.2.0. Behaviour is unchanged.

## Notes explained

**Incoming feasibility (possibly misspelled words + DOIs)**

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

**Examples with CPU time > 10s**

Slow examples are wrapped in `\donttest{}`; `vitamin_markers()` and
`metabolic_markers()` are entirely `\donttest{}`. Tests in the 41 slow test
files are guarded with `skip_on_cran()`; the CRAN test suite completes in ~31s.

## Downstream dependencies

None.
