# HealthMarkers Coding Standard (HM-CS) — v3

Purpose
- Define a consistent, CRAN-ready, and scientifically sound standard for all HealthMarkers functions and tests.
- Provide a per-function checklist, naming conventions, and templates.
Centralized behavior.
Consistent error classes and messages.

Scope
- Applies to all exported “marker” functions, wrappers/aggregators, and internal utilities that support them.

----------------------------------------------------------------
1) API STANDARD
----------------------------------------------------------------
Function signature (exported markers)
function(
  data,
  col_map = NULL,
  verbose = FALSE,
  na_action = c("keep","omit","error"),
  na_warn_prop = 0.2,
  check_extreme = FALSE,
  extreme_action = c("warn","cap","error","ignore","NA"),
  extreme_rules = NULL,
  normalize = FALSE,
  ...
)

Rules
- First arg is data (data.frame/tibble).
- Second arg is col_map (named list), nullable if the function infers internally or doesn’t need mapping.
- Return a tibble with snake_case column names; deterministic column order.
- Use rlang::abort/warn with documented classes.

----------------------------------------------------------------
2) INPUT HANDLING
----------------------------------------------------------------
- validate_inputs(data, col_map, fun_name) when specific keys are required.
- Column inference via infer_cols() or hm_infer_cols() where needed.
- Numeric coercion: coerce character/factor to numeric; if NAs introduced, warn once with count.
- NA policy via na_action:
  - "keep": leave NAs.
  - "omit": drop rows with NA in required inputs (document which).
  - "error": abort with class.
- Zero denominators: avoid division by zero; return NA and warn once with count.

----------------------------------------------------------------
3) EXTREME VALUES
----------------------------------------------------------------
- check_extreme = FALSE by default.
- If TRUE, apply extreme_rules (per-variable ranges). Missing rules => do nothing for that variable.
- extreme_action:
  - "warn": compute; warn listing variables hit.
  - "cap": cap to bounds; warn once.
  - "NA": set offending values to NA; warn once.
  - "error": abort with class.
  - "ignore": compute without intervention.

----------------------------------------------------------------
4) VERBOSITY
----------------------------------------------------------------
- Provide verbose arg; when TRUE, emit progress via hm_inform().
- Package option: options(healthmarkers.verbose = "none"|"info"|"debug") to globally control logs (future-proof). If tests rely on message(), hm_inform should call message().

----------------------------------------------------------------
5) ERROR/WARNING CONVENTIONS
----------------------------------------------------------------
- Abort classes: healthmarkers_<topic>_error_<condition>
  e.g., healthmarkers_inflammatory_error_missing_map
- Warn classes: healthmarkers_<topic>_warn_<condition>
- Messages via hm_inform(level = "info"|"debug", msg = "...").

----------------------------------------------------------------
6) DOCUMENTATION (roxygen2)
----------------------------------------------------------------
- @title, @description with clinical context and units.
- @param: for each arg; note expected units explicitly.
- @return: tibble with listed columns and units.
- @examples: small, runnable examples with synthetic data.
- @references with \doi{...} (no raw URLs).
- Add lifecycle tags: @keywords internal where needed; @family markers; @seealso related funcs.

----------------------------------------------------------------
7) TESTING
----------------------------------------------------------------
- Happy path: correct columns returned, row count preserved.
- NA/extreme handling: behavior per na_action/extreme_action.
- Failure modes: missing keys (validate_inputs), bad types, zero denominators.
- No uncaptured warnings: use expect_warning() with regex, or suppressWarnings() if asserting results only.
- Target coverage ≥ 90% (covr).

----------------------------------------------------------------
8) WRAPPERS / AGGREGATORS
----------------------------------------------------------------
- Registry of groups with function pointers and whether col_map is required.
- Safe dispatcher filters args to formals; suppress warnings when verbose = FALSE.
- Aliases allowed (e.g., "atherogenic" -> "atherogenic_indices"); avoid duplicates for which = "all".
- Provide fallbacks where external components may be missing (e.g., .hm_mets_fallback()).

----------------------------------------------------------------
9) UX & SITE
----------------------------------------------------------------
- Vignettes:
  - healthmarkers-overview: intro and quick start.
  - marker-categories: workflows by system (liver, lipid, renal, insulin).
- pkgdown site with grouped references; README badge.
- Example data: data/example_liver, data/example_lipids; data-raw scripts for reproducibility.

----------------------------------------------------------------
10) METADATA & CRAN
----------------------------------------------------------------
- inst/CITATION; lifecycle tags; NEWS.md changelog.
- DESCRIPTION: URL, BugReports, Suggests (testthat, knitr, rmarkdown, pkgdown, covr, cli, lifecycle).
- VignetteBuilder: knitr; NAMESPACE generated by roxygen2.
- R CMD check --as-cran: 0 ERROR/WARNING/NOTE.

----------------------------------------------------------------
11) PER-FUNCTION ACCEPTANCE CHECKLIST
----------------------------------------------------------------
For FUNCTION_NAME():
- [ ] Signature matches API STANDARD (see Section 1).
- [ ] validate_inputs() used if required keys exist; errors have classes.
- [ ] Numeric coercion informs/warns on NA introduction once.
- [ ] NA policy honored for na_action; documented.
- [ ] Extreme policy honored; documented.
- [ ] Returns tibble; snake_case; stable order; units in docs.
- [ ] hm_inform used for progress when verbose = TRUE.
- [ ] Roxygen complete with \doi{}, runnable examples.
- [ ] Tests: happy path, NA/extreme, failure modes; no stray warnings.
- [ ] Added to registry (if aggregator-relevant) with needs_col_map flag.
- [ ] Coverage updated (covr).

----------------------------------------------------------------
12) IMPLEMENTATION TEMPLATES
----------------------------------------------------------------
Function skeleton
```r
#' Title: <Function purpose>
#' @description <Clinical context; required units>
#' @param data data.frame/tibble with ...
#' @param col_map named list mapping keys to columns; NULL to infer or not needed
#' @param verbose logical; default FALSE
#' @param na_action one of c("keep","omit","error"); default "keep"
#' @param na_warn_prop numeric in [0,1]; warn threshold; default 0.2
#' @param check_extreme logical; default FALSE
#' @param extreme_action one of c("warn","cap","error","ignore","NA"); default "warn"
#' @param extreme_rules named list of c(min, max) per variable; NULL for defaults
#' @param normalize logical; default FALSE
#' @return tibble with columns: <list columns with units>
#' @references \doi{10.xxxx/xxxxx}
#' @examples
#' df <- tibble::tibble(...)
#' cm <- list(...)
#' FUNCTION_NAME(df, cm, verbose = FALSE)
#' @export
FUNCTION_NAME <- function(
  data,
  col_map = NULL,
  verbose = FALSE,
  na_action = c("keep","omit","error"),
  na_warn_prop = 0.2,
  check_extreme = FALSE,
  extreme_action = c("warn","cap","error","ignore","NA"),
  extreme_rules = NULL,
  normalize = FALSE,
  ...
) {
  na_action <- match.arg(na_action)
  extreme_action <- match.arg(extreme_action)

  stopifnot(is.data.frame(data))

  # Optional: validate required keys
  # validate_inputs(data, col_map, fun_name = "FUNCTION_NAME")

  # Coercion helper
  coerce_numeric <- function(x, nm) {
    if (is.numeric(x)) return(x)
    old_na <- sum(is.na(x))
    y <- suppressWarnings(as.numeric(x))
    new_na <- sum(is.na(y))
    if (new_na > old_na) {
      rlang::warn(
        sprintf("FUNCTION_NAME(): '%s' coerced to numeric; NAs introduced: %d", nm, new_na - old_na),
        class = "healthmarkers_FUNCTION_NAME_warn_na_coercion"
      )
    }
    y
  }

  # ... compute outputs ...

  tibble::tibble(
    # columns...
  )
}
```

Test skeleton
```r
test_that("FUNCTION_NAME returns expected columns", {
  df <- tibble::tibble(...)
  cm <- list(...)
  out <- FUNCTION_NAME(df, cm, verbose = FALSE, na_action = "keep")
  expect_s3_class(out, "tbl_df")
  expect_equal(nrow(out), nrow(df))
  expect_true(all(c("col_a","col_b") %in% names(out)))
})

test_that("FUNCTION_NAME NA/extreme handling", {
  df <- tibble::tibble(...)
  cm <- list(...)
  expect_warning(
    FUNCTION_NAME(df, cm, check_extreme = TRUE, extreme_action = "cap"),
    "capped|out-of-range",
    ignore.case = TRUE
  )
})

test_that("FUNCTION_NAME fails on missing required map", {
  df <- tibble::tibble(...)
  cm <- list()  # missing
  expect_error(
    validate_inputs(df, cm, fun_name = "FUNCTION_NAME"),
    class = "rlang_error"
  )
})
```

----------------------------------------------------------------
13) MIGRATION PLAN (ORDER)
----------------------------------------------------------------
1. utils_infer-cols.R
   - Status: DONE (infer_cols, hm_infer_cols); keep as-is.
   - Action: none.

2. zzz-validate_inputs.R
   - Status: DONE (3-arg validator).
   - Action: ensure @export and tests.

3. zzz-options.R
   - Status: ADD minimal hm_inform shim.
   - Action: implement logger; replace direct message() gradually.

4. health_markers.R (wrappers/aggregator)
   - Status: PARTIAL (registry/alias/safe_call/mets fallback).
   - Action: finalize roxygen; ensure hm_inform usage; verify unknown-group errors.

5. glycemic_markers.R
   - Status: ALIGN to API standard.
   - Action: implement coercion warnings, NA/extreme policies; add roxygen & tests.

6. lipid_markers.R, atherogenic_indices.R
   - Status: ALIGN.
   - Action: apply API standard; validate_inputs for lipid set; tests.

7. liver_markers.R, liver_fat_markers.R
   - Status: ALIGN.
   - Action: API/units/extreme rules; tests.

8. renal_markers.R, ckd_stage.R
   - Status: ALIGN.
   - Action: API/units; tests.

9. inflammatory_markers.R
   - Status: GOOD; move to hm_inform; ensure docs with \doi{}; tests suppress/expect warnings.

10. Remaining families (hormone, oxidative, pulmo, etc.)
    - Status: ALIGN similarly.

11. Vignettes, pkgdown, example data, CITATION, NEWS
    - Status: ADD.

12. Coverage and R CMD check
    - Status: TARGET ≥ 90%; clean checks.

----------------------------------------------------------------
End of HM-CS v3.