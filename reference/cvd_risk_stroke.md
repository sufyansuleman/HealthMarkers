# Stroke 10-year risk

Wrapper around
[`PooledCohort::predict_10yr_stroke_risk()`](https://bcjaeger.github.io/PooledCohort/reference/predict_10yr_ascvd_risk.html)
with quiet fallback to NA if the backend errors.

## Usage

``` r
cvd_risk_stroke(data, col_map = NULL, na_warn_prop = 0.2, verbose = TRUE, ...)
```

## Arguments

- data:

  A data frame with the required cardiovascular risk columns.

- col_map:

  Optional named list mapping internal keys (`age`, `sex`, `race`,
  `smoker`, `total_chol`, `HDL_c`, `sbp`, `bp_treated`, `diabetes`,
  `bmi`) to actual column names in `data`. If `NULL` (default), column
  names are auto-inferred then fall back to the key names themselves.
  `sex` accepts `1`/`0`, `"m"`/`"f"`, or `"male"`/`"female"`
  (case-insensitive).

- na_warn_prop:

  Proportion (0-1) to flag high missingness warnings (default 0.2). Only
  used when `verbose = TRUE`.

- verbose:

  Logical; if TRUE, prints progress and a short summary.

- ...:

  Passed to
  [`PooledCohort::predict_10yr_stroke_risk()`](https://bcjaeger.github.io/PooledCohort/reference/predict_10yr_ascvd_risk.html).

## Value

A tibble with `model`, `year`, `risk`.

## References

Goff DC, Lloyd-Jones DM, Bennett G, Coady S, D'Agostino RB, et al.
(2014). “2013/2014 ACC/AHA Guideline on the Assessment of Cardiovascular
Risk.” *Circulation*, **129**(25 Suppl 2), S49–S73.
[doi:10.1161/01.cir.0000437741.48606.98](https://doi.org/10.1161/01.cir.0000437741.48606.98)
, Pooled Cohort Equations; ACC/AHA Task Force on Practice Guidelines.

## Examples

``` r
if (requireNamespace("PooledCohort", quietly = TRUE)) {
  df <- data.frame(age = 55, sex = 1, race = "white", smoker = FALSE,
    total_chol = 200, HDL_c = 50, sbp = 140, bp_treated = FALSE,
    diabetes = FALSE, bmi = 27)
  cvd_risk_stroke(df)
}
#> cvd_risk_stroke(): preparing inputs; non-finite=1, high-NA=1, all-NA=1
#> cvd_risk_stroke(): results: 1 row(s)
#> # A tibble: 1 × 3
#>   model   year   risk
#>   <chr>  <int>  <dbl>
#> 1 Stroke    10 0.0652
```
