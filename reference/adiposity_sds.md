# Calculate standardized scores (SDS) for adiposity measures

Computes standard deviation (z) scores for anthropometric variables
relative to a single (non-sex-stratified) reference set of means and
standard deviations. Includes input validation, optional raw-value
extreme screening/capping, configurable handling of extreme SDS values,
NA row policies, optional concise summary output, and optional verbose
progress messages.

## Usage

``` r
adiposity_sds(
  data,
  col_map = NULL,
  ref,
  na_action = c("keep", "omit", "error"),
  extreme_action = c("cap", "NA", "error", "warn", "ignore"),
  sds_cap = 6,
  check_extreme = FALSE,
  extreme_rules = NULL,
  diagnostics = FALSE,
  warn_thresholds = list(na_prop = 0.05, extreme_prop = 0.01),
  id_col = NULL,
  return_summary = FALSE,
  verbose = FALSE,
  na_strategy = NULL,
  extreme_strategy = NULL,
  check_raw_extreme = NULL,
  raw_extreme_rules = NULL
)
```

## Arguments

- data:

  data.frame or tibble containing the measurement columns.

- col_map:

  Optional named list mapping reference variable names to column names
  in `data`. If NULL, identity mapping is assumed (names(ref) must be in
  data).

- ref:

  Named list where each element is a numeric vector with names `mean`
  and `sd`, e.g. `list(BMI = c(mean = 23, sd = 4))`.

- na_action:

  One of `c("keep","omit","error")` for row handling when any mapped
  variable is missing.

- extreme_action:

  One of `c("cap","NA","error","warn","ignore")` for SDS values
  exceeding `sds_cap`.

- sds_cap:

  Positive numeric; absolute cap used when `extreme_action = "cap"`.

- check_extreme:

  Logical; if TRUE run raw-value extreme screening using `extreme_rules`
  before SDS computation.

- extreme_rules:

  Optional named list of c(min, max) ranges for raw variables; if NULL,
  broad defaults are used for common anthropometric measures.

- diagnostics:

  Logical; if TRUE emit informational/warning messages (coercions,
  missingness, extremes). FALSE suppresses non-critical warnings.

- warn_thresholds:

  Named list with optional elements `na_prop` (default 0.05) and
  `extreme_prop` (default 0.01) used for proportion-based diagnostic
  warnings.

- id_col:

  Optional column name used only in verbose summaries.

- return_summary:

  Logical; if TRUE return a list with elements `data`, `summary`, and
  `warnings` instead of just the SDS tibble.

- verbose:

  Logical; if TRUE print progress and completion summaries.

- na_strategy:

  Soft-deprecated alias for `na_action` (if provided and `na_action`
  missing, it is used).

- extreme_strategy:

  Soft-deprecated alias for `extreme_action` (if provided and
  `extreme_action` missing, it is used).

- check_raw_extreme:

  Soft-deprecated alias for `check_extreme`.

- raw_extreme_rules:

  Soft-deprecated alias for `extreme_rules`.

## Value

A tibble with one `<var>_SDS` column per reference variable, or a list
when `return_summary = TRUE`.

## Details

SDS are computed as: (observed - mean) / sd. Rows are removed only when
`na_action = 'omit'`. Raw-value extreme screening (if enabled) is
applied before SDS computation. Extreme SDS handling (cap / warn / error
/ ignore) is controlled by `extreme_action`. Legacy argument aliases
(`na_strategy`, `extreme_strategy`) are soft-deprecated but still
accepted.

## References

Cole TJ, Green PJ (1992). “Smoothing Reference Centile Curves: The LMS
Method and Penalized Likelihood.” *Statistics in Medicine*, **11**(10),
1305–1319.
[doi:10.1002/sim.4780111005](https://doi.org/10.1002/sim.4780111005) .

## Examples

``` r
ref <- list(BMI = c(mean = 23, sd = 4), waist = c(mean = 80, sd = 12))
df <- data.frame(BMI = c(25, NA, 60, 18), waist = c(85, 70, 300, 55))
adiposity_sds(df, ref)
#> adiposity_sds(): preparing inputs (4 rows, 2 vars)
#> adiposity_sds(): column map: BMI -> 'BMI', waist -> 'waist'
#> adiposity_sds(): results: BMI_SDS 3/4, waist_SDS 4/4
#> # A tibble: 4 × 2
#>   BMI_SDS waist_SDS
#>     <dbl>     <dbl>
#> 1    0.5      0.417
#> 2   NA       -0.833
#> 3    6        6    
#> 4   -1.25    -2.08 
```
