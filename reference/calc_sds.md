# Calculate Standard Deviation Scores (SDS; z-scores) for health markers

Computes per-variable SDS as (x - mean) / sd using supplied reference
statistics. Includes input validation, NA/error handling, data quality
warnings, and verbose progress via the package logger (hm_inform),
aligned with HM-CS v3.

## Usage

``` r
calc_sds(
  data,
  vars,
  ref,
  id_col = NULL,
  sds_cap = 6,
  na_strategy = c("omit", "error", "keep"),
  extreme_strategy = c("cap", "warn", "error", "NA"),
  warn_thresholds = list(na_prop = 0.05, extreme_prop = 0.01),
  return = c("data", "list"),
  verbose = FALSE,
  na_action = NULL,
  check_extreme = TRUE,
  extreme_action = NULL
)
```

## Arguments

- data:

  A data.frame/tibble containing the variables.

- vars:

  Character vector of variable names in `data` to compute SDS for. Must
  be non-empty and unique.

- ref:

  A data.frame with columns: `variable`, `mean`, `sd` supplying
  reference statistics for each variable listed in `vars`. Must contain
  exactly one row per requested variable, with finite `mean` and
  `sd > 0`.

- id_col:

  Optional character scalar naming an ID column in `data` (used only in
  messages and duplicate-ID notes).

- sds_cap:

  Numeric scalar; absolute cap for SDS when `extreme_strategy = "cap"`.
  Must be positive. Default 6.

- na_strategy:

  One of c("omit","error","keep"):

  - "omit": drop rows with missing values across any of `vars` (default)

  - "error": stop if any missing values among `vars`

  - "keep": keep rows; SDS for missing inputs will be NA

- extreme_strategy:

  One of c("cap","warn","error","NA"):

  - "cap": cap \|SDS\| at `sds_cap` and warn (default)

  - "warn": keep extreme SDS, but warn

  - "error": stop if any \|SDS\| \> `sds_cap`

  - "NA": set extreme SDS to NA

- warn_thresholds:

  Named list controlling warnings (proportions in \\\[0,1\]\\):

  - na_prop: warn if proportion of rows with NA among `vars` exceeds
    this (default 0.05)

  - extreme_prop: warn if proportion of extreme SDS (cells) exceeds this
    (default 0.01)

- return:

  One of c("data","list"). "data" returns a tibble with added
  `[var]_sds` columns (default). "list" returns a list with components
  `data`, `summary`, and `warnings` (backward compatible).

- verbose:

  Logical; if TRUE, emit progress via hm_inform(). Also controlled by
  options(healthmarkers.verbose = "none"\|"inform"\|"debug").

- na_action:

  Optional HM-CS alias for `na_strategy` (accepted values:
  keep/omit/error; used if provided).

- check_extreme:

  Logical; if TRUE, enables SDS extreme handling (alias for legacy
  behavior; defaults to TRUE in this implementation).

- extreme_action:

  Optional HM-CS alias for `extreme_strategy` (accepted values:
  cap/NA/error; used if provided).

## Value

- If `return = "data"` (default): a tibble with added `[var]_sds`
  columns.

- If `return = "list"`: a list with:

  - data: tibble with added SDS columns

  - summary: list with input/output row counts, omitted rows, total
    extremes, and per-variable summary

  - warnings: character vector of warning messages emitted

## Details

By default, returns a tibble with added `[var]_sds` columns
(tidyverse-friendly). For backward compatibility, you can request the
previous list output.

## References

Cole TJ, Green PJ (1992). “Smoothing Reference Centile Curves: The LMS
Method and Penalized Likelihood.” *Statistics in Medicine*, **11**(10),
1305–1319.
[doi:10.1002/sim.4780111005](https://doi.org/10.1002/sim.4780111005) . ;
de Onis M, World Health Organization (2006). *WHO Child Growth
Standards: Methods and Development*. World Health Organization. ISBN
924154693X, No DOI for this WHO report; see ISBN/URL,
<https://www.who.int/publications/i/item/924154693X>. ; Kuczmarski RJ,
Ogden CL, Grummer-Strawn LM, et al. (2000). “CDC Growth Charts: United
States.” Technical Report 314, National Center for Health Statistics. No
DOI for this CDC technical report; see URL,
<https://www.cdc.gov/growthcharts/cdc_charts.htm>.

## Examples

``` r
ref <- data.frame(
  variable = c("bmi","sbp"),
  mean     = c(25, 120),
  sd       = c(4, 15)
)
df <- data.frame(
  id  = 1:6,
  bmi = c(24, 30, NA, 29, 10, 26),
  sbp = c(118, 200, 119, 121, 500, 120)
)
out_tbl <- calc_sds(
  data = df,
  vars = c("bmi","sbp"),
  ref = ref,
  id_col = "id",
  na_strategy = "omit",
  extreme_strategy = "cap",
  sds_cap = 6,
  verbose = FALSE
)
#> Warning: calc_sds(): high NA proportion among `vars`: 16.7% of rows
#> Warning: Capped 1 SDS beyond +/-6 for `sbp`
#> Warning: calc_sds(): high proportion of extreme SDS: 10.00% of computed cells
```
