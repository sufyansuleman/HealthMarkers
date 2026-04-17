# Compute sex-stratified standardized scores (SDS) for adiposity measures

Computes sex-specific SDS (z-scores) for selected anthropometric
variables using reference means and SDs provided separately for males
and females.

## Usage

``` r
adiposity_sds_strat(
  data,
  col_map,
  ref,
  na_action = c("keep", "omit", "error"),
  allow_partial = FALSE,
  prefix = "",
  verbose = TRUE
)
```

## Arguments

- data:

  Data frame or tibble with variables and a sex column

- col_map:

  Named list mapping:

  - sex: column name with sex values ("M","F","m","f", 1, 2)

  - vars: optional named list mapping reference variable names -\> data
    column names. If omitted, expects the reference variable names to
    exist in `data`.

- ref:

  Named list with elements "M" and "F". Each is a named list of numeric
  vectors c(mean=, sd=) keyed by variable name, e.g.: list( M = list(BMI
  = c(mean=23, sd=3.5), waist = c(mean=85, sd=10)), F = list(BMI =
  c(mean=21, sd=3.0), waist = c(mean=75, sd=9)) )

- na_action:

  One of:

  - "keep" - keep rows with NA (propagates to outputs)

  - "omit" - drop rows with NA in any required variable

  - "error" - abort if any required variable has NA

- allow_partial:

  If TRUE, skip variables absent in data (with a warning); if FALSE
  error

- prefix:

  Optional prefix for output columns (default "")

- verbose:

  Logical; when TRUE emit progress via package logger; by default
  logging is controlled by options(healthmarkers.verbose)

## Value

A tibble with one SDS column per retained variable: `varname_SDS`, where
`varname` is the original variable name (optionally prefixed by
`prefix`).

## References

Cole TJ, Green PJ (1992). “Smoothing Reference Centile Curves: The LMS
Method and Penalized Likelihood.” *Statistics in Medicine*, **11**(10),
1305–1319.
[doi:10.1002/sim.4780111005](https://doi.org/10.1002/sim.4780111005) . ;
World Health Organization (1995). *Physical Status: The Use and
Interpretation of Anthropometry*, volume 854 of *Technical Report
Series*. World Health Organization. ISBN 9241208546, No DOI for this WHO
report; see ISBN/URL,
<https://www.who.int/publications/i/item/9241208546>.

## Examples

``` r
ref <- list(
  M = list(BMI = c(mean=24.5, sd=3.8), waist = c(mean=88, sd=12)),
  F = list(BMI = c(mean=22.1, sd=4.2), waist = c(mean=76, sd=11))
)
df <- data.frame(BMI=c(25.2,21.8,27.1), waist=c(85,72,95), sex=c("M","F","M"))
adiposity_sds_strat(
  df,
  col_map = list(sex = "sex", vars = list(BMI = "BMI", waist = "waist")),
  ref = ref
)
#> adiposity_sds_strat(): reading input 'df' — 3 rows × 3 variables
#> adiposity_sds_strat(): col_map (2 columns — 2 specified)
#>   BMI               ->  'BMI'
#>   waist             ->  'waist'
#> adiposity_sds_strat(): computing markers:
#>   BMI_SDS, waist_SDS
#> adiposity_sds_strat(): results: BMI_SDS 3/3, waist_SDS 3/3
#> # A tibble: 3 × 2
#>   BMI_SDS waist_SDS
#>     <dbl>     <dbl>
#> 1  0.184     -0.25 
#> 2 -0.0714    -0.364
#> 3  0.684      0.583
```
