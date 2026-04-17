# K6 scoring

K6 scoring

## Usage

``` r
k6_score(
  data,
  col_map = list(),
  na_action = c("keep", "omit", "error"),
  missing_prop_max = 0.2,
  impute = c("none", "mean"),
  prefix = "K6",
  cutoff = 13,
  verbose = TRUE
)
```

## Arguments

- data:

  Data frame containing questionnaire item columns.

- col_map:

  Named list mapping canonical item IDs to column names; defaults assume
  items are already named.

- na_action:

  How to handle rows with missing items: `keep`, `omit`, or `error`.

- missing_prop_max:

  Maximum allowed proportion of missing items per row before the score
  is set to `NA`.

- impute:

  Imputation strategy for missing items when under the threshold: `none`
  or `mean` (row-wise mean).

- prefix:

  Prefix for output column names.

- cutoff:

  Threshold for the K6 case flag.

- verbose:

  Logical; if `TRUE`, emits informational messages about column
  resolution and scoring progress via `hm_inform()`.

## Value

A tibble of score columns only: `K6_total` and `K6_case`. Input columns
are not included.

## References

Prochaska JJ, Sung H, Max W, Shi Y, Ong M (2012). “Validity Study of the
K6 Scale as a Measure of Moderate Mental Distress Based on Mental Health
Treatment Need and Utilization.” *International Journal of Methods in
Psychiatric Research*, **21**(2), 88–97.
[doi:10.1002/mpr.1349](https://doi.org/10.1002/mpr.1349) .

## Examples

``` r
df <- data.frame(k6_01 = 0, k6_02 = 1, k6_03 = 2, k6_04 = 1, k6_05 = 0, k6_06 = 1)
k6_score(df)
#> # A tibble: 1 × 2
#>   K6_total K6_case
#>      <dbl> <lgl>  
#> 1        5 FALSE  
```
