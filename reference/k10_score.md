# K10 scoring

K10 scoring

## Usage

``` r
k10_score(
  data,
  col_map = list(),
  na_action = c("keep", "omit", "error"),
  missing_prop_max = 0.2,
  impute = c("none", "mean"),
  prefix = "K10",
  verbose = FALSE
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

- verbose:

  Logical flag for verbose messaging (reserved).

## Examples

``` r
df <- data.frame(k10_01 = 0, k10_02 = 1, k10_03 = 2, k10_04 = 1, k10_05 = 0,
                 k10_06 = 1, k10_07 = 2, k10_08 = 1, k10_09 = 0, k10_10 = 1)
k10_score(df)
#> # A tibble: 1 × 11
#>   k10_01 k10_02 k10_03 k10_04 k10_05 k10_06 k10_07 k10_08 k10_09 k10_10
#>    <dbl>  <dbl>  <dbl>  <dbl>  <dbl>  <dbl>  <dbl>  <dbl>  <dbl>  <dbl>
#> 1      0      1      2      1      0      1      2      1      0      1
#> # ℹ 1 more variable: K10_total <dbl>
```
