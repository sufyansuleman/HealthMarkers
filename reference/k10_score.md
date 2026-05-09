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

- verbose:

  Logical; if `TRUE`, emits informational messages about column
  resolution and scoring progress via `hm_inform()`.

## Value

A tibble of score columns only: `K10_total`. Input columns are not
included.

## Note

K10 items are summed as provided. The original scale uses 1–5 coding
(total 10–50); some implementations subtract 1 (0–4, total 0–40). The
function accepts either coding, as no reverse-scored items are involved
and `min_val`/`max_val` only affect reversal.

## References

Kessler RC, Andrews G, Colpe LJ, Hiripi E, Mroczek DK, Normand ST,
Walters EE, Zaslavsky AM (2002). “Short screening scales to monitor
population prevalences and trends in non-specific psychological
distress.” *Psychological Medicine*, **32**(6), 959–976.
[doi:10.1017/S0033291702006074](https://doi.org/10.1017/S0033291702006074)
.

## Examples

``` r
df <- data.frame(k10_01 = 0, k10_02 = 1, k10_03 = 2, k10_04 = 1, k10_05 = 0,
                 k10_06 = 1, k10_07 = 2, k10_08 = 1, k10_09 = 0, k10_10 = 1)
k10_score(df)
#> # A tibble: 1 × 1
#>   K10_total
#>       <dbl>
#> 1         9
```
