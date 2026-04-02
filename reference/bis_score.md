# Barratt Impulsiveness Scale (key-driven)

Barratt Impulsiveness Scale (key-driven)

## Usage

``` r
bis_score(
  data,
  col_map = list(),
  key,
  na_action = c("keep", "omit", "error"),
  missing_prop_max = 0.2,
  impute = c("none", "mean"),
  prefix = "BIS",
  verbose = FALSE
)
```

## Arguments

- data:

  Data frame containing questionnaire item columns.

- col_map:

  Named list mapping canonical item IDs to column names; defaults assume
  items are already named.

- key:

  List with `items`, `min_val`, `max_val`, optional `reverse` and
  `subscales`.

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

## Value

A tibble of score columns only (total and optional subscales). Input
columns are not included.

## References

Patton JH, Stanford MS, Barratt ES (1995). “Factor Structure of the
Barratt Impulsiveness Scale.” *Journal of Clinical Psychology*,
**51**(6), 768–774.
[doi:10.1002/1097-4679(199511)51:6\<768::AID-JCLP2270510607\>3.0.CO;2-1](https://doi.org/10.1002/1097-4679%28199511%2951%3A6%3C768%3A%3AAID-JCLP2270510607%3E3.0.CO%3B2-1)
.

## Examples

``` r
bis_key <- list(items = sprintf("bis_%02d", 1:5), min_val = 1, max_val = 4)
df <- data.frame(bis_01 = 1, bis_02 = 2, bis_03 = 3, bis_04 = 4, bis_05 = 2)
bis_score(df, key = bis_key)
#> # A tibble: 1 × 1
#>   BIS_total
#>       <dbl>
#> 1        12
```
