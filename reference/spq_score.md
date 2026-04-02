# Schizotypal Personality Questionnaire (key-driven)

Schizotypal Personality Questionnaire (key-driven)

## Usage

``` r
spq_score(
  data,
  col_map = list(),
  key,
  na_action = c("keep", "omit", "error"),
  missing_prop_max = 0.2,
  impute = c("none", "mean"),
  prefix = "SPQ",
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

Raine A (1991). “The SPQ: A Scale for the Assessment of Schizotypal
Personality Based on DSM-III-R Criteria.” *Schizophrenia Bulletin*,
**17**(4), 555–564.
[doi:10.1093/schbul/17.4.555](https://doi.org/10.1093/schbul/17.4.555) .

## Examples

``` r
spq_key <- list(items = sprintf("spq_%02d", 1:5), min_val = 0, max_val = 1)
df <- data.frame(spq_01 = 0, spq_02 = 1, spq_03 = 0, spq_04 = 1, spq_05 = 0)
spq_score(df, key = spq_key)
#> # A tibble: 1 × 1
#>   SPQ_total
#>       <dbl>
#> 1         2
```
