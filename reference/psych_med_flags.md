# Psychiatric medication flags aggregator

Psychiatric medication flags aggregator

## Usage

``` r
psych_med_flags(
  data,
  col_map = list(),
  prefix = "med",
  na_action = c("keep", "omit", "error")
)
```

## Arguments

- data:

  Data frame containing questionnaire item columns.

- col_map:

  Named list `med` mapping medication classes (e.g., ssri, snri) to
  columns of boolean/numeric flags.

- prefix:

  Prefix for output flag columns.

- na_action:

  How to handle rows with missing items: `keep`, `omit`, or `error`.

## Examples

``` r
df <- data.frame(med_ssri = c(1, 0), med_antipsychotic = c(0, 1))
cm <- list(med = list(
  ssri = "med_ssri",
  antipsychotic = "med_antipsychotic"
))
psych_med_flags(df, col_map = cm)
#> # A tibble: 2 × 4
#>   med_ssri med_antipsychotic med_any_psych med_count
#>      <dbl>             <dbl> <lgl>             <dbl>
#> 1        1                 0 TRUE                  1
#> 2        0                 1 TRUE                  1
```
