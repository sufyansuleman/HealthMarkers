# Psychiatric diagnosis flags aggregator

Psychiatric diagnosis flags aggregator

## Usage

``` r
psych_dx_flags(
  data,
  col_map = list(),
  prefix = "dx",
  na_action = c("keep", "omit", "error")
)
```

## Arguments

- data:

  Data frame containing questionnaire item columns.

- col_map:

  Named list `dx` mapping condition ids (e.g., mdd, bipolar) to columns
  of boolean/numeric flags.

- prefix:

  Prefix for output flag columns.

- na_action:

  How to handle rows with missing items: `keep`, `omit`, or `error`.

## Value

A tibble of flag columns only: `dx_any_psych`, `dx_internalizing`,
`dx_externalizing`, `dx_psychotic`, `dx_count`. Input columns are not
included.

## Examples

``` r
df <- data.frame(dx_mdd = c(1, 0), dx_bipolar = c(0, 1))
psych_dx_flags(df, col_map = list(dx = list(mdd = "dx_mdd", bipolar = "dx_bipolar")))
#> # A tibble: 2 × 5
#>   dx_any_psych dx_internalizing dx_externalizing dx_psychotic dx_count
#>   <lgl>        <lgl>            <lgl>            <lgl>           <dbl>
#> 1 TRUE         TRUE             NA               NA                  1
#> 2 TRUE         NA               NA               NA                  1
```
