# WHO-5 scoring

WHO-5 scoring

## Usage

``` r
who5_score(
  data,
  col_map = list(),
  na_action = c("keep", "omit", "error"),
  missing_prop_max = 0.2,
  impute = c("none", "mean"),
  prefix = "WHO5",
  low_cutoff_percent = 50,
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

- low_cutoff_percent:

  Percentage threshold for low well-being flag.

- verbose:

  Logical flag for verbose messaging (reserved).

## Value

A tibble of score columns only: `WHO5_raw`, `WHO5_percent`,
`WHO5_low_wellbeing`. Input columns are not included.

## References

Topp CW, Østergaard SrD, Sø ndergaard S, Bech P (2015). “The WHO-5
Well-Being Index: A Systematic Review of the Literature.” *Psychotherapy
and Psychosomatics*, **84**(3), 167–176.
[doi:10.1159/000376585](https://doi.org/10.1159/000376585) .

## Examples

``` r
df <- data.frame(who5_01 = 0, who5_02 = 1, who5_03 = 2, who5_04 = 3, who5_05 = 4)
who5_score(df)
#> # A tibble: 1 × 3
#>   WHO5_raw WHO5_percent WHO5_low_wellbeing
#>      <dbl>        <dbl> <lgl>             
#> 1       10           40 TRUE              
```
