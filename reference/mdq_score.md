# Mood Disorder Questionnaire scoring

Mood Disorder Questionnaire scoring

## Usage

``` r
mdq_score(
  data,
  col_map = list(),
  na_action = c("keep", "omit", "error"),
  missing_prop_max = 0.2,
  prefix = "MDQ",
  symptom_cutoff = 7,
  require_clustering = TRUE,
  require_impairment = TRUE,
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

- prefix:

  Prefix for output column names.

- symptom_cutoff:

  Minimum symptom count for a positive screen.

- require_clustering:

  Require clustering item == 1 to be positive.

- require_impairment:

  Require impairment item == 1 to be positive.

- verbose:

  Logical flag for verbose messaging (reserved).

## Value

A tibble of score columns only: `MDQ_symptom_count`, `MDQ_clustering`,
`MDQ_impairment`, `MDQ_positive_screen`. Input columns are not included.

## References

Hirschfeld RMA, Williams JBW, Spitzer RL, Calabrese JR, Flynn L, Keck
PE, Lewis L, McElroy SL, Post RM, Rapport DJ, Russell JM, Sachs GS,
Zajecka J (2000). “Development and Validation of a Screening Instrument
for Bipolar Spectrum Disorder: The Mood Disorder Questionnaire.”
*American Journal of Psychiatry*, **157**(11), 1873–1875.
[doi:10.1176/appi.ajp.157.11.1873](https://doi.org/10.1176/appi.ajp.157.11.1873)
.

## Examples

``` r
df <- data.frame(matrix(0, nrow = 1, ncol = 13))
names(df) <- sprintf("mdq_%02d", 1:13)
df$mdq_cluster <- 1; df$mdq_impair <- 1
mdq_score(df)
#> # A tibble: 1 × 4
#>   MDQ_symptom_count MDQ_clustering MDQ_impairment MDQ_positive_screen
#>               <dbl> <lgl>          <lgl>          <lgl>              
#> 1                 0 TRUE           TRUE           FALSE              
```
