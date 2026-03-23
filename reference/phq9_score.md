# PHQ-9 / PHQ-8 scoring

PHQ-9 / PHQ-8 scoring

## Usage

``` r
phq9_score(
  data,
  col_map = list(),
  na_action = c("keep", "omit", "error"),
  missing_prop_max = 0.2,
  impute = c("none", "mean"),
  variant = c("PHQ9", "PHQ8"),
  prefix = "PHQ9",
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

- variant:

  Choose PHQ9 (9 items) or PHQ8 (drops suicidal ideation item).

- prefix:

  Prefix for output column names.

- verbose:

  Logical flag for verbose messaging (reserved).

## References

Kroenke K, Spitzer RL, Williams JBW (2001). “The PHQ-9: Validity of a
Brief Depression Severity Measure.” *Journal of General Internal
Medicine*, **16**(9), 606–613.
[doi:10.1046/j.1525-1497.2001.016009606.x](https://doi.org/10.1046/j.1525-1497.2001.016009606.x)
.

## Examples

``` r
df <- data.frame(phq9_01 = 0, phq9_02 = 1, phq9_03 = 2, phq9_04 = 1,
                 phq9_05 = 0, phq9_06 = 1, phq9_07 = 2, phq9_08 = 1,
                 phq9_09 = 0)
phq9_score(df)
#> # A tibble: 1 × 11
#>   phq9_01 phq9_02 phq9_03 phq9_04 phq9_05 phq9_06 phq9_07 phq9_08 phq9_09
#>     <dbl>   <dbl>   <dbl>   <dbl>   <dbl>   <dbl>   <dbl>   <dbl>   <dbl>
#> 1       0       1       2       1       0       1       2       1       0
#> # ℹ 2 more variables: PHQ9_total <dbl>, PHQ9_severity <fct>
```
