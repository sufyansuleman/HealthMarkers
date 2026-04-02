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

## Value

A tibble of score columns only: `PHQ9_total` and `PHQ9_severity`
(factor). Input columns are not included.

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
#> # A tibble: 1 × 2
#>   PHQ9_total PHQ9_severity
#>        <dbl> <fct>        
#> 1          8 mild         
```
