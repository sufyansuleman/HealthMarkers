# Insomnia Severity Index (ISI) scoring

Insomnia Severity Index (ISI) scoring

## Usage

``` r
isi_score(
  data,
  col_map = list(),
  na_action = c("keep", "omit", "error"),
  missing_prop_max = 0.2,
  impute = c("none", "mean"),
  prefix = "ISI",
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

## Value

A tibble of score columns only: `ISI_total` and `ISI_severity` (factor).
Input columns are not included.

## References

Bastien CH, Vallières A, Morin CM (2001). “Validation of the Insomnia
Severity Index as an Outcome Measure for Insomnia Research.” *Sleep
Medicine*, **2**(4), 297–307.
[doi:10.1016/S1389-9457(00)00065-4](https://doi.org/10.1016/S1389-9457%2800%2900065-4)
.

## Examples

``` r
df <- data.frame(isi_01 = 0, isi_02 = 1, isi_03 = 2, isi_04 = 1, isi_05 = 0, isi_06 = 1, isi_07 = 2)
isi_score(df)
#> # A tibble: 1 × 2
#>   ISI_total ISI_severity
#>       <dbl> <fct>       
#> 1         7 none        
```
