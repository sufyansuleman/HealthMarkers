# GAD-7 scoring

GAD-7 scoring

## Usage

``` r
gad7_score(
  data,
  col_map = list(),
  na_action = c("keep", "omit", "error"),
  missing_prop_max = 0.2,
  impute = c("none", "mean"),
  prefix = "GAD7",
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

## References

Spitzer RL, Kroenke K, Williams JBW, Löwe B (2006). “A Brief Measure for
Assessing Generalized Anxiety Disorder: The GAD-7.” *Archives of
Internal Medicine*, **166**(10), 1092–1097.
[doi:10.1001/archinte.166.10.1092](https://doi.org/10.1001/archinte.166.10.1092)
. ; Plummer F, Manea L, Trepel D, McMillan D (2016). “Screening for
Anxiety Disorders with the GAD-7 and GAD-2: A Systematic Review and
Diagnostic Meta-Analysis.” *General Hospital Psychiatry*, **39**, 24–31.
[doi:10.1016/j.genhosppsych.2015.11.005](https://doi.org/10.1016/j.genhosppsych.2015.11.005)
.

## Examples

``` r
df <- data.frame(gad7_01 = 0, gad7_02 = 1, gad7_03 = 2, gad7_04 = 1,
                 gad7_05 = 0, gad7_06 = 1, gad7_07 = 2)
gad7_score(df)
#> # A tibble: 1 × 9
#>   gad7_01 gad7_02 gad7_03 gad7_04 gad7_05 gad7_06 gad7_07 GAD7_total
#>     <dbl>   <dbl>   <dbl>   <dbl>   <dbl>   <dbl>   <dbl>      <dbl>
#> 1       0       1       2       1       0       1       2          7
#> # ℹ 1 more variable: GAD7_severity <fct>
```
