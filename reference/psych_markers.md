# Psychometric markers dispatcher

Psychometric markers dispatcher

## Usage

``` r
psych_markers(
  data,
  col_map = list(),
  which = c("phq9", "gad7", "k6", "k10", "ghq12_likert", "ghq12_binary", "who5", "isi",
    "mdq", "asrs", "bis", "spq", "cognitive", "dx_flags", "med_flags"),
  na_action = c("keep", "omit", "error"),
  missing_prop_max = 0.2,
  impute = c("none", "mean"),
  bis_key = NULL,
  spq_key = NULL,
  cognitive_method = c("z_mean", "pca1"),
  verbose = FALSE
)
```

## Arguments

- data:

  Data frame containing questionnaire item columns.

- col_map:

  Nested list of mappings per instrument (e.g., col_map\$phq9,
  col_map\$bis, col_map\$dx_flags, ...).

- which:

  Vector of modules to compute (e.g., "phq9", "gad7", "bis").

- na_action:

  How to handle rows with missing items: `keep`, `omit`, or `error`.

- missing_prop_max:

  Maximum allowed proportion of missing items per row before the score
  is set to `NA`.

- impute:

  Imputation strategy for missing items when under the threshold: `none`
  or `mean` (row-wise mean).

- bis_key:

  SPQ/BIS key list passed to `bis_score` when requested.

- spq_key:

  SPQ key list passed to `spq_score` when requested.

- cognitive_method:

  Method passed to `cognitive_score` ("z_mean" or "pca1").

- verbose:

  Logical flag for verbose messaging (reserved).

## Examples

``` r
df <- data.frame(
  phq9_01 = 0, phq9_02 = 1, phq9_03 = 2, phq9_04 = 1, phq9_05 = 0,
  phq9_06 = 1, phq9_07 = 2, phq9_08 = 1, phq9_09 = 0,
  gad7_01 = 0, gad7_02 = 1, gad7_03 = 2, gad7_04 = 1, gad7_05 = 0,
  gad7_06 = 1, gad7_07 = 2
)
psych_markers(df, which = c("phq9", "gad7"))
#> # A tibble: 1 × 20
#>   phq9_01 phq9_02 phq9_03 phq9_04 phq9_05 phq9_06 phq9_07 phq9_08 phq9_09
#>     <dbl>   <dbl>   <dbl>   <dbl>   <dbl>   <dbl>   <dbl>   <dbl>   <dbl>
#> 1       0       1       2       1       0       1       2       1       0
#> # ℹ 11 more variables: gad7_01 <dbl>, gad7_02 <dbl>, gad7_03 <dbl>,
#> #   gad7_04 <dbl>, gad7_05 <dbl>, gad7_06 <dbl>, gad7_07 <dbl>,
#> #   PHQ9_total <dbl>, PHQ9_severity <fct>, GAD7_total <dbl>,
#> #   GAD7_severity <fct>
```
