# GHQ-12 scoring (Likert or binary)

GHQ-12 scoring (Likert or binary)

## Usage

``` r
ghq12_score(
  data,
  col_map = list(),
  na_action = c("keep", "omit", "error"),
  missing_prop_max = 0.2,
  impute = c("none", "mean"),
  prefix = "GHQ12",
  method = c("likert", "binary"),
  case_cutoff_binary = 3,
  verbose = TRUE
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

- method:

  Scoring method: `likert` (0-3 per item) or `binary` (0/1 per item).

- case_cutoff_binary:

  Cut-off for case status when using binary scoring.

- verbose:

  Logical; if `TRUE`, emits informational messages about column
  resolution and scoring progress via `hm_inform()`.

## Value

A tibble of score columns only: `GHQ12_total_likert` (likert method) or
`GHQ12_total_binary` and `GHQ12_case_binary` (binary method). Input
columns are not included.

## Examples

``` r
df <- data.frame(ghq12_01 = 0, ghq12_02 = 1, ghq12_03 = 2, ghq12_04 = 1,
                 ghq12_05 = 0, ghq12_06 = 1, ghq12_07 = 0, ghq12_08 = 1,
                 ghq12_09 = 2, ghq12_10 = 1, ghq12_11 = 0, ghq12_12 = 1)
ghq12_score(df, method = "likert")
#> # A tibble: 1 × 1
#>   GHQ12_total_likert
#>                <dbl>
#> 1                 10
```
