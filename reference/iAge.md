# Compute a simplified Inflammatory Age Index (iAge) with QA and verbose summaries

Implements a linear proxy for immunosenescence based on key inflammatory
biomarkers, following the approach introduced by Sayed et al. for the
inflammatory aging clock (iAge). This simplified iAge is computed as a
weighted sum of C-reactive protein (CRP), interleukin-6 (IL6), and tumor
necrosis factor-alpha (TNFa).

## Usage

``` r
iAge(
  data,
  col_map = NULL,
  weights = c(CRP = 0.33, IL6 = 0.33, TNFa = 0.34),
  verbose = TRUE,
  na_action = c("keep", "omit", "error", "ignore", "warn"),
  na_warn_prop = 0.2
)
```

## Arguments

- data:

  A data.frame or tibble containing the biomarker columns mapped by
  `col_map`.

- col_map:

  Named list mapping:

  - CRP -\> column name for C-reactive protein (mg/L)

  - IL6 -\> column name for interleukin-6 (pg/mL)

  - TNFa -\> column name for tumor necrosis factor-alpha (pg/mL)

- weights:

  Named numeric vector of weights for each marker (must sum to 1).
  Defaults to c(CRP = 0.33, IL6 = 0.33, TNFa = 0.34).

- verbose:

  Logical; if TRUE, prints column mapping and computing messages.

- na_action:

  One of c("keep","omit","error","ignore","warn") controlling how
  missing inputs affect iAge:

  - "keep": return NA for rows where any required marker is NA (default;
    consistent with other functions).

  - "omit": ignore NAs in the weighted sum (treat missing markers as 0).

  - "error": abort if any required marker contains NA.

  - "ignore": alias of "omit".

  - "warn": alias of "omit" but emits missingness warnings (per
    na_warn_prop).

- na_warn_prop:

  Proportion in \\\[0,1\]\\ above which a high-missingness warning is
  emitted when `na_action = "warn"`. Default 0.2.

## Value

A tibble with one column:

- iAge (numeric): the computed inflammatory age index.

## Details

By default, missing inputs are omitted in the sum (consistent with prior
behavior). Optional diagnostics can warn on high missingness and scan
for extreme values, with the ability to cap, warn, or error on extremes.
Verbose mode prints step-by-step progress and a final summary.

Assumed units (no automatic unit conversion):

- CRP: mg/L

- IL6: pg/mL

- TNFa: pg/mL

Note:

- The original iAge model in Sayed et al. (Nature Aging, 2021) is a
  multi-marker machine learning model. This function provides a simple,
  linear proxy using three canonical inflammatory biomarkers. It is not
  identical to the original published iAge but is inspired by its
  rationale.

## References

Sayed N, others (2021). “An inflammatory aging clock (iAge) predicts
multimorbidity, immunosenescence, frailty and cardiovascular aging.”
*Nature Aging*, **1**, 598–610.
[doi:10.1038/s43587-021-00082-y](https://doi.org/10.1038/s43587-021-00082-y)
.

## See also

[`impute_missing()`](https://sufyansuleman.github.io/HealthMarkers/reference/impute_missing.md),
[`glycemic_markers()`](https://sufyansuleman.github.io/HealthMarkers/reference/glycemic_markers.md)

## Examples

``` r
library(tibble)
df <- tibble(
  CRP  = c(1.2, 3.5, NA),  # mg/L
  IL6  = c(2.0, 4.1, 1.5), # pg/mL
  TNFa = c(1.0, 1.8, 0.9)  # pg/mL
)
# Default behavior (omit NAs in row-wise sum)
iAge(
  df,
  col_map = list(CRP = "CRP", IL6 = "IL6", TNFa = "TNFa")
)
#> iAge(): reading input 'df' — 3 rows × 3 variables
#> iAge(): col_map (3 columns — 3 specified)
#>   CRP               ->  'CRP'
#>   IL6               ->  'IL6'
#>   TNFa              ->  'TNFa'
#> iAge(): computing markers:
#>   iAge  [weighted sum: CRP*0.33 + IL6*0.33 + TNFa*0.34]
#> iAge(): results: iAge 2/3
#> # A tibble: 3 × 1
#>    iAge
#>   <dbl>
#> 1  1.40
#> 2  3.12
#> 3 NA   

# Keep NA if any marker missing in a row
iAge(
  df,
  col_map = list(CRP = "CRP", IL6 = "IL6", TNFa = "TNFa"),
  na_action = "keep"
)
#> iAge(): reading input 'df' — 3 rows × 3 variables
#> iAge(): col_map (3 columns — 3 specified)
#>   CRP               ->  'CRP'
#>   IL6               ->  'IL6'
#>   TNFa              ->  'TNFa'
#> iAge(): computing markers:
#>   iAge  [weighted sum: CRP*0.33 + IL6*0.33 + TNFa*0.34]
#> iAge(): results: iAge 2/3
#> # A tibble: 3 × 1
#>    iAge
#>   <dbl>
#> 1  1.40
#> 2  3.12
#> 3 NA   

# Verbose output
iAge(
  df,
  col_map = list(CRP = "CRP", IL6 = "IL6", TNFa = "TNFa"),
  verbose = TRUE
)
#> iAge(): reading input 'df' — 3 rows × 3 variables
#> iAge(): col_map (3 columns — 3 specified)
#>   CRP               ->  'CRP'
#>   IL6               ->  'IL6'
#>   TNFa              ->  'TNFa'
#> iAge(): computing markers:
#>   iAge  [weighted sum: CRP*0.33 + IL6*0.33 + TNFa*0.34]
#> iAge(): results: iAge 2/3
#> # A tibble: 3 × 1
#>    iAge
#>   <dbl>
#> 1  1.40
#> 2  3.12
#> 3 NA   
```
