# Calculate metabolic risk feature flags (pediatric-friendly thresholds)

Compute four binary risk flags from routine clinical measures:

- dyslipidemia

- insulin_resistance

- hyperglycemia (prediabetes-range glycemia)

- hypertension (BP \>=95th percentile via z \> 1.64)

## Usage

``` r
metabolic_risk_features(
  data,
  col_map = NULL,
  na_action = c("keep", "omit", "error", "ignore", "warn"),
  na_warn_prop = 0.2,
  verbose = TRUE
)
```

## Arguments

- data:

  A data.frame or tibble containing at least these numeric columns:

  - chol_total, chol_ldl, chol_hdl, triglycerides (mmol/L)

  - age_year (years)

  - z_HOMA (standardized HOMA-IR)

  - glucose (mmol/L)

  - HbA1c (mmol/mol; IFCC units)

  - bp_sys_z, bp_dia_z (BP z-scores)

- col_map:

  Optional named list to map required keys to column names in `data`.
  Keys: c("chol_total","chol_ldl","chol_hdl","triglycerides","age_year",
  "z_HOMA","glucose","HbA1c","bp_sys_z","bp_dia_z"). Default NULL (use
  same names).

- na_action:

  One of c("keep","omit","error","ignore","warn") controlling
  missing-data policy.

  - "keep": keep NA; outputs become NA where inputs are NA.

  - "omit": drop rows with NA in any required input.

  - "error": abort if any required input contains NA.

  - "ignore"/"warn": aliases of "keep"; "warn" also emits missingness
    diagnostics.

- na_warn_prop:

  Numeric in \\\[0,1\]\\; per-variable threshold for high-missingness
  warnings. Default 0.2.

- verbose:

  Logical; if TRUE, prints column mapping and computing messages.

## Value

A tibble with four factor columns (levels c("0","1")):

- dyslipidemia

- insulin_resistance

- hyperglycemia

- hypertension

## Details

By default, behavior matches prior implementation: required columns are
validated, NA values are kept (propagate to outputs), no extreme-value
checks or capping are applied, and a tibble with 0/1 factor flags is
returned.

Units and criteria (no automatic unit conversion):

- Lipids (mmol/L): total cholesterol \> 5.2 OR LDL-C \> 3.4 OR HDL-C \<
  1.0 OR triglycerides \> 1.1 (age 0-9) OR \> 1.5 (age 10-19) =\>
  dyslipidemia = 1.

- Insulin resistance: z_HOMA \> 1.28 (~=90th percentile) =\>
  insulin_resistance = 1. z_HOMA is a within-sample or external z-score
  of HOMA-IR.

- Hyperglycemia: fasting glucose in (5.6, 6.9) mmol/L OR HbA1c in
  (39, 47) mmol/mol =\> hyperglycemia = 1.

- Hypertension: either BP z-score \> 1.64 (~=95th percentile) for
  systolic or diastolic =\> hypertension = 1.

## See also

[`liver_markers()`](https://sufyansuleman.github.io/HealthMarkers/reference/liver_markers.md),
[`lipid_markers()`](https://sufyansuleman.github.io/HealthMarkers/reference/lipid_markers.md),
[`kidney_failure_risk()`](https://sufyansuleman.github.io/HealthMarkers/reference/kidney_failure_risk.md),
[`inflammatory_markers()`](https://sufyansuleman.github.io/HealthMarkers/reference/inflammatory_markers.md)

## Examples

``` r
df <- data.frame(
  chol_total = c(5.2, 6.4), chol_ldl = c(3.2, 4.1), chol_hdl = c(1.3, 1.0),
  triglycerides = c(1.8, 2.5), age_year = c(45, 60), z_HOMA = c(0.5, 1.2),
  glucose = c(5.5, 6.8), HbA1c = c(38, 46), bp_sys_z = c(0.2, 1.1),
  bp_dia_z = c(0.1, 0.9)
)
metabolic_risk_features(df)
#> metabolic_risk_features(): reading input 'df' — 2 rows × 10 variables
#> metabolic_risk_features(): col_map (10 columns — 10 inferred from data)
#>   chol_total        ->  'chol_total'    (inferred)
#>   chol_ldl          ->  'chol_ldl'    (inferred)
#>   chol_hdl          ->  'chol_hdl'    (inferred)
#>   triglycerides     ->  'triglycerides'    (inferred)
#>   age_year          ->  'age_year'    (inferred)
#>   z_HOMA            ->  'z_HOMA'    (inferred)
#>   glucose           ->  'glucose'    (inferred)
#>   HbA1c             ->  'HbA1c'    (inferred)
#>   bp_sys_z          ->  'bp_sys_z'    (inferred)
#>   bp_dia_z          ->  'bp_dia_z'    (inferred)
#> metabolic_risk_features(): computing markers:
#>   dyslipidemia       [chol_total, chol_ldl, chol_hdl, triglycerides, age_year]
#>   insulin_resistance [z_HOMA]
#>   hyperglycemia      [glucose, HbA1c]
#>   hypertension       [bp_sys_z, bp_dia_z]
#> metabolic_risk_features(): results: dyslipidemia 2/2, insulin_resistance 2/2, hyperglycemia 2/2, hypertension 2/2
#> # A tibble: 2 × 4
#>   dyslipidemia insulin_resistance hyperglycemia hypertension
#>   <fct>        <fct>              <fct>         <fct>       
#> 1 0            0                  0             0           
#> 2 1            0                  1             0           
```
