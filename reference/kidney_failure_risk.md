# Kidney Failure Risk Equation (KFRE, 2- and 5-year risk)

Compute 2- and 5-year risk of end-stage kidney disease using the
original 4-variable KFRE (Tangri et al., 2011) with optional
data-quality diagnostics, extreme-value handling, and verbose progress
reporting.

## Usage

``` r
kidney_failure_risk(
  data,
  col_map = list(age = "age", sex = "sex", eGFR = "eGFR", UACR = "UACR"),
  na_action = c("keep", "error", "omit", "warn"),
  na_warn_prop = 0.2,
  check_extreme = FALSE,
  extreme_action = c("warn", "cap", "error", "ignore", "NA"),
  extreme_rules = NULL,
  verbose = FALSE
)
```

## Arguments

- data:

  A data.frame or tibble containing at least the columns mapped in
  `col_map`.

- col_map:

  Named list mapping:

  - `age` -\> age in years

  - `sex` -\> sex code (1 = male, 2 = female)

  - `eGFR` -\> estimated GFR (mL/min/1.73 m^2)

  - `UACR` -\> urine albumin-to-creatinine ratio (mg/g)

- na_action:

  One of c("keep","error","omit","warn"). Default "keep" to preserve
  previous behavior:

  - "keep": propagate NA/NaN through logs and outputs.

  - "error": abort if any required input contains missing values.

  - "omit": drop rows with NA in required inputs before computation.

  - "warn": like "keep" but emits high-missingness warnings.

- na_warn_prop:

  Numeric in \\\[0,1\]\\; per-variable threshold for high-missingness
  warnings. Default 0.2.

- check_extreme:

  Logical; if TRUE, scan for out-of-range values (see `extreme_rules`).
  Default FALSE.

- extreme_action:

  One of c("warn","cap","error","ignore","NA") used when
  `check_extreme = TRUE`.

  - "warn": only warn about out-of-range values (default).

  - "cap": truncate to range and warn.

  - "error": abort if any out-of-range is detected.

  - "ignore": do nothing.

  - "NA": set out-of-range input values to NA before computation.

- extreme_rules:

  Optional named list of numeric c(min,max) ranges for c(age, eGFR,
  UACR). If NULL, broad defaults are used: age (18, 120), eGFR (1, 200),
  UACR (0.1, 10000) (mg/g).

- verbose:

  Logical; if TRUE, prints stepwise messages and a completion summary.
  Default FALSE.

## Value

A tibble with:

- `KFRE_2yr` risk (0-1) at 2 years

- `KFRE_5yr` risk (0-1) at 5 years

## Details

This function preserves prior behavior by default:

- Inputs are taken as-is; NA values propagate to outputs (na_action =
  "keep").

- No capping or out-of-range checks are applied unless requested.

Units (no automatic conversion):

- age: years; sex: 1 = male, 2 = female

- eGFR: mL/min/1.73 m^2

- UACR: mg/g (albumin-to-creatinine ratio)

Details

- Prognostic index: PI = 0.220*log(age) + (-0.556)*log(eGFR) +
  0.451*log(UACR) + 0.391*(male) where male = 1 if sex == 1, else 0.

- Baseline survival: S0(2y) = 0.934, S0(5y) = 0.881 (Tangri 2011).

- Risks: KFRE_t = 1 - (S0_t ^ exp(PI)).

- The 2016 JAMA study provides a large, multinational validation of the
  KFRE in humans.

## References

Tangri N, Stevens LA, Griffith J, others (2011). “A predictive model for
progression of chronic kidney disease to kidney failure.” *JAMA*,
**305**(15), 1553–1559.
[doi:10.1001/jama.2011.451](https://doi.org/10.1001/jama.2011.451) . ;
Tangri N, Grams ME, Levey AS, others (2016). “Multinational assessment
of accuracy of equations for predicting risk of kidney failure: a
meta-analysis.” *JAMA*, **315**(2), 164–174.
[doi:10.1001/jama.2015.18202](https://doi.org/10.1001/jama.2015.18202) .
; Gansevoort RT, Matsushita K, van der Velde M, Astor BC, others (2011).
“Lower estimated GFR and higher albuminuria are associated with adverse
kidney outcomes. A collaborative meta-analysis of general and high-risk
population cohorts.” *Kidney International*, **80**(1), 93–104.
[doi:10.1038/ki.2010.531](https://doi.org/10.1038/ki.2010.531) . ;
Hundemer GL, Tangri N, Sood MM, Ramsay T, Bugeja A, Brown PA, Clark EG,
Biyani M, White CA, Akbari A (2020). “Performance of the Kidney Failure
Risk Equation by Disease Etiology in Advanced CKD.” *Clinical Journal of
the American Society of Nephrology*, **15**(10), 1424–1432.
[doi:10.2215/CJN.03940320](https://doi.org/10.2215/CJN.03940320) .

## See also

[`inflammatory_markers()`](https://sufyansuleman.github.io/HealthMarkers/reference/inflammatory_markers.md),
[`iAge()`](https://sufyansuleman.github.io/HealthMarkers/reference/iAge.md),
[`impute_missing()`](https://sufyansuleman.github.io/HealthMarkers/reference/impute_missing.md)

## Examples

``` r
library(tibble)
df <- tibble(
  age  = c(65, 72),
  sex  = c(1, 2),          # 1 = male, 2 = female
  eGFR = c(45, 22),        # mL/min/1.73 m^2
  UACR = c(300, 1200)      # mg/g
)
# Default behavior (NA propagate, no extreme checks)
kidney_failure_risk(
  data = df,
  col_map = list(age = "age", sex = "sex", eGFR = "eGFR", UACR = "UACR")
)
#> # A tibble: 2 × 2
#>   KFRE_2yr KFRE_5yr
#>      <dbl>    <dbl>
#> 1    0.329    0.523
#> 2    0.536    0.759

# With diagnostics and capping
# \donttest{
kidney_failure_risk(
  data = df,
  col_map = list(age = "age", sex = "sex", eGFR = "eGFR", UACR = "UACR"),
  check_extreme = TRUE, extreme_action = "cap", verbose = TRUE
)
#> kidney_failure_risk(): preparing inputs
#> kidney_failure_risk(): column map: age -> 'age', sex -> 'sex', eGFR -> 'eGFR', UACR -> 'UACR'
#> kidney_failure_risk(): results: KFRE_2yr 2/2, KFRE_5yr 2/2
#> # A tibble: 2 × 2
#>   KFRE_2yr KFRE_5yr
#>      <dbl>    <dbl>
#> 1    0.329    0.523
#> 2    0.536    0.759
# }
```
