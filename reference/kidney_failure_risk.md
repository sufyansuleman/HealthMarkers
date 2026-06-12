# Kidney Failure Risk Equation (KFRE, 2- and 5-year risk)

Compute 2- and 5-year risk of end-stage kidney disease using the
original 4-variable KFRE (Tangri et al., 2011) with optional
data-quality diagnostics, and verbose progress reporting.

## Usage

``` r
kidney_failure_risk(
  data,
  col_map = list(age = "age", sex = "sex", eGFR = "eGFR", UACR = "UACR"),
  na_action = c("keep", "error", "omit", "warn"),
  na_warn_prop = 0.2,
  verbose = TRUE
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

- verbose:

  Logical; if TRUE, prints stepwise messages and a completion summary.
  Default TRUE.

## Value

A tibble with:

- `KFRE_2yr` risk (0-1) at 2 years

- `KFRE_5yr` risk (0-1) at 5 years

## Details

This function preserves prior behavior by default:

- Inputs are taken as-is; NA values propagate to outputs (na_action =
  "keep").

- No capping or out-of-range checks are applied.

Units:

- age: years; sex: 1 = male, 2 = female

- eGFR: mL/min/1.73 m^2

- UACR: mg/g (albumin-to-creatinine ratio); converted internally to
  mg/mmol (/ 8.84) for the equation, which is defined in mg/mmol.

Details

- 4-variable KFRE (Tangri 2011), non-North-American recalibration:
  `PI = -0.2201*(age/10 - 7.036) + 0.2467*(male - 0.5642) - 0.5567*(eGFR/5 - 7.222) + 0.4510*(ln(ACR_mmol) - 5.137)`
  where male = 1 if sex == 1 else 0, and `ACR_mmol = UACR[mg/g] / 8.84`.

- Baseline survival (non-North-American): S0(2y) = 0.9832, S0(5y) =
  0.9365.

- Risks: KFRE_t = 1 - (S0_t ^ exp(PI)).

- The 2016 JAMA study provides a large, multinational validation of the
  KFRE in humans.

- Uses the non-North-American recalibration; the North-American original
  uses different baseline-survival constants.

## References

Tangri N, Stevens LA, Griffith J, others (2011). “A predictive model for
progression of chronic kidney disease to kidney failure.” *JAMA*,
**305**(15), 1553–1559.
[doi:10.1001/jama.2011.451](https://doi.org/10.1001/jama.2011.451) .
Tangri N, Grams ME, Levey AS, others (2016). “Multinational assessment
of accuracy of equations for predicting risk of kidney failure: a
meta-analysis.” *JAMA*, **315**(2), 164–174.
[doi:10.1001/jama.2015.18202](https://doi.org/10.1001/jama.2015.18202) .

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
#> kidney_failure_risk(): reading input 'df' — 2 rows × 4 variables
#> kidney_failure_risk(): col_map: age -> 'age', sex -> 'sex', eGFR -> 'eGFR', UACR -> 'UACR'
#> kidney_failure_risk(): computing markers:
#>   KFRE_2yr  [age, sex, eGFR, UACR -- 2-year kidney failure risk]
#>   KFRE_5yr  [age, sex, eGFR, UACR -- 5-year kidney failure risk]
#> kidney_failure_risk(): results: KFRE_2yr 2/2, KFRE_5yr 2/2
#> # A tibble: 2 × 2
#>   KFRE_2yr KFRE_5yr
#>      <dbl>    <dbl>
#> 1  0.00381   0.0147
#> 2  0.0599    0.213 

# With verbose output
# \donttest{
kidney_failure_risk(
  data = df,
  col_map = list(age = "age", sex = "sex", eGFR = "eGFR", UACR = "UACR"),
  verbose = TRUE
)
#> kidney_failure_risk(): reading input 'df' — 2 rows × 4 variables
#> kidney_failure_risk(): col_map: age -> 'age', sex -> 'sex', eGFR -> 'eGFR', UACR -> 'UACR'
#> kidney_failure_risk(): computing markers:
#>   KFRE_2yr  [age, sex, eGFR, UACR -- 2-year kidney failure risk]
#>   KFRE_5yr  [age, sex, eGFR, UACR -- 5-year kidney failure risk]
#> kidney_failure_risk(): results: KFRE_2yr 2/2, KFRE_5yr 2/2
#> # A tibble: 2 × 2
#>   KFRE_2yr KFRE_5yr
#>      <dbl>    <dbl>
#> 1  0.00381   0.0147
#> 2  0.0599    0.213 
# }
```
