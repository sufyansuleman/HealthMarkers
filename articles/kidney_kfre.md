# Kidney failure risk (KFRE)

## Scope

4-variable Kidney Failure Risk Equation (KFRE) for 2- and 5-year ESKD
risk. Includes NA policies, high-missingness warnings, and verbose
summaries. Units: age (years), sex coded 1=male/2=female, eGFR
(mL/min/1.73 m^2), UACR (mg/g). Supply UACR in mg/g; it is converted
internally to mg/mmol (÷8.84) for the equation, which is defined in
mg/mmol.

## Clinical context

The KFRE (Tangri *et al.*, 2011, *JAMA*; validated in 3-continent
cohort) estimates 2- and 5-year probability of kidney failure (ESKD)
from four routine variables. It is now embedded in the KDIGO 2022 CKD
guidelines for risk stratification and nephrology referral decisions.

| CKD Stage | eGFR (mL/min/1.73 m²) | 5-yr KFRE (typical range) |
|-----------|-----------------------|---------------------------|
| G3a       | 45–59                 | \< 1%                     |
| G3b       | 30–44                 | 1–5%                      |
| G4        | 15–29                 | 5–40%                     |
| G5        | \< 15                 | \> 40% (often \> 70%)     |

Outputs `KFRE_2yr` and `KFRE_5yr` are probabilities (0–1); multiply by
100 for percentages. The equation was developed in adults with CKD
G3–G5; interpret cautiously outside this range.

## Load packages and demo data

``` r

library(HealthMarkers)
library(tibble)

# Ten participants spanning CKD G3a → G5; mixed sex; UACR A1 → A3
df <- tibble::tibble(
  age  = c(45, 52, 60, 68, 72, 55, 63, 75, 80, 48),
  sex  = c(1,  2,  1,  2,  1,  2,  1,  2,  1,  2 ),
  eGFR = c(58, 50, 42, 35, 28, 22, 18, 14, 10,  8 ),
  UACR = c(25, 80, 200, 500, 800, 1200, 2500, 4500, 8000, 6000)
)
```

## Column map (required)

``` r

col_map <- list(age = "age", sex = "sex", eGFR = "eGFR", UACR = "UACR")
```

## Core calculation

Default `na_action = "keep"`: missing inputs propagate to `NA` risks; no
extreme checks.

``` r

kfre_default <- kidney_failure_risk(
  data = df,
  col_map = col_map,
  na_action = "keep",
  verbose = FALSE
)

kfre_default
#> # A tibble: 10 × 2
#>    KFRE_2yr KFRE_5yr
#>       <dbl>    <dbl>
#>  1 0.000454  0.00176
#>  2 0.00125   0.00484
#>  3 0.00494   0.0190 
#>  4 0.0106    0.0405 
#>  5 0.0332    0.123  
#>  6 0.0859    0.294  
#>  7 0.189     0.556  
#>  8 0.226     0.629  
#>  9 0.448     0.900  
#> 10 0.643     0.981
```

## Extreme values

Extreme values will produce extreme risk estimates. Pre-filter
implausible inputs before calling.

``` r

# Pre-filter example
df_filtered <- df
df_filtered$eGFR[df_filtered$eGFR > 200] <- NA
df_filtered$UACR[df_filtered$UACR > 10000] <- NA

kfre_filtered <- kidney_failure_risk(
  data = df_filtered,
  col_map = col_map,
  na_action = "warn",
  verbose = FALSE
)

kfre_filtered
#> # A tibble: 10 × 2
#>    KFRE_2yr KFRE_5yr
#>       <dbl>    <dbl>
#>  1 0.000454  0.00176
#>  2 0.00125   0.00484
#>  3 0.00494   0.0190 
#>  4 0.0106    0.0405 
#>  5 0.0332    0.123  
#>  6 0.0859    0.294  
#>  7 0.189     0.556  
#>  8 0.226     0.629  
#>  9 0.448     0.900  
#> 10 0.643     0.981
```

## Missing data handling

`keep` propagates NA risks; `omit` drops rows with missing required
inputs; `error` stops on missingness.

``` r

df_na <- df
df_na$eGFR[2] <- NA

keep_out <- kidney_failure_risk(df_na, col_map, na_action = "keep")
omit_out <- kidney_failure_risk(df_na, col_map, na_action = "omit")

list(keep_rows = nrow(keep_out), omit_rows = nrow(omit_out))
#> $keep_rows
#> [1] 10
#> 
#> $omit_rows
#> [1] 9
```

## Expectations

- All four predictors must be mapped and present; `sex` must be 1/2.
- Inputs must be numeric; non-finite become `NA` and may propagate
  depending on `na_action`.
- Output: tibble with `KFRE_2yr`, `KFRE_5yr`; row count follows
  `na_action` and any row filtering you apply.

## Verbose diagnostics

``` r

old_opt <- options(healthmarkers.verbose = "inform")
kidney_failure_risk(
  data    = df,
  col_map = col_map,
  verbose = TRUE
)
#> kidney_failure_risk(): reading input 'df' — 10 rows × 4 variables
#> kidney_failure_risk(): col_map: age -> 'age', sex -> 'sex', eGFR -> 'eGFR', UACR -> 'UACR'
#> kidney_failure_risk(): computing markers:
#>   KFRE_2yr  [age, sex, eGFR, UACR -- 2-year kidney failure risk]
#>   KFRE_5yr  [age, sex, eGFR, UACR -- 5-year kidney failure risk]
#> kidney_failure_risk(): results: KFRE_2yr 10/10, KFRE_5yr 10/10
#> # A tibble: 10 × 2
#>    KFRE_2yr KFRE_5yr
#>       <dbl>    <dbl>
#>  1 0.000454  0.00176
#>  2 0.00125   0.00484
#>  3 0.00494   0.0190 
#>  4 0.0106    0.0405 
#>  5 0.0332    0.123  
#>  6 0.0859    0.294  
#>  7 0.189     0.556  
#>  8 0.226     0.629  
#>  9 0.448     0.900  
#> 10 0.643     0.981
options(old_opt)
```

## Using real cohort data

The bundled simulated dataset includes `eGFR` and `UACR` columns. Age
and sex are also present, so the KFRE can run directly on `sim_small`:

``` r

library(dplyr)
sim_path <- system.file("extdata", "simulated_hm_data.rds", package = "HealthMarkers")
sim_small <- dplyr::slice_head(readRDS(sim_path), n = 50)

kfre_sim <- kidney_failure_risk(
  data    = sim_small,
  col_map = list(age = "age", sex = "sex", eGFR = "eGFR", UACR = "UACR"),
  na_action   = "keep"
)

dplyr::bind_cols(dplyr::select(sim_small, age, sex, eGFR, UACR), kfre_sim) |> head()
#>    age sex eGFR UACR   id   KFRE_2yr  KFRE_5yr
#> 1 62.6   1    1  5.0 P001 0.08155450 0.2806587
#> 2 38.0   1    1  4.8 P002 0.13370487 0.4263735
#> 3 38.0   1    1  9.5 P003 0.17739473 0.5305345
#> 4 48.3   1    1 26.6 P004 0.21937989 0.6167306
#> 5 81.5   2    1 16.8 P005 0.07294955 0.2542086
#> 6 82.8   2    1 13.2 P006 0.06389205 0.2255952
```

> **Column recognition tip:** For biobank extracts where eGFR or UACR
> are named differently (e.g., `eGFR_CKD_EPI`, `ACR_mg_g`), run
> `hm_col_report(your_data)` to check what is auto-detected before
> building your `col_map`.

## Tips

- Provide UACR in mg/g (the US convention); the function converts to
  mg/mmol (÷8.84) internally. Do **not** pre-convert to mg/mmol, or it
  will be double-converted.
- Use `na_action = "error"` for strict QA; `warn` to surface high
  missingness while retaining rows.
