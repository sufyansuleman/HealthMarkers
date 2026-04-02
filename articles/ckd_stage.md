# Chronic Kidney Disease Stage

## Scope

Classify eGFR into KDIGO G1-G5, albuminuria (UACR mg/g) into A1-A3, and
combine to KDIGO risk. If UACR is absent, albuminuria is NA and KDIGO
risk assumes A1 for mapping.

## When to use

- You have eGFR (mL/min/1.73 m^2) and optionally UACR (mg/g) and want
  KDIGO staging.
- You need explicit handling of missing inputs and optional
  extreme-value screening.
- You want a lightweight KDIGO risk label without age adjustment.

## Requirements checklist

- Packages: HealthMarkers, dplyr (for display).
- Columns: eGFR required; UACR optional. Units: eGFR mL/min/1.73 m^2;
  UACR mg/g.
- Column map: list(eGFR = …, UACR = …) with UACR optional; mapping
  missing columns errors (except optional UACR).
- Row policy: na_action = keep (default), omit, error.
- Extreme screening: check_extreme + extreme_action (cap/NA/error);
  defaults cap eGFR 0-200, UACR 0-5000 when enabled.

## Load packages and example data

Example uses a 50-row slice of simulated data.

``` r
library(HealthMarkers)
library(dplyr)

sim_path <- system.file("extdata", "simulated_hm_data.rds", package = "HealthMarkers")
sim <- readRDS(sim_path)
sim_small <- dplyr::slice_head(sim, n = 30) %>%
  dplyr::select(eGFR, UACR)
```

## Map columns

UACR is optional; omit it from col_map if unavailable.

``` r
col_map <- list(eGFR = "eGFR", UACR = "UACR")
```

## Quick start: stage CKD

Defaults keep rows with missing inputs and return NA stages for them.

``` r
ckd_out <- ckd_stage(
  data = sim_small,
  col_map = col_map,
  na_action = "keep",
  check_extreme = FALSE,
  extreme_action = "cap"
)

head(ckd_out)
#> # A tibble: 6 × 3
#>   CKD_stage Albuminuria_stage KDIGO_risk
#>   <fct>     <fct>             <fct>     
#> 1 G1        A2                Moderate  
#> 2 G2        A1                Low       
#> 3 G2        A2                Moderate  
#> 4 G3a       A1                Moderate  
#> 5 G1        A1                Low       
#> 6 G2        A2                Moderate
```

## Arguments that matter

- col_map: eGFR required; UACR optional. If UACR is mapped but missing
  in data, albuminuria is NA and a warning is issued.
- na_action: keep (default, retain rows; stages become NA), omit (drop
  rows with missing inputs), error (abort on missing).
- check_extreme: FALSE by default; TRUE enables bounds checks (eGFR
  0-200; UACR 0-5000 mg/g by default).
- extreme_action: cap (default), NA, or error when check_extreme = TRUE.

## Handling missing inputs

- Non-numeric inputs are coerced; NA introduced are warned. Non-finite
  become NA.
- Missing eGFR yields NA stages unless na_action = omit/error. Missing
  UACR yields NA albuminuria; KDIGO risk treats missing albuminuria as
  A1 for mapping.

### Compare row policies

``` r
demo <- sim_small[1:8, ]
demo$eGFR[3] <- NA

a_keep <- ckd_stage(demo, col_map, na_action = "keep", check_extreme = FALSE)
a_omit <- ckd_stage(demo, col_map, na_action = "omit", check_extreme = FALSE)

list(
  keep_rows = nrow(a_keep),
  omit_rows = nrow(a_omit),
  preview = head(a_keep)
)
#> $keep_rows
#> [1] 8
#> 
#> $omit_rows
#> [1] 7
#> 
#> $preview
#> # A tibble: 6 × 3
#>   CKD_stage Albuminuria_stage KDIGO_risk
#>   <fct>     <fct>             <fct>     
#> 1 G1        A2                Moderate  
#> 2 G2        A1                Low       
#> 3 NA        A2                NA        
#> 4 G3a       A1                Moderate  
#> 5 G1        A1                Low       
#> 6 G2        A2                Moderate
```

## Extreme-value screening (optional)

Cap or error on out-of-range values.

``` r
demo2 <- demo
demo2$UACR[5] <- 6000  # extreme albuminuria

a_cap <- ckd_stage(
  data = demo2,
  col_map = col_map,
  na_action = "keep",
  check_extreme = TRUE,
  extreme_action = "cap"
)

head(select(a_cap, Albuminuria_stage, KDIGO_risk))
#> # A tibble: 6 × 2
#>   Albuminuria_stage KDIGO_risk
#>   <fct>             <fct>     
#> 1 A2                Moderate  
#> 2 A1                Low       
#> 3 A2                NA        
#> 4 A1                Moderate  
#> 5 A3                Moderate  
#> 6 A2                Moderate
```

## Outputs

- CKD_stage: G1, G2, G3a, G3b, G4, G5
- Albuminuria_stage: A1, A2, A3 (NA if UACR missing)
- KDIGO_risk: Low, Moderate, High, Very High (assumes A1 when
  albuminuria is missing for risk mapping) Rows drop only with na_action
  = “omit” or when na_action = “error” aborts.

## Pitfalls and tips

- Use consistent units: eGFR mL/min/1.73 m^2; UACR mg/g. Do not mix mg/g
  with mg/mmol.
- If you do not have UACR, remove it from col_map; albuminuria stays NA
  and risk assumes A1.
- Use extreme_action = “error” for strict QC; cap for gentle cleaning.

## Validation ideas

- eGFR 92, UACR 10 -\> G1/A1, KDIGO risk Low.
- eGFR 52, UACR 400 -\> G3a/A3, KDIGO risk High.
- eGFR 22, UACR missing -\> G4, Albuminuria NA, KDIGO risk Very High.

## Verbose diagnostics

Set `verbose = TRUE` to emit three structured messages per call:

1.  **Preparing inputs** — start-of-function signal.
2.  **Column map** — confirms which data column each required key
    (`eGFR`, `UACR`) resolved to. Example:
    `ckd_stage(): column map: eGFR -> 'eGFR', UACR -> 'UACR'`
3.  **Results summary** — shows how many rows computed successfully
    (non-NA) per output column. Example:
    `ckd_stage(): results: ckd_g_stage 30/30, ckd_a_stage 28/30, ckd_stage 28/30, ...`

`verbose = TRUE` emits at the `"inform"` level; you also need
`options(healthmarkers.verbose = "inform")` active:

``` r
old_opt <- options(healthmarkers.verbose = "inform")

df_v <- data.frame(eGFR = c(95, 52, 22), UACR = c(10, 400, NA))
ckd_stage(
  df_v,
  col_map = list(eGFR = "eGFR", UACR = "UACR"),
  verbose = TRUE
)
#> ckd_stage(): column map: eGFR -> 'eGFR', UACR -> 'UACR'
#> ckd_stage(): completed
#> ckd_stage(): results: CKD_stage 3/3, Albuminuria_stage 2/3, KDIGO_risk 3/3
#> # A tibble: 3 × 3
#>   CKD_stage Albuminuria_stage KDIGO_risk
#>   <fct>     <fct>             <fct>     
#> 1 G1        A1                Low       
#> 2 G3a       A3                High      
#> 3 G4        NA                Very High

options(old_opt)
```

Reset with `options(healthmarkers.verbose = NULL)` or `"none"`.

## See also

- kidney_kfre() for KFRE risk.
- renal_markers() for additional renal metrics.
