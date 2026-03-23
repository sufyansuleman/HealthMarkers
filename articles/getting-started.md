# Getting Started with HealthMarkers

## Overview

Load the bundled simulated dataset, pick which markers to compute, and
view only the outputs you care about. All examples use shipped data;
swap in your data when ready.

## When to use

- You want a quick tour of core helpers and the
  [`all_health_markers()`](https://sufyansuleman.github.io/HealthMarkers/reference/all_health_markers.md)
  orchestrator.
- You need to see column inference vs explicit mapping for your data.
- You prefer ready-to-run code snippets (no optional dependencies
  required for these examples).

## What the package offers

- Many clinical/metabolic markers: glycemic, lipid, renal, inflammatory,
  liver, anthropometric/obesity, atherogenic indices, frailty/Charlson,
  sweat/urine panels, micronutrients, pulmonary indices, and more.
- Single-purpose helpers (e.g.,
  [`cvd_marker_aip()`](https://sufyansuleman.github.io/HealthMarkers/reference/cvd_marker_aip.md),
  [`sweat_markers()`](https://sufyansuleman.github.io/HealthMarkers/reference/sweat_markers.md),
  [`fasting_is()`](https://sufyansuleman.github.io/HealthMarkers/reference/fasting_is.md))
  plus
  [`all_health_markers()`](https://sufyansuleman.github.io/HealthMarkers/reference/all_health_markers.md)
  to run multiple groups at once.
- Column inference with
  [`infer_cols()`](https://sufyansuleman.github.io/HealthMarkers/reference/infer_cols.md)
  and override via `col_map` when your labels differ.
- Light missing-data utilities
  ([`impute_missing()`](https://sufyansuleman.github.io/HealthMarkers/reference/impute_missing.md))
  to prep data before computing markers.

## Quick starts

- Targeted helper: `cvd_marker_aip(sim_small)` or
  `renal_markers(sim_small)`.
- Multiple groups:
  `all_health_markers(sim_small, which = c("glycemic", "lipid"))`.
- Everything: `all_health_markers(sim_small, which = "all")` (wide
  output; filter afterward).
- Insulin sensitivity/resistance:
  [`fasting_is()`](https://sufyansuleman.github.io/HealthMarkers/reference/fasting_is.md),
  [`ogtt_is()`](https://sufyansuleman.github.io/HealthMarkers/reference/ogtt_is.md),
  [`adipo_is()`](https://sufyansuleman.github.io/HealthMarkers/reference/adipo_is.md),
  or include insulin groups in
  [`all_health_markers()`](https://sufyansuleman.github.io/HealthMarkers/reference/all_health_markers.md)
  with a `col_map` for glucose/insulin.

## Common helpers (at a glance)

- `all_health_markers(data, which = ...)` — run multiple groups or all
  markers at once.
- `cvd_marker_aip(data, col_map = list(TG = "TG", HDL_c = "HDL_c"))` —
  atherogenic index of plasma.
- `renal_markers(data)` — renal and eGFR-related outputs.
- `glycemic_markers(data)` — glycemic metrics; pair with
  [`fasting_is()`](https://sufyansuleman.github.io/HealthMarkers/reference/fasting_is.md)
  /
  [`ogtt_is()`](https://sufyansuleman.github.io/HealthMarkers/reference/ogtt_is.md)
  /
  [`adipo_is()`](https://sufyansuleman.github.io/HealthMarkers/reference/adipo_is.md)
  for insulin indices.
- `sweat_markers(data, check_extreme = TRUE)` and `urine_markers(data)`
  — alternate biofluid panels.

## Load the package and simulated data

Inputs: the package plus the bundled RDS file at
`inst/extdata/simulated_hm_data.rds`.

Outputs: two data frames—`sim` (full) and `sim_small` (first 30 rows for
fast examples).

``` r
if (!requireNamespace("HealthMarkers", quietly = TRUE)) {
  if (requireNamespace("pkgload", quietly = TRUE)) {
    pkgload::load_all("..")
  } else {
    stop("Install HealthMarkers (or pkgload for development) before knitting.", call. = FALSE)
  }
}
library(HealthMarkers)

sim_path <- system.file("extdata", "simulated_hm_data.rds", package = "HealthMarkers")
sim <- readRDS(sim_path)

# Work with a small subset using slice_head
sim_small <- dplyr::slice_head(sim, n = 30)

dim(sim_small)
#> [1]  30 225
```

## Minimal example: broad panels

Input: `sim_small` with inferred columns.

What it does:
[`all_health_markers()`](https://sufyansuleman.github.io/HealthMarkers/reference/all_health_markers.md)
auto-detects columns and computes the requested groups.

Output: a tibble of computed markers. Below we print only the new
columns.

``` r
hm_out <- all_health_markers(
  data = sim_small,
  which = c("glycemic", "lipid", "renal", "inflammatory"),
  verbose = FALSE
)

hm_new_cols <- setdiff(names(hm_out), names(sim_small))
head(dplyr::select(hm_out, dplyr::all_of(hm_new_cols)))
#>      SPISE METS_IR prediabetes HOMA_CP LAR ASI TyG_index non_HDL_c  remnant_c
#> 1 8.297275      NA           0      NA  NA  NA        NA  4.472486  0.4091206
#> 2 4.177562      NA           0      NA  NA  NA        NA  3.338557  0.2301283
#> 3 6.351097      NA           0      NA  NA  NA        NA  4.154120  1.4407099
#> 4 5.963605      NA           0      NA  NA  NA        NA  4.112558  1.1387383
#> 5 7.139034      NA           0      NA  NA  NA        NA  3.324661  0.2533190
#> 6 4.529109      NA           0      NA  NA  NA        NA  3.572372 -0.2791537
#>   ratio_TC_HDL ratio_TG_HDL ratio_LDL_HDL ApoB_ApoA1
#> 1     5.952950    1.0716354      4.499879  0.5855594
#> 2     4.340045    1.8606939      3.109814  1.0015916
#> 3     3.679324    0.7970881      1.750095  0.7404117
#> 4     5.058502    1.5935284      2.934731  0.6027401
#> 5     3.088419    1.0120564      1.929294  0.4739219
#> 6     3.564597    1.5878714      2.765001  0.5349333

# New columns added (names only):
head(hm_new_cols)
#> [1] "SPISE"       "METS_IR"     "prediabetes" "HOMA_CP"     "LAR"        
#> [6] "ASI"
```

## Data readiness: columns at a glance

Most functions infer columns automatically. If your names differ,
provide a `col_map`. Typical defaults:

| Panel                                     | Typical columns                                  | Notes                                                                                                                                                        |
|-------------------------------------------|--------------------------------------------------|--------------------------------------------------------------------------------------------------------------------------------------------------------------|
| Glycemic                                  | `FPG`, `insulin_fasting`, `HbA1c`                | Uses fasting and HbA1c where available                                                                                                                       |
| Lipid                                     | `TG`, `HDL_c`, `LDL_c`, `TC`                     | Supports TG/HDL aliasing for AIP                                                                                                                             |
| Renal                                     | `creatinine`, `uacr`, `age`, `sex`               | Infers eGFR and kidney markers                                                                                                                               |
| Blood pressure (used by CVD risk helpers) | `SBP`, `DBP`                                     | Inputs to CVD risk functions; not a standalone [`all_health_markers()`](https://sufyansuleman.github.io/HealthMarkers/reference/all_health_markers.md) group |
| Anthropometrics                           | `height`, `weight`, `BMI`, `waist_circumference` | Supports z-score and obesity indices                                                                                                                         |

Column inference is usually enough; if your data use different labels,
provide a `col_map` override.

Tip: To inspect what the package would infer on your data without
running calculations, use:

``` r
infer_cols(sim_small)
```

## Prepare your data (optional but recommended)

- **Check columns**: `infer_cols(your_data)` to see what will be picked
  up.
- **Handle missing values**:
  `impute_missing(your_data, method = "mice" | "missForest" | "mean" | "median" | "constant")`
  and use the completed data in calculators.
- **Normalize (insulin helpers)**: insulin-related functions accept
  normalization choices (e.g., `normalize = "z"`,
  `normalize = "range"`); leave defaults unless you need scaled outputs.

## Column inference vs explicit mapping

Input: the same data but with explicit column aliases.

What it does: provides TG/HDL names to compute AIP when your columns
differ.

Output: a tibble with the AIP result.

``` r
lipid_cols <- list(TG = "TG", HDL_c = "HDL_c")

aip <- cvd_marker_aip(sim_small, col_map = lipid_cols, na_action = "keep")
aip_new <- setdiff(names(aip), names(sim_small))
head(dplyr::select(aip, dplyr::all_of(aip_new)))
#> # A tibble: 6 × 2
#>   model    value
#>   <chr>    <dbl>
#> 1 AIP    0.0300 
#> 2 AIP    0.270  
#> 3 AIP   -0.0985 
#> 4 AIP    0.202  
#> 5 AIP    0.00520
#> 6 AIP    0.201
```

## Handling missingness

Most functions accept `na_action`. This shows how outputs differ when
required inputs are missing.

``` r
missing_demo <- sim_small[1:5, c("TG", "HDL_c")]
missing_demo$TG[2] <- NA

# Keep rows (produces NA where inputs are missing)
cvd_marker_aip(missing_demo, col_map = lipid_cols, na_action = "keep")
#> # A tibble: 5 × 2
#>   model    value
#>   <chr>    <dbl>
#> 1 AIP    0.0300 
#> 2 AIP   NA      
#> 3 AIP   -0.0985 
#> 4 AIP    0.202  
#> 5 AIP    0.00520

# Drop rows with required NA
cvd_marker_aip(missing_demo, col_map = lipid_cols, na_action = "omit")
#> # A tibble: 4 × 2
#>   model    value
#>   <chr>    <dbl>
#> 1 AIP    0.0300 
#> 2 AIP   -0.0985 
#> 3 AIP    0.202  
#> 4 AIP    0.00520
```

If you want to impute before computing markers:

``` r
imputed <- impute_missing(sim_small, method = "mice", m = 1, maxit = 3)
head(imputed)
```

### Imputation options (what happens under the hood)

- [`impute_missing()`](https://sufyansuleman.github.io/HealthMarkers/reference/impute_missing.md)
  is a preprocessing helper; marker functions do not impute internally.
  Run it first, then call calculators.
- Methods:
  - `method = "mice"`: multiple imputation by chained equations via
    **mice**; control `m` (imputations) and `maxit` (iterations).
  - `method = "missForest"`: nonparametric random-forest imputation via
    **missForest**; good for mixed data types.
  - `method = "mean" | "median" | "constant"`: simple deterministic
    fills for numeric columns.
- Output: a data frame with imputed values replacing missing entries;
  then pass it to
  [`all_health_markers()`](https://sufyansuleman.github.io/HealthMarkers/reference/all_health_markers.md)
  or any helper (e.g.,
  [`cvd_marker_aip()`](https://sufyansuleman.github.io/HealthMarkers/reference/cvd_marker_aip.md),
  [`renal_markers()`](https://sufyansuleman.github.io/HealthMarkers/reference/renal_markers.md)).

## Extreme value checks

Scan for extreme sweat values and warn without stopping. Output: sweat
markers with any warnings emitted.

``` r
sweat <- sweat_markers(sim_small, check_extreme = TRUE, extreme_action = "warn")
sweat_new <- setdiff(names(sweat), names(sim_small))
head(dplyr::select(sweat, dplyr::all_of(sweat_new)))
#> # A tibble: 6 × 2
#>   Na_K_ratio sweat_rate
#>        <dbl>      <dbl>
#> 1       7.84      0.388
#> 2       4.78      0.123
#> 3       5.42      0.443
#> 4       2.28      0.157
#> 5       8.53      0.296
#> 6      10.1       0.534
```

## Common functions

### Cardio-metabolic

Input: `sim_small`; groups: glycemic, lipid, renal, inflammatory.

Output: a tibble with those markers (first rows shown).

``` r
cardio_panel <- all_health_markers(
  data = sim_small,
  which = c("glycemic", "lipid", "renal", "inflammatory"),
  verbose = FALSE
)

cardio_new <- setdiff(names(cardio_panel), names(sim_small))
head(dplyr::select(cardio_panel, dplyr::all_of(cardio_new)))
#>      SPISE METS_IR prediabetes HOMA_CP LAR ASI TyG_index non_HDL_c  remnant_c
#> 1 8.297275      NA           0      NA  NA  NA        NA  4.472486  0.4091206
#> 2 4.177562      NA           0      NA  NA  NA        NA  3.338557  0.2301283
#> 3 6.351097      NA           0      NA  NA  NA        NA  4.154120  1.4407099
#> 4 5.963605      NA           0      NA  NA  NA        NA  4.112558  1.1387383
#> 5 7.139034      NA           0      NA  NA  NA        NA  3.324661  0.2533190
#> 6 4.529109      NA           0      NA  NA  NA        NA  3.572372 -0.2791537
#>   ratio_TC_HDL ratio_TG_HDL ratio_LDL_HDL ApoB_ApoA1
#> 1     5.952950    1.0716354      4.499879  0.5855594
#> 2     4.340045    1.8606939      3.109814  1.0015916
#> 3     3.679324    0.7970881      1.750095  0.7404117
#> 4     5.058502    1.5935284      2.934731  0.6027401
#> 5     3.088419    1.0120564      1.929294  0.4739219
#> 6     3.564597    1.5878714      2.765001  0.5349333
```

### Urine markers

Input: `sim_small`; output: urine markers with rows containing required
inputs.

``` r
urine <- urine_markers(sim_small, na_action = "omit")
urine_new <- setdiff(names(urine), names(sim_small))
head(dplyr::select(urine, dplyr::all_of(urine_new)))
#> # A tibble: 6 × 11
#>   albuminuria_stage microalbuminuria  UPCR U_Na_K_ratio NGAL_per_gCr
#>   <fct>             <fct>            <dbl>        <dbl>        <dbl>
#> 1 A3                normal           226.         2.67        112.  
#> 2 A3                normal           142.         2.36         80.6 
#> 3 A3                normal           252.         0.856        14.0 
#> 4 A3                normal           159.         2.63         61.9 
#> 5 A3                normal           390.         1.49        222.  
#> 6 A3                normal            36.1        5.13          6.01
#> # ℹ 6 more variables: KIM1_per_gCr <dbl>, NAG_per_gCr <dbl>,
#> #   Beta2Micro_per_gCr <dbl>, A1Micro_per_gCr <dbl>, IL18_per_gCr <dbl>,
#> #   L_FABP_per_gCr <dbl>
```

### All markers (wide; optional)

To compute every available group on your data (can produce many
columns), set `which = "all"`. Here we keep it example-only to avoid
long output:

``` r
all_out <- all_health_markers(data = sim_small, which = "all", verbose = FALSE)
head(setdiff(names(all_out), names(sim_small)))
```

## Export-ready output

Select columns and write to a CSV (disabled during vignette build):

``` r
export_cols <- c("aip", "egfr_ckd_epi", "homa_ir")
out_path <- tempfile("healthmarkers_export_", fileext = ".csv")
hm_out_subset <- dplyr::select(cardio_panel, dplyr::any_of(export_cols))
utils::write.csv(hm_out_subset, out_path, row.names = FALSE)
out_path
```

## Next steps

- Explore focused vignettes for insulin sensitivity, cardio-renal
  panels, and frailty.
- See individual help pages (e.g.,
  [`?fasting_is`](https://sufyansuleman.github.io/HealthMarkers/reference/fasting_is.md),
  [`?cvd_risk`](https://sufyansuleman.github.io/HealthMarkers/reference/cvd_risk.md),
  [`?frailty_index`](https://sufyansuleman.github.io/HealthMarkers/reference/frailty_index.md))
  for detailed arguments and references.
