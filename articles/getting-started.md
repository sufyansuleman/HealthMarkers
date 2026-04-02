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
#> [1]  30 282
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
#>      SPISE    METS_IR prediabetes HOMA_CP LAR ASI TyG_index non_HDL_c
#> 1 6.538621   175.4422           0      NA  NA  NA  8.390992  2.805635
#> 2 7.040211   163.2440           0      NA  NA  NA  9.133717  4.411394
#> 3 6.738432   342.1412           0      NA  NA  NA  8.423506  3.306616
#> 4 4.125126 -1869.7322           0      NA  NA  NA  9.073867  3.719980
#> 5 6.245599   270.7383           0      NA  NA  NA  8.947845  4.025115
#> 6 5.338716   347.0725           0      NA  NA  NA  8.365320  3.879587
#>    remnant_c ratio_TC_HDL ratio_TG_HDL ratio_LDL_HDL ApoB_ApoA1
#> 1 -0.1707940     2.828547    0.6934188      1.939860  0.4725922
#> 2  1.8086884     3.946180    1.4946281      1.738235  0.6374380
#> 3  0.9604294     3.702865    0.9518317      1.917799  0.3916076
#> 4 -0.1980635     4.916000    2.0409466      4.124501  0.8039860
#> 5  0.7886167     4.085650    1.4183511      2.481097  0.5155962
#> 6  0.6680692     3.971257    0.6777942      2.459603  0.8798657

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
#>   model   value
#>   <chr>   <dbl>
#> 1 AIP   -0.159 
#> 2 AIP    0.175 
#> 3 AIP   -0.0214
#> 4 AIP    0.310 
#> 5 AIP    0.152 
#> 6 AIP   -0.169
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
#>   model   value
#>   <chr>   <dbl>
#> 1 AIP   -0.159 
#> 2 AIP   NA     
#> 3 AIP   -0.0214
#> 4 AIP    0.310 
#> 5 AIP    0.152

# Drop rows with required NA
cvd_marker_aip(missing_demo, col_map = lipid_cols, na_action = "omit")
#> # A tibble: 4 × 2
#>   model   value
#>   <chr>   <dbl>
#> 1 AIP   -0.159 
#> 2 AIP   -0.0214
#> 3 AIP    0.310 
#> 4 AIP    0.152
```

> **`na_action` aliases:** `"ignore"` is a backward-compatible alias for
> `"keep"` (same behavior; retained so older code continues to work).
> `"warn"` is also an alias for `"keep"` that additionally emits a
> missingness warning. If you are reading another function’s help page
> and see `"ignore"` listed first in the choices, it behaves identically
> to `"keep"`.

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
#> 1       7.36     0.594 
#> 2       4.51     0.579 
#> 3       4.67     0.128 
#> 4      14.2      0.0497
#> 5      10.6      0.131 
#> 6       5.89     0.0629
```

## Verbose mode: skipped optional markers

When a function supports optional inputs (columns that extend outputs
when present), set `verbose = TRUE` to see which optional markers were
not computed because their columns were absent from the data. This helps
you distinguish “column not found” from “marker intentionally absent”.

``` r
# Provide only required columns; optional columns are absent
glyc_min <- sim_small[, c("HDL_c", "TG", "BMI")]

# verbose = TRUE lists which optional markers were skipped
glyc_out <- glycemic_markers(glyc_min, verbose = TRUE)
```

The informational message lists all optional keys (glucose, HbA1c,
C_peptide, G0, I0, leptin, adiponectin) that had no matching column, so
the caller knows which derived metrics (e.g., prediabetes flag, HOMA_CP,
LAR) will be `NA` in the output. Once you add those columns to your
data, the message disappears and the metrics are computed automatically.

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
#>      SPISE    METS_IR prediabetes HOMA_CP LAR ASI TyG_index non_HDL_c
#> 1 6.538621   175.4422           0      NA  NA  NA  8.390992  2.805635
#> 2 7.040211   163.2440           0      NA  NA  NA  9.133717  4.411394
#> 3 6.738432   342.1412           0      NA  NA  NA  8.423506  3.306616
#> 4 4.125126 -1869.7322           0      NA  NA  NA  9.073867  3.719980
#> 5 6.245599   270.7383           0      NA  NA  NA  8.947845  4.025115
#> 6 5.338716   347.0725           0      NA  NA  NA  8.365320  3.879587
#>    remnant_c ratio_TC_HDL ratio_TG_HDL ratio_LDL_HDL ApoB_ApoA1
#> 1 -0.1707940     2.828547    0.6934188      1.939860  0.4725922
#> 2  1.8086884     3.946180    1.4946281      1.738235  0.6374380
#> 3  0.9604294     3.702865    0.9518317      1.917799  0.3916076
#> 4 -0.1980635     4.916000    2.0409466      4.124501  0.8039860
#> 5  0.7886167     4.085650    1.4183511      2.481097  0.5155962
#> 6  0.6680692     3.971257    0.6777942      2.459603  0.8798657
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
#> 1 A2                micro             67.4         1.08         94.3
#> 2 A2                micro             66.6         2.36         77.0
#> 3 A3                normal            65.0         2.71         35.1
#> 4 A2                micro             78.3         1.48         75.3
#> 5 A3                normal           110.          2.33         34.5
#> 6 A3                normal            95.7         1.86         54.6
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

> **All 46 domain vignettes** are available on the package website —
> only 12 core vignettes are bundled with the CRAN package.  
> Browse the full collection at
> <https://sufyansuleman.github.io/HealthMarkers/articles/>
