# Fasting insulin sensitivity indices

## Scope

Compute 10 fasting insulin sensitivity/resistance indices from fasting
glucose (mmol/L) and insulin (pmol/L). Internally converts glucose to
mg/dL (x18) and insulin to muU/mL (/6). Handles column mapping, numeric
coercion, NA policy, optional extreme screening, and optional
normalization.

## When to use

- You have fasting glucose (mmol/L) and insulin (pmol/L) and need
  multiple sensitivity/resistance indices at once.
- You want explicit NA handling (keep/omit/error) plus optional
  extreme-value screening and normalization.
- Your column names differ from the defaults and need mapping.

## Inputs and requirements

- Required columns (mapped via `col_map`): `G0` (fasting glucose,
  mmol/L), `I0` (fasting insulin, pmol/L).
- Units are converted internally (mmol/L -\> mg/dL; pmol/L -\> muU/mL).
  Convert beforehand if your inputs are already in those units to avoid
  double-scaling.
- `na_action`: `keep` (default), `omit`, or `error` governs
  required-input missingness.
- `check_extreme`: optional screening of computed indices against
  `extreme_limit`; actions: `cap`, `NA`, or `error`.
- `normalize`: `none` (default) or `z`, `range`, `inverse`, `robust`
  applied after extreme handling.

## Load packages and data

Use a small subset of the simulated data (30-50 rows). Swap `sim_small`
with your data frame in practice.

``` r
library(HealthMarkers)

sim_path <- system.file("extdata", "simulated_hm_data.rds", package = "HealthMarkers")
sim <- readRDS(sim_path)
sim_small <- dplyr::slice_head(sim, n = 30)
```

Expected units before internal conversion: glucose mmol/L, insulin
pmol/L. If your units differ, convert first or adjust screening
parameters.

## Column map (required)

Map the required inputs to your column names; you provide the
right-hand-side strings (your column names). The left-hand keys (`G0`,
`I0`) are fixed.

### What each input represents

- `G0`: fasting plasma glucose (mmol/L).
- `I0`: fasting insulin (pmol/L).

``` r
col_map <- list(
  G0 = "G0",
  I0 = "I0"
)
```

If you use different labels (e.g., `glucose_mmol`, `insulin_pmol`),
remap accordingly; any column names are fine as long as they hold the
right values.

## Core calculation

Compute all indices and display only the newly calculated columns (not
the full input data).

``` r
fis <- fasting_is(
  data = sim_small,
  col_map = col_map,
  na_action = "keep",
  normalize = "none",
  check_extreme = FALSE,
  verbose = FALSE
)

fis_new <- setdiff(names(fis), names(sim_small))
head(dplyr::select(fis, dplyr::all_of(fis_new)))
#> # A tibble: 6 × 10
#>   Fasting_inv Raynaud HOMA_IR_inv  FIRI QUICKI Belfiore_basal Ig_ratio_basal
#>         <dbl>   <dbl>       <dbl> <dbl>  <dbl>          <dbl>          <dbl>
#> 1      -19.5     2.05       -78.6  70.8  0.134        0.00113         -0.215
#> 2      -13.4     2.98       -61.6  55.5  0.138        0.00144         -0.130
#> 3      -15.8     2.54       -65.9  59.3  0.137        0.00135         -0.167
#> 4       -9.25    4.32       -36.5  32.8  0.149        0.00244         -0.104
#> 5      -17.0     2.35       -83.1  74.8  0.133        0.00107         -0.155
#> 6      -14.4     2.77       -67.3  60.6  0.137        0.00132         -0.138
#> # ℹ 3 more variables: Isi_basal <dbl>, Bennett <dbl>, HOMA_IR_rev_inv <dbl>
```

The preview shows only the newly computed indices. `_inv` columns are
inverted so larger negatives indicate greater insulin resistance;
`QUICKI`, `Raynaud`, and `Isi_basal` decrease as resistance worsens. All
rows are retained here because `na_action = "keep"`.

### Outputs at a glance

- `Fasting_inv`: inverted fasting insulin (higher negative = higher
  insulin)
- `Raynaud`: 40 / insulin
- `HOMA_IR_inv`, `HOMA_IR_rev_inv`: inverted HOMA formulations
- `FIRI`: fasting insulin resistance index
- `QUICKI`: insulin sensitivity index (higher = more sensitive)
- `Belfiore_basal`: 2 / (insulin x glucose + 1)
- `Ig_ratio_basal`: -insulin/glucose (inverted)
- `Isi_basal`: 10000 / (glucose x insulin)
- `Bennett`: 1 / (log insulin x log glucose)

## Inputs and units (quick reminder)

- Required columns (map via `col_map`): `G0` glucose (mmol/L), `I0`
  insulin (pmol/L).
- Internal conversions: glucose x18 -\> mg/dL; insulin /6 -\> muU/mL.
- Extremes: default `check_extreme = FALSE`; when TRUE, outputs are
  screened against `extreme_limit`. `extreme_action = "cap"` trims to
  +/-limit.
- Missingness: `na_action` controls row handling
  (`keep`/`omit`/`error`).
- Normalization: `normalize = "none"` by default; other options apply
  columnwise scaling after any extreme handling.

## Missing data and extremes

Show how row handling and extreme scanning behave on a tiny slice.

``` r
demo <- sim_small[1:6, c("G0", "I0")]
demo$G0[2] <- NA

fis_keep <- fasting_is(demo, col_map, na_action = "keep", check_extreme = FALSE)
fis_omit <- fasting_is(demo, col_map, na_action = "omit", check_extreme = FALSE)

fis_extreme <- fasting_is(
  data = demo,
  col_map = col_map,
  na_action = "keep",
  check_extreme = TRUE,
  extreme_limit = 1e3,
  extreme_action = "cap",
  normalize = "none",
  verbose = FALSE
)

list(
  keep_rows = nrow(fis_keep),
  omit_rows = nrow(fis_omit),
  extreme_head = head(dplyr::select(fis_extreme, dplyr::all_of(fis_new)))
)
#> $keep_rows
#> [1] 6
#> 
#> $omit_rows
#> [1] 5
#> 
#> $extreme_head
#> # A tibble: 6 × 10
#>   Fasting_inv Raynaud HOMA_IR_inv  FIRI QUICKI Belfiore_basal Ig_ratio_basal
#>         <dbl>   <dbl>       <dbl> <dbl>  <dbl>          <dbl>          <dbl>
#> 1      -19.5     2.05       -78.6  70.8  0.134        0.00113         -0.215
#> 2      -13.4     2.98        NA    NA   NA           NA               NA    
#> 3      -15.8     2.54       -65.9  59.3  0.137        0.00135         -0.167
#> 4       -9.25    4.32       -36.5  32.8  0.149        0.00244         -0.104
#> 5      -17.0     2.35       -83.1  74.8  0.133        0.00107         -0.155
#> 6      -14.4     2.77       -67.3  60.6  0.137        0.00132         -0.138
#> # ℹ 3 more variables: Isi_basal <dbl>, Bennett <dbl>, HOMA_IR_rev_inv <dbl>
```

Here `na_action = "keep"` preserves all six rows (with NA-derived
outputs), while `na_action = "omit"` drops the row with missing `G0`.
Extreme scanning with `extreme_action = "cap"` would trim outputs
exceeding `extreme_limit`; none were capped in this slice.

## Expectations

- Both required inputs must be mapped and present; missing mappings or
  columns abort.
- `na_action`: `keep` leaves NA-derived outputs; `omit` drops rows;
  `error` stops on any missing input.
- `check_extreme` screens outputs for magnitudes beyond `extreme_limit`;
  adjust `extreme_limit` and choose `extreme_action`
  (`cap`/`NA`/`error`).
- `normalize` is optional post-hoc scaling; set to `"none"` to preserve
  native scales.

## Verbose diagnostics

Set `verbose = TRUE` to emit three structured messages per call:

1.  **Preparing inputs** — start-of-function signal.
2.  **Column map** — confirms which data column each required key
    resolved to. Example:
    `fasting_is(): column map: G0 -> 'G0', I0 -> 'I0'`
3.  **Results summary** — shows how many rows computed successfully
    (non-NA) per output column. Example:
    `fasting_is(): results: Fasting_inv 28/30, HOMA_IR_inv 28/30, ...`

`verbose = TRUE` emits at the `"inform"` level; you also need
`options(healthmarkers.verbose = "inform")` active:

``` r
old_opt <- options(healthmarkers.verbose = "inform")

invisible(fasting_is(
  data    = sim_small[1:5, ],
  col_map = col_map,
  verbose = TRUE
))
#> fasting_is(): preparing inputs
#> fasting_is(): column map: G0 -> 'G0', I0 -> 'I0'
#> fasting_is(): results: Fasting_inv 5/5, Raynaud 5/5, HOMA_IR_inv 5/5, FIRI 5/5, QUICKI 5/5, Belfiore_basal 5/5, Ig_ratio_basal 5/5, Isi_basal 5/5, Bennett 5/5, HOMA_IR_rev_inv 5/5

options(old_opt)
```

Reset with `options(healthmarkers.verbose = NULL)` or `"none"`.

## Tips

- Non-numeric inputs are coerced to numeric; coercion that introduces
  NAs triggers a warning.
- For pipelines, prefer `na_action = "omit"` or `"error"` to avoid
  silent gaps; use `"keep"` for exploratory summaries.
- Enable `check_extreme = TRUE` when working with noisy or simulated
  data; use `extreme_action = "error"` for strict QA.
- Set `normalize` to align scales across indices if feeding into
  distance-based models; leave as `"none"` for interpretability.
- See
  [`?fasting_is`](https://sufyansuleman.github.io/HealthMarkers/reference/fasting_is.md)
  for full argument reference and diagnostics.
