# Fasting insulin sensitivity indices

## Scope

Compute 10 fasting insulin sensitivity/resistance indices from fasting
glucose (mmol/L) and insulin (pmol/L). Internally converts glucose to
mg/dL (x18) and insulin to muU/mL (/6). Handles column mapping, numeric
coercion, NA policy, and optional normalization.

## When to use

- You have fasting glucose (mmol/L) and insulin (pmol/L) and need
  multiple sensitivity/resistance indices at once.
- You want explicit NA handling (keep/omit/error) plus optional
  normalization.
- Your column names differ from the defaults and need mapping.

## Inputs and requirements

- Required columns (mapped via `col_map`): `G0` (fasting glucose,
  mmol/L), `I0` (fasting insulin, pmol/L).
- Units are converted internally (mmol/L -\> mg/dL; pmol/L -\> muU/mL).
  Convert beforehand if your inputs are already in those units to avoid
  double-scaling.
- `na_action`: `keep` (default), `omit`, or `error` governs
  required-input missingness.
- `normalize`: `none` (default) or `z`, `range`, `inverse`, `robust`
  applied after computation.

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
  verbose = FALSE
)

fis_new <- setdiff(names(fis), names(sim_small))
head(dplyr::select(fis, dplyr::all_of(fis_new)))
#> # A tibble: 6 × 10
#>   Fasting_inv Raynaud HOMA_IR_inv  FIRI QUICKI Belfiore_basal Ig_ratio_basal
#>         <dbl>   <dbl>       <dbl> <dbl>  <dbl>          <dbl>          <dbl>
#> 1      -11.4     3.52       -57.1  51.4  0.140        0.00156        -0.101 
#> 2       -4.25    9.41       -18.9  17.0  0.165        0.00470        -0.0425
#> 3       -9.77    4.10       -42.1  37.9  0.146        0.00211        -0.101 
#> 4       -8.43    4.74       -34.5  31.0  0.150        0.00258        -0.0917
#> 5      -14.6     2.75       -44.3  39.9  0.145        0.00200        -0.212 
#> 6       -7.88    5.07       -28.7  25.8  0.155        0.00309        -0.0963
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
- Missingness: `na_action` controls row handling
  (`keep`/`omit`/`error`).
- Normalization: `normalize = "none"` by default; other options apply
  columnwise scaling after computation.

## Missing data handling

Show how row handling behaves on a tiny slice.

``` r
demo <- sim_small[1:6, c("G0", "I0")]
demo$G0[2] <- NA

fis_keep <- fasting_is(demo, col_map, na_action = "keep")
fis_omit <- fasting_is(demo, col_map, na_action = "omit")

list(
  keep_rows = nrow(fis_keep),
  omit_rows = nrow(fis_omit),
  keep_head = head(dplyr::select(fis_keep, dplyr::all_of(fis_new)))
)
#> $keep_rows
#> [1] 6
#> 
#> $omit_rows
#> [1] 5
#> 
#> $keep_head
#> # A tibble: 6 × 10
#>   Fasting_inv Raynaud HOMA_IR_inv  FIRI QUICKI Belfiore_basal Ig_ratio_basal
#>         <dbl>   <dbl>       <dbl> <dbl>  <dbl>          <dbl>          <dbl>
#> 1      -11.4     3.52       -57.1  51.4  0.140        0.00156        -0.101 
#> 2       -4.25    9.41        NA    NA   NA           NA              NA     
#> 3       -9.77    4.10       -42.1  37.9  0.146        0.00211        -0.101 
#> 4       -8.43    4.74       -34.5  31.0  0.150        0.00258        -0.0917
#> 5      -14.6     2.75       -44.3  39.9  0.145        0.00200        -0.212 
#> 6       -7.88    5.07       -28.7  25.8  0.155        0.00309        -0.0963
#> # ℹ 3 more variables: Isi_basal <dbl>, Bennett <dbl>, HOMA_IR_rev_inv <dbl>
```

Here `na_action = "keep"` preserves all six rows (with NA-derived
outputs for the row with missing `G0`), while `na_action = "omit"` drops
that row.

## Expectations

- Both required inputs must be mapped and present; missing mappings or
  columns abort.
- `na_action`: `keep` leaves NA-derived outputs; `omit` drops rows;
  `error` stops on any missing input.
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
#> fasting_is(): reading input 'data' — 5 rows × 519 variables
#> fasting_is(): col_map (2 columns — 2 specified)
#>   G0                ->  'G0'
#>   I0                ->  'I0'
#> fasting_is(): computing markers:
#>   Fasting_inv          [G0, I0]
#>   Raynaud              [G0, I0]
#>   HOMA_IR_inv          [G0, I0]
#>   FIRI                 [G0, I0]
#>   QUICKI               [G0, I0]
#>   Belfiore_basal       [G0, I0]
#>   Ig_ratio_basal       [G0, I0]
#>   Isi_basal            [G0, I0]
#>   Bennett              [G0, I0]
#>   HOMA_IR_rev_inv      [G0, I0]
#> fasting_is(): results: id 5/5, Fasting_inv 5/5, Raynaud 5/5, HOMA_IR_inv 5/5, FIRI 5/5, QUICKI 5/5, Belfiore_basal 5/5, Ig_ratio_basal 5/5, Isi_basal 5/5, Bennett 5/5, HOMA_IR_rev_inv 5/5

options(old_opt)
```

Reset with `options(healthmarkers.verbose = NULL)` or `"none"`.

## Tips

- Non-numeric inputs are coerced to numeric; coercion that introduces
  NAs triggers a warning.
- For pipelines, prefer `na_action = "omit"` or `"error"` to avoid
  silent gaps; use `"keep"` for exploratory summaries.
- Set `normalize` to align scales across indices if feeding into
  distance-based models; leave as `"none"` for interpretability.
- See
  [`?fasting_is`](https://sufyansuleman.github.io/HealthMarkers/reference/fasting_is.md)
  for full argument reference and diagnostics.
