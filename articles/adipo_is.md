# Adipose insulin sensitivity indices

## Scope

Fasting adipose insulin sensitivity/resistance indices in one call:
Revised_QUICKI, McAuley_index, Adipo_inv, Belfiore_inv_FFA, TyG_inv,
TG_HDL_C_inv, and sex-specific VAI/LAP (inverted so more negative =
worse). Inputs are fasting; common glucose/insulin/TG/HDL unit
conversions are handled internally.

## When to use this

- You need multiple adipose IR markers for metabolic phenotyping or
  clustering.
- You have fasting glucose, insulin, lipids, and adiposity (waist/BMI);
  sex is optional but improves VAI/LAP labeling.
- You want built-in plausibility screening and NA policy controls.

## What you need

- Required columns: `G0` (mmol/L), `I0` (pmol/L), `TG` (mmol/L), `HDL_c`
  (mmol/L), `FFA` (mmol/L), `waist` (cm), `bmi` (kg/m^2).
- Fasting values only; post-prandial inputs will misstate IR.
- Units before conversion: glucose mmol/L, insulin pmol/L, TG mmol/L,
  HDL mmol/L, FFA mmol/L, waist cm, BMI kg/m^2. If different, convert
  first.
- Optional but helpful: `sex` (1/2 or labels) to understand which
  VAI/LAP variant applies.

## Load packages and data

Use a small subset of the simulated data. Swap `sim_small` for your own
data.

``` r
library(HealthMarkers)
library(dplyr)

sim_path <- system.file("extdata", "simulated_hm_data.rds", package = "HealthMarkers")
sim <- readRDS(sim_path)
sim_small <- dplyr::slice_head(sim, n = 30)
```

## Column map

Map required inputs; left-hand keys are fixed.

``` r
col_map <- list(
  G0 = "G0",
  I0 = "I0",
  TG = "TG",
  HDL_c = "HDL_c",
  FFA = "FFA",
  waist = "waist",
  bmi = "BMI"
)
```

## Walkthrough (compute and read results)

[`adipo_is()`](https://sufyansuleman.github.io/HealthMarkers/reference/adipo_is.md)
returns a tibble of **only the computed index columns** — not the
original data. Bind back to your source rows with
[`dplyr::bind_cols()`](https://dplyr.tidyverse.org/reference/bind_cols.html)
if needed.

``` r
ais <- adipo_is(
  data = sim_small,
  col_map = col_map,
  na_action = "keep",
  check_extreme = FALSE,
  normalize = "none"
)

# Group 1 — non-sex-specific indices
ais %>%
  slice_head(n = 5) %>%
  select(Revised_QUICKI, McAuley_index, Adipo_inv, Belfiore_inv_FFA, TyG_inv)
#> # A tibble: 5 × 5
#>   Revised_QUICKI McAuley_index Adipo_inv Belfiore_inv_FFA TyG_inv
#>            <dbl>         <dbl>     <dbl>            <dbl>   <dbl>
#> 1          0.340          6.10     -9.65           -0.188   -8.26
#> 2          0.398          5.53     -3.14           -0.484   -9.05
#> 3          0.335          6.00    -10.3            -0.177   -8.55
#> 4          0.377          6.41     -5.10           -0.328   -8.75
#> 5          0.318          5.41    -12.8            -0.145   -8.97

# Group 2 — sex-specific and ratio indices
ais %>%
  slice_head(n = 5) %>%
  select(TG_HDL_C_inv, VAI_Men_inv, VAI_Women_inv, LAP_Men_inv, LAP_Women_inv)
#> # A tibble: 5 × 5
#>   TG_HDL_C_inv VAI_Men_inv VAI_Women_inv LAP_Men_inv LAP_Women_inv
#>          <dbl>       <dbl>         <dbl>       <dbl>         <dbl>
#> 1        -2.45      -1.36          -2.08       -19.5         -26.3
#> 2        -4.26      -2.22          -3.35       -70.5         -83.5
#> 3        -1.83      -0.953         -1.45       -33.2         -41.8
#> 4        -3.65      -2.46          -3.74       -80.5         -91.8
#> 5        -2.32      -1.14          -1.74       -25.2         -36.5
```

Values ending in `_inv` are inverted so **more negative = worse adipose
IR**. Both sex variants (VAI_Men/Women, LAP_Men/Women) are always
returned; pick the appropriate column per subject based on biological
sex:

``` r
# Bind sex back from source, then derive a single VAI and LAP per subject.
# Adjust the if_else condition to match your sex coding (e.g. 1/2, "M"/"F").
ais_with_sex <- dplyr::bind_cols(dplyr::select(sim_small, sex), ais)

ais_with_sex %>%
  dplyr::mutate(
    VAI_inv = dplyr::if_else(sex == 1, VAI_Men_inv, VAI_Women_inv),
    LAP_inv = dplyr::if_else(sex == 1, LAP_Men_inv, LAP_Women_inv)
  ) %>%
  dplyr::select(sex, Revised_QUICKI, TyG_inv, VAI_inv, LAP_inv) %>%
  slice_head(n = 5)
```

## Missing data and extremes

Screen a small slice, cap extremes, and compare row handling.

``` r
demo <- sim_small[1:6, c("G0", "I0", "TG", "HDL_c", "FFA", "waist", "BMI")]
demo$G0[2] <- NA        # missing glucose
demo$TG[3] <- 30        # extreme TG (mmol/L)

ais_keep <- adipo_is(
  data = demo,
  col_map = col_map,
  na_action = "keep",
  check_extreme = TRUE,
  extreme_action = "cap",
  normalize = "none",
  verbose = FALSE
)

ais_omit <- adipo_is(
  data = demo,
  col_map = col_map,
  na_action = "omit",
  check_extreme = TRUE,
  extreme_action = "cap",
  normalize = "none",
  verbose = FALSE
)

list(
  keep_rows = nrow(ais_keep),
  omit_rows = nrow(ais_omit),
  capped_preview = ais_keep %>% select(ends_with("_inv")) %>% slice_head(n = 3)
)
#> $keep_rows
#> [1] 6
#> 
#> $omit_rows
#> [1] 5
#> 
#> $capped_preview
#> # A tibble: 3 × 7
#>   VAI_Men_inv VAI_Women_inv TG_HDL_C_inv TyG_inv LAP_Men_inv LAP_Women_inv
#>         <dbl>         <dbl>        <dbl>   <dbl>       <dbl>         <dbl>
#> 1       -1.36         -2.08        -2.45   -8.26       -19.5         -26.3
#> 2       -2.22         -3.35        -4.26   NA          -70.5         -83.5
#> 3      -15.4         -23.4        -29.5   -11.3       -536.         -676. 
#> # ℹ 1 more variable: Adipo_inv <dbl>
```

## Custom extreme rules

Override the built-in plausibility bounds with `extreme_rules` (values
in original units before any conversion):

``` r
custom_rules <- list(
  G0    = c(2, 25),    # mmol/L — tighter glucose cap
  TG    = c(0.1, 15),  # mmol/L — tighter TG cap
  I0    = c(5, 1500),  # pmol/L
  FFA   = c(0.05, 2),  # mmol/L
  waist = c(50, 200)   # cm
)

ais_custom <- adipo_is(
  data           = demo,
  col_map        = col_map,
  na_action      = "keep",
  check_extreme  = TRUE,
  extreme_action = "cap",
  extreme_rules  = custom_rules,
  normalize      = "none"
)

ais_custom %>%
  select(Revised_QUICKI, TyG_inv, TG_HDL_C_inv) %>%
  slice_head(n = 6)
#> # A tibble: 6 × 3
#>   Revised_QUICKI TyG_inv TG_HDL_C_inv
#>            <dbl>   <dbl>        <dbl>
#> 1          0.340   -8.26        -2.45
#> 2         NA       NA           -4.26
#> 3          0.335  -11.0        -22.2 
#> 4          0.377   -8.75        -3.65
#> 5          0.318   -8.97        -2.32
#> 6          0.339   -9.24        -3.64
```

## Normalise outputs

Use `normalize` to put all indices on a common scale before modelling or
dimension reduction:

``` r
ais_z <- adipo_is(
  data      = sim_small,
  col_map   = col_map,
  na_action = "keep",
  normalize = "z"      # z-score: mean 0, SD 1
)

ais_z %>%
  slice_head(n = 5) %>%
  select(Revised_QUICKI, McAuley_index, TyG_inv, Adipo_inv)
#> # A tibble: 5 × 4
#>   Revised_QUICKI McAuley_index TyG_inv Adipo_inv
#>            <dbl>         <dbl>   <dbl>     <dbl>
#> 1         -0.584         0.573   2.17     -0.842
#> 2          2.03         -0.460  -0.569     1.54 
#> 3         -0.818         0.398   1.18     -1.09 
#> 4          1.06          1.15    0.460     0.820
#> 5         -1.58         -0.686  -0.275    -1.99
```

Available options: `"none"` (default — raw scale), `"z"` (mean 0 / SD
1), `"range"` (0–1 min–max), `"robust"` (median / IQR), `"inverse"`
(1/x).

## Verbose diagnostics

Set `verbose = TRUE` to emit three structured messages per call:

1.  **Preparing inputs** — start-of-function signal.
2.  **Column map** — confirms which data column each required key
    resolved to. Example:
    `adipo_is(): column map: G0 -> 'G0', I0 -> 'I0', TG -> 'TG', ...`
3.  **Results summary** — shows how many rows computed successfully
    (non-NA) per output column. Example:
    `adipo_is(): results: Revised_QUICKI 28/30, VAI_Men_inv 28/30, McAuley_index 27/30, ...`

`verbose = TRUE` emits at the `"inform"` level; you also need
`options(healthmarkers.verbose = "inform")` (or `"debug"`) active in the
session for the messages to print:

``` r
old_opt <- options(healthmarkers.verbose = "inform")

invisible(adipo_is(
  data          = sim_small[1:5, ],
  col_map       = col_map,
  na_action     = "keep",
  check_extreme = TRUE,
  normalize     = "none",
  verbose       = TRUE
))
#> adipo_is(): preparing inputs
#> adipo_is(): column map: G0 -> 'G0', I0 -> 'I0', TG -> 'TG', HDL_c -> 'HDL_c', FFA -> 'FFA', waist -> 'waist', bmi -> 'BMI'
#> adipo_is(): results: Revised_QUICKI 5/5, VAI_Men_inv 5/5, VAI_Women_inv 5/5, TG_HDL_C_inv 5/5, TyG_inv 5/5, LAP_Men_inv 5/5, LAP_Women_inv 5/5, McAuley_index 5/5, Adipo_inv 5/5, Belfiore_inv_FFA 5/5

options(old_opt)  # restore
```

Enable globally for an entire session:
`options(healthmarkers.verbose = "inform")`. Reset with
`options(healthmarkers.verbose = NULL)` or
`options(healthmarkers.verbose = "none")`.

## Expectations

- All seven required mappings must be provided; missing mappings or
  columns abort.
- `na_action` controls row handling: `keep` propagates NA outputs;
  `omit` drops rows with missing required inputs; `error` aborts.
- `check_extreme` screens raw inputs before conversion; `extreme_action`
  can warn, cap, NA, or error.
- Built-in conversions: glucose *18 (mg/dL), insulin /6 (muU/mL), TG*
  88.57 (mg/dL), HDL \*38.67 (mg/dL); FFA/waist/BMI unchanged.

## Common pitfalls

- Non-fasting samples inflate TG/glucose and distort TyG/VAI/LAP.
- Mis-specified units (e.g., mg/dL glucose without converting) will skew
  every index; convert upstream if your lab units differ.
- Missing `sex` does not block computation but only one of VAI/LAP
  applies per subject; interpret accordingly.
- For modelling, use `normalize = "z"` (z-score), `"range"` (0–1), or
  `"robust"` (median/IQR); default `"none"` keeps raw index scales. See
  the *Normalise outputs* section above.

## Validation notes

- **Revised_QUICKI**: typical adults ~0.3–0.6; lower is worse IR.
  Computed from log10(I0 \[mU/L\]) + log10(G0 \[mg/dL\]) + log10(FFA
  \[mmol/L\]).
- **McAuley_index**: typical adults ~3–10; higher is better insulin
  sensitivity. Uses `ln(TG [mmol/L])` and `ln(I0 [mU/L])` — TG is kept
  in mmol/L before the internal mg/dL conversion.
- **TyG_inv**: typically −8 to −12 for adults; more negative = worse IR.
  Computed from TG and G0 both in mg/dL.
- **LAP_Men_inv / LAP_Women_inv**: computed as
  `−(WC−threshold) × TG [mmol/L]` (Kahn 2005). Typical values −10 to
  −80; more negative = worse.
- **VAI_Men_inv / VAI_Women_inv**: uses TG and HDL in mmol/L with the
  Amato 2010 reference constants (1.03, 1.31, 0.81, 1.52). Typical range
  −1 to −5; more negative = worse.
- Spot-check `TG_HDL_C_inv`: should equal `−(TG_mg/dL / HDL_mg/dL)`.
- Use `check_extreme = TRUE` with lab-appropriate `extreme_rules` before
  capping or erroring.

## See also

- [`glycemic_markers()`](https://sufyansuleman.github.io/HealthMarkers/reference/glycemic_markers.md)
  for glucose-centric indices.
- `insulin_secretion()` and `insulin_sensitivity()` vignettes for
  complementary beta-cell metrics.
