# BODE Index

## Scope

Compute the 0-10 BODE score (BMI, Obstruction, Dyspnea, Exercise
capacity) from FEV1 % predicted, 6-minute walk distance, mMRC dyspnea,
and BMI. Accepts a percent-predicted column or derives it from
raw/predicted FEV1. Higher scores indicate worse prognosis.

Component scoring: FEV1% (\>=65 -\> 0, 50-64 -\> 1, 36-49 -\> 2, \<=35
-\> 3); 6MWD meters (\>=350 -\> 0, 250-349 -\> 1, 150-249 -\> 2, \<=149
-\> 3); mMRC (0-1 -\> 0, 2 -\> 1, 3 -\> 2, 4 -\> 3); BMI (\>21 -\> 0,
\<=21 -\> 1).

## When to use

- You have spirometry plus 6MWD, mMRC, and BMI for COPD risk
  stratification.
- You want to derive FEV1% from raw/predicted FEV1 or use an existing
  percent-predicted column.

## Requirements checklist

- Packages: HealthMarkers, dplyr (for display).
- Data columns: choose exactly one FEV1 source: fev1_pct OR (fev1 +
  fev1_pred) OR fev1_pp; plus sixmwd, mmrc, bmi.
- Numeric inputs; non-numeric are coerced with warnings; non-finite
  become NA.
- Row policy via na_action: keep (default), omit, error; warn/ignore
  behave like keep but emit messages.

## Load packages and example data

Replace the example slice with your data frame.

``` r
library(HealthMarkers)
library(dplyr)
#> 
#> Attaching package: 'dplyr'
#> The following objects are masked from 'package:stats':
#> 
#>     filter, lag
#> The following objects are masked from 'package:base':
#> 
#>     intersect, setdiff, setequal, union

sim_path <- system.file("extdata", "simulated_hm_data.rds", package = "HealthMarkers")
sim <- readRDS(sim_path)
sim_small <- dplyr::slice_head(sim, n = 30)
```

## Map columns

Here we use FEV1 percent-predicted from the example data.

``` r
col_map <- list(
  fev1_pct = "FEV1pct",
  sixmwd = "sixmwd",
  mmrc = "mmrc",
  bmi = "BMI"
)
```

## Quick start: compute BODE

Defaults keep rows with missing inputs and return NA for their scores.

``` r
bode_out <- bode_index(
  data = sim_small,
  col_map = col_map,
  na_action = "keep",
  verbose = FALSE
)

new_cols <- setdiff(names(bode_out), names(sim_small))
head(select(bode_out, all_of(new_cols)))
#> # A tibble: 6 × 5
#>   bode_index fev1_score walk_score mmrc_score bmi_score
#>        <int>      <int>      <int>      <int>     <int>
#> 1          3          1          2          0         0
#> 2          0          0          0          0         0
#> 3          3          0          0          3         0
#> 4          0          0          0          0         0
#> 5          2          0          2          0         0
#> 6          1          0          0          1         0
```

## Arguments that matter

- col_map: pick one FEV1 source (fev1_pct OR fev1 + fev1_pred OR
  fev1_pp) plus sixmwd, mmrc, bmi; missing required keys error.
- na_action: keep (default), omit (drop rows with any missing required
  inputs), error (abort on missing); warn/ignore act like keep but warn.
- verbose: emit step messages.

## Handling missing inputs

- Non-numeric inputs are coerced; NA introduced are warned. Non-finite
  become NA.
- Missing required inputs yield NA scores when na_action is
  keep/warn/ignore; rows drop with omit; error aborts.

### Compare row policies

``` r
demo <- sim_small
demo$mmrc[c(2, 6)] <- NA

a_keep <- bode_index(demo, col_map, na_action = "keep")
#> bode_index(): reading input 'demo' — 30 rows × 519 variables
#> bode_index(): col_map (6 columns — 4 specified, 2 inferred from data)
#>   fev1_pct          ->  'FEV1pct'
#>   sixmwd            ->  'sixmwd'
#>   mmrc              ->  'mmrc'
#>   bmi               ->  'BMI'
#>   fev1_pp           ->  'FEV1pct'    (inferred)
#>   fev1              ->  'FEV1'    (inferred)
#> bode_index(): computing markers:
#>   bode_index  [0-10 COPD severity score]
#> bode_index(): results: id 30/30, bode_index 28/30, fev1_pct 30/30, fev1_score 30/30, walk_score 30/30, mmrc_score 28/30, bmi_score 30/30
a_omit <- bode_index(demo, col_map, na_action = "omit")
#> bode_index(): reading input 'demo' — 30 rows × 519 variables
#> bode_index(): col_map (6 columns — 4 specified, 2 inferred from data)
#>   fev1_pct          ->  'FEV1pct'
#>   sixmwd            ->  'sixmwd'
#>   mmrc              ->  'mmrc'
#>   bmi               ->  'BMI'
#>   fev1_pp           ->  'FEV1pct'    (inferred)
#>   fev1              ->  'FEV1'    (inferred)
#> bode_index(): computing markers:
#>   bode_index  [0-10 COPD severity score]
#> bode_index(): results: id 28/28, bode_index 28/28, fev1_pct 28/28, fev1_score 28/28, walk_score 28/28, mmrc_score 28/28, bmi_score 28/28

list(
  keep_rows = nrow(a_keep),
  omit_rows = nrow(a_omit),
  sample_scores = head(select(a_keep, bode_index, fev1_score, walk_score, mmrc_score, bmi_score))
)
#> $keep_rows
#> [1] 30
#> 
#> $omit_rows
#> [1] 28
#> 
#> $sample_scores
#> # A tibble: 6 × 5
#>   bode_index fev1_score walk_score mmrc_score bmi_score
#>        <int>      <int>      <int>      <int>     <int>
#> 1          3          1          2          0         0
#> 2         NA          0          0         NA         0
#> 3          3          0          0          3         0
#> 4          0          0          0          0         0
#> 5          2          0          2          0         0
#> 6         NA          0          0         NA         0
```

## Extreme values

Extreme inputs will produce extreme scores. Pre-filter implausible
values before calling.

``` r
demo2 <- sim_small
demo2$sixmwd[5] <- 20   # extreme low walk
demo2$BMI[6] <- 80      # extreme high BMI

head(select(bode_index(demo2, col_map = col_map, na_action = "keep"), bode_index, fev1_score, walk_score, mmrc_score, bmi_score))
#> bode_index(): reading input 'demo2' — 30 rows × 519 variables
#> bode_index(): col_map (6 columns — 4 specified, 2 inferred from data)
#>   fev1_pct          ->  'FEV1pct'
#>   sixmwd            ->  'sixmwd'
#>   mmrc              ->  'mmrc'
#>   bmi               ->  'BMI'
#>   fev1_pp           ->  'FEV1pct'    (inferred)
#>   fev1              ->  'FEV1'    (inferred)
#> bode_index(): computing markers:
#>   bode_index  [0-10 COPD severity score]
#> bode_index(): results: id 30/30, bode_index 30/30, fev1_pct 30/30, fev1_score 30/30, walk_score 30/30, mmrc_score 30/30, bmi_score 30/30
#> # A tibble: 6 × 5
#>   bode_index fev1_score walk_score mmrc_score bmi_score
#>        <int>      <int>      <int>      <int>     <int>
#> 1          3          1          2          0         0
#> 2          0          0          0          0         0
#> 3          3          0          0          3         0
#> 4          0          0          0          0         0
#> 5          3          0          3          0         0
#> 6          1          0          0          1         0
```

## Outputs

- bode_index (0-10)
- Component scores: fev1_score, walk_score, mmrc_score, bmi_score
- fev1_pct (derived or mapped) Rows only drop with na_action = “omit” or
  when na_action = “error” aborts.

## Pitfalls and tips

- Map exactly one FEV1 source; providing multiple percent-predicted
  sources is not allowed.
- Keep units consistent: FEV1 in liters when deriving percent, 6MWD in
  meters, BMI in kg/m^2.
- mMRC must be 0-4; true zeros are valid.
- warn/ignore behave like keep; choose omit or error for strict
  pipelines.

## Validation ideas

- Spot-check: FEV1% = 40, 6MWD = 200, mMRC = 3, BMI = 20 yields scores
  2 + 2 + 2 + 1 = bode_index 7.
- Verify that rows with NA mmrc are retained vs dropped according to
  na_action.
- Confirm percent-predicted derivation: fev1_pp should match 100 \* fev1
  / fev1_pred for a test row.

## Verbose diagnostics

Set `verbose = TRUE` to emit three structured messages per call:

1.  **Preparing inputs** — start-of-function signal.
2.  **Column map** — confirms which data column each required key
    resolved to. Example:
    `bode_index(): column map: fev1_pct -> 'FEV1pct', sixmwd -> 'Walk_m', mmrc -> 'mMRC', bmi -> 'BMI'`
3.  **Results summary** — shows how many rows computed successfully
    (non-NA) per output column. Example:
    `bode_index(): results: bode_index 30/30, ...`

`verbose = TRUE` emits at the `"inform"` level; you also need
`options(healthmarkers.verbose = "inform")` active:

``` r
old_opt <- options(healthmarkers.verbose = "inform")

df_v <- data.frame(
  FEV1pct = c(68, 45, 30),
  Walk_m  = c(400, 280, 140),
  mMRC    = c(1, 2, 3),
  BMI     = c(24, 19, 18)
)
bode_index(
  df_v,
  col_map = list(fev1_pct = "FEV1pct", sixmwd = "Walk_m", mmrc = "mMRC", bmi = "BMI"),
  verbose = TRUE
)
#> bode_index(): reading input 'df_v' — 3 rows × 4 variables
#> bode_index(): col_map (5 columns — 4 specified, 1 inferred from data)
#>   fev1_pct          ->  'FEV1pct'
#>   sixmwd            ->  'Walk_m'
#>   mmrc              ->  'mMRC'
#>   bmi               ->  'BMI'
#>   fev1_pp           ->  'FEV1pct'    (inferred)
#> bode_index(): computing markers:
#>   bode_index  [0-10 COPD severity score]
#> bode_index(): results: bode_index 3/3, fev1_pct 3/3, fev1_score 3/3, walk_score 3/3, mmrc_score 3/3, bmi_score 3/3
#> # A tibble: 3 × 6
#>   bode_index fev1_pct fev1_score walk_score mmrc_score bmi_score
#>        <int>    <dbl>      <int>      <int>      <int>     <int>
#> 1          0       68          0          0          0         0
#> 2          5       45          2          1          1         1
#> 3          9       30          3          3          2         1

options(old_opt)
```

Reset with `options(healthmarkers.verbose = NULL)` or `"none"`.

## See also

- spirometry_markers() for broader spirometry-derived metrics.
- pulmo_markers() for additional pulmonary markers.
