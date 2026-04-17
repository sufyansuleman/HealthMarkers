# Bone Turnover Markers

## Scope

Compute OSTA, ALMI, FMI, and BMD_Tscore from DXA and anthropometry, and
optionally pass through bone-turnover markers (TBS, HSA, PINP, CTX,
BSAP, Osteocalcin). Inputs are coerced to numeric; non-finite become NA.
BMD_Tscore uses supplied reference mean and SD; SD must be positive.

## When to use

- You have DXA outputs (ALM, FM, BMD) plus age, weight, height, and
  reference BMD stats.
- You want OSTA and mass indexes (ALMI/FMI) alongside BMD T-scores.
- You may have additional bone markers to pass through unchanged.

## Requirements checklist

- Packages: HealthMarkers, dplyr (for display).
- Required columns: age, weight, height (meters), ALM, FM (kg), BMD
  (g/cm^2), BMD_ref_mean, BMD_ref_sd (positive).
- Optional pass-through: TBS, HSA, PINP, CTX, BSAP, Osteocalcin (coerced
  to numeric if mapped and present).
- Row policy via na_action: keep (default), omit, or error on
  missing/non-finite required inputs.

## Load packages and example data

The simulated data lack DXA values; we create simple placeholders for
illustration only; replace with your measurements.

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

# Illustrative stand-ins; replace with real DXA outputs
sim_small$ALM <- pmax(12, pmin(30, sim_small$weight * 0.32))
sim_small$FM  <- pmax(8,  pmin(40, sim_small$weight * 0.28))
sim_small$BMD <- 0.9 + (sim_small$age - mean(sim_small$age, na.rm = TRUE)) * 0.001
sim_small$BMD_ref_mean <- 1.0
sim_small$BMD_ref_sd   <- 0.12
```

## Map columns

Required keys plus optional markers (commented out below).

``` r
col_map <- list(
  age = "age",
  weight = "weight",
  height = "height",
  ALM = "ALM",
  FM = "FM",
  BMD = "BMD",
  BMD_ref_mean = "BMD_ref_mean",
  BMD_ref_sd   = "BMD_ref_sd"
  # Optional: TBS = "TBS", HSA = "HSA", PINP = "PINP", CTX = "CTX", BSAP = "BSAP", Osteocalcin = "Osteocalcin"
)
```

## Quick start: compute markers

Defaults keep rows with missing inputs and return NA for their derived
scores.

``` r
bm_out <- bone_markers(
  data = sim_small,
  col_map = col_map,
  na_action = "keep"
)
#> bone_markers(): reading input 'sim_small' — 30 rows × 519 variables
#> bone_markers(): col_map (13 columns — 8 specified, 5 inferred from data)
#>   age               ->  'age'
#>   weight            ->  'weight'
#>   height            ->  'height'
#>   ALM               ->  'ALM'
#>   FM                ->  'FM'
#>   BMD               ->  'BMD'
#>   BMD_ref_mean      ->  'BMD_ref_mean'
#>   BMD_ref_sd        ->  'BMD_ref_sd'
#>   TBS               ->  'TBS'    (inferred)
#>   PINP              ->  'PINP'    (inferred)
#>   CTX               ->  'CTX'    (inferred)
#>   BSAP              ->  'BSAP'    (inferred)
#>   Osteocalcin       ->  'Osteocalcin'    (inferred)
#> bone_markers(): computing markers:
#>   OSTA       [(weight - age) * 0.2]
#>   ALMI       [ALM / height^2]
#>   FMI        [FM / height^2]
#>   BMD_Tscore [(BMD - ref_mean) / ref_sd]
#> bone_markers(): results: id 30/30, OSTA 30/30, ALMI 30/30, FMI 30/30, BMD_Tscore 30/30, TBS 30/30, HSA 0/30, PINP 30/30, CTX 30/30, BSAP 30/30, Osteocalcin 30/30

new_cols <- setdiff(names(bm_out), names(sim_small))
head(select(bm_out, all_of(new_cols)))
#> # A tibble: 6 × 5
#>    OSTA     ALMI      FMI BMD_Tscore   HSA
#>   <dbl>    <dbl>    <dbl>      <dbl> <dbl>
#> 1  7.56 0.000958 0.000897     -0.799    NA
#> 2 12.1  0.000903 0.000832     -1.00     NA
#> 3  9.4  0.000865 0.000757     -1.00     NA
#> 4  7.74 0.000840 0.000735     -0.919    NA
#> 5 -4.02 0.000772 0.000676     -0.642    NA
#> 6  1.5  0.00117  0.00103      -0.631    NA
```

## Arguments that matter

- col_map: required mappings for age, weight, height, ALM, FM, BMD,
  BMD_ref_mean, BMD_ref_sd; optional TBS/HSA/PINP/CTX/BSAP/Osteocalcin
  pass through when mapped.
- na_action: keep (default, retain rows; derived scores become NA when
  inputs missing), omit (drop rows with missing/non-finite required
  inputs), error (abort if any missing/non-finite required inputs).

## Handling missing inputs

- Non-numeric inputs are coerced; NA introduced are warned. Non-finite
  become NA.
- BMD_ref_sd and height must be positive or the function aborts.
- Row policy: keep vs omit vs error is governed by na_action.

### Compare row policies

``` r
demo <- sim_small[1:8, c("age","weight","height","ALM","FM","BMD","BMD_ref_mean","BMD_ref_sd")]
demo$ALM[c(2, 5)] <- NA

a_keep <- bone_markers(demo, col_map, na_action = "keep")
#> bone_markers(): reading input 'demo' — 8 rows × 8 variables
#> bone_markers(): col_map (8 columns — 8 specified)
#>   age               ->  'age'
#>   weight            ->  'weight'
#>   height            ->  'height'
#>   ALM               ->  'ALM'
#>   FM                ->  'FM'
#>   BMD               ->  'BMD'
#>   BMD_ref_mean      ->  'BMD_ref_mean'
#>   BMD_ref_sd        ->  'BMD_ref_sd'
#> bone_markers(): computing markers:
#>   OSTA       [(weight - age) * 0.2]
#>   ALMI       [ALM / height^2]
#>   FMI        [FM / height^2]
#>   BMD_Tscore [(BMD - ref_mean) / ref_sd]
#> bone_markers(): results: OSTA 8/8, ALMI 6/8, FMI 8/8, BMD_Tscore 8/8, TBS 0/8, HSA 0/8, PINP 0/8, CTX 0/8, BSAP 0/8, Osteocalcin 0/8
a_omit <- bone_markers(demo, col_map, na_action = "omit")
#> bone_markers(): reading input 'demo' — 8 rows × 8 variables
#> bone_markers(): col_map (8 columns — 8 specified)
#>   age               ->  'age'
#>   weight            ->  'weight'
#>   height            ->  'height'
#>   ALM               ->  'ALM'
#>   FM                ->  'FM'
#>   BMD               ->  'BMD'
#>   BMD_ref_mean      ->  'BMD_ref_mean'
#>   BMD_ref_sd        ->  'BMD_ref_sd'
#> bone_markers(): computing markers:
#>   OSTA       [(weight - age) * 0.2]
#>   ALMI       [ALM / height^2]
#>   FMI        [FM / height^2]
#>   BMD_Tscore [(BMD - ref_mean) / ref_sd]
#> bone_markers(): results: OSTA 6/6, ALMI 6/6, FMI 6/6, BMD_Tscore 6/6, TBS 0/6, HSA 0/6, PINP 0/6, CTX 0/6, BSAP 0/6, Osteocalcin 0/6

list(
  keep_rows = nrow(a_keep),
  omit_rows = nrow(a_omit),
  preview = head(select(a_keep, OSTA, ALMI, FMI, BMD_Tscore))
)
#> $keep_rows
#> [1] 8
#> 
#> $omit_rows
#> [1] 6
#> 
#> $preview
#> # A tibble: 6 × 4
#>    OSTA      ALMI      FMI BMD_Tscore
#>   <dbl>     <dbl>    <dbl>      <dbl>
#> 1  7.56  0.000958 0.000897     -0.799
#> 2 12.1  NA        0.000832     -1.00 
#> 3  9.4   0.000865 0.000757     -1.00 
#> 4  7.74  0.000840 0.000735     -0.919
#> 5 -4.02 NA        0.000676     -0.642
#> 6  1.5   0.00117  0.00103      -0.631
```

## Optional marker pass-through

Supply any of the optional keys (`TBS`, `HSA`, `PINP`, `CTX`, `BSAP`,
`Osteocalcin`) in `col_map` to include them in the output; unmapped
optional columns return `NA`.

``` r
demo2 <- demo
demo2$PINP <- rnorm(nrow(demo2), mean = 55, sd = 20)
col_map_opt <- c(col_map, list(PINP = "PINP"))

bm_with_opt <- bone_markers(
  data = demo2,
  col_map = col_map_opt,
  na_action = "keep"
)
#> bone_markers(): reading input 'demo2' — 8 rows × 9 variables
#> bone_markers(): col_map (9 columns — 9 specified)
#>   age               ->  'age'
#>   weight            ->  'weight'
#>   height            ->  'height'
#>   ALM               ->  'ALM'
#>   FM                ->  'FM'
#>   BMD               ->  'BMD'
#>   BMD_ref_mean      ->  'BMD_ref_mean'
#>   BMD_ref_sd        ->  'BMD_ref_sd'
#>   PINP              ->  'PINP'
#> bone_markers(): computing markers:
#>   OSTA       [(weight - age) * 0.2]
#>   ALMI       [ALM / height^2]
#>   FMI        [FM / height^2]
#>   BMD_Tscore [(BMD - ref_mean) / ref_sd]
#> bone_markers(): results: OSTA 8/8, ALMI 6/8, FMI 8/8, BMD_Tscore 8/8, TBS 0/8, HSA 0/8, PINP 8/8, CTX 0/8, BSAP 0/8, Osteocalcin 0/8

head(select(bm_with_opt, OSTA, ALMI, FMI, BMD_Tscore, PINP))
#> # A tibble: 6 × 5
#>    OSTA      ALMI      FMI BMD_Tscore  PINP
#>   <dbl>     <dbl>    <dbl>      <dbl> <dbl>
#> 1  7.56  0.000958 0.000897     -0.799 27.0 
#> 2 12.1  NA        0.000832     -1.00  60.1 
#> 3  9.4   0.000865 0.000757     -1.00   6.25
#> 4  7.74  0.000840 0.000735     -0.919 54.9 
#> 5 -4.02 NA        0.000676     -0.642 67.4 
#> 6  1.5   0.00117  0.00103      -0.631 78.0
```

## Outputs

- OSTA, ALMI, FMI, BMD_Tscore
- Optional passthrough (if mapped and present): TBS, HSA, PINP, CTX,
  BSAP, Osteocalcin Rows drop only with na_action = “omit” or when
  na_action = “error” aborts.

## Pitfalls and tips

- Ensure BMD_ref_sd \> 0 and height \> 0; otherwise the function aborts
  before row filtering.
- Replace placeholder ALM/FM/BMD values with actual DXA measurements;
  keep units consistent (height meters; masses kg; BMD g/cm^2).
- Optional markers are numeric-coerced; if missing or not mapped, they
  return NA columns.

## Validation ideas

- Manual check: OSTA for age 70 and weight 60 is (60 - 70) \* 0.2 = -2.
- ALMI should equal ALM / height^2; FMI equals FM / height^2.
- BMD_Tscore should drop by ~1 when BMD is one SD below reference.

## Verbose diagnostics

Set `verbose = TRUE` to emit three structured messages per call:

1.  **Preparing inputs** — start-of-function signal.
2.  **Column map** — confirms which data column each required key
    resolved to. Example:
    `bone_markers(): column map: age -> 'age', weight -> 'weight', height -> 'height', ...`
3.  **Results summary** — shows how many rows computed successfully
    (non-NA) per output column. Example:
    `bone_markers(): results: T_score 28/30, Z_score 28/30, PINP 29/30, ...`

`verbose = TRUE` emits at the `"inform"` level; you also need
`options(healthmarkers.verbose = "inform")` active:

``` r
old_opt <- options(healthmarkers.verbose = "inform")

df_v <- tibble::tibble(
  age = c(60, 72), weight = c(65, 50), height = c(1.65, 1.58),
  ALM = c(18.2, 14.7), FM = c(22.0, 20.5),
  BMD = c(0.95, 0.80), BMD_ref_mean = c(1.00, 1.00), BMD_ref_sd = c(0.12, 0.12)
)
cm_v <- list(
  age = "age", weight = "weight", height = "height",
  ALM = "ALM", FM = "FM", BMD = "BMD",
  BMD_ref_mean = "BMD_ref_mean", BMD_ref_sd = "BMD_ref_sd"
)
bone_markers(df_v, col_map = cm_v, verbose = TRUE)
#> bone_markers(): reading input 'df_v' — 2 rows × 8 variables
#> bone_markers(): col_map (8 columns — 8 specified)
#>   age               ->  'age'
#>   weight            ->  'weight'
#>   height            ->  'height'
#>   ALM               ->  'ALM'
#>   FM                ->  'FM'
#>   BMD               ->  'BMD'
#>   BMD_ref_mean      ->  'BMD_ref_mean'
#>   BMD_ref_sd        ->  'BMD_ref_sd'
#> bone_markers(): computing markers:
#>   OSTA       [(weight - age) * 0.2]
#>   ALMI       [ALM / height^2]
#>   FMI        [FM / height^2]
#>   BMD_Tscore [(BMD - ref_mean) / ref_sd]
#> bone_markers(): results: OSTA 2/2, ALMI 2/2, FMI 2/2, BMD_Tscore 2/2, TBS 0/2, HSA 0/2, PINP 0/2, CTX 0/2, BSAP 0/2, Osteocalcin 0/2
#> # A tibble: 2 × 10
#>    OSTA  ALMI   FMI BMD_Tscore   TBS   HSA  PINP   CTX  BSAP Osteocalcin
#>   <dbl> <dbl> <dbl>      <dbl> <dbl> <dbl> <dbl> <dbl> <dbl>       <dbl>
#> 1   1    6.69  8.08     -0.417    NA    NA    NA    NA    NA          NA
#> 2  -4.4  5.89  8.21     -1.67     NA    NA    NA    NA    NA          NA

options(old_opt)
```

Reset with `options(healthmarkers.verbose = NULL)` or `"none"`.

## Column recognition

Run `hm_col_report(your_data)` to check which bone/calcium/phosphate
columns are auto-detected before building your `col_map`. See the
[Multi-Biobank
Compatibility](https://sufyansuleman.github.io/HealthMarkers/articles/multi_biobank.md)
article for recognised synonyms.

``` r
hm_col_report(your_data)
```

## See also

- sarc_f_score() and alm_bmi_index() for sarcopenia-related assessments.
- frax_score() for fracture risk estimation.
