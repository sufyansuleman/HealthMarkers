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
- Optional SDS screening: check_extreme on any mapped key containing
  “sds”; control with sds_limit and extreme_action.

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
  na_action = "keep",
  check_extreme = FALSE,
  sds_limit = 6,
  extreme_action = "cap"
)

new_cols <- setdiff(names(bm_out), names(sim_small))
head(select(bm_out, all_of(new_cols)))
#> # A tibble: 6 × 5
#>    OSTA     ALMI      FMI BMD_Tscore   HSA
#>   <dbl>    <dbl>    <dbl>      <dbl> <dbl>
#> 1  1.73 0.000986 0.000862     -0.696    NA
#> 2  2.83 0.000832 0.000728     -0.827    NA
#> 3  8.59 0.000905 0.000806     -0.868    NA
#> 4 11.9  0.00105  0.00104      -0.921    NA
#> 5  8.42 0.000918 0.000803     -0.972    NA
#> 6 13.7  0.000892 0.00101      -0.867    NA
```

## Arguments that matter

- col_map: required mappings for age, weight, height, ALM, FM, BMD,
  BMD_ref_mean, BMD_ref_sd; optional TBS/HSA/PINP/CTX/BSAP/Osteocalcin
  pass through when mapped.
- na_action: keep (default, retain rows; derived scores become NA when
  inputs missing), omit (drop rows with missing/non-finite required
  inputs), error (abort if any missing/non-finite required inputs).
- check_extreme: FALSE by default; TRUE scans mapped keys containing
  “sds” (case-insensitive) for \|value\| \> sds_limit.
- extreme_action (when check_extreme = TRUE): cap, NA, or error.
- sds_limit: threshold for SDS-like screening (default 6).

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
a_omit <- bone_markers(demo, col_map, na_action = "omit")

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
#> 1  1.73  0.000986 0.000862     -0.696
#> 2  2.83 NA        0.000728     -0.827
#> 3  8.59  0.000905 0.000806     -0.868
#> 4 11.9   0.00105  0.00104      -0.921
#> 5  8.42 NA        0.000803     -0.972
#> 6 13.7   0.000892 0.00101      -0.867
```

## Optional SDS screening

If you supply any mapped key containing “sds”, you can cap/NA/error on
extremes.

``` r
demo2 <- demo
# Example SDS-like column for illustration
set.seed(1)
demo2$BMD_z_sds <- rnorm(nrow(demo2), mean = 0, sd = 7)
col_map_sds <- c(col_map, list(BMD_z_sds = "BMD_z_sds"))

bm_screen <- bone_markers(
  data = demo2,
  col_map = col_map_sds,
  na_action = "keep",
  check_extreme = TRUE,
  sds_limit = 6,
  extreme_action = "cap"
)

summary(abs(demo2$BMD_z_sds) > 6)
#>    Mode   FALSE    TRUE 
#> logical       7       1
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
- SDS screening applies only to mapped keys containing “sds”; defaults
  do nothing unless you provide such keys.

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
#> bone_markers(): column map: age -> 'age', weight -> 'weight', height -> 'height', ALM -> 'ALM', FM -> 'FM', BMD -> 'BMD', BMD_ref_mean -> 'BMD_ref_mean', BMD_ref_sd -> 'BMD_ref_sd'
#> bone_markers(): completed
#> bone_markers(): results: OSTA 2/2, ALMI 2/2, FMI 2/2, BMD_Tscore 2/2, TBS 0/2, HSA 0/2, PINP 0/2, CTX 0/2, BSAP 0/2, Osteocalcin 0/2
#> # A tibble: 2 × 10
#>    OSTA  ALMI   FMI BMD_Tscore   TBS   HSA  PINP   CTX  BSAP Osteocalcin
#>   <dbl> <dbl> <dbl>      <dbl> <dbl> <dbl> <dbl> <dbl> <dbl>       <dbl>
#> 1   1    6.69  8.08     -0.417    NA    NA    NA    NA    NA          NA
#> 2  -4.4  5.89  8.21     -1.67     NA    NA    NA    NA    NA          NA

options(old_opt)
```

Reset with `options(healthmarkers.verbose = NULL)` or `"none"`.

## See also

- sarc_f_score() and alm_bmi_index() for sarcopenia-related assessments.
- frax_score() for fracture risk estimation.
