# Corrected Calcium

## Scope

Apply the Payne formula to albumin-adjust serum calcium. Works with
conventional (mg/dL Ca, g/dL Alb) or SI (mmol/L Ca, g/L Alb); can
auto-detect units; supports optional extreme screening.

## When to use

- You have total serum calcium and albumin and need albumin-corrected
  calcium.
- Your labs may be in mg/dL+g/dL or mmol/L+g/L, and you want unit-aware
  handling.
- You need row-retention rules for missing inputs.

## Requirements checklist

- Packages: HealthMarkers, dplyr (for display).
- Columns: calcium and albumin; units must be consistent with the chosen
  `units` argument.
- Column map: list(calcium = …, albumin = …). Defaults assume Ca/Alb.
- Units: conventional (mg/dL, g/dL), si (mmol/L, g/L), or auto (infers;
  warns when assuming SI).
- Row policy: na_action = keep (default), omit, error; warn/ignore
  behave like keep but warn.

## Load packages and example data

Simulated data lack Ca/Alb; we generate illustrative valuesreplace with
real labs.

``` r
library(HealthMarkers)
library(dplyr)

sim_path <- system.file("extdata", "simulated_hm_data.rds", package = "HealthMarkers")
sim <- readRDS(sim_path)
sim_small <- dplyr::slice_head(sim, n = 30)

set.seed(123)
# Illustrative labs (mg/dL Ca, g/dL Alb)
sim_small$Ca <- pmax(7.5, pmin(11.5, rnorm(nrow(sim_small), 9.2, 0.6)))
sim_small$Alb <- pmax(2.5, pmin(5.0, rnorm(nrow(sim_small), 4.0, 0.4)))
```

## Map columns

``` r
col_map <- list(calcium = "Ca", albumin = "Alb")
```

## Quick start: compute corrected calcium

Defaults keep rows with missing inputs and do not screen extremes.

``` r
cc_out <- corrected_calcium(
  data = sim_small,
  col_map = col_map,
  units = "conventional",  # use "si" for mmol/L; "auto" to infer
  na_action = "keep"
)

head(cc_out)
#> # A tibble: 6 × 2
#>   id    corrected_calcium
#>   <chr>             <dbl>
#> 1 P001               8.73
#> 2 P002               9.16
#> 3 P003               9.85
#> 4 P004               8.96
#> 5 P005               9.01
#> 6 P006              10.0
```

## Arguments that matter

- col_map: calcium and albumin mappings are required; empty or missing
  mappings abort.
- units: conventional \| si \| auto. Auto infers based on medians and
  warns if assuming SI.
- na_action: keep (rows retained, NA outputs for missing), omit (drop
  rows with missing inputs), error (abort on missing).

## Handling missing and non-numeric inputs

- Non-numeric Ca/Alb are coerced; NA introduced are warned. Non-finite
  become NA.
- Missing inputs yield NA outputs unless na_action = omit/error.

### Compare row policies

``` r
demo <- sim_small[1:8, c("Ca", "Alb")]
demo$Ca[3] <- NA

a_keep <- corrected_calcium(demo, col_map, units = "conventional", na_action = "keep")
a_omit <- corrected_calcium(demo, col_map, units = "conventional", na_action = "omit")

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
#> # A tibble: 6 × 1
#>   corrected_calcium
#>               <dbl>
#> 1              8.73
#> 2              9.16
#> 3             NA   
#> 4              8.96
#> 5              9.01
#> 6             10.0
```

## Extreme values

Implausible Ca or Alb values will propagate through the Payne formula.
Pre-filter before calling.

``` r
demo2 <- demo
demo2$Ca[5] <- 2.0  # implausibly low

# Pre-filter:
demo2$Ca[demo2$Ca < 4] <- NA
head(corrected_calcium(demo2, col_map, units = "conventional", na_action = "keep"))
#> # A tibble: 6 × 1
#>   corrected_calcium
#>               <dbl>
#> 1              8.73
#> 2              9.16
#> 3             NA   
#> 4              8.96
#> 5             NA   
#> 6             10.0
```

## Verbose diagnostics

Set `verbose = TRUE` to emit three structured messages per call:

1.  **Preparing inputs** — start-of-function signal.
2.  **Column map** — confirms which data column each required key
    (`calcium`, `albumin`) resolved to. Example:
    `corrected_calcium(): column map: calcium -> 'Ca', albumin -> 'Alb'`
3.  **Results summary** — shows how many rows computed successfully
    (non-NA) per output column. Example:
    `corrected_calcium(): results: corrected_calcium 28/30, ...`

`verbose = TRUE` emits at the `"inform"` level; you also need
`options(healthmarkers.verbose = "inform")` active. Reset with
`options(healthmarkers.verbose = NULL)` or
`options(healthmarkers.verbose = "none")`.

``` r
old_opt <- options(healthmarkers.verbose = "inform")

cc_verbose <- corrected_calcium(
  data    = sim_small,
  col_map = col_map,
  units   = "conventional",
  verbose = TRUE
)
#> corrected_calcium(): reading input 'sim_small' — 30 rows × 521 variables
#> corrected_calcium(): col_map (2 columns — 2 specified)
#>   calcium           ->  'Ca'
#>   albumin           ->  'Alb'
#> corrected_calcium(): computing markers:
#>   corrected_calcium  [Payne formula: Ca + 0.8 * (4.0 - Alb)]
#> corrected_calcium(): results: id 30/30, corrected_calcium 30/30

options(old_opt)   # restore original setting
```

## Outputs

- corrected_calcium (mg/dL when units = conventional; mmol/L when units
  = si or auto-inferred SI)
- Rows drop only with na_action = “omit”; na_action = “error” aborts on
  missing.

## Pitfalls and tips

- Prefer explicit units; auto mode may warn and convert if SI is
  inferred.
- Ensure Ca and Alb units match the chosen mode; mismatched units will
  mis-scale results.
- Downstream consumers expecting mg/dL should keep units =
  “conventional” or convert explicitly.

## Validation

``` r
# Payne formula spot check (conventional): Ca = 9.0, Alb = 3.0 => 9.0 + 0.8*(4-3) = 9.8
v1 <- corrected_calcium(data.frame(Ca = 9.0, Alb = 3.0), col_map, units = "conventional")
stopifnot(abs(v1$corrected_calcium - 9.8) < 1e-10)

# SI round-trip: Ca = 2.25 mmol/L, Alb = 35 g/L
# working: Ca_mg = 2.25*4 = 9.0, Alb_gdl = 35/10 = 3.5
# corrected_mg = 9.0 + 0.8*(4 - 3.5) = 9.4; corrected_mmol = 9.4/4 = 2.35
v2 <- corrected_calcium(data.frame(Ca = 2.25, Alb = 35), col_map, units = "si")
stopifnot(abs(v2$corrected_calcium - 2.35) < 1e-10)

cat("Payne conventional:", v1$corrected_calcium, "mg/dL\n")
#> Payne conventional: 9.8 mg/dL
cat("Payne SI round-trip:", v2$corrected_calcium, "mmol/L\n")
#> Payne SI round-trip: 2.35 mmol/L
```

## See also

- renal_markers() for broader kidney panels.
- health_summary() to combine lab markers.
