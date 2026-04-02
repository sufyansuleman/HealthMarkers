# Atherogenic Indices

## Scope

Compute atherogenic risk ratios from a lipid panel: - AIP = log10(TG /
HDL_c) - Castelli Risk Index I (CRI_I) = TC / HDL_c - Castelli Risk
Index II (CRI_II) = LDL_c / HDL_c

All ratios are unitless; use consistent lipid units (mg/dL or mmol/L).

## When to use

- You have triglycerides and HDL cholesterol and want AIP.
- You also have total and/or LDL cholesterol and want Castelli indices.
- You need optional extreme-value screening or strict missing-data
  handling.

## Requirements checklist

- Packages: HealthMarkers, dplyr (for display).
- Data frame with lipid columns; required keys: TG and HDL_c. Optional:
  TC and LDL_c.
- Numeric lipids; non-numeric are coerced with warnings; non-finite
  become NA.
- Decide row policy: na_action = keep (default), omit, or error.
- Optional extreme screening: enable check_extreme and choose
  extreme_action.

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

## Map lipid columns

TG and HDL_c are required. TC and LDL_c are optional but needed for
Castelli indices.

``` r
col_map <- list(
  TG = "TG",
  HDL_c = "HDL_c",
  TC = "TC",
  LDL_c = "LDL_c"
)
```

## Quick start: compute indices

Defaults keep rows with missing lipids and return NA for their ratios.

``` r
aip_out <- atherogenic_indices(
  data = sim_small,
  col_map = col_map,
  na_action = "keep",
  check_extreme = FALSE,
  extreme_action = "warn",
  verbose = FALSE
)

new_cols <- setdiff(names(aip_out), names(sim_small))
head(select(aip_out, all_of(new_cols)))
#> # A tibble: 6 × 3
#>       AIP CRI_I CRI_II
#>     <dbl> <dbl>  <dbl>
#> 1 -0.159   2.83   1.94
#> 2  0.175   3.95   1.74
#> 3 -0.0214  3.70   1.92
#> 4  0.310   4.92   4.12
#> 5  0.152   4.09   2.48
#> 6 -0.169   3.97   2.46
```

## Arguments that matter

- col_map: named list for TG, HDL_c (required), and optional TC,
  LDL_c. Missing required keys error.
- na_action: keep (default, rows retained; ratios become NA), omit (drop
  rows with NA in used lipids), error (abort on NA).
- check_extreme: FALSE by default; if TRUE, screen lipids using
  extreme_rules.
- extreme_action: warn (default, no change), cap, error, ignore, or NA.
  Defaults use wide mg/dL ranges: TG 0-10000, HDL_c 0-1000, LDL_c
  0-10000, TC 0-10000.
- verbose: emit step messages.

## Handling missing and zero denominators

- Non-numeric lipids are coerced to numeric; NA introduced are warned.
  Non-finite become NA.
- Zero HDL_c denominators yield NA ratios and a warning; Castelli
  indices use HDL_c as the divisor.
- Row policy: keep vs omit vs error via na_action.

### Compare row policies

``` r
demo <- sim_small
demo$HDL_c[c(2, 5)] <- NA

aip_keep <- atherogenic_indices(demo, col_map, na_action = "keep")
aip_omit <- atherogenic_indices(demo, col_map, na_action = "omit")

list(
  keep_rows = nrow(aip_keep),
  omit_rows = nrow(aip_omit),
  sample_ratios = head(select(aip_keep, AIP, CRI_I, CRI_II))
)
#> $keep_rows
#> [1] 30
#> 
#> $omit_rows
#> [1] 28
#> 
#> $sample_ratios
#> # A tibble: 6 × 3
#>       AIP CRI_I CRI_II
#>     <dbl> <dbl>  <dbl>
#> 1 -0.159   2.83   1.94
#> 2 NA      NA     NA   
#> 3 -0.0214  3.70   1.92
#> 4  0.310   4.92   4.12
#> 5 NA      NA     NA   
#> 6 -0.169   3.97   2.46
```

## Extreme-value screening (optional)

Enable check_extreme to cap, warn, error, ignore, or set NA for
out-of-range lipids.

``` r
demo2 <- sim_small
demo2$TG[5] <- 12000  # extreme TG

aip_screen <- atherogenic_indices(
  data = demo2,
  col_map = col_map,
  na_action = "keep",
  check_extreme = TRUE,
  extreme_action = "cap",
  extreme_rules = list(TG = c(0, 10000), HDL_c = c(0, 1000), LDL_c = c(0, 10000), TC = c(0, 10000)),
  verbose = FALSE
)

head(select(aip_screen, AIP, CRI_I, CRI_II))
#> # A tibble: 6 × 3
#>       AIP CRI_I CRI_II
#>     <dbl> <dbl>  <dbl>
#> 1 -0.159   2.83   1.94
#> 2  0.175   3.95   1.74
#> 3 -0.0214  3.70   1.92
#> 4  0.310   4.92   4.12
#> 5  3.88    4.09   2.48
#> 6 -0.169   3.97   2.46
```

## Outputs

- AIP: log10(TG / HDL_c)
- CRI_I: TC / HDL_c (NA if TC not mapped/present)
- CRI_II: LDL_c / HDL_c (NA if LDL_c not mapped/present) Rows only drop
  when na_action = “omit” or when na_action = “error” encounters
  missing.

## Pitfalls and tips

- Keep lipid units consistent; ratios are unitless but inputs must share
  units.
- Mapping omissions for TG or HDL_c throw errors; missing TC/LDL_c just
  yield NA Castelli indices.
- Zero HDL_c produces NA ratios and a warning; address true zeros before
  computing.
- Tighten extreme_rules if your lab ranges are narrower; use
  extreme_action = “error” for strict QA.
- Use na_action = “error” for pipelines that must fail fast on missing
  lipids.

## Validation ideas

- Spot-check AIP: if TG = 150 and HDL_c = 50, expected AIP = log10(3) ~
  0.477.
- CRI_I should decrease if HDL_c rises with other lipids unchanged;
  similar for CRI_II.
- Confirm that rows with NA in TG/HDL_c are dropped only when na_action
  = “omit”.

## Verbose diagnostics

Set `verbose = TRUE` to emit three structured messages per call:

1.  **Preparing inputs** — start-of-function signal.
2.  **Column map** — confirms which data column each lipid key resolved
    to. Example:
    `atherogenic_indices(): column map: TG -> 'TG', HDL_c -> 'HDL_c', TC -> 'TC', LDL_c -> 'LDL_c'`
3.  **Results summary** — shows how many rows computed successfully
    (non-NA) per output column. Example:
    `atherogenic_indices(): results: AIP 28/30, CRI_I 30/30, CRI_II 30/30, ...`

`verbose = TRUE` emits at the `"inform"` level; you also need
`options(healthmarkers.verbose = "inform")` active:

``` r
old_opt <- options(healthmarkers.verbose = "inform")

df_v <- tibble::tibble(
  TG    = c(150, 200),
  HDL_c = c(50,  40),
  TC    = c(200, 220),
  LDL_c = c(120, 150)
)
atherogenic_indices(
  df_v,
  col_map = list(TG = "TG", HDL_c = "HDL_c", TC = "TC", LDL_c = "LDL_c"),
  verbose = TRUE
)
#> atherogenic_indices(): preparing inputs
#> atherogenic_indices(): column map: TG -> 'TG', HDL_c -> 'HDL_c', TC -> 'TC', LDL_c -> 'LDL_c'
#> atherogenic_indices(): results: AIP 2/2, CRI_I 2/2, CRI_II 2/2
#> # A tibble: 2 × 3
#>     AIP CRI_I CRI_II
#>   <dbl> <dbl>  <dbl>
#> 1 0.477   4     2.4 
#> 2 0.699   5.5   3.75

options(old_opt)
```

Reset with `options(healthmarkers.verbose = NULL)` or `"none"`.

## See also

- health_summary() for combined marker panels.
- glycemic_markers() for glucose and insulin-derived indices.
