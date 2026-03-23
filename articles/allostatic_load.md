# Allostatic Load

## Scope

Compute a composite Allostatic Load (AL) score: each biomarker exceeding
its threshold contributes 1 to the count. With multiple biomarkers the
rule is `>`; with a single biomarker it is `>=`. Returns a tibble with a
single integer column `AllostaticLoad`. Optional extreme screening
applies only to columns whose names contain “sds”.

## When to use this

- You want a simple count of biomarkers exceeding risk cut-points.
- You have a small set of biomarkers with clear thresholds you control.
- You may include pre-computed SDS columns and optionally cap
  implausible SDS values.

## What you need

- Thresholds: named list of scalar numeric cutoffs; names must match
  columns (or be mapped via `col_map`).
- Data: matching biomarker columns; units should align with the
  thresholds you supply.
- Policies: `na_action` (`keep` treats missing as unflagged; `omit`
  drops rows; `error` aborts).
  `check_extreme`/`sds_limit`/`extreme_action` only affect columns whose
  names contain “sds”.
- Optional: `return_summary = TRUE` for counts of flags/omissions;
  `verbose` for progress.

## Load packages and data

Use a small subset of the simulated data. Replace `sim_small` with your
data.

``` r
library(HealthMarkers)
library(dplyr)

sim_path <- system.file("extdata", "simulated_hm_data.rds", package = "HealthMarkers")
sim <- readRDS(sim_path)
sim_small <- dplyr::slice_head(sim, n = 30)
```

## Column map and thresholds

Provide thresholds and, if needed, map them to your column names.

``` r
thresholds <- list(sbp = 130, dbp = 85, CRP = 3)
col_map <- list(sbp = "sbp", dbp = "dbp", CRP = "CRP")
```

## Walkthrough (compute Allostatic Load)

``` r
al <- allostatic_load(
  data = sim_small,
  thresholds = thresholds,
  col_map = col_map,
  na_action = "keep",
  check_extreme = FALSE,
  verbose = FALSE
)

al %>% slice_head(n = 5)
#> # A tibble: 5 × 1
#>   AllostaticLoad
#>            <int>
#> 1              2
#> 2              2
#> 3              1
#> 4              2
#> 5              2
```

Interpretation: `AllostaticLoad` is the count of biomarkers above
thresholds. With \>1 biomarker the rule is `>`; with a single biomarker
it is `>=`.

## Missing data and screening

Demonstrate row handling and SDS screening (only affects columns
containing “sds”).

``` r
demo <- sim_small[1:8, c("sbp", "dbp", "CRP")]
demo$sbp[3] <- NA

demo$CRP_sds <- c(0, 7, -8, 1, NA, 0, 0, 0)  # example SDS column to show screening

al_keep <- allostatic_load(
  data = demo,
  thresholds = thresholds,
  col_map = c(col_map, CRP_sds = "CRP_sds"),
  na_action = "keep",
  check_extreme = TRUE,
  extreme_action = "cap",
  sds_limit = 6,
  verbose = FALSE
)

al_omit <- allostatic_load(
  data = demo,
  thresholds = thresholds,
  col_map = c(col_map, CRP_sds = "CRP_sds"),
  na_action = "omit",
  check_extreme = TRUE,
  extreme_action = "cap",
  sds_limit = 6,
  verbose = FALSE
)

list(
  keep_rows = nrow(al_keep),
  omit_rows = nrow(al_omit),
  head_keep = al_keep %>% slice_head(n = 3)
)
#> $keep_rows
#> [1] 8
#> 
#> $omit_rows
#> [1] 7
#> 
#> $head_keep
#> # A tibble: 3 × 1
#>   AllostaticLoad
#>            <int>
#> 1              2
#> 2              2
#> 3              0
```

## Diagnostics summary with `return_summary`

`return_summary = TRUE` returns `list(data, summary, warnings)` instead
of a bare tibble. The `summary` element contains `rows`, `biomarkers`,
`total_flags`, and `mean_flags`.

``` r
df_rs <- data.frame(
  SBP = c(118, 142, 130, 155, 120),
  DBP = c(76,   92,  85,  98,  78),
  CRP = c(1.2,  4.8, 2.1, 5.0, 0.9)
)
thr_rs <- list(SBP = 130, DBP = 85, CRP = 3)

res <- allostatic_load(
  data      = df_rs,
  thresholds = thr_rs,
  na_action  = "keep",
  return_summary = TRUE,
  verbose    = FALSE
)
res$summary
#> $rows
#> [1] 5
#> 
#> $biomarkers
#> [1] 3
#> 
#> $total_flags
#> [1] 6
#> 
#> $mean_flags
#> [1] 1.2
res$data
#> # A tibble: 5 × 1
#>   AllostaticLoad
#>            <int>
#> 1              0
#> 2              3
#> 3              0
#> 4              3
#> 5              0
```

## Verbose diagnostics

Set `verbose = TRUE` to emit three structured messages per call:

1.  **Preparing inputs** — start-of-function signal.
2.  **Column map** — confirms which data column each threshold key
    resolved to. Example:
    `allostatic_load(): column map: SBP -> 'SBP', DBP -> 'DBP', ...`
3.  **Results summary** — shows how many rows computed successfully
    (non-NA) per output column. Example:
    `allostatic_load(): results: AllostaticLoad 30/30, ...`

`verbose = TRUE` emits at the `"inform"` level; you also need
`options(healthmarkers.verbose = "inform")` active:

``` r
old_opt <- options(healthmarkers.verbose = "inform")

df_v <- data.frame(SBP = c(118, 142), DBP = c(76, 92))
allostatic_load(df_v, thresholds = list(SBP = 130, DBP = 85), verbose = TRUE)
#> allostatic_load(): preparing inputs
#> allostatic_load(): column map: SBP -> 'SBP', DBP -> 'DBP'
#> allostatic_load(): results: AllostaticLoad 2/2
#> # A tibble: 2 × 1
#>   AllostaticLoad
#>            <int>
#> 1              0
#> 2              2

options(old_opt)
```

Reset with `options(healthmarkers.verbose = NULL)` or `"none"`.

## Expectations

- Threshold names must match columns (or be mapped); missing columns
  abort.
- `na_action`: `keep` treats missing as unflagged (flag = 0), `omit`
  drops rows, `error` aborts on missing.
- `check_extreme`/`sds_limit`/`extreme_action` affect only columns whose
  names contain “sds” (case-insensitive).
- Multi-biomarker rule is `>`; single-biomarker rule is `>=`.

## Common pitfalls

- Threshold units mismatched to data units will misflag results; align
  units first.
- Passing only one biomarker changes the rule to `>=`; set thresholds
  accordingly.
- SDS screening does nothing unless column names contain “sds”; rename
  if you expect screening.
- `na_action = keep` leaves missing inputs unflagged (zero); use `omit`
  for stricter handling.

## Validation notes

- Spot-check one biomarker: flag = 1 if value exceeds the threshold (or
  equals when only one biomarker), else 0.
- Sum of per-biomarker flags should equal `AllostaticLoad` for each row.
- If using `check_extreme`, verify capped SDS values are truncated to
  +/- `sds_limit` when `extreme_action = "cap"`.

## See also

- [`adiposity_sds()`](https://sufyansuleman.github.io/HealthMarkers/reference/adiposity_sds.md)
  and
  [`adiposity_sds_strat()`](https://sufyansuleman.github.io/HealthMarkers/reference/adiposity_sds_strat.md)
  for SDS creation.
- `health_markers()` and
  [`health_summary()`](https://sufyansuleman.github.io/HealthMarkers/reference/health_summary.md)
  for broader panels.
