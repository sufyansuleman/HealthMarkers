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
  verbose = FALSE
)

al %>% slice_head(n = 5)
#> # A tibble: 5 × 2
#>   id    AllostaticLoad
#>   <chr>          <int>
#> 1 P001               1
#> 2 P002               1
#> 3 P003               0
#> 4 P004               0
#> 5 P005               2
```

Interpretation: `AllostaticLoad` is the count of biomarkers above
thresholds. With \>1 biomarker the rule is `>`; with a single biomarker
it is `>=`.

## Missing data handling

Demonstrate row handling.

``` r
demo <- sim_small[1:8, c("sbp", "dbp", "CRP")]
demo$sbp[3] <- NA

al_keep <- allostatic_load(
  data = demo,
  thresholds = thresholds,
  col_map = col_map,
  na_action = "keep",
  verbose = FALSE
)

al_omit <- allostatic_load(
  data = demo,
  thresholds = thresholds,
  col_map = col_map,
  na_action = "omit",
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
#> 1              1
#> 2              1
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
#> allostatic_load(): reading input 'df_v' — 2 rows × 2 variables
#> allostatic_load(): col_map (2 columns — 2 inferred from data)
#>   SBP               ->  'SBP'    (inferred)
#>   DBP               ->  'DBP'    (inferred)
#> allostatic_load(): computing markers:
#>   AllostaticLoad [SBP, DBP]
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
- Multi-biomarker rule is `>`; single-biomarker rule is `>=`.

## Common pitfalls

- Threshold units mismatched to data units will misflag results; align
  units first.
- Passing only one biomarker changes the rule to `>=`; set thresholds
  accordingly.
- `na_action = keep` leaves missing inputs unflagged (zero); use `omit`
  for stricter handling.

## Validation notes

- Spot-check one biomarker: flag = 1 if value exceeds the threshold (or
  equals when only one biomarker), else 0.
- Sum of per-biomarker flags should equal `AllostaticLoad` for each row.

## Column recognition

Run `hm_col_report(your_data)` to check which biomarker columns are
auto-detected before building your `col_map`. See the [Multi-Biobank
Compatibility](https://sufyansuleman.github.io/HealthMarkers/articles/multi_biobank.md)
article for recognised synonyms.

``` r
hm_col_report(your_data)
```

## See also

- [`adiposity_sds()`](https://sufyansuleman.github.io/HealthMarkers/reference/adiposity_sds.md)
  and
  [`adiposity_sds_strat()`](https://sufyansuleman.github.io/HealthMarkers/reference/adiposity_sds_strat.md)
  for SDS creation.
- `health_markers()` and
  [`health_summary()`](https://sufyansuleman.github.io/HealthMarkers/reference/health_summary.md)
  for broader panels.
