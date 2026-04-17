# Adiposity SDS

## Scope

Non-stratified SDS (z-scores) for anthropometric measures using a single
reference (mean/sd), with NA policies and optional SDS capping.

## When to use this

- You want simple z-scores against one reference (no sex
  stratification).
- You need NA handling and optional capping of implausible raw values or
  extreme SDS magnitudes.
- You plan to standardize BMI/waist/height (or similar) before modeling
  or QC.

## What you need (rich guide)

- Required: a reference list (`ref`) with `mean` and `sd > 0` per
  variable, and matching columns in your data (via `col_map$vars` or
  identity names).
- Units: data units must match the reference (e.g., BMI kg/m^2, waist
  cm, height cm).
- Policies: choose `na_action` (keep/omit/error) and `sds_cap` +
  `extreme_action` to cap large SDS magnitudes.
- Optional: `return_summary = TRUE` to get diagnostics,
  `diagnostics = TRUE` for warnings, `verbose = TRUE` for progress.

## Load packages and data

Use a small slice of the packaged simulated data. Replace `sim_small`
with your data.

``` r
library(HealthMarkers)
library(dplyr)

sim_path <- system.file("extdata", "simulated_hm_data.rds", package = "HealthMarkers")
sim <- readRDS(sim_path)
sim_small <- dplyr::slice_head(sim, n = 30)
```

## Column map

Map each variable to your column names.

``` r
col_map <- list(
  vars = list(
    BMI = "BMI",
    waist = "waist",
    height = "height"
  )
)
```

## Walkthrough (build ref, compute SDS)

[`adiposity_sds()`](https://sufyansuleman.github.io/HealthMarkers/reference/adiposity_sds.md)
returns a tibble of **only the `<var>_SDS` columns** — not the original
data. Bind back with
[`dplyr::bind_cols()`](https://dplyr.tidyverse.org/reference/bind_cols.html)
if needed.

``` r
ref <- list(
  BMI = c(mean = mean(sim_small$BMI, na.rm = TRUE), sd = sd(sim_small$BMI, na.rm = TRUE)),
  waist = c(mean = mean(sim_small$waist, na.rm = TRUE), sd = sd(sim_small$waist, na.rm = TRUE)),
  height = c(mean = mean(sim_small$height, na.rm = TRUE), sd = sd(sim_small$height, na.rm = TRUE))
)

asds <- adiposity_sds(
  data = sim_small,
  col_map = col_map,
  ref = ref,
  na_action = "keep",
  extreme_action = "cap",
  sds_cap = 6,
  verbose = FALSE
)

new_cols <- setdiff(names(asds), names(sim_small))
asds %>% slice_head(n = 5) %>% select(all_of(new_cols))
#> # A tibble: 5 × 3
#>   BMI_SDS waist_SDS height_SDS
#>     <dbl>     <dbl>      <dbl>
#> 1   0.696     0.506      0.625
#> 2   0.279     0.617      1.15 
#> 3  -0.209    -0.935      0.654
#> 4  -0.354    -0.182      1.13 
#> 5  -0.734    -2.05      -1.11
```

Interpretation: each `<var>_SDS` is `(x - mean)/sd`; values near 0 are
average, positive above, negative below the reference.
`extreme_action = "cap"` trims SDS values beyond `sds_cap`.

## Missing data and SDS capping

Compare NA policies and SDS capping.

``` r
demo <- sim_small[1:8, c("BMI", "waist", "height")]
demo$BMI[3] <- NA        # missing

demo_keep <- adiposity_sds(
  data = demo,
  col_map = col_map,
  ref = ref,
  na_action = "keep",
  extreme_action = "cap",
  sds_cap = 5,
  verbose = FALSE
)

demo_omit <- adiposity_sds(
  data = demo,
  col_map = col_map,
  ref = ref,
  na_action = "omit",
  extreme_action = "cap",
  sds_cap = 5,
  verbose = FALSE
)

list(
  keep_rows = nrow(demo_keep),
  omit_rows = nrow(demo_omit),
  capped_preview = demo_keep %>% select(ends_with("_SDS")) %>% slice_head(n = 3)
)
#> $keep_rows
#> [1] 8
#> 
#> $omit_rows
#> [1] 7
#> 
#> $capped_preview
#> # A tibble: 3 × 3
#>   BMI_SDS waist_SDS height_SDS
#>     <dbl>     <dbl>      <dbl>
#> 1   0.696     0.506      0.625
#> 2   0.279     0.617      1.15 
#> 3  NA        -0.935      0.654
```

## Diagnostics summary with `return_summary`

Set `return_summary = TRUE` to get a list with the SDS tibble plus
counts of omitted rows, raw extreme adjustments, and SDS extremes:

``` r
res <- adiposity_sds(
  data           = demo,
  col_map        = col_map,
  ref            = ref,
  na_action      = "omit",
  extreme_action = "cap",
  sds_cap        = 5,
  return_summary = TRUE,
  verbose        = FALSE
)

res$summary
#> $rows_in
#> [1] 8
#> 
#> $rows_out
#> [1] 7
#> 
#> $omitted_rows
#> [1] 1
#> 
#> $total_extreme
#> [1] 0
#> 
#> $per_var
#>   variable n_missing n_extreme
#> 1      BMI         0         0
#> 2    waist         0         0
#> 3   height         0         0
head(res$data)     # SDS tibble
#> # A tibble: 6 × 3
#>   BMI_SDS waist_SDS height_SDS
#>     <dbl>     <dbl>      <dbl>
#> 1   0.696     0.506      0.625
#> 2   0.279     0.617      1.15 
#> 3  -0.354    -0.182      1.13 
#> 4  -0.734    -2.05      -1.11 
#> 5   1.53     -0.972     -1.36 
#> 6  -1.77     -1.12      -0.761
res$warnings       # any diagnostic messages collected
#> [1] "adiposity_sds(): 'BMI' missingness 12.5%"
```

## Verbose diagnostics

Set `verbose = TRUE` to emit three structured messages per call:

1.  **Preparing inputs** — start-of-function signal.
2.  **Column map** — confirms which data column each mapped variable
    resolved to. Example:
    `adiposity_sds(): column map: BMI -> 'BMI', waist -> 'waist', ...`
3.  **Results summary** — shows how many rows computed successfully
    (non-NA) per output column. Example:
    `adiposity_sds(): results: BMI_SDS 28/30, waist_SDS 30/30, ...`

`verbose = TRUE` emits at the `"inform"` level; you also need
`options(healthmarkers.verbose = "inform")` active:

``` r
old_opt <- options(healthmarkers.verbose = "inform")
invisible(adiposity_sds(
  data  = sim_small[1:5, ],
  col_map = col_map,
  ref   = ref,
  na_action = "keep"
))
#> adiposity_sds(): col_map (3 columns — 3 specified)
#>   BMI               ->  'BMI'
#>   waist             ->  'waist'
#>   height            ->  'height'
#> adiposity_sds(): computing markers:
#>   BMI_SDS, waist_SDS, height_SDS
#> adiposity_sds(): results: BMI_SDS 5/5, waist_SDS 5/5, height_SDS 5/5
options(old_opt)
```

Reset with `options(healthmarkers.verbose = NULL)` or `"none"`.

## Expectations

- Each mapped variable must exist; `ref` must provide finite `mean` and
  `sd > 0` for it.
- `na_action` controls row handling: `keep` propagates NA, `omit` drops,
  `error` aborts.
- `sds_cap` + `extreme_action` govern large SDS magnitudes.
- Numeric coercion warns if NAs are introduced; align data units with
  reference units.
- `return_summary = TRUE` returns data plus summaries of omissions and
  extremes.

## Common pitfalls

- Unit mismatches (e.g., inches vs cm) distort SDS; ensure units match
  the reference.
- Using a biased or tiny sample to build `ref` yields unstable SDS; use
  an appropriate reference population.
- Forgetting to map all variables in `col_map$vars` causes
  missing-column errors.

## Validation notes

- SDS should roughly center at 0 with SD near 1 if your sample matches
  the reference.
- Spot-check: `(value - ref_mean)/ref_sd` for one row should equal
  `<var>_SDS`.

## Column recognition

Run `hm_col_report(your_data)` to check which anthropometric columns are
auto-detected before building your `col_map`. See the [Multi-Biobank
Compatibility](https://sufyansuleman.github.io/HealthMarkers/articles/multi_biobank.md)
article for recognised synonyms.

``` r
hm_col_report(your_data)
```

## See also

- [`adiposity_sds_strat()`](https://sufyansuleman.github.io/HealthMarkers/reference/adiposity_sds_strat.md)
  for sex-stratified SDS.
- [`obesity_indices()`](https://sufyansuleman.github.io/HealthMarkers/reference/obesity_indices.md)
  and `obesity_metrics()` for related adiposity measures.
