# Calculate SDS

## Scope

Standardize selected variables as SDS (z-scores) using supplied
reference means and SDs. Supports row-retention policies, extreme SDS
handling, and an optional diagnostic list output.

## When to use

- You need z-scores for one or more variables using a chosen reference.
- You want explicit control over missing-row handling and extreme SDS
  treatment.
- You may need a diagnostic list summarizing missing and extreme counts.

## Requirements checklist

- Packages: HealthMarkers, dplyr (for display).
- Data columns: variables listed in vars must exist and be
  numeric/coercible; id_col optional.
- Reference table ref: columns variable, mean, sd; one finite mean and
  positive sd per var.
- Row policy: na_strategy = keep (default), omit, or error.
- Extreme policy: check_extreme + extreme_strategy (cap/warn/error/NA)
  with sds_cap threshold.

## Load packages and example data

Build a small example reference for BMI and systolic BP; replace with
your variables and reference stats.

``` r
library(HealthMarkers)
library(dplyr)

sim_path <- system.file("extdata", "simulated_hm_data.rds", package = "HealthMarkers")
sim <- readRDS(sim_path)
sim_small <- dplyr::slice_head(sim, n = 30) %>%
  dplyr::select(id, BMI, sbp)

vars <- c("BMI", "sbp")
ref <- tibble::tibble(
  variable = vars,
  mean = c(mean(sim_small$BMI, na.rm = TRUE), mean(sim_small$sbp, na.rm = TRUE)),
  sd   = c(sd(sim_small$BMI, na.rm = TRUE),   sd(sim_small$sbp, na.rm = TRUE))
)
```

## Quick start: compute SDS

Defaults keep rows with missing inputs and return NA SDS for those
cells.

``` r
sds_tbl <- calc_sds(
  data = sim_small,
  vars = vars,
  ref = ref,
  id_col = "id",
  na_strategy = "keep",
  check_extreme = TRUE,
  extreme_strategy = "cap",
  sds_cap = 6,
  verbose = FALSE
)

new_cols <- setdiff(names(sds_tbl), names(sim_small))
head(select(sds_tbl, id, all_of(new_cols)))
#> # A tibble: 6 × 3
#>      id BMI_sds sbp_sds
#>   <int>   <dbl>   <dbl>
#> 1     1 -0.971    0.599
#> 2     2  1.64     1.67 
#> 3     3  0.339    1.50 
#> 4     4  0.0267   1.51 
#> 5     5 -0.379    0.577
#> 6     6  1.35     0.757
```

## Arguments that matter

- vars: character vector of columns to standardize; must be present and
  unique.
- ref: table with variable/mean/sd; one row per var; sd must be
  positive.
- na_strategy: keep (retain rows, NA SDS), omit (drop rows with any
  missing vars), error (abort if missing vars).
- check_extreme + extreme_strategy: cap/warn/error/NA for \|SDS\| \>
  sds_cap; cap is applied symmetrically.
- return: data (default tibble) or list (adds summary and warning
  strings).
- id_col: optional ID for diagnostics only.

## Handling missing inputs

- Non-numeric vars are coerced to numeric with warnings; non-finite
  become NA.
- Bypass vs drop vs fail is controlled by na_strategy.

### Compare row policies

``` r
demo <- sim_small[1:8, ]
demo$BMI[3] <- NA

a_keep <- calc_sds(demo, vars, ref, na_strategy = "keep")
a_omit <- calc_sds(demo, vars, ref, na_strategy = "omit")

list(
  keep_rows = nrow(a_keep),
  omit_rows = nrow(a_omit),
  preview = head(select(a_keep, BMI_sds, sbp_sds))
)
#> $keep_rows
#> [1] 8
#> 
#> $omit_rows
#> [1] 7
#> 
#> $preview
#> # A tibble: 6 × 2
#>   BMI_sds sbp_sds
#>     <dbl>   <dbl>
#> 1 -0.971    0.599
#> 2  1.64     1.67 
#> 3 NA        1.50 
#> 4  0.0267   1.51 
#> 5 -0.379    0.577
#> 6  1.35     0.757
```

## Extreme-value handling

Cap, warn, error, or set NA for SDS beyond the chosen limit.

``` r
demo2 <- demo
demo2$sbp[5] <- 500  # extreme SBP

a_cap <- calc_sds(
  data = demo2,
  vars = vars,
  ref = ref,
  na_strategy = "keep",
  check_extreme = TRUE,
  extreme_strategy = "cap",
  sds_cap = 4
)

head(select(a_cap, BMI_sds, sbp_sds))
#> # A tibble: 6 × 2
#>   BMI_sds sbp_sds
#>     <dbl>   <dbl>
#> 1 -0.971    0.599
#> 2  1.64     1.67 
#> 3 NA        1.50 
#> 4  0.0267   1.51 
#> 5 -0.379    4    
#> 6  1.35     0.757
```

## List output for diagnostics

Request per-variable missing/extreme counts and any warnings.

``` r
sds_list <- calc_sds(
  data = sim_small,
  vars = vars,
  ref = ref,
  na_strategy = "keep",
  check_extreme = TRUE,
  extreme_strategy = "warn",
  sds_cap = 5,
  return = "list",
  verbose = FALSE
)

str(sds_list$summary)
#> List of 5
#>  $ rows_in      : int 30
#>  $ rows_out     : int 30
#>  $ omitted_rows : int 0
#>  $ total_extreme: int 0
#>  $ per_var      :'data.frame':   2 obs. of  3 variables:
#>   ..$ variable : chr [1:2] "BMI" "sbp"
#>   ..$ n_missing: int [1:2] 0 0
#>   ..$ n_extreme: int [1:2] 0 0
unique(sds_list$warnings)
#> character(0)
```

## Outputs

- Added `_sds columns (tibble by default)`
- Optional list: data, summary (row counts, omitted rows, extremes,
  per-variable missing/extreme), warnings Rows drop only when
  na_strategy = “omit” or when na_strategy = “error” aborts.

## Pitfalls and tips

- ref must align with vars and use the same units; sd must be positive
  and finite.
- Lower sds_cap to limit leverage of outliers in plots; use
  extreme_strategy = “error” for strict QC.
- Provide id_col to make troubleshooting easier when reviewing
  diagnostics.

## Validation ideas

- Manual check: if mean BMI is 25 and sd is 4, BMI 29 should yield
  (29-25)/4 = 1.0.
- Confirm that rows with missing BMI are retained vs dropped according
  to na_strategy.
- Verify that capping clamps \|SDS\| at sds_cap symmetrically.

## Verbose diagnostics

Set `verbose = TRUE` to emit three structured messages per call:

1.  **Preparing inputs** — start-of-function signal.
2.  **Column map** — lists each variable passed via `vars`. Example:
    `calc_sds(): column map: BMI -> 'BMI', sbp -> 'sbp'`
3.  **Results summary** — shows how many rows computed successfully
    (non-NA) per `_sds` output column. Example:
    `calc_sds(): results: BMI_sds 28/30, sbp_sds 30/30, ...`

`verbose = TRUE` emits at the `"inform"` level; you also need
`options(healthmarkers.verbose = "inform")` active:

``` r
old_opt <- options(healthmarkers.verbose = "inform")

ref_v <- data.frame(variable = c("BMI", "sbp"), mean = c(25, 120), sd = c(4, 15))
df_v  <- data.frame(id = 1:3, BMI = c(24, 29, 30), sbp = c(118, 121, 135))
calc_sds(
  data = df_v,
  vars = c("BMI", "sbp"),
  ref  = ref_v,
  id_col = "id",
  na_strategy = "keep",
  extreme_strategy = "cap",
  sds_cap = 6,
  verbose = TRUE
)
#> calc_sds: starting on 3 row(s), 2 variable(s) (id: id)
#> calc_sds(): column map: BMI -> 'BMI', sbp -> 'sbp'
#> Computing SDS for 2 variable(s)...
#> calc_sds: completed
#> calc_sds(): results: BMI_sds 3/3, sbp_sds 3/3
#> # A tibble: 3 × 5
#>      id   BMI   sbp BMI_sds sbp_sds
#>   <int> <dbl> <dbl>   <dbl>   <dbl>
#> 1     1    24   118   -0.25 -0.133 
#> 2     2    29   121    1     0.0667
#> 3     3    30   135    1.25  1

options(old_opt)
```

Reset with `options(healthmarkers.verbose = NULL)` or `"none"`.

## See also

- infer_cols() to infer data types prior to scoring.
- calc_sds() is used by other helpers; pair with health_summary() for
  broader panels.
