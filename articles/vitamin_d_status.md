# Vitamin D Status

## Scope

[`vitamin_d_status()`](https://sufyansuleman.github.io/HealthMarkers/reference/vitamin_d_status.md)
classifies serum 25(OH)D (assumed ng/mL) into Deficient (\<20),
Insufficient (20-29), or Sufficient (\>=30). It validates mappings,
enforces your missing-data policy, and can scan/cap implausible values.

## When to use this

- Standardize vitamin D categorization for dashboards, cohorts, or QC
  summaries.
- Guard ETL steps: abort or omit rows with missing/implausible 25(OH)D
  before downstream models.
- Spot potential unit mix-ups: nmol/L inputs often appear ~2.5x higher
  than ng/mL.

## Required inputs and mapping

- `data`: data frame/tibble containing a 25(OH)D column.
- `col_map`: named list mapping either `vitd` or `vitamin_d` to the
  column name. The column must exist and be coercible to numeric;
  coercion that introduces NAs emits a warning.

## Options that change behavior

- `na_action = c("keep","omit","error","ignore","warn")` (default
  “keep”): policy for missing 25(OH)D. `warn`/`ignore` behave like keep;
  `omit` drops those rows; `error` aborts.
- `check_extreme = FALSE`: turn on plausible-range screening.
- `extreme_action = c("warn","cap","error","ignore","NA")` (default
  “warn”): response to values outside bounds.
- `extreme_rules = NULL`: override default bounds for vitamin D
  (defaults to 0 to 250 ng/mL).
- `verbose = FALSE`: emit progress messages.

## How missingness and extremes are handled

- Missing 25(OH)D follows `na_action`; `omit` shrinks the output;
  `error` aborts.
- Non-numeric inputs are coerced to numeric; any NAs introduced trigger
  a warning.
- With `check_extreme = TRUE`, values outside the bounds are handled per
  `extreme_action`: warn only, cap into range, abort, ignore, or blank
  to NA.
- Negative values and very high medians (suggesting nmol/L) warn when
  extreme scanning is off.

## Outputs

- Tibble with ordered factor `vitamin_d_status` (Deficient \<
  Insufficient \< Sufficient).
- Rows drop only when `na_action = "omit"`; otherwise the output aligns
  with input row count.

## Examples

Quick categorization

``` r
library(HealthMarkers)
library(tibble)

df <- tibble::tibble(VitD = c(12, 25, 36, NA))

vitamin_d_status(
  data = df,
  col_map = list(vitd = "VitD"),
  na_action = "keep"
)
#> # A tibble: 4 × 1
#>   vitamin_d_status
#>   <ord>           
#> 1 Deficient       
#> 2 Insufficient    
#> 3 Sufficient      
#> 4 NA
```

Screen and cap extremes

``` r
df2 <- tibble::tibble(d = c(15, 280, 8, 65, NA))  # 280 is extreme by default (0-250)

vitamin_d_status(
  data = df2,
  col_map = list(vitd = "d"),
  na_action = "warn",
  check_extreme = TRUE,
  extreme_action = "cap",
  verbose = TRUE
)
#> # A tibble: 5 × 1
#>   vitamin_d_status
#>   <ord>           
#> 1 Deficient       
#> 2 Sufficient      
#> 3 Deficient       
#> 4 Sufficient      
#> 5 NA
```

## Verbose diagnostics

``` r
old_opt <- options(healthmarkers.verbose = "inform")
vitamin_d_status(
  data = tibble::tibble(VitD = c(15, 32)),
  col_map = list(vitamin_d = "VitD"),
  verbose = TRUE
)
#> vitamin_d_status(): preparing inputs
#> vitamin_d_status(): column map: vitamin_d -> 'VitD'
#> vitamin_d_status(): results: vitamin_d_status 2/2
#> # A tibble: 2 × 1
#>   vitamin_d_status
#>   <ord>           
#> 1 Deficient       
#> 2 Sufficient
options(old_opt)
```

## Troubleshooting and tips

- If medians are ~150-250 without obvious outliers, verify units (nmol/L
  should be divided by 2.5 to get ng/mL).
- Set `na_action = "omit"` when downstream steps cannot handle NA
  statuses.
- Tighten `extreme_rules` to your assay range, or leave
  `check_extreme = FALSE` when values are already QCed.
- Keep `col_map` named; missing or empty mappings abort with classed
  errors you can assert in tests.
