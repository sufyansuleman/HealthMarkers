# 

title: “Inflammatory age (iAge proxy)” output: rmarkdown::html_vignette
—

## Scope

Compute a simplified inflammatory age proxy (`iAge`) as a weighted sum
of CRP, IL6, and TNFa. Supports NA policies, high-missingness warnings,
and optional extreme scanning (warn/cap/NA/error). Units assumed: CRP
mg/L; IL6, TNFa pg/mL (no internal conversions).

## Load packages and demo data

Synthetic data with one missing value to show NA handling.

``` r
library(HealthMarkers)
library(tibble)

df <- tibble::tibble(
  CRP  = c(1.2, 3.5, NA),  # mg/L
  IL6  = c(2.0, 4.1, 1.5), # pg/mL
  TNFa = c(1.0, 1.8, 0.9)  # pg/mL
)
```

## Column map (required)

Map each marker to your column names; all three are mandatory.

``` r
col_map <- list(CRP = "CRP", IL6 = "IL6", TNFa = "TNFa")
```

## Core calculation

Default behavior (`na_action = "omit"`/`ignore`/`warn`): NAs are ignored
in the weighted sum (treated as 0 contribution), preserving row count.

``` r
ia_default <- iAge(
  data = df,
  col_map = col_map,
  na_action = "omit",
  check_extreme = FALSE,
  verbose = FALSE
)

ia_default
#> # A tibble: 3 × 1
#>    iAge
#>   <dbl>
#> 1 1.40 
#> 2 3.12 
#> 3 0.801
```

## Keep NA vs drop

`na_action = "keep"` returns NA if any required marker is missing in a
row. `na_action = "error"` stops on missing required inputs.

``` r
ia_keep <- iAge(
  data = df,
  col_map = col_map,
  na_action = "keep",
  check_extreme = FALSE
)

ia_keep
#> # A tibble: 3 × 1
#>    iAge
#>   <dbl>
#> 1  1.40
#> 2  3.12
#> 3 NA
```

## Extreme screening

Scan for out-of-range inputs and cap into allowed bounds.

``` r
df_ext <- df
df_ext$CRP[2] <- 500  # extreme on purpose

ia_cap <- iAge(
  data = df_ext,
  col_map = col_map,
  check_extreme = TRUE,
  extreme_action = "cap",
  verbose = TRUE
)

ia_cap
#> # A tibble: 3 × 1
#>     iAge
#>    <dbl>
#> 1   1.40
#> 2 101.  
#> 3  NA
```

`extreme_action = "warn"` would only warn; `error` aborts; `NA` blanks
flagged values before computing.

## Weights

- Default weights: CRP 0.33, IL6 0.33, TNFa 0.34 (sum to 1). Named
  weights are reordered to CRP/IL6/TNFa.
- For cohorts where one marker is more informative, adjust weights
  (ensure they sum to 1):

``` r
iAge(df, col_map = col_map, weights = c(CRP = 0.5, IL6 = 0.25, TNFa = 0.25))
#> # A tibble: 3 × 1
#>    iAge
#>   <dbl>
#> 1  1.35
#> 2  3.22
#> 3 NA
```

## Expectations

- All three markers must be present and numeric; coercion to numeric
  will error if NAs are introduced.
- `na_action` behaviors:
  - `omit`/`ignore`/`warn`: missing markers contribute 0 to the sum;
    rows preserved.
  - `keep`: any missing required marker yields `NA` for that row.
  - `error`: abort if any required marker is missing/NA.
- Extreme scan uses broad defaults (CRP 0–300, IL6 0–1000, TNFa 0–2000);
  adjust via `extreme_rules` for your lab ranges.
- Output: one-column tibble `iAge`; rows match input unless you subset
  beforehand.

## Verbose diagnostics

Set `verbose = TRUE` (and `healthmarkers.verbose = "inform"`) to surface
three structured messages on each call: preparing inputs, the column
map, and a results summary.

``` r
old_opt <- options(healthmarkers.verbose = "inform")
df_v <- tibble::tibble(CRP = 1.2, IL6 = 2.0, TNFa = 1.0)
iAge(df_v, col_map = list(CRP = "CRP", IL6 = "IL6", TNFa = "TNFa"), verbose = TRUE)
#> iAge(): preparing inputs
#> iAge(): column map: CRP -> 'CRP', IL6 -> 'IL6', TNFa -> 'TNFa'
#> iAge(): results: iAge 1/1
#> # A tibble: 1 × 1
#>    iAge
#>   <dbl>
#> 1  1.40
options(old_opt)
```

## Tips

- Keep units consistent (CRP mg/L; IL6/TNFa pg/mL). Convert before
  calling if needed.
- Prefer `na_action = "keep"` when you want to see which rows are
  incomplete; use `omit` when you want continuity of the score despite
  single missing markers.
- Tighten `extreme_rules` for cleaner QA; use `verbose = TRUE` during
  setup to see mapping and warnings.
- Impute upstream (e.g.,
  [`impute_missing()`](https://sufyansuleman.github.io/HealthMarkers/reference/impute_missing.md))
  if you prefer not to down-weight missing markers to zero.
