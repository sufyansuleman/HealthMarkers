# Neurofilament light chain (NfL) marker

## Scope

Incorporate a neurofilament light chain (NfL) concentration measurement
into the analysis pipeline. NfL is a biomarker of neuroaxonal injury;
elevated plasma or CSF levels indicate neurodegeneration and typically
increase with age and neurological disease. This function validates and
passes through the input values (assumed plasma pg/mL) with optional NA
handling, returning a tidy tibble ready for downstream analysis.

## Load packages and demo data

``` r
library(HealthMarkers)
library(tibble)

df <- tibble::tibble(
  NfL = c(8.5, 14.2, 22.1, NA, 35.0)
)
```

## Column map

The only required key is `nfl`.

``` r
cm <- list(nfl = "NfL")
```

## Core calculation

``` r
out <- nfl_marker(
  data = df,
  col_map = cm,
  na_action = "keep",
  verbose = FALSE
)

out
#> # A tibble: 5 × 1
#>   nfl_value
#>       <dbl>
#> 1       8.5
#> 2      14.2
#> 3      22.1
#> 4      NA  
#> 5      35
```

## Missing data handling

`na_action = "keep"` preserves rows with `NA` outputs; `omit` drops rows
with missing required inputs; `error` aborts if any required input is
missing; `warn` keeps rows but emits a warning.

``` r
df_na <- tibble::tibble(NfL = c(8.5, NA, 22.1))

keep_out <- nfl_marker(df_na, cm, na_action = "keep")
omit_out <- nfl_marker(df_na, cm, na_action = "omit")

list(keep_rows = nrow(keep_out), omit_rows = nrow(omit_out))
#> $keep_rows
#> [1] 3
#> 
#> $omit_rows
#> [1] 2
```

## Extreme values

Negative or implausibly high NfL values will be passed through as-is.
Pre-filter before calling.

``` r
df_ext <- tibble::tibble(NfL = c(8.5, -5, 22.1))  # negative value is implausible; pre-filter
# Pre-filter:
df_ext$NfL[df_ext$NfL < 0] <- NA
nfl_marker(df_ext, col_map = cm, na_action = "keep")
#> # A tibble: 3 × 1
#>   nfl_value
#>       <dbl>
#> 1       8.5
#> 2      NA  
#> 3      22.1
```

## Verbose diagnostics

Set `verbose = TRUE` (and `healthmarkers.verbose = "inform"`) to surface
three structured messages on each call: preparing inputs, the column
map, and a results summary.

``` r
old_opt <- options(healthmarkers.verbose = "inform")
nfl_marker(tibble::tibble(NfL = c(8.5, 14.2, 22.1)), cm, verbose = TRUE)
#> nfl_marker(): reading input 'data' — 3 rows × 1 variables
#> nfl_marker(): col_map (1 column — 1 specified)
#>   nfl               ->  'NfL'
#> nfl_marker(): computing markers:
#>   nfl_value  [passthrough of NfL input]
#> nfl_marker(): results: nfl_value 3/3
#> # A tibble: 3 × 1
#>   nfl_value
#>       <dbl>
#> 1       8.5
#> 2      14.2
#> 3      22.1
options(old_opt)
```

## Column recognition

Run `hm_col_report(your_data)` to check which analyte columns are
auto-detected before building your `col_map`. See the [Multi-Biobank
Compatibility](https://sufyansuleman.github.io/HealthMarkers/articles/multi_biobank.md)
article for recognised synonyms across major biobanks.

``` r
hm_col_report(your_data)
```

## Tips

- Units are preserved as-is; ensure all values are in the same unit
  (e.g., pg/mL) before calling.
- Age-adjusted reference ranges are not applied by this function; use
  downstream tools or stratified z-scores for age normalisation.
- For batch processing with imputed datasets, combine with
  [`impute_missing()`](https://sufyansuleman.github.io/HealthMarkers/reference/impute_missing.md)
  upstream to handle systematic missingness before running
  [`nfl_marker()`](https://sufyansuleman.github.io/HealthMarkers/reference/nfl_marker.md).
