# Oxidative Markers

## Scope

Compute the glutathione redox ratio (`GSH_GSSG_Ratio`) from reduced
(GSH) and oxidized (GSSG) glutathione, with robust input validation,
missing-data handling, and clear output. No unit conversion is
performed.

## When to use this

- You need a reliable marker of oxidative stress for clinical or
  research data.
- Your dataset includes both reduced (GSH) and oxidized (GSSG)
  glutathione measurements.
- You want to handle missing or non-numeric values gracefully, with
  options to omit, propagate, or error on missing data.

## What you need (detailed inputs)

| Argument  | Purpose/Options                                                       | Details/Notes                               |
|-----------|-----------------------------------------------------------------------|---------------------------------------------|
| data      | Data frame/tibble with GSH and GSSG columns                           | Columns can be any name, mapped via col_map |
| col_map   | Named list: GSH, GSSG → your column names                             | Defaults to identity mapping                |
| units     | No conversion performed; must match (e.g., µmol/L)                    | Mismatched units distort results            |
| na_action | “keep” (propagate NA), “omit” (drop rows), “error” (abort if missing) | Controls missing data handling              |
| verbose   | TRUE/FALSE: print progress and summary                                | Useful for diagnostics                      |

### Example input table

| GSH | GSSG |
|-----|------|
| 5   | 1    |
| 3   | 0.5  |
| NA  | 2    |
| 2   | 0    |

## Handling and expectations

- **Input validation:** Non-numeric columns are coerced; warnings are
  issued if NAs are introduced. Non-finite values become NA.
- **Missing data:**
  - `na_action = "keep"`: Output is NA for rows with missing GSH or
    GSSG.
  - `na_action = "omit"`: Rows with missing GSH or GSSG are dropped.
  - `na_action = "error"`: Function aborts if any required value is
    missing.
- **Division by zero:** If GSSG is zero or non-finite, the ratio is set
  to NA with a warning.
- **Verbose output:** Shows counts of omitted/kept rows and NA ratios
  for troubleshooting.
- **Troubleshooting:** If you see many NAs, check for zero or
  non-numeric GSSG values, or mismatched column names in `col_map`.

## Output

- Returns a tibble with one column: `GSH_GSSG_Ratio`.
- NA values indicate missing or invalid input, or division by zero.

## Worked example 1: Basic usage

``` r
library(HealthMarkers)
library(tibble)

df <- tibble::tibble(GSH = c(5, 3, NA), GSSG = c(1, 0.5, 2))
oxidative_markers(
  data = df,
  col_map = list(GSH = "GSH", GSSG = "GSSG"),
  na_action = "keep"
)
#> # A tibble: 3 × 1
#>   GSH_GSSG_Ratio
#>            <dbl>
#> 1              5
#> 2              6
#> 3             NA
```

*Interpretation:* The third row yields NA because GSH is missing. Ratios
are computed as GSH/GSSG for each row.

## Worked example 2: Omit missing data

``` r
oxidative_markers(
  data = df,
  col_map = list(GSH = "GSH", GSSG = "GSSG"),
  na_action = "omit"
)
#> # A tibble: 2 × 1
#>   GSH_GSSG_Ratio
#>            <dbl>
#> 1              5
#> 2              6
```

*Interpretation:* Only complete rows are returned; the third row (NA
GSH) is dropped.

## Troubleshooting & common pitfalls

- **Unit mismatch:** If GSH and GSSG are in different units, the ratio
  is meaningless. Always check units.
- **Zero or near-zero GSSG:** Division by zero yields NA. Inspect your
  data for implausible values.
- **Non-numeric input:** Non-numeric columns are coerced; check for
  unexpected NAs after coercion.
- **High missingness:** If many NAs are present, check your input data
  and consider using `na_action = "omit"`.
- **Column mapping errors:** If you get a missing column error, check
  your `col_map` and input names.

## Verbose diagnostics

``` r
old_opt <- options(healthmarkers.verbose = "inform")
oxidative_markers(
  data = df,
  col_map = list(GSH = "GSH", GSSG = "GSSG"),
  verbose = TRUE
)
#> oxidative_markers(): reading input 'df' — 3 rows × 2 variables
#> oxidative_markers(): col_map (2 columns — 2 specified)
#>   GSH               ->  'GSH'
#>   GSSG              ->  'GSSG'
#> oxidative_markers(): computing markers:
#>   GSH_GSSG_Ratio  [GSH / GSSG]
#> oxidative_markers(): results: GSH_GSSG_Ratio 2/3
#> # A tibble: 3 × 1
#>   GSH_GSSG_Ratio
#>            <dbl>
#> 1              5
#> 2              6
#> 3             NA
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

## Tips for best results

- Use `na_action = "omit"` for modeling or summary statistics to avoid
  NA propagation.
- Set `verbose = TRUE` to get a summary of omitted/kept rows and NA
  counts.
- Always verify assay units before computing ratios.
- Spot-check a few rows manually to confirm expected output.
- If you see many NAs, check for zero or non-numeric GSSG values.
- For large datasets, run a summary (e.g., `summary(df$GSH)`,
  `summary(df$GSSG)`) before analysis.
- Use
  [`dplyr::filter()`](https://dplyr.tidyverse.org/reference/filter.html)
  to pre-screen for implausible values if needed.

## Validation notes

- Spot-check: For a row, `GSH_GSSG_Ratio = GSH / GSSG`.
- NA output means missing or invalid input, or division by zero.
- Compare summary statistics (mean, SD) to expected ranges for your
  population.
- If output is all NA, check for column mapping errors or all-zero GSSG.

## See also

- Other marker functions in HealthMarkers for related stress and
  metabolic indices: `inflammatory_markers`, `metabolic_markers`,
  `nutrient_markers`.
- Package vignettes for metabolic, inflammatory, and nutrient markers.
- HealthMarkers documentation for advanced usage and troubleshooting.
- **Non-numeric input:** Non-numeric columns are coerced; check for
  unexpected NAs after coercion.
