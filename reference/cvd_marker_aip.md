# Atherogenic Index of Plasma (AIP)

Computes log10(TG / HDL_c) with input validation and HM-CS NA handling.

## Usage

``` r
cvd_marker_aip(
  data,
  col_map = NULL,
  na_action = c("keep", "omit", "error"),
  verbose = TRUE
)
```

## Arguments

- data:

  A data frame with numeric columns TG and HDL_c (mg/dL).

- col_map:

  Named list mapping required keys:

  - TG: triglycerides

  - HDL_c: HDL cholesterol

- na_action:

  One of:

  - "keep" (retain rows; AIP is NA where inputs missing/non-finite)

  - "omit" (drop rows with any missing/non-finite inputs)

  - "error" (abort if any required input missing/non-finite)

- verbose:

  Logical; if TRUE, emit hm_inform() progress messages.

## Value

A tibble with columns model = "AIP" and value.

## Details

Note on units: AIP is defined by Dobiasova (2004) with TG and HDL_c in
mmol/L, and its published risk strata assume mmol/L. AIP is NOT
scale-invariant (TG and HDL-c convert with different factors), so mg/dL
inputs shift AIP by about +0.36. This wrapper follows the mg/dL
convention used by the other `cvd_*` calculators; supply mmol/L (and
divide intent accordingly) if you need values comparable to the original
strata. See
[`atherogenic_indices()`](https://sufyansuleman.github.io/HealthMarkers/reference/atherogenic_indices.md)
for the mmol/L implementation.

## References

Dobiášová M (2004). “Atherogenic Index of Plasma (AIP)
log(Triglycerides/HDL-Cholesterol): Theoretical and Practical
Implications.” *Clinical Chemistry*, **50**(7), 1113–1115.
[doi:10.1373/clinchem.2004.033175](https://doi.org/10.1373/clinchem.2004.033175)
.

## Examples

``` r
df <- data.frame(TG = c(150, 200), HDL_c = c(50, 40))
cvd_marker_aip(df)
#> cvd_marker_aip(): col_map (inferred): TG -> 'TG', HDL_c -> 'HDL_c'
#> cvd_marker_aip(): computing markers:
#>   AIP [TG, HDL_c]
#> cvd_marker_aip(): results: AIP 2/2
#> # A tibble: 2 × 2
#>   model value
#>   <chr> <dbl>
#> 1 AIP   0.477
#> 2 AIP   0.699
```
