# Atherogenic Index of Plasma (AIP)

Computes log10(TG / HDL_c) with input validation and HM-CS NA handling.

## Usage

``` r
cvd_marker_aip(
  data,
  col_map = list(TG = "TG", HDL_c = "HDL_c"),
  na_action = c("keep", "omit", "error"),
  verbose = FALSE
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

## Examples

``` r
df <- data.frame(TG = c(150, 200), HDL_c = c(50, 40))
cvd_marker_aip(df)
#> # A tibble: 2 × 2
#>   model value
#>   <chr> <dbl>
#> 1 AIP   0.477
#> 2 AIP   0.699
```
