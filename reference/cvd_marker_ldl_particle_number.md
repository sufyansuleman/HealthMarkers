# LDL Particle Number Estimate (via ApoB)

Returns ApoB as a proxy for LDL particle number with HM-CS NA handling.

## Usage

``` r
cvd_marker_ldl_particle_number(
  data,
  col_map = list(ApoB = "ApoB"),
  na_action = c("keep", "omit", "error"),
  verbose = FALSE
)
```

## Arguments

- data:

  A data frame with numeric column ApoB (mg/dL).

- col_map:

  Named list mapping required key:

  - ApoB

- na_action:

  One of:

  - "keep" (retain rows; value is NA where input missing/non-finite)

  - "omit" (drop rows with missing/non-finite input)

  - "error" (abort if input missing/non-finite)

- verbose:

  Logical; if TRUE, emit hm_inform() progress messages.

## Value

A tibble with columns model = "LDL_PN" and value.

## Examples

``` r
df <- data.frame(ApoB = c(80, 120, 100))
cvd_marker_ldl_particle_number(df)
#> # A tibble: 3 × 2
#>   model  value
#>   <chr>  <dbl>
#> 1 LDL_PN    80
#> 2 LDL_PN   120
#> 3 LDL_PN   100
```
