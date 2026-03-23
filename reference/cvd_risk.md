# Compute cardiovascular risk or marker by selected model

Dispatch to the appropriate risk or marker function, or run all of them.
Includes basic argument validation and robust fallback to NA rows if
individual calculators fail.

## Usage

``` r
cvd_risk(
  data,
  model = c("ALL", "ASCVD", "QRISK3", "Stroke", "RiskScorescvd", "AIP", "LDL_PN"),
  year = 10,
  ...,
  verbose = FALSE
)
```

## Arguments

- data:

  Data frame required by your chosen model.

- model:

  One of:

  - "ALL"

  - Risk calculators: "ASCVD","QRISK3","Stroke","RiskScorescvd"

  - Lipid markers: "AIP","LDL_PN"

- year:

  Risk horizon (10 or 30) for applicable models; ignored for lipid
  markers.

- ...:

  Forwarded to underlying wrappers (e.g., col_map, na_action).

- verbose:

  Logical; if TRUE, prints progress (legacy; messages now routed via
  hm_inform).

## Value

A tibble.

## Examples

``` r
df <- data.frame(TG = c(150, 200), HDL_c = c(50, 40))
cvd_risk(df, model = "AIP")
#> # A tibble: 2 × 2
#>   model value
#>   <chr> <dbl>
#> 1 AIP   0.477
#> 2 AIP   0.699
```
