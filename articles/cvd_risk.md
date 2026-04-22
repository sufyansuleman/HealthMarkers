# CVD Risk Dispatcher

## Scope

Dispatch to cardiovascular risk calculators and lipid markers via a
single entry point. Supports `model` = `ASCVD`, `QRISK3`, `Stroke`,
`RiskScorescvd`, `AIP`, `LDL_PN`, or `ALL` (runs them sequentially).

## When to use

- You want a single API to compute one or many CVD risks/markers.
- You are comfortable installing optional Suggests packages for the
  calculators you need (PooledCohort, QRISK3, RiskScorescvd).
- You want graceful fallback: `model = "ALL"` returns NA placeholders
  for calculators whose dependencies or inputs fail.

## Inputs and requirements

- `data`: a data.frame/tibble containing the columns required by the
  selected model(s).
- `model`: one of `ASCVD`, `QRISK3`, `Stroke`, `RiskScorescvd`, `AIP`,
  `LDL_PN`, or `ALL`.
- `year`: only used by ASCVD/Stroke (10 or 30 for ASCVD; 10 for Stroke).
- `...`: forwarded to the underlying calculator (e.g., `col_map`,
  `na_action` for lipid markers; backend-specific args otherwise).
- Units: follow each calculator’s expectations (e.g., mg/dL lipids, mmHg
  SBP, BMI kg/m^2).

## Load packages and data

Use a 30-row slice of the simulated data for the examples. We
demonstrate lipid markers (no optional packages required) and guarded
runs for optional calculators.

``` r
library(HealthMarkers)

sim_path <- system.file("extdata", "simulated_hm_data.rds", package = "HealthMarkers")
sim <- readRDS(sim_path)
sim_small <- dplyr::slice_head(sim, n = 30)
```

## Lipid markers (no optional packages)

Compute AIP and LDL particle number (ApoB proxy).

``` r
aip_out <- cvd_risk(
  data = sim_small,
  model = "AIP",
  col_map = list(TG = "TG", HDL_c = "HDL_c"),
  na_action = "keep"
)

ldlpn_out <- cvd_risk(
  data = sim_small,
  model = "LDL_PN",
  col_map = list(ApoB = "ApoB"),
  na_action = "keep"
)

list(
  aip_head = head(aip_out),
  ldlpn_head = head(ldlpn_out)
)
#> $aip_head
#> # A tibble: 6 × 2
#>   model   value
#>   <chr>   <dbl>
#> 1 AIP    0.384 
#> 2 AIP   -0.229 
#> 3 AIP   -0.0710
#> 4 AIP   -0.204 
#> 5 AIP    0.0134
#> 6 AIP   -0.118 
#> 
#> $ldlpn_head
#> # A tibble: 6 × 2
#>   model  value
#>   <chr>  <dbl>
#> 1 LDL_PN  0.89
#> 2 LDL_PN  0.75
#> 3 LDL_PN  1.32
#> 4 LDL_PN  1.31
#> 5 LDL_PN  0.97
#> 6 LDL_PN  0.69
```

## Optional risk calculators

The code below runs only if each required package is available.

``` r
ascvd10 <- cvd_risk(sim_small, model = "ASCVD", year = 10)
stroke10 <- cvd_risk(sim_small, model = "Stroke", year = 10)
head(dplyr::bind_rows(ascvd10, stroke10))
#> # A tibble: 2 × 3
#>   model   year  risk
#>   <chr>  <int> <dbl>
#> 1 ASCVD     10    NA
#> 2 Stroke    10    NA
```

``` r
qrisk10 <- cvd_risk(sim_small, model = "QRISK3")
head(qrisk10)
#> # A tibble: 1 × 3
#>   model   year  risk
#>   <chr>  <int> <dbl>
#> 1 QRISK3    10    NA
```

``` r
rs_out <- cvd_risk(sim_small, model = "RiskScorescvd")
head(rs_out)
#> # A tibble: 1 × 4
#>   model          year  risk value
#>   <chr>         <int> <dbl> <dbl>
#> 1 RiskScorescvd    NA    NA    NA
```

## Outputs

- Each model returns a tibble; `model = "ALL"` row-binds results and
  substitutes `NA` rows for models that fail validation or lack optional
  packages.
- Lipid markers accept `na_action` (keep/omit/error) via `...`; risk
  calculators rely on their backend behavior.

## Verbose diagnostics

Set `verbose = TRUE` to emit three structured messages per call for the
lipid marker functions (`cvd_marker_aip`,
`cvd_marker_ldl_particle_number`):

1.  **Preparing inputs** — start-of-function signal.
2.  **Column map** — confirms which data column each required key
    resolved to. Example:
    `cvd_marker_aip(): column map: TG -> 'TG', HDL_c -> 'HDL_c'`
3.  **Results summary** — shows how many rows computed successfully
    (non-NA). Example: `cvd_marker_aip(): results: AIP 28/30`

For the risk calculator wrappers (ASCVD, QRISK3, Stroke),
`verbose = TRUE` also emits quality-scan summaries (non-finite, high-NA
counts). The
[`cvd_risk()`](https://sufyansuleman.github.io/HealthMarkers/reference/cvd_risk.md)
dispatcher always emits a dispatch message at the debug level.

`verbose = TRUE` emits at the `"inform"` level; you also need
`options(healthmarkers.verbose = "inform")` active:

``` r
old_opt <- options(healthmarkers.verbose = "inform")

invisible(cvd_risk(
  data    = sim_small,
  model   = "AIP",
  col_map = list(TG = "TG", HDL_c = "HDL_c"),
  verbose = TRUE
))
#> cvd_risk: dispatching model 'AIP'
#> cvd_marker_aip(): col_map (user-provided): TG -> 'TG', HDL_c -> 'HDL_c'
#> cvd_marker_aip(): computing markers:
#>   AIP [TG, HDL_c]
#> cvd_marker_aip(): results: AIP 30/30

options(old_opt)
```

Reset with `options(healthmarkers.verbose = NULL)` or `"none"`. \##
Column recognition

Run `hm_col_report(your_data)` to check which analyte columns are
auto-detected before building your `col_map`. See the [Multi-Biobank
Compatibility](https://sufyansuleman.github.io/HealthMarkers/articles/multi_biobank.md)
article for recognised synonyms across major biobanks.

``` r
hm_col_report(your_data)
```

## Tips

- Install optional packages: PooledCohort (ASCVD, Stroke), QRISK3,
  RiskScorescvd. Without them, guarded chunks skip and `model = "ALL"`
  yields `NA` placeholders.
- Ensure required columns match each model’s spec; units follow backend
  expectations (e.g., mg/dL lipids, mmHg SBP, BMI kg/m^2).
- Use `verbose = TRUE` to surface simple quality scans where supported
  (ASCVD/Stroke) and to get dispatch messages.
