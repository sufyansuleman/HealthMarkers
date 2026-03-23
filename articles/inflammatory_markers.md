# Inflammatory markers

## Scope

Compute inflammatory ratios/panels. Panels: - Classic: NLR, PLR, LMR,
dNLR, SII, SIRI, AISI, CRP_category. - Eosinophil: NLR, PLR, LMR, NER,
SII, SIRI, PIV, CLR, CAR, PCR, mGPS, ESR passthrough. - Both: union of
classic + eos metrics. Inputs are coerced to numeric; non-finite become
`NA`. Zero denominators warn. Optional extreme handling can
warn/cap/blank/error.

## Load packages and demo data

Synthetic counts to run both panels.

``` r
library(HealthMarkers)
library(tibble)

df <- tibble::tibble(
  neutrophils = c(4, 2, 3.5),
  lymphocytes = c(2, 1.0, 1.5),
  monocytes = c(0.5, 0.3, 0.4),
  platelets = c(200, 150, 180),
  WBC = c(7, 4.5, 6),
  CRP = c(2.5, 0.8, 12),
  albumin = c(40, 42, 33),
  eosinophils = c(0.2, 0.1, 0.15),
  ESR = c(12, 15, 18)
)
```

## Column map (required keys per panel)

- Classic: `neutrophils`, `lymphocytes` (monocytes/platelets/WBC/CRP
  improve coverage).
- Eos: classic keys plus `eosinophils`, `CRP`, `albumin` (for
  CLR/CAR/mGPS); `ESR` optional passthrough.

``` r
cm <- list(
  neutrophils = "neutrophils",
  lymphocytes = "lymphocytes",
  monocytes = "monocytes",
  platelets = "platelets",
  WBC = "WBC",
  CRP = "CRP",
  albumin = "albumin",
  eosinophils = "eosinophils",
  ESR = "ESR"
)
```

## Core calculation (classic panel)

``` r
classic_out <- inflammatory_markers(
  data = df,
  col_map = cm,
  panel = "classic",
  na_action = "keep",
  check_extreme = FALSE,
  verbose = FALSE
)

classic_out
#> # A tibble: 3 × 8
#>     NLR   PLR   LMR  dNLR   SII  SIRI  AISI CRP_category
#>   <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <ord>       
#> 1  2      100  4     1.33   400 1       200 moderate    
#> 2  2      150  3.33  0.8    300 0.6      90 low         
#> 3  2.33   120  3.75  1.4    420 0.933   168 high
```

## Eosinophil panel with extreme capping

``` r
df_ext <- df
df_ext$CRP[3] <- 400  # intentional extreme

eos_out <- inflammatory_markers(
  data = df_ext,
  col_map = cm,
  panel = "eos",
  na_action = "keep",
  check_extreme = TRUE,
  extreme_action = "cap",
  verbose = TRUE
)

eos_out
#> # A tibble: 3 × 12
#>     NLR   PLR   LMR   NER   SII  SIRI   PIV    CLR    CAR   PCR  mGPS   ESR
#>   <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl>  <dbl>  <dbl> <dbl> <int> <dbl>
#> 1  2      100  4      4     400 1       200   1.25 0.0625  80       0    12
#> 2  2      150  3.33   2     300 0.6      90   0.8  0.0190 188.      0    15
#> 3  2.33   120  3.75   3.5   420 0.933   168 200    9.09     0.6     2    18
```

`extreme_action = "cap"` trims out-of-range inputs; `warn` only warns;
`error` aborts; `NA` blanks flagged values.

## Missing data handling

`na_action = "keep"` preserves rows with `NA` outputs; `omit` drops rows
with missing required inputs; `error` aborts if any required input is
missing/non-finite.

``` r
df_na <- df
df_na$lymphocytes[2] <- NA

keep_out <- inflammatory_markers(df_na, cm, panel = "classic", na_action = "keep")
omit_out <- inflammatory_markers(df_na, cm, panel = "classic", na_action = "omit")

list(keep_rows = nrow(keep_out), omit_rows = nrow(omit_out))
#> $keep_rows
#> [1] 3
#> 
#> $omit_rows
#> [1] 2
```

## Expectations

- Provide required keys per panel; missing mappings abort.
- Inputs are coerced to numeric; non-finite become `NA`; zero
  denominators warn.
- Extreme scan uses broad defaults for blood counts/CRP/albumin; adjust
  via `extreme_action` and built-in ranges.
- Panel selection: `auto` uses eos if `eosinophils` mapping is present;
  `both` returns the union.

## Verbose diagnostics

Set `verbose = TRUE` (and `healthmarkers.verbose = "inform"`) to surface
three structured messages on each call: preparing inputs, the column
map, and a results summary.

``` r
old_opt <- options(healthmarkers.verbose = "inform")
df_v <- tibble::tibble(neutrophils = 4, lymphocytes = 2, monocytes = 0.5, platelets = 200, CRP = 2.5)
cm_v <- list(neutrophils = "neutrophils", lymphocytes = "lymphocytes",
             monocytes = "monocytes", platelets = "platelets", CRP = "CRP")
inflammatory_markers(df_v, cm_v, panel = "classic", verbose = TRUE)
#> inflammatory_markers(): preparing inputs
#> inflammatory_markers(): column map: neutrophils -> 'neutrophils', lymphocytes -> 'lymphocytes'
#> inflammatory_markers(): results: NLR 1/1, PLR 1/1, LMR 1/1, dNLR 0/1, SII 1/1, SIRI 1/1, AISI 1/1, CRP_category 1/1
#> # A tibble: 1 × 8
#>     NLR   PLR   LMR  dNLR   SII  SIRI  AISI CRP_category
#>   <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <ord>       
#> 1     2   100     4    NA   400     1   200 moderate
options(old_opt)
```

## Tips

- If your lymphocyte/eosinophil counts can be zero, consider
  `na_action = "keep"` to avoid dropping rows; zeros still trigger
  divide-by-zero warnings.
- Tighten ranges when using capping to match lab references.
- If your data use different units (e.g., CRP mg/dL), convert before
  calling to keep ratios interpretable.
