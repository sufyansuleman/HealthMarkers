# Tracer DXA Insulin Sensitivity

## Scope

Compute tracer/DXA-based insulin sensitivity indices: LIRI_inv,
Lipo_inv, ATIRI_inv (adipose-only mode), and, when OGTT and lipid data
are present, I_AUC, FFA_AUC, tracer_palmitate_SI, tracer_glycerol_SI.
Includes mapping validation, NA/extreme handling, safe division/logs,
and built-in unit conversions for insulin, TG, HDL-c.

## When to use this

- You have tracer infusion rates, DXA fat mass, and optionally
  OGTT/lipid data and want validated insulin sensitivity indices.
- You want explicit NA/extreme handling, safe log/division, and
  automatic mode selection based on available columns.

## What you need (inputs & options)

| Argument     | Purpose / Options                                           | Notes                                |
|--------------|-------------------------------------------------------------|--------------------------------------|
| data         | Data frame/tibble with tracer, DXA, and optional OGTT/lipid | Columns mapped via `col_map`         |
| col_map      | Named list mapping required keys                            | See below for required keys per mode |
| na_action    | Missing-data policy for required inputs                     | “keep” (default), “omit”, “error”    |
| na_warn_prop | Proportion threshold for high-missingness diagnostics       | Default 0.2 (shown in debug/verbose) |
| verbose      | Emit progress and completion summaries                      | Default FALSE                        |

**Adipose-only required keys:** I0, rate_glycerol, rate_palmitate,
fat_mass, weight, HDL_c, bmi.

**Full mode additional required keys:** G0, G30, G120, I30, I120, TG,
FFA.

**Units:** - Glucose: mmol/L (no conversion except for formulas needing
mg/dL) - Insulin: pmol/L (internally /6 to microU/mL) - TG: mmol/L
(×88.57 to mg/dL) - HDL-c: mmol/L (×38.67 to mg/dL) - Tracer rates:
micromol/min - Fat mass, weight: kg; BMI: kg/m²

## Handling and expectations

- Validation & coercion: mapped columns must exist; non-numeric inputs
  are coerced with warnings if NAs are introduced; non-finite become NA.
- Missingness: `keep` propagates NA; `omit` drops rows with any required
  NA; `error` aborts if required inputs contain NA.
- High-missing diagnostics: controlled by `na_warn_prop`; shown when
  `verbose` (or debug) is enabled.
- Safe division/log: all ratios/logs use helpers that set NA for
  zero/invalid denominators or non-positive log arguments, with
  consolidated warnings.
- Mode selection: if all OGTT/lipid keys are present, full mode is used;
  otherwise, adipose-only mode.
- Empty result: if `na_action = "omit"` drops all rows, returns a
  zero-row tibble with expected columns.

## Defaults and validation details

- Safe logs: only positive, finite values are logged; others become NA.
- Consolidated zero-denominator warnings are emitted for all divisions.

## Outputs

- **Adipose-only:** LIRI_inv, Lipo_inv, ATIRI_inv.
- **Full mode:** I_AUC, FFA_AUC, tracer_palmitate_SI,
  tracer_glycerol_SI, LIRI_inv, Lipo_inv, ATIRI_inv.
- NA arises from missing/invalid inputs, zero/invalid denominators, or
  dropped rows under `omit`.

## Worked example 1: Adipose-only mode

``` r
library(HealthMarkers)
library(tibble)

df <- tibble::tibble(
  I0 = c(60, 80, NA),              # pmol/L
  rate_glycerol = c(150, 180, 160), # micromol/min
  rate_palmitate = c(120, 140, 130),
  fat_mass = c(25, 30, 28),        # kg
  weight = c(70, 82, 75),          # kg
  HDL_c = c(1.2, 1.0, 1.1),        # mmol/L
  bmi = c(24, 27, 25)
)

tracer_dxa_is(
  data = df,
  col_map = list(
    I0 = "I0",
    rate_glycerol = "rate_glycerol",
    rate_palmitate = "rate_palmitate",
    fat_mass = "fat_mass",
    weight = "weight",
    HDL_c = "HDL_c",
    bmi = "bmi"
  ),
  na_action = "keep",
  verbose = TRUE
)
#> # A tibble: 3 × 3
#>   LIRI_inv Lipo_inv ATIRI_inv
#>      <dbl>    <dbl>     <dbl>
#> 1    -1.08    -1500    -1200 
#> 2    -1.19    -2400    -1867.
#> 3    NA          NA       NA
```

*Interpretation:* Returns adipose indices; NA propagates where required
inputs are missing or invalid.

## Worked example 2: Full mode, drop incomplete

``` r
df2 <- tibble::tibble(
  G0 = c(5.0, 6.2, 7.5), G30 = c(8.1, 9.5, 10.2), G120 = c(6.7, 8.8, 9.1),
  I0 = c(55, 90, 5100),   I30 = c(220, 260, 5200), I120 = c(150, 200, 5300),
  TG = c(1.1, 1.8, 55), HDL_c = c(1.3, 0.9, 11), FFA = c(0.5, 0.7, 6),
  rate_glycerol = c(140, 200, 12000), rate_palmitate = c(110, 190, 11000),
  fat_mass = c(24, 32, 0.05), weight = c(72, 85, 500), bmi = c(25, 28, 120)
)

# Pre-filter implausible values before calling
df2$I0[df2$I0 > 5000] <- NA
df2$fat_mass[df2$fat_mass < 0.1] <- NA

tracer_dxa_is(
  data = df2,
  col_map = list(
    G0 = "G0", G30 = "G30", G120 = "G120",
    I0 = "I0", I30 = "I30", I120 = "I120",
    TG = "TG", HDL_c = "HDL_c", FFA = "FFA",
    rate_glycerol = "rate_glycerol",
    rate_palmitate = "rate_palmitate",
    fat_mass = "fat_mass",
    weight = "weight",
    bmi = "bmi"
  ),
  na_action = "omit",
  verbose = TRUE
)
#> # A tibble: 2 × 7
#>   I_AUC FFA_AUC tracer_palmitate_SI tracer_glycerol_SI LIRI_inv Lipo_inv
#>   <dbl>   <dbl>               <dbl>              <dbl>    <dbl>    <dbl>
#> 1 3462.      60                4.58               5.83    -1.21   -1283.
#> 2 4325       84                5.94               6.25    -1.35   -3000 
#> # ℹ 1 more variable: ATIRI_inv <dbl>
```

*Interpretation:* Implausible values are set to NA before calling;
incomplete rows are then dropped.

## Troubleshooting & common pitfalls

- Units: ensure all values match expected units; incorrect units can
  silently distort results.
- Missing columns: ensure every required `col_map` key is mapped to an
  existing column.
- Zero/invalid denominators: produce NA outputs and a warning; check for
  zero fat mass, weight, or invalid rates.
- All NA outputs: usually due to missing/invalid inputs, zero
  denominators, or aggressive `na_action = "omit"`.

## Verbose diagnostics

``` r
old_opt <- options(healthmarkers.verbose = "inform")
tracer_dxa_is(
  data = tibble::tibble(
    I0             = 60,
    rate_glycerol  = 150,
    rate_palmitate = 120,
    fat_mass       = 25,
    weight         = 70,
    HDL_c          = 1.2,
    bmi            = 24
  ),
  col_map = list(
    I0             = "I0",
    rate_glycerol  = "rate_glycerol",
    rate_palmitate = "rate_palmitate",
    fat_mass       = "fat_mass",
    weight         = "weight",
    HDL_c          = "HDL_c",
    bmi            = "bmi"
  ),
  verbose = TRUE
)
#> tracer_dxa_is(): reading input 'data' — 1 rows × 7 variables
#> tracer_dxa_is(): preparing inputs
#> tracer_dxa_is(): col_map (7 columns — 7 specified)
#>   I0                ->  'I0'
#>   rate_glycerol     ->  'rate_glycerol'
#>   rate_palmitate    ->  'rate_palmitate'
#>   fat_mass          ->  'fat_mass'
#>   weight            ->  'weight'
#>   HDL_c             ->  'HDL_c'
#>   bmi               ->  'bmi'
#> tracer_dxa_is(): computing markers:
#>   LIRI_inv, Lipo_inv, ATIRI_inv
#> tracer_dxa_is(): adipose-only indices
#> tracer_dxa_is(): results: LIRI_inv 1/1, Lipo_inv 1/1, ATIRI_inv 1/1
#> # A tibble: 1 × 3
#>   LIRI_inv Lipo_inv ATIRI_inv
#>      <dbl>    <dbl>     <dbl>
#> 1    -1.08    -1500     -1200
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

- Choose `na_action = "omit"` for modeling datasets that require
  complete cases; use `keep` for exploratory review.
- Review warnings for zero denominators; these often indicate data entry
  or assay issues.
- Always check that units match the function’s expectations, especially
  for insulin, TG, and HDL-c.

## Validation notes

- LIRI_inv, Lipo_inv, ATIRI_inv: see function docs for formulas; all use
  safe division/logs.
- Full mode: I_AUC is insulin trapezoid (0-30, 30-120 min); FFA_AUC is
  flat over 0-120 min.
- All divisions/logs are safe: zero/invalid denominators or non-positive
  log arguments yield NA and a warning.

## See also

- Function docs:
  [`?tracer_dxa_is`](https://sufyansuleman.github.io/HealthMarkers/reference/tracer_dxa_is.md)
- Related vignettes: fasting_is, ogtt_is, health_markers
