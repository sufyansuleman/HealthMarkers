# Nutrient Markers

## Scope

Compute nutrient-related ratios/products (FerritinTS, AGR, Omega3Index,
Mg_Cr_Ratio, GlycatedAlbuminPct, UA_Cr_Ratio, BUN_Cr_Ratio,
Ca_x_Phosphate, AnionGap, Tyr_Phe_Ratio) with NA/extreme handling and
safe divisions; no unit conversion.

## When to use

- You have routine nutrient labs (iron, protein, omega-3, renal,
  electrolytes, amino acids) and want quick derived ratios.
- You need built-in NA policies, high-missingness diagnostics, and
  optional extreme-value scanning/capping.
- You can supply inputs in expected units and accept that missing inputs
  return NA for dependent markers.

## Inputs

- `data`: data frame/tibble containing any subset of recognized inputs.
- `col_map`: optional named list mapping keys (defaults to identity):
  `ferritin`, `transferrin_sat`, `albumin`, `total_protein`, `EPA`,
  `DHA`, `Mg`, `creatinine`, `glycated_albumin`, `uric_acid`, `BUN`,
  `phosphate`, `calcium`, `Na`, `K`, `Cl`, `HCO3`, `Tyr`, `Phe`.
  Unrecognized keys are ignored. Non-numeric values are coerced with
  warnings; non-finite set to `NA`.
- `na_action`: `keep` (default) propagates NA; `omit` drops rows with NA
  in used inputs; `error` aborts when used inputs contain NA.
- `na_warn_prop`: threshold for high-missingness diagnostics (default
  0.2; shown in verbose/debug).
- `verbose`: optional progress and summary logging.

## Quick start

``` r
library(HealthMarkers)
library(tibble)

df <- tibble::tibble(
  ferritin = c(50, 100), transferrin_sat = c(30, 50),
  albumin = c(45, 40), total_protein = c(70, 75),
  EPA = c(2.0, 2.5), DHA = c(4.0, 4.5),
  Mg = c(0.85, 0.90), creatinine = c(80, 90),
  glycated_albumin = c(12, 14), uric_acid = c(300, 400), BUN = c(14, 16),
  phosphate = c(1.0, 1.2), calcium = c(2.3, 2.4),
  Na = c(140, 138), K = c(4.2, 4.0), Cl = c(100, 102), HCO3 = c(24, 26),
  Tyr = c(60, 70), Phe = c(50, 55)
)

nutrient_markers(
  data = df,
  col_map = NULL,
  na_action = "keep",
  verbose = FALSE
)
#> # A tibble: 2 × 10
#>   FerritinTS   AGR Omega3Index Mg_Cr_Ratio GlycatedAlbuminPct UA_Cr_Ratio
#>        <dbl> <dbl>       <dbl>       <dbl>              <dbl>       <dbl>
#> 1       1.67  1.8            6      0.0106               26.7        3.75
#> 2       2     1.14           7      0.01                 35          4.44
#> # ℹ 4 more variables: BUN_Cr_Ratio <dbl>, Ca_x_Phosphate <dbl>, AnionGap <dbl>,
#> #   Tyr_Phe_Ratio <dbl>
```

## Extreme values

Extreme inputs will produce extreme derived markers. Pre-filter
implausible values before calling.

``` r
# Pre-filter extreme ferritin
df_clean <- df
df_clean$ferritin[df_clean$ferritin > 2000] <- NA

nutrient_markers(
  data = df_clean,
  col_map = NULL,
  na_action = "omit",
  verbose = TRUE
)
#> # A tibble: 2 × 10
#>   FerritinTS   AGR Omega3Index Mg_Cr_Ratio GlycatedAlbuminPct UA_Cr_Ratio
#>        <dbl> <dbl>       <dbl>       <dbl>              <dbl>       <dbl>
#> 1       1.67  1.8            6      0.0106               26.7        3.75
#> 2       2     1.14           7      0.01                 35          4.44
#> # ℹ 4 more variables: BUN_Cr_Ratio <dbl>, Ca_x_Phosphate <dbl>, AnionGap <dbl>,
#> #   Tyr_Phe_Ratio <dbl>
```

## Missing-data policy

``` r
try(
  nutrient_markers(
    data = df,
    col_map = NULL,
    na_action = "error"
  )
)
#> # A tibble: 2 × 10
#>   FerritinTS   AGR Omega3Index Mg_Cr_Ratio GlycatedAlbuminPct UA_Cr_Ratio
#>        <dbl> <dbl>       <dbl>       <dbl>              <dbl>       <dbl>
#> 1       1.67  1.8            6      0.0106               26.7        3.75
#> 2       2     1.14           7      0.01                 35          4.44
#> # ℹ 4 more variables: BUN_Cr_Ratio <dbl>, Ca_x_Phosphate <dbl>, AnionGap <dbl>,
#> #   Tyr_Phe_Ratio <dbl>
```

## Outputs and expectations

- Returns a tibble with: FerritinTS, AGR, Omega3Index, Mg_Cr_Ratio,
  GlycatedAlbuminPct, UA_Cr_Ratio, BUN_Cr_Ratio, Ca_x_Phosphate,
  AnionGap, Tyr_Phe_Ratio.
- Each marker is computed only when its inputs are present; otherwise
  `NA` is returned for that marker.
- Zero/non-finite denominators are set to NA with consolidated warnings;
  no unit harmonization is applied.

## Verbose diagnostics

``` r
old_opt <- options(healthmarkers.verbose = "inform")
nutrient_markers(
  data = df,
  col_map = NULL,
  verbose = TRUE
)
#> nutrient_markers(): reading input 'df' — 2 rows × 19 variables
#> nutrient_markers(): col_map (19 columns — 19 inferred from data)
#>   ferritin          ->  'ferritin'    (inferred)
#>   transferrin_sat   ->  'transferrin_sat'    (inferred)
#>   albumin           ->  'albumin'    (inferred)
#>   total_protein     ->  'total_protein'    (inferred)
#>   EPA               ->  'EPA'    (inferred)
#>   DHA               ->  'DHA'    (inferred)
#>   Mg                ->  'Mg'    (inferred)
#>   creatinine        ->  'creatinine'    (inferred)
#>   glycated_albumin  ->  'glycated_albumin'    (inferred)
#>   uric_acid         ->  'uric_acid'    (inferred)
#>   BUN               ->  'BUN'    (inferred)
#>   phosphate         ->  'phosphate'    (inferred)
#>   calcium           ->  'calcium'    (inferred)
#>   Na                ->  'Na'    (inferred)
#>   K                 ->  'K'    (inferred)
#>   Cl                ->  'Cl'    (inferred)
#>   HCO3              ->  'HCO3'    (inferred)
#>   Tyr               ->  'Tyr'    (inferred)
#>   Phe               ->  'Phe'    (inferred)
#> nutrient_markers(): optional inputs
#>   present:  ferritin, transferrin_sat, albumin, total_protein, EPA, DHA, Mg, creatinine, glycated_albumin, uric_acid, BUN, phosphate, calcium, Na, K, Cl, HCO3, Tyr, Phe
#> nutrient_markers(): computing markers:
#>   FerritinTS           [ferritin, transferrin_sat]
#>   AGR                  [albumin, total_protein]
#>   Omega3Index          [EPA, DHA]
#>   Mg_Cr_Ratio          [Mg, creatinine]
#>   GlycatedAlbuminPct   [glycated_albumin, albumin]
#>   UA_Cr_Ratio          [uric_acid, creatinine]
#>   BUN_Cr_Ratio         [BUN, creatinine]
#>   Ca_x_Phosphate       [calcium, phosphate]
#>   AnionGap             [Na, K, Cl, HCO3]
#>   Tyr_Phe_Ratio        [Tyr, Phe]
#> nutrient_markers(): results: FerritinTS 2/2, AGR 2/2, Omega3Index 2/2, Mg_Cr_Ratio 2/2, GlycatedAlbuminPct 2/2, UA_Cr_Ratio 2/2, BUN_Cr_Ratio 2/2, Ca_x_Phosphate 2/2, AnionGap 2/2, Tyr_Phe_Ratio 2/2
#> # A tibble: 2 × 10
#>   FerritinTS   AGR Omega3Index Mg_Cr_Ratio GlycatedAlbuminPct UA_Cr_Ratio
#>        <dbl> <dbl>       <dbl>       <dbl>              <dbl>       <dbl>
#> 1       1.67  1.8            6      0.0106               26.7        3.75
#> 2       2     1.14           7      0.01                 35          4.44
#> # ℹ 4 more variables: BUN_Cr_Ratio <dbl>, Ca_x_Phosphate <dbl>, AnionGap <dbl>,
#> #   Tyr_Phe_Ratio <dbl>
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

- Provide only the inputs you have; missing inputs yield NA for
  dependent markers.
- Omega3Index expects EPA/DHA as percentages; AGR uses globulin =
  total_protein - albumin.
- Use `na_action = "omit"` when you prefer row-complete outputs; use
  `keep` during QA to inspect missingness.
