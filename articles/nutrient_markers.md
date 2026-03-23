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
- `check_extreme`: set TRUE to scan inputs; `extreme_action`
  (`warn`/`cap`/`error`/`ignore`/`NA`) controls handling. Defaults are
  broad (e.g., ferritin 0–2000 ng/mL; transferrin_sat 0–100%; albumin
  10–60 g/L; total_protein 40–100 g/L; EPA/DHA 0–20%; Mg 0.2–3 mmol/L;
  creatinine 20–2000 umol/L; glycated_albumin 0–60 g/L; uric_acid
  50–1000 umol/L; BUN 1–150 mg/dL; phosphate 0.1–5 mmol/L; calcium 0.5–4
  mmol/L; Na 100–200 mmol/L; K 2–8 mmol/L; Cl 70–130 mmol/L; HCO3 5–45
  mmol/L; Tyr 10–300 umol/L; Phe 20–300 umol/L).
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
  check_extreme = FALSE,
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

## Extreme scan and cap

``` r
nutrient_markers(
  data = df,
  col_map = NULL,
  na_action = "omit",
  check_extreme = TRUE,
  extreme_action = "cap",
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
#> nutrient_markers(): preparing inputs
#> nutrient_markers(): column map: ferritin -> 'ferritin', transferrin_sat -> 'transferrin_sat', albumin -> 'albumin', total_protein -> 'total_protein', EPA -> 'EPA', DHA -> 'DHA', Mg -> 'Mg', creatinine -> 'creatinine', glycated_albumin -> 'glycated_albumin', uric_acid -> 'uric_acid', BUN -> 'BUN', phosphate -> 'phosphate', calcium -> 'calcium', Na -> 'Na', K -> 'K', Cl -> 'Cl', HCO3 -> 'HCO3', Tyr -> 'Tyr', Phe -> 'Phe'
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

## Tips

- Provide only the inputs you have; missing inputs yield NA for
  dependent markers.
- Omega3Index expects EPA/DHA as percentages; AGR uses globulin =
  total_protein - albumin.
- Tighten `extreme_rules` to your lab ranges before using
  `extreme_action = "cap"` or `"error"`.
- Use `na_action = "omit"` when you prefer row-complete outputs; use
  `keep` during QA to inspect missingness.
