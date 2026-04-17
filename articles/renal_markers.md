# Renal Markers

## Scope

Compute renal function, excretion, and injury markers: CKD-EPI 2009
creatinine eGFR (race factor retained), optional cystatin C and combined
eGFR, BUN/Cr ratio, fractional excretion of urea (FE_Urea), plus
pass-through urine injury markers (NGAL, KIM1, NAG, Beta2Micro, IL18,
L_FABP). Includes mapping validation, missingness policies, optional
extreme screening/capping, and zero-denominator warnings. No unit
conversion is performed.

## When to use this

- You need eGFR (creatinine ± cystatin C) and FE_Urea for renal function
  or AKI assessment.
- Your data includes required labs (creatinine, age, sex, race, BUN) and
  optionally cystatin C or urine assays.
- You want clear handling of missingness, extreme-value screening, and
  transparent NA propagation.

## What you need (inputs & options)

| Argument     | Purpose / Options                                                        | Notes                                          |
|--------------|--------------------------------------------------------------------------|------------------------------------------------|
| data         | Data frame/tibble with renal labs                                        | See required columns below                     |
| col_map      | Named list mapping required/optional fields                              | Required keys: creatinine, age, sex, race, BUN |
| na_action    | keep (propagate NA), omit (drop rows), error (abort if required missing) | Default keep                                   |
| na_warn_prop | Proportion to trigger high-missingness warnings (default 0.2)            | Applies to required inputs                     |
| verbose      | TRUE/FALSE for progress and summaries                                    | Default FALSE                                  |

**Required columns (col_map):** creatinine (mg/dL), age (years), sex
(1/0 or “male”/“female”), race (aliases to white/black/other), BUN
(mg/dL).

**Optional columns:** cystatin_C (mg/L), urea_serum (mg/dL),
creatinine_urine (mg/dL), urea_urine (mg/dL), NGAL, KIM1, NAG,
beta2_micro (outputs as Beta2Micro), IL18, L_FABP.

**Units:** No conversion. Serum creatinine/BUN/urea mg/dL; cystatin C
mg/L; urine creatinine/urea mg/dL. FE_Urea requires urea_serum +
creatinine_urine + urea_urine.

## Handling and expectations

- Validation & coercion: required columns must exist; numeric coercion
  applied with warnings if NAs introduced.
- Missingness: keep = propagate NA; omit = drop rows with required NA;
  error = abort if required NA.
- High-missingness diagnostics: `na_warn_prop` governs when debug
  warnings appear (verbose/debug).
- Zero denominators: divisions to NA with a consolidated warning (e.g.,
  BUN/Cr, FE_Urea components).
- Race factor: CKD-EPI 2009 race coefficient retained (race-free not
  applied here).
- Injury markers: NGAL, KIM1, NAG, beta2_micro, IL18, L_FABP are passed
  through as.numeric without transformation or range checks.

## Defaults and validation details

- Sex mapping: accepts 1/0, 1/2, or male/female strings; unmapped values
  become NA with a warning.
- Race mapping: common aliases map to “black” or “white”; everything
  else becomes “other” (no NA).
- Zero denominators: consolidated warning lists which ratios hit zero
  (e.g., BUN_Cr_ratio, FE_Urea components) and how many.
- NA diagnostics: high-missingness messages are emitted at debug/verbose
  levels; set `verbose = TRUE` during QC to see them.
- Empty result: if `na_action = "omit"` removes all rows, returns a
  zero-row tibble with expected columns.

## Outputs

- Returns a tibble (rows follow `na_action`).
- Columns: eGFR_cr, eGFR_cys (if cystatin_C provided), eGFR_combined (if
  Cys provided), BUN_Cr_ratio, FE_Urea (needs urea_serum +
  creatinine_urine + urea_urine), NGAL, KIM1, NAG, Beta2Micro, IL18,
  L_FABP (pass-through if mapped).
- NA where inputs are missing/invalid, denominators zero, or optional
  inputs absent.

## Worked example 1: Creatinine-only, keep NAs

``` r
library(HealthMarkers)
library(tibble)

df <- tibble::tibble(
  Cr = c(1.0, 1.3, NA),
  Age = c(40, 72, 55),
  Sex = c(1, 0, 1),
  Race = c("white", "black", "other"),
  BUN = c(14, 22, 18)
)

renal_markers(
  data = df,
  col_map = list(
    creatinine = "Cr",
    age = "Age",
    sex = "Sex",
    race = "Race",
    BUN = "BUN"
  ),
  na_action = "keep",
  verbose = TRUE
)
#> # A tibble: 3 × 11
#>   eGFR_cr eGFR_cys eGFR_combined BUN_Cr_ratio FE_Urea  NGAL  KIM1   NAG
#>     <dbl>    <dbl>         <dbl>        <dbl>   <dbl> <dbl> <dbl> <dbl>
#> 1    93.7       NA            NA         14        NA    NA    NA    NA
#> 2    46.6       NA            NA         16.9      NA    NA    NA    NA
#> 3    NA         NA            NA         NA        NA    NA    NA    NA
#> # ℹ 3 more variables: Beta2Micro <dbl>, IL18 <dbl>, L_FABP <dbl>
```

*Interpretation:* eGFR_cr and BUN/Cr are returned; the third row yields
NA outputs because creatinine is missing.

## Worked example 2: Cystatin C, FE_Urea, drop incomplete

``` r
df2 <- tibble::tibble(
  Cr = c(0.9, 2.2, 18),
  Age = c(55, 68, 50),
  Sex = c("male", "female", "male"),
  Race = c("white", "other", "black"),
  BUN = c(16, 40, 210),
  Cys = c(1.0, 1.4, 0.6),
  UreaS = c(28, 35, 18),
  CrU = c(120, 95, 80),
  UreaU = c(480, 520, 300),
  NGAL = c(20, 35, 15)
)

renal_markers(
  data = df2,
  col_map = list(
    creatinine = "Cr",
    age = "Age",
    sex = "Sex",
    race = "Race",
    BUN = "BUN",
    cystatin_C = "Cys",
    urea_serum = "UreaS",
    creatinine_urine = "CrU",
    urea_urine = "UreaU",
    NGAL = "NGAL"
  ),
  na_action = "omit",
  verbose = TRUE
)
#> # A tibble: 3 × 11
#>   eGFR_cr eGFR_cys eGFR_combined BUN_Cr_ratio FE_Urea  NGAL  KIM1   NAG
#>     <dbl>    <dbl>         <dbl>        <dbl>   <dbl> <dbl> <dbl> <dbl>
#> 1   95.8      79.3          87.4         17.8    12.9    20    NA    NA
#> 2   21.9      44.9          32.4         18.2    34.4    35    NA    NA
#> 3    3.07    126.           22.4         11.7   375      15    NA    NA
#> # ℹ 3 more variables: Beta2Micro <dbl>, IL18 <dbl>, L_FABP <dbl>
```

*Interpretation:* Rows with required NAs are dropped; eGFR_cys,
eGFR_combined, FE_Urea, and NGAL are returned where inputs exist.

## Troubleshooting & common pitfalls

- Missing columns: ensure required col_map entries exist and mapped
  columns are in data.
- Units: no conversion; convert upstream (mg/dL for creatinine/BUN/urea,
  mg/L for cystatin C).
- Race factor: CKD-EPI 2009 race coefficient retained; interpret
  accordingly.
- Zero denominators: check for zeros in creatinine or
  urea_serum/urea_urine; warnings list counts.
- All NA outputs: often missing required inputs, unmapped sex/race, or
  missing cystatin_C when expecting combined eGFR.

## Verbose diagnostics

Enable verbose output to inspect column mapping, row counts, and result
summaries during QC:

``` r
old_opt <- options(healthmarkers.verbose = "inform")
renal_markers(
  data = tibble::tibble(Cr = 0.9, Age = 45, Sex = "male", Race = "white", BUN = 12),
  col_map = list(creatinine = "Cr", age = "Age", sex = "Sex", race = "Race", BUN = "BUN"),
  verbose = TRUE
)
#> renal_markers(): reading input 'data' — 1 rows × 5 variables
#> renal_markers(): col_map (5 columns — 5 specified)
#>   creatinine        ->  'Cr'
#>   age               ->  'Age'
#>   sex               ->  'Sex'
#>   race              ->  'Race'
#>   BUN               ->  'BUN'
#> renal_markers(): optional inputs
#>   missing:  cystatin_C, urea_serum, creatinine_urine, urea_urine, NGAL, KIM1, NAG, beta2_micro, IL18, L_FABP
#>   indices -> NA:
#>   eGFR_cys -> NA  [missing: cystatin_C]
#>   eGFR_combined -> NA  [missing: cystatin_C]
#>   FE_Urea -> NA  [missing: urea_serum, creatinine_urine, urea_urine]
#> renal_markers(): computing markers:
#>   eGFR_cr        [creatinine, age, sex, race]
#>   eGFR_cys       NA [cystatin_C missing]
#>   eGFR_combined  NA [cystatin_C missing]
#>   BUN_Cr_ratio   [BUN, creatinine]
#>   FE_Urea        NA [urea/urine inputs missing]
#> renal_markers(): results: eGFR_cr 1/1, eGFR_cys 0/1, eGFR_combined 0/1, BUN_Cr_ratio 1/1, FE_Urea 0/1, NGAL 0/1, KIM1 0/1, NAG 0/1, Beta2Micro 0/1, IL18 0/1, L_FABP 0/1
#> # A tibble: 1 × 11
#>   eGFR_cr eGFR_cys eGFR_combined BUN_Cr_ratio FE_Urea  NGAL  KIM1   NAG
#>     <dbl>    <dbl>         <dbl>        <dbl>   <dbl> <dbl> <dbl> <dbl>
#> 1    103.       NA            NA         13.3      NA    NA    NA    NA
#> # ℹ 3 more variables: Beta2Micro <dbl>, IL18 <dbl>, L_FABP <dbl>
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

- Validate units and mappings first; run
  [`summary()`](https://rdrr.io/r/base/summary.html) on key columns to
  catch implausible values.
- Use `na_action = "omit"` for modeling to avoid NA propagation; `keep`
  for descriptive review.
- Set `verbose = TRUE` during QC to see omission counts, extreme
  handling, and zero-denominator diagnostics.
- FE_Urea requires urea_serum, creatinine_urine, and urea_urine;
  otherwise it will be NA.

## Validation notes

- Check: `BUN_Cr_ratio = BUN / creatinine`;
  `FE_Urea = 100 * (urea_urine/urea_serum) / (creatinine_urine/creatinine)`
  when finite.
- eGFR_cr declines with higher creatinine and older age; race factor
  increases eGFR for Black race (CKD-EPI 2009).

## See also

- Related vignettes: metabolic_markers, inflammatory_markers,
  nutrient_markers.
- Function docs: renal_markers() and CKD-EPI equation references.
