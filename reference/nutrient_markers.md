# Compute a Suite of Nutrient-Based Health Markers

Given a data frame or tibble of routine biochemical labs,
`nutrient_markers()` returns a set of widely used ratios, products, and
simple percentages that summarize iron metabolism, protein status,
omega-3 balance, renal excretion, mineral homeostasis, and aromatic
amino-acid patterns.

## Usage

``` r
nutrient_markers(
  data,
  col_map = NULL,
  na_action = c("keep", "omit", "error"),
  na_warn_prop = 0.2,
  check_extreme = FALSE,
  extreme_action = c("warn", "cap", "error", "ignore", "NA"),
  extreme_rules = NULL,
  verbose = FALSE
)
```

## Arguments

- data:

  A data frame or tibble containing subject-level data.

- col_map:

  Optional named list mapping variable keys (see Details) to column
  names in `data`. You only need to supply the keys you have; any
  markers with missing inputs return `NA`. If NULL, defaults to identity
  mapping for all known keys.

- na_action:

  One of c("keep","omit","error") controlling missing-data policy across
  the columns referenced by `col_map`.

  - "keep" (default): keep NA; outputs become NA where inputs are NA.

  - "omit": drop rows with NA in any used input column.

  - "error": abort if any used input contains NA.

- na_warn_prop:

  Numeric in \\\[0,1\]\\; per-variable threshold for high-missingness
  diagnostics on used input columns. Default 0.2.

- check_extreme:

  Logical; if TRUE, scan used input columns for out-of-range values (see
  `extreme_rules`). Default FALSE.

- extreme_action:

  One of c("warn","cap","error","ignore","NA") used when extremes are
  detected (only when `check_extreme = TRUE`). Default "warn".

  - "warn": only warn, "cap": truncate to range and warn, "error":
    abort,

  - "ignore": no-op, "NA": set flagged inputs to NA.

- extreme_rules:

  Optional named list from input keys to c(min,max) ranges. If NULL,
  broad defaults are used (see Details).

- verbose:

  Logical; if TRUE, prints stepwise messages and a final summary via
  hm_inform. Default FALSE.

## Value

A tibble with one row per input row and these columns: FerritinTS, AGR,
Omega3Index, Mg_Cr_Ratio, GlycatedAlbuminPct, UA_Cr_Ratio, BUN_Cr_Ratio,
Ca_x_Phosphate, AnionGap, Tyr_Phe_Ratio.

## Details

Recognized markers (returned as columns):

- FerritinTS: Ferritin / Transferrin saturation

- AGR: Albumin / Globulin, where Globulin = Total protein - Albumin

- Omega3Index: EPA + DHA (percentage points)

- Mg_Cr_Ratio: Magnesium / Creatinine

- GlycatedAlbuminPct: (Glycated albumin / Albumin) x 100

- UA_Cr_Ratio: Uric acid / Creatinine

- BUN_Cr_Ratio: BUN / Creatinine

- Ca_x_Phosphate: Calcium x Phosphate

- AnionGap: (Na + K) - (Cl + HCO3)

- Tyr_Phe_Ratio: Tyrosine / Phenylalanine

Recognized `col_map` keys and expected units (no automatic conversion):

- ferritin: Serum ferritin (ng/mL)

- transferrin_sat: Transferrin saturation (%)

- albumin: Serum albumin (g/L)

- total_protein: Total serum protein (g/L)

- EPA: Red-cell EPA as % of total fatty acids

- DHA: Red-cell DHA as % of total fatty acids

- Mg: Serum magnesium (mmol/L)

- creatinine: Serum creatinine (umol/L)

- glycated_albumin: Glycated albumin (g/L)

- uric_acid: Serum uric acid (umol/L)

- BUN: Blood urea nitrogen (mg/dL)

- phosphate: Serum phosphate (mmol/L)

- calcium: Serum calcium (mmol/L)

- Na: Serum sodium (mmol/L)

- K: Serum potassium (mmol/L)

- Cl: Serum chloride (mmol/L)

- HCO3: Serum bicarbonate (mmol/L)

- Tyr: Serum tyrosine (umol/L)

- Phe: Serum phenylalanine (umol/L)

Default `extreme_rules` (inputs) are broad and intended for unit/entry
checks: ferritin (0, 2000), transferrin_sat (0, 100), albumin (10, 60),
total_protein (40, 100), EPA (0, 20), DHA (0, 20), Mg (0.2, 3),
creatinine (20, 2000), glycated_albumin (0, 60), uric_acid (50, 1000),
BUN (1, 150), phosphate (0.1, 5), calcium (0.5, 4), Na (100, 200), K (2,
8), Cl (70, 130), HCO3 (5, 45), Tyr (10, 300), Phe (20, 300).

## References

Harris WS, von Schacky C (2004). “The Omega-3 Index: a new risk factor
for death from coronary heart disease?” *Preventive Medicine*,
**39**(1), 212–220.
[doi:10.1016/j.ypmed.2004.02.030](https://doi.org/10.1016/j.ypmed.2004.02.030)
. Koga M, Kasayama S (2010). “Clinical impact of glycated albumin as
another glycemic control marker.” *Endocrine Journal*, **57**(9),
751–762.
[doi:10.1507/endocrj.k10e-138](https://doi.org/10.1507/endocrj.k10e-138)
. Block GA, Hulbert-Shearon TE, Levin NW, Port FK (1998). “Association
of serum phosphorus and calcium-phosphate product with mortality risk in
chronic hemodialysis patients: a national study.” *American Journal of
Kidney Diseases*, **31**(2), 607–617.
[doi:10.1053/ajkd.1998.v31.pm9531176](https://doi.org/10.1053/ajkd.1998.v31.pm9531176)
. Waikar SS, Bonventre JV (2009). “Creatinine kinetics and the
definition of acute kidney injury.” *Journal of the American Society of
Nephrology*, **20**(3), 672–679.
[doi:10.1681/ASN.2008070669](https://doi.org/10.1681/ASN.2008070669) .

## Examples

``` r
df <- tibble::tibble(
  ferritin         = c(50, 100),
  transferrin_sat  = c(30, 50),
  albumin          = c(45, 40),
  total_protein    = c(70, 75),
  EPA              = c(2.0, 2.5),
  DHA              = c(4.0, 4.5),
  Mg               = c(0.85, 0.90),
  creatinine       = c(80, 90),
  glycated_albumin = c(12, 14),
  uric_acid        = c(300, 400),
  BUN              = c(14, 16),
  phosphate        = c(1.0, 1.2),
  calcium          = c(2.3, 2.4),
  Na               = c(140, 138),
  K                = c(4.2, 4.0),
  Cl               = c(100, 102),
  HCO3             = c(24, 26),
  Tyr              = c(60, 70),
  Phe              = c(50, 55)
)
nutrient_markers(df, verbose = TRUE)
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
```
