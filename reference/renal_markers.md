# Calculate a Suite of Renal Function, Injury, and Excretion Markers

Given routine blood and urine assays, `renal_markers()` computes:

- eGFR_cr: CKD-EPI creatinine equation (2009 variant; race factor
  retained to preserve prior behavior)

- eGFR_cys: CKD-EPI cystatin C equation (if `cystatin_C` provided)

- eGFR_combined: CKD-EPI combined creatinine+cystatin C (if both
  provided)

- BUN_Cr_ratio: Blood urea nitrogen / serum creatinine

- FE_Urea: Fractional excretion of urea (%)

- NGAL, KIM1, NAG, Beta2Micro, IL18, L_FABP: pass-through urinary injury
  markers (if mapped)

## Usage

``` r
renal_markers(
  data,
  col_map,
  na_action = c("keep", "omit", "error"),
  na_warn_prop = 0.2,
  check_extreme = FALSE,
  extreme_action = c("warn", "cap", "error", "ignore"),
  extreme_rules = NULL,
  verbose = FALSE
)
```

## Arguments

- data:

  A data.frame or tibble with renal lab data.

- col_map:

  Named list mapping:

  - creatinine -\> serum creatinine (mg/dL)

  - age -\> age (years)

  - sex -\> sex indicator (1 = male, 0 = female). Also accepts
    "male"/"female".

  - race -\> race ("white", "black", or "other"). Also accepts common
    aliases.

  - BUN -\> blood urea nitrogen (mg/dL)

  - optional cystatin_C -\> serum cystatin C (mg/L)

  - optional urea_serum -\> serum urea (mg/dL)

  - optional creatinine_urine -\> urine creatinine (mg/dL)

  - optional urea_urine -\> urine urea (mg/dL)

  - optional NGAL, KIM1, NAG, beta2_micro, IL18, L_FABP -\> urine injury
    markers

- na_action:

  One of c("keep","omit","error") for handling missing values in
  required inputs. Default "keep".

- na_warn_prop:

  Proportion \\\[0,1\]\\ to trigger high-missingness warnings for
  required inputs. Default 0.2.

- check_extreme:

  Logical; if TRUE, scans selected inputs for out-of-range values using
  simple heuristics. Default FALSE.

- extreme_action:

  One of c("warn","cap","error","ignore") when extremes are detected.
  Default "warn".

- extreme_rules:

  Optional named list of c(min,max) bounds keyed by input names (e.g.,
  "creatinine","BUN","cystatin_C","urea_serum","creatinine_urine","urea_urine").
  If NULL, built-in defaults are used.

- verbose:

  Logical; if TRUE, prints progress messages and a completion summary.

## Value

A tibble with columns: eGFR_cr, eGFR_cys, eGFR_combined, BUN_Cr_ratio,
FE_Urea, NGAL, KIM1, NAG, Beta2Micro, IL18, L_FABP

## Details

Robust validation is applied, including NA handling (`na_action`),
high-missingness diagnostics, safe divisions with a consolidated
zero-denominator warning, and an optional input extremes scan/cap. New
arguments are appended for backward compatibility.

Expected units (no automatic conversion performed):

- creatinine (serum): mg/dL

- cystatin C (serum): mg/L

- BUN (serum): mg/dL

- urea_serum, urea_urine: mg/dL

- creatinine_urine: mg/dL

## References

Levey AS, Stevens LA, Schmid CH, others (2009). “A new equation to
estimate glomerular filtration rate.” *Annals of Internal Medicine*,
**150**(9), 604–612.
[doi:10.7326/0003-4819-150-9-200905050-00006](https://doi.org/10.7326/0003-4819-150-9-200905050-00006)
. Inker LA, Schmid CH, Tighiouart H, others (2012). “Estimating
glomerular filtration rate from serum cystatin C.” *New England Journal
of Medicine*, **367**(1), 20–29.
[doi:10.1056/NEJMoa1114248](https://doi.org/10.1056/NEJMoa1114248) .
Inker LA, Eneanya ND, Coresh J, others (2021). “New creatinine- and
cystatin C-based equations for estimating GFR without race.” *New
England Journal of Medicine*, **385**(19), 1737–1749.
[doi:10.1056/NEJMoa2102953](https://doi.org/10.1056/NEJMoa2102953) .
Kaplan AA, Kohn OF (1992). “Fractional Excretion of Urea as a Guide to
Renal Dysfunction.” *American Journal of Nephrology*, **12**(1–2),
49–54. [doi:10.1159/000168417](https://doi.org/10.1159/000168417) .
Parikh CR, Coca SG, Thiessen-Philbrook H, others (2011). “Postoperative
Biomarkers Predict Acute Kidney Injury and Poor Outcomes after Adult
Cardiac Surgery.” *Journal of the American Society of Nephrology*,
**22**(12), 1737–1747.
[doi:10.1681/ASN.2010121302](https://doi.org/10.1681/ASN.2010121302) .
Vaidya VS, Ramirez V, Ichimura T, others (2010). “Kidney injury
molecule-1 outperforms traditional biomarkers of kidney injury in
preclinical biomarker qualification studies.” *Nature Biotechnology*,
**28**(5), 478–485.
[doi:10.1038/nbt.1623](https://doi.org/10.1038/nbt.1623) . Portilla D,
Dent C, Sugaya T, others (2008). “Urinary liver-type fatty acid-binding
protein as a biomarker of acute kidney injury.” *Kidney International*,
**73**(4), 465–472.
[doi:10.1038/sj.ki.5002721](https://doi.org/10.1038/sj.ki.5002721) .

## Examples

``` r
df <- tibble::tibble(Cr = 1.0, Age = 40, Sex = 1, Race = "white", BUN = 14)
cm <- list(creatinine = "Cr", age = "Age", sex = "Sex", race = "Race", BUN = "BUN")
renal_markers(df, cm)
#> # A tibble: 1 × 11
#>   eGFR_cr eGFR_cys eGFR_combined BUN_Cr_ratio FE_Urea  NGAL  KIM1   NAG
#>     <dbl>    <dbl>         <dbl>        <dbl>   <dbl> <dbl> <dbl> <dbl>
#> 1    93.7       NA            NA           14      NA    NA    NA    NA
#> # ℹ 3 more variables: Beta2Micro <dbl>, IL18 <dbl>, L_FABP <dbl>
```
