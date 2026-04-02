# CKD staging (GFR and albuminuria) and KDIGO risk

Categorizes eGFR into G1-G5, albuminuria into A1-A3 (by UACR mg/g), and
maps KDIGO risk.

## Usage

``` r
ckd_stage(
  data,
  col_map,
  na_action = c("keep", "omit", "error"),
  check_extreme = FALSE,
  extreme_action = c("cap", "NA", "error"),
  verbose = FALSE
)
```

## Arguments

- data:

  Data frame with renal measures.

- col_map:

  Named list with required key: eGFR; optional key: UACR.

- na_action:

  One of:

  - "keep" (retain rows; stages become NA where inputs missing)

  - "omit" (drop rows with any missing eGFR/UACR that are mapped)

  - "error" (abort if any mapped input missing)

- check_extreme:

  Logical; if TRUE, screen eGFR/UACR for extreme values.

- extreme_action:

  One of:

  - "cap" - winsorize to bounds (eGFR: (0, 200); UACR: (0, 5000) mg/g)

  - "NA" - replace out-of-range with NA

  - "error" - abort on any out-of-range value

- verbose:

  Logical; if TRUE, emits progress messages via `hm_inform()`.

## Value

Tibble with CKD_stage, Albuminuria_stage, KDIGO_risk.

## References

Kidney Disease: Improving Global Outcomes (KDIGO) CKD Work Group (2013).
“KDIGO 2012 Clinical Practice Guideline for the Evaluation and
Management of Chronic Kidney Disease.” *Kidney International
Supplements*, **3**(1), 1–150. Guideline without reliable DOI metadata
in Crossref; related synopsis PMID: 23732715,
<https://kdigo.org/guidelines/ckd-evaluation-and-management/>.

## Examples

``` r
df <- data.frame(eGFR = c(95, 50), UACR = c(10, 200))
ckd_stage(df, list(eGFR = "eGFR", UACR = "UACR"))
#> # A tibble: 2 × 3
#>   CKD_stage Albuminuria_stage KDIGO_risk
#>   <fct>     <fct>             <fct>     
#> 1 G1        A1                Low       
#> 2 G3a       A2                High      
```
