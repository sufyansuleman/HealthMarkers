# Calculate urine-only renal and tubular markers (research-ready)

Computes (urine-only):

- UACR (Albumin-to-Creatinine Ratio, mg/g)

- albuminuria_stage (KDIGO A1/A2/A3 by UACR)

- microalbuminuria flag ("normal" vs "micro")

- UPCR (Urine Protein-to-Creatinine Ratio, mg/g; if urine_protein
  available)

- U_Na_K_ratio (urine Na+/K+; if urine_Na and urine_K available)

- Creatinine-normalized tubular markers (if present, per g creatinine):
  NGAL_per_gCr, KIM1_per_gCr, NAG_per_gCr, Beta2Micro_per_gCr,
  A1Micro_per_gCr, IL18_per_gCr, L_FABP_per_gCr

## Usage

``` r
urine_markers(
  data,
  col_map = NULL,
  verbose = TRUE,
  na_action = c("keep", "omit", "error"),
  na_warn_prop = 0.2
)
```

## Arguments

- data:

  A data.frame or tibble with at least urine_albumin and
  urine_creatinine.

- col_map:

  Optional named list mapping canonical keys (e.g., `urine_albumin`,
  `urine_creatinine`) to actual column names in `data`. If `NULL`,
  column names are inferred automatically.

- verbose:

  Logical; if `TRUE`, prints progress messages and a completion summary.
  Default FALSE.

- na_action:

  One of `c("keep","omit","error")` for handling missing values in
  required inputs. Default "keep".

- na_warn_prop:

  Proportion \\\[0,1\]\\ to trigger high-missingness warnings for
  required inputs. Default 0.2.

## Value

A tibble with columns: UACR, albuminuria_stage, microalbuminuria, UPCR,
U_Na_K_ratio, NGAL_per_gCr, KIM1_per_gCr, NAG_per_gCr,
Beta2Micro_per_gCr, A1Micro_per_gCr, IL18_per_gCr, L_FABP_per_gCr

## Details

Inputs are validated, missingness handled via `na_action`, divisions are
safeguarded (Inf/NaN -\> NA) with a consolidated zero-denominator
warning, and an optional extremes scan/cap is available.

Expected units:

- urine_albumin: mg/L

- urine_protein: mg/L (optional)

- urine_creatinine: mg/dL

- urine_Na, urine_K: mmol/L (optional)

- Optional tubular markers above assumed mg/L when normalized per g
  creatinine

## References

### Original derivations

- Mogensen CE. Microalbuminuria predicts clinical proteinuria and early
  mortality in maturity-onset diabetes. N Engl J Med.
  1984;310(6):356-360.
  [doi:10.1056/NEJM198402093100602](https://doi.org/10.1056/NEJM198402093100602)
  (UACR and microalbuminuria concept)

- Ginsberg JM, Chang BS, Matarese RA, Garella S. Use of single voided
  urine samples to estimate quantitative proteinuria. N Engl J Med.
  1983;309(25):1543-1546.
  [doi:10.1056/NEJM198312223092503](https://doi.org/10.1056/NEJM198312223092503)
  (UPCR derivation and validation)

- Bokenkamp A, Domanetzki M, Zinck R, Schumann G, Byrd D, Brodehl J.
  Reference values for urinary albumin excretion in healthy children.
  Pediatr Nephrol. 1998;12(6):478-483.
  [doi:10.1007/s004670050480](https://doi.org/10.1007/s004670050480)
  (Albumin excretion normative values)

### Validation and consensus

- Kidney Disease: Improving Global Outcomes (KDIGO) CKD Work Group
  (2013). “KDIGO 2012 Clinical Practice Guideline for the Evaluation and
  Management of Chronic Kidney Disease.” *Kidney International
  Supplements*, **3**(1), 1–150. Guideline without reliable DOI metadata
  in Crossref; related synopsis PMID: 23732715,
  <https://kdigo.org/guidelines/ckd-evaluation-and-management/>.
  (Albuminuria stages A1-A3; UACR cutoffs)

- de Zeeuw D, Parving HH, Henning RH. Microalbuminuria as an early
  marker for cardiovascular disease. J Am Soc Nephrol.
  2006;17(8):2100-2105.
  [doi:10.1681/ASN.2006040388](https://doi.org/10.1681/ASN.2006040388)
  (Prognostic validation of UACR)

- Ichimura T, Hung CC, Yang SA, Stevens JL, Bonventre JV. Kidney injury
  molecule-1: a tissue and urinary biomarker for nephrotoxicant-induced
  renal injury. Am J Physiol Renal Physiol. 2004;286(3):F552-F563.
  [doi:10.1152/ajprenal.00285.2002](https://doi.org/10.1152/ajprenal.00285.2002)
  (KIM-1 as tubular marker)

- Portilla D, Dent C, Sugaya T, et al. Liver fatty acid-binding protein
  as a biomarker of acute kidney injury after cardiac surgery. Kidney
  Int. 2008;73(4):465-472.
  [doi:10.1038/sj.ki.5002688](https://doi.org/10.1038/sj.ki.5002688)
  (L-FABP biomarker validation)

## Examples

``` r
df <- tibble::tibble(
  urine_albumin    = 30,
  urine_creatinine = 1.2,
  serum_creatinine = 0.9,
  plasma_Na        = 140,
  urine_Na         = 100,
  age              = 55,
  sex              = 2,
  urine_protein    = 150
)
urine_markers(df)
#> urine_markers(): reading input 'df' — 1 rows × 8 variables
#> urine_markers(): col_map (4 columns — 4 inferred from data)
#>   urine_albumin     ->  'urine_albumin'    (inferred)
#>   urine_creatinine  ->  'urine_creatinine'    (inferred)
#>   urine_protein     ->  'urine_protein'    (inferred)
#>   urine_Na          ->  'urine_Na'    (inferred)
#> urine_markers(): computing markers:
#>   UACR, albuminuria_stage, microalbuminuria [urine_albumin, urine_creatinine]
#>   UPCR [urine_protein, urine_creatinine]
#>   U_Na_K_ratio [urine_Na, urine_K]
#>   NGAL/KIM1/NAG/Beta2Micro/A1Micro/IL18/L_FABP per gCr [optional]
#> urine_markers(): results: UACR 1/1, albuminuria_stage 1/1, microalbuminuria 1/1, UPCR 1/1, U_Na_K_ratio 0/1, NGAL_per_gCr 0/1, KIM1_per_gCr 0/1, NAG_per_gCr 0/1, Beta2Micro_per_gCr 0/1, A1Micro_per_gCr 0/1, IL18_per_gCr 0/1, L_FABP_per_gCr 0/1
#> # A tibble: 1 × 12
#>    UACR albuminuria_stage microalbuminuria  UPCR U_Na_K_ratio NGAL_per_gCr
#>   <dbl> <fct>             <fct>            <dbl>        <dbl>        <dbl>
#> 1 25000 A3                normal           12500           NA           NA
#> # ℹ 6 more variables: KIM1_per_gCr <dbl>, NAG_per_gCr <dbl>,
#> #   Beta2Micro_per_gCr <dbl>, A1Micro_per_gCr <dbl>, IL18_per_gCr <dbl>,
#> #   L_FABP_per_gCr <dbl>
```
