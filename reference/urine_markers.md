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

## Note

UACR formula: albumin (mg/L) / creatinine (g/L) = albumin (mg/L)
\\\times\\ 100 / creatinine (mg/dL). UPCR and per-gCr tubular markers
use the same creatinine denominator:
`gCr_den = creatinine (mg/dL) \times 0.01` (= g/L). Tubular markers
(NGAL, KIM-1, NAG, Beta-2-microglobulin, alpha-1-microglobulin, IL-18,
L-FABP) are **pass-through** columns normalised per g creatinine; no
formula other than creatinine adjustment is applied.

## References

Mogensen CE (1984). “Microalbuminuria predicts clinical proteinuria and
early mortality in maturity-onset diabetes.” *New England Journal of
Medicine*, **310**(6), 356–360.
[doi:10.1056/NEJM198402093100602](https://doi.org/10.1056/NEJM198402093100602)
. Ginsberg JM, Chang BS, Matarese RA, Garella S (1983). “Use of single
voided urine samples to estimate quantitative proteinuria.” *New England
Journal of Medicine*, **309**(25), 1543–1546.
[doi:10.1056/NEJM198312223092503](https://doi.org/10.1056/NEJM198312223092503)
. Kidney Disease: Improving Global Outcomes (KDIGO) CKD Work Group
(2013). “KDIGO 2012 Clinical Practice Guideline for the Evaluation and
Management of Chronic Kidney Disease.” *Kidney International
Supplements*, **3**(1), 1–150.
[doi:10.1038/kisup.2012.73](https://doi.org/10.1038/kisup.2012.73) .
Related synopsis: Stevens and Levin (2013), Ann Intern Med,
doi:10.7326/0003-4819-158-11-201306040-00007,
<https://kdigo.org/guidelines/ckd-evaluation-and-management/>.
(albuminuria staging A1–A3 UACR cutoffs) de Zeeuw D, Parving H, Henning
RH (2006). “Microalbuminuria as an early marker for cardiovascular
disease.” *Journal of the American Society of Nephrology*, **17**(8),
2100–2105.
[doi:10.1681/ASN.2006040388](https://doi.org/10.1681/ASN.2006040388) .
(prognostic UACR validation; background) Ichimura T, Hung CC, Yang SA,
Stevens JL, Bonventre JV (2004). “Kidney injury molecule-1: a tissue and
urinary biomarker for nephrotoxicant-induced renal injury.” *American
Journal of Physiology: Renal Physiology*, **286**(3), F552–F563.
[doi:10.1152/ajprenal.00285.2002](https://doi.org/10.1152/ajprenal.00285.2002)
. (KIM-1 tubular biomarker; pass-through normalization, background)
Portilla D, Dent C, Sugaya T, others (2008). “Urinary liver-type fatty
acid-binding protein as a biomarker of acute kidney injury.” *Kidney
International*, **73**(4), 465–472.
[doi:10.1038/sj.ki.5002721](https://doi.org/10.1038/sj.ki.5002721) .
(L-FABP tubular biomarker; pass-through normalization, background)

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
#> 1  2500 A3                normal           12500           NA           NA
#> # ℹ 6 more variables: KIM1_per_gCr <dbl>, NAG_per_gCr <dbl>,
#> #   Beta2Micro_per_gCr <dbl>, A1Micro_per_gCr <dbl>, IL18_per_gCr <dbl>,
#> #   L_FABP_per_gCr <dbl>
```
