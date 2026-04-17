# Calculate fasting-based insulin sensitivity indices

Compute 10 fasting indices from glucose (mmol/L) and insulin (pmol/L):
Fasting_inv, Raynaud, HOMA_IR_inv, FIRI, QUICKI, Belfiore_basal,
Ig_ratio_basal, Isi_basal, Bennett, HOMA_IR_rev_inv. Units converted
internally: G0_mg = G0\*18 (mg/dL), I0_u = I0/6 (muU/mL).

## Usage

``` r
fasting_is(
  data,
  col_map = NULL,
  normalize = c("none", "z", "inverse", "range", "robust"),
  na_action = c("keep", "omit", "error", "warn"),
  verbose = TRUE
)
```

## Arguments

- data:

  Data frame with required inputs.

- col_map:

  Named list mapping required keys:

  - G0: fasting glucose (mmol/L)

  - I0: fasting insulin (pmol/L)

- normalize:

  One of: "none","z","inverse","range","robust".

- na_action:

  One of:

  - "keep" (retain all rows; indices become NA where inputs
    missing/non-finite)

  - "omit" (drop rows with any missing/non-finite required inputs)

  - "error" (abort if any required input is missing/non-finite)

  - "warn" (emit a warning for rows with missing inputs, then keep them)

- verbose:

  Logical; if `TRUE` (default), prints column mapping, the list of
  indices being computed, and a per-column results summary.

## Value

Tibble with 10 columns (indices listed above). If an ID column is
detected in `data` (e.g. `id`, `IID`, `participant_id`), it is prepended
as the first output column.

## References

Matthews DR, Hosker JP, Rudenski AS, Naylor BA, Treacher DF, Turner RC
(1985). “Homeostasis Model Assessment: Insulin Resistance and Beta-Cell
Function from Fasting Plasma Glucose and Insulin Concentrations in Man.”
*Diabetologia*, **28**(7), 412–419.
[doi:10.1007/BF00280883](https://doi.org/10.1007/BF00280883) . ; Katz A,
Nambi SS, Mather K, Baron AD, Follmann DA, Sullivan G, Quon MJ (2000).
“Quantitative Insulin Sensitivity Check Index: A Simple, Accurate Method
for Assessing Insulin Sensitivity in Humans.” *Journal of Clinical
Endocrinology & Metabolism*, **85**(7), 2402–2410.
[doi:10.1210/jcem.85.7.6661](https://doi.org/10.1210/jcem.85.7.6661) . ;
Raynaud E, Pérez-Martin A, Brun J, Benhaddad AA, Mercier J (1999).
“Fasting Plasma Insulin and Insulin Resistance Indices.” *Diabetes &
Metabolism*, **25**(6), 524–532. No DOI identified in Crossref/PubMed as
of 2026-03-16; see URL,
<https://pubmed.ncbi.nlm.nih.gov/?term=Fasting+Plasma+Insulin+and+Insulin+Resistance+Indices>.
; Avignon A, Charles M, Rabasa-Lhoret R, et al. (1999). “Assessment of
Insulin Sensitivity from Oral Glucose Tolerance Test in Normal Subjects
and in Insulin-Resistant Patients.” *International Journal of Obesity*,
**23**(5), 512–517.
[doi:10.1038/sj.ijo.0800864](https://doi.org/10.1038/sj.ijo.0800864) . ;
Belfiore F, Iannello S, Volpicelli G (1998). “Insulin Sensitivity
Indices Calculated from Basal and OGTT-Related Insulin and Glucose
Levels.” *Molecular Genetics and Metabolism*, **63**(2), 134–141.
[doi:10.1006/mgme.1997.2658](https://doi.org/10.1006/mgme.1997.2658) . ;
Sluiter D, Erkelens DW, Reitsma WD, Doorenbos H (1976). “Glucose
Tolerance and Insulin Release: A Mathematical Approach.” *Diabetes*,
**25**, 245–249.
[doi:10.2337/diabetes.25.4.245](https://doi.org/10.2337/diabetes.25.4.245)
. ; Hanson RL, Pratley RE, Bogardus C, Narayan KMV, Roumain J,
Imperatore G, Fagot-Campagna A, Pettitt DJ, Bennett PH, Knowler WC
(2000). “Evaluation of Simple Indices of Insulin Sensitivity and Insulin
Secretion for Use in Epidemiologic Studies.” *American Journal of
Epidemiology*, **151**(2), 190–198.
[doi:10.1093/oxfordjournals.aje.a010187](https://doi.org/10.1093/oxfordjournals.aje.a010187)
. ; Anderson RL, Hamman RF, Savage PJ, Saad MF, Laws A, Kades WW, Sands
RE, Cefalu WT (1995). “Exploration of Simple Measures of Insulin
Resistance.” *American Journal of Epidemiology*, **142**(7), 724–732.
[doi:10.1093/aje/142.7.724](https://doi.org/10.1093/aje/142.7.724) . ;
Suleman S, Madsen AL, Ängquist LH, Schubert M, Linneberg A, Loos RJF,
Hansen T, Grarup N (2024). “Genetic Underpinnings of Fasting and Oral
Glucose-stimulated Based Insulin Sensitivity Indices.” *The Journal of
Clinical Endocrinology & Metabolism*, **109**(11), 2754–2763.
[doi:10.1210/clinem/dgae275](https://doi.org/10.1210/clinem/dgae275) .

## Examples

``` r
# Minimal example (units: G0 in mmol/L, I0 in pmol/L)
df <- data.frame(G0 = c(5.2, 6.1, 4.8), I0 = c(60, 120, 80))
res <- fasting_is(df, col_map = list(G0 = "G0", I0 = "I0"))
#> fasting_is(): reading input 'df' — 3 rows × 2 variables
#> fasting_is(): col_map (2 columns — 2 specified)
#>   G0                ->  'G0'
#>   I0                ->  'I0'
#> fasting_is(): computing markers:
#>   Fasting_inv          [G0, I0]
#>   Raynaud              [G0, I0]
#>   HOMA_IR_inv          [G0, I0]
#>   FIRI                 [G0, I0]
#>   QUICKI               [G0, I0]
#>   Belfiore_basal       [G0, I0]
#>   Ig_ratio_basal       [G0, I0]
#>   Isi_basal            [G0, I0]
#>   Bennett              [G0, I0]
#>   HOMA_IR_rev_inv      [G0, I0]
#> fasting_is(): results: Fasting_inv 3/3, Raynaud 3/3, HOMA_IR_inv 3/3, FIRI 3/3, QUICKI 3/3, Belfiore_basal 3/3, Ig_ratio_basal 3/3, Isi_basal 3/3, Bennett 3/3, HOMA_IR_rev_inv 3/3
head(res)
#> # A tibble: 3 × 10
#>   Fasting_inv Raynaud HOMA_IR_inv  FIRI QUICKI Belfiore_basal Ig_ratio_basal
#>         <dbl>   <dbl>       <dbl> <dbl>  <dbl>          <dbl>          <dbl>
#> 1       -10         4       -41.6  37.4  0.146       0.00213          -0.107
#> 2       -20         2       -97.6  87.8  0.130       0.000910         -0.182
#> 3       -13.3       3       -51.2  46.1  0.142       0.00173          -0.154
#> # ℹ 3 more variables: Isi_basal <dbl>, Bennett <dbl>, HOMA_IR_rev_inv <dbl>

# With NA handling
df2 <- data.frame(G0 = c(5.0, NA), I0 = c(90, 150))
fasting_is(df2, col_map = list(G0 = "G0", I0 = "I0"), na_action = "keep")
#> fasting_is(): reading input 'df2' — 2 rows × 2 variables
#> fasting_is(): col_map (2 columns — 2 specified)
#>   G0                ->  'G0'
#>   I0                ->  'I0'
#> fasting_is(): computing markers:
#>   Fasting_inv          [G0, I0]
#>   Raynaud              [G0, I0]
#>   HOMA_IR_inv          [G0, I0]
#>   FIRI                 [G0, I0]
#>   QUICKI               [G0, I0]
#>   Belfiore_basal       [G0, I0]
#>   Ig_ratio_basal       [G0, I0]
#>   Isi_basal            [G0, I0]
#>   Bennett              [G0, I0]
#>   HOMA_IR_rev_inv      [G0, I0]
#> fasting_is(): results: Fasting_inv 2/2, Raynaud 2/2, HOMA_IR_inv 1/2, FIRI 1/2, QUICKI 1/2, Belfiore_basal 1/2, Ig_ratio_basal 1/2, Isi_basal 1/2, Bennett 1/2, HOMA_IR_rev_inv 1/2
#> # A tibble: 2 × 10
#>   Fasting_inv Raynaud HOMA_IR_inv  FIRI QUICKI Belfiore_basal Ig_ratio_basal
#>         <dbl>   <dbl>       <dbl> <dbl>  <dbl>          <dbl>          <dbl>
#> 1         -15    2.67         -60    54  0.139        0.00148         -0.167
#> 2         -25    1.6           NA    NA NA           NA               NA    
#> # ℹ 3 more variables: Isi_basal <dbl>, Bennett <dbl>, HOMA_IR_rev_inv <dbl>
```
