# Compute composite vitamin and endocrine marker ratios and z-scores

Given serum/plasma vitamins and related analytes, `vitamin_markers()`
computes:

- VitD_Z: z-score of 25(OH)D using provided reference mean/sd

- B12_Fol_Ratio: vitamin B12 / folate

- Ferr_TSat_R: ferritin / transferrin saturation (TSat)

- Cort_DHEA_R: cortisol / DHEA-S

- T_E2_Ratio: testosterone / estradiol

- TSH_fT4_R: TSH / free T4

- Retinol_Z: z-score of retinol using provided reference mean/sd

- Toco_Lip_R: alpha-tocopherol / total lipids

- Mg_Zn_R: magnesium / zinc

- Cu_Zn_R: copper / zinc Plus pass-through: PIVKA_II, VitC,
  Homocysteine, MMA

## Usage

``` r
vitamin_markers(
  data,
  col_map = NULL,
  na_action = c("keep", "omit", "error"),
  na_warn_prop = 0.2,
  verbose = TRUE
)
```

## Arguments

- data:

  A data.frame or tibble with vitamin/analyte columns.

- col_map:

  Named list mapping required keys to column names: VitD, VitD_ref_mean,
  VitD_ref_sd, B12, Folate, Ferritin, TSat, Cortisol, DHEAS,
  Testosterone, Estradiol, TSH, free_T4, Retinol, Retinol_ref_mean,
  Retinol_ref_sd, Tocopherol, Total_lipids, PIVKA_II, VitC,
  Homocysteine, MMA, Magnesium, Zinc, Copper.

- na_action:

  One of c("keep","omit","error") for required inputs. Default "keep".

- na_warn_prop:

  Proportion \\\[0,1\]\\ to trigger high-missingness debug notices.
  Default 0.2.

- verbose:

  Logical; if `TRUE` (default), prints column mapping, input
  availability, physiological range information (informational only,
  values not altered), the list of markers being computed with their
  inputs, and a per-column results summary.

## Value

A tibble with columns: VitD_Z, B12_Fol_Ratio, Ferr_TSat_R, Cort_DHEA_R,
T_E2_Ratio, TSH_fT4_R, Retinol_Z, Toco_Lip_R, PIVKA_II, VitC,
Homocysteine, MMA, Mg_Zn_R, Cu_Zn_R. If an ID column is detected in
`data` (e.g. `id`, `IID`, `participant_id`), it is prepended as the
first output column.

## Details

HM-CS v2:

- Validation via `hm_validate_inputs(data, col_map, required_keys, fn)`

- User errors via `rlang::abort(..., class=...)`

- Verbosity via `hm_inform(level)` controlled by
  `options(healthmarkers.verbose)`

- High-missingness diagnostics at debug level only

## References

Holick MF. Vitamin D deficiency. N Engl J Med. 2007;357:266-281.
[doi:10.1056/NEJMra070553](https://doi.org/10.1056/NEJMra070553) O'Leary
F, Samman S. Vitamin B12 in health and disease. Nutrients.
2010;2(3):299-316.
[doi:10.3390/nu2030299](https://doi.org/10.3390/nu2030299) Ganz T,
Nemeth E. Iron homeostasis in host defence and inflammation. Nat Rev
Immunol. 2015;15:500-510.
[doi:10.1038/nri3863](https://doi.org/10.1038/nri3863) Huxtable RJ.
Physiological actions of taurine. Physiol Rev. 1992;72(1):101-163.
(endocrine ratios context)

## Examples

``` r
df <- tibble::tibble(
  VitD = 50, VitD_ref_mean = 40, VitD_ref_sd = 5,
  B12 = 300, Folate = 15,
  Ferritin = 100, TSat = 0.25,
  Cortisol = 200, DHEAS = 100,
  Testosterone = 12, Estradiol = 120,
  TSH = 2, free_T4 = 14,
  Retinol = 0.8, Retinol_ref_mean = 0.9, Retinol_ref_sd = 0.2,
  Tocopherol = 30, Total_lipids = 3,
  PIVKA_II = 5, VitC = 60, Homocysteine = 10, MMA = 0.3,
  Magnesium = 0.8, Zinc = 15, Copper = 15
)
cm <- as.list(names(df)); names(cm) <- names(df)
vitamin_markers(df, cm)
#> vitamin_markers(): reading input 'df' — 1 rows × 25 variables
#> vitamin_markers(): col_map (25 columns — 25 specified)
#>   VitD              ->  'VitD'
#>   VitD_ref_mean     ->  'VitD_ref_mean'
#>   VitD_ref_sd       ->  'VitD_ref_sd'
#>   B12               ->  'B12'
#>   Folate            ->  'Folate'
#>   Ferritin          ->  'Ferritin'
#>   TSat              ->  'TSat'
#>   Cortisol          ->  'Cortisol'
#>   DHEAS             ->  'DHEAS'
#>   Testosterone      ->  'Testosterone'
#>   Estradiol         ->  'Estradiol'
#>   TSH               ->  'TSH'
#>   free_T4           ->  'free_T4'
#>   Retinol           ->  'Retinol'
#>   Retinol_ref_mean  ->  'Retinol_ref_mean'
#>   Retinol_ref_sd    ->  'Retinol_ref_sd'
#>   Tocopherol        ->  'Tocopherol'
#>   Total_lipids      ->  'Total_lipids'
#>   PIVKA_II          ->  'PIVKA_II'
#>   VitC              ->  'VitC'
#>   Homocysteine      ->  'Homocysteine'
#>   MMA               ->  'MMA'
#>   Magnesium         ->  'Magnesium'
#>   Zinc              ->  'Zinc'
#>   Copper            ->  'Copper'
#> vitamin_markers(): optional inputs
#>   present:  VitD, VitD_ref_mean, VitD_ref_sd, B12, Folate, Ferritin, TSat, Cortisol, DHEAS, Testosterone, Estradiol, TSH, free_T4, Retinol, Retinol_ref_mean, Retinol_ref_sd, Tocopherol, Total_lipids, PIVKA_II, VitC, Homocysteine, MMA, Magnesium, Zinc, Copper
#> vitamin_markers(): column map: VitD -> 'VitD', VitD_ref_mean -> 'VitD_ref_mean', VitD_ref_sd -> 'VitD_ref_sd', B12 -> 'B12', Folate -> 'Folate', Ferritin -> 'Ferritin', TSat -> 'TSat', Cortisol -> 'Cortisol', DHEAS -> 'DHEAS', Testosterone -> 'Testosterone', Estradiol -> 'Estradiol', TSH -> 'TSH', free_T4 -> 'free_T4', Retinol -> 'Retinol', Retinol_ref_mean -> 'Retinol_ref_mean', Retinol_ref_sd -> 'Retinol_ref_sd', Tocopherol -> 'Tocopherol', Total_lipids -> 'Total_lipids', PIVKA_II -> 'PIVKA_II', VitC -> 'VitC', Homocysteine -> 'Homocysteine', MMA -> 'MMA', Magnesium -> 'Magnesium', Zinc -> 'Zinc', Copper -> 'Copper'
#> vitamin_markers(): computing markers:
#>   VitD_Z           [VitD, VitD_ref_mean, VitD_ref_sd]
#>   B12_Fol_Ratio    [B12, Folate]
#>   Ferr_TSat_R      [Ferritin, TSat]
#>   Cort_DHEA_R      [Cortisol, DHEAS]
#>   T_E2_Ratio       [Testosterone, Estradiol]
#>   TSH_fT4_R        [TSH, free_T4]
#>   Retinol_Z        [Retinol, Retinol_ref_mean, Retinol_ref_sd]
#>   Toco_Lip_R       [Tocopherol, Total_lipids]
#>   PIVKA_II         [PIVKA_II]
#>   VitC             [VitC]
#>   Homocysteine     [Homocysteine]
#>   MMA              [MMA]
#>   Mg_Zn_R          [Magnesium, Zinc]
#>   Cu_Zn_R          [Copper, Zinc]
#> vitamin_markers(): results: VitD_Z 1/1, B12_Fol_Ratio 1/1, Ferr_TSat_R 1/1, Cort_DHEA_R 1/1, T_E2_Ratio 1/1, TSH_fT4_R 1/1, Retinol_Z 1/1, Toco_Lip_R 1/1, PIVKA_II 1/1, VitC 1/1, Homocysteine 1/1, MMA 1/1, Mg_Zn_R 1/1, Cu_Zn_R 1/1
#> # A tibble: 1 × 14
#>   VitD_Z B12_Fol_Ratio Ferr_TSat_R Cort_DHEA_R T_E2_Ratio TSH_fT4_R Retinol_Z
#>    <dbl>         <dbl>       <dbl>       <dbl>      <dbl>     <dbl>     <dbl>
#> 1      2            20         400           2        0.1     0.143      -0.5
#> # ℹ 7 more variables: Toco_Lip_R <dbl>, PIVKA_II <dbl>, VitC <dbl>,
#> #   Homocysteine <dbl>, MMA <dbl>, Mg_Zn_R <dbl>, Cu_Zn_R <dbl>
```
