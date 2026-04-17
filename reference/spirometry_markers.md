# Spirometry markers: FEV1/FVC, LLN-based obstruction, GOLD grade, bronchodilator response

Spirometry markers: FEV1/FVC, LLN-based obstruction, GOLD grade,
bronchodilator response

## Usage

``` r
spirometry_markers(
  data,
  col_map = NULL,
  na_action = c("keep", "omit", "error", "ignore", "warn"),
  verbose = TRUE
)
```

## Arguments

- data:

  Data frame with spirometry inputs.

- col_map:

  Named list: fev1, fvc, fev1_post, fvc_post, age, height, sex,
  ethnicity

- na_action:

  One of c("keep","omit","error","ignore","warn").

- verbose:

  Logical; if TRUE (default), emits progress via rlang::inform.

## Value

Tibble with ratio_pre, ratio_post, copd_flag_fixed, obstruction_lln,
fev1_pp, fvc_pp, fev1_z, fvc_z, ratio_z, gold_grade, bdr_fev1, bdr_fvc.

## References

Miller MR, Hankinson J, Brusasco V, et al. (2005). “Standardisation of
spirometry.” *European Respiratory Journal*, **26**(2), 319–338.
[doi:10.1183/09031936.05.00034805](https://doi.org/10.1183/09031936.05.00034805)
. ; Quanjer PH, Stanojevic S, Cole TJ, Baur X, Hall GL, Culver BH, et
al. (2012). “Multi-ethnic reference values for spirometry for the
3–95-yr age range: the global lung function 2012 equations.” *European
Respiratory Journal*, **40**, 1324–1343.
[doi:10.1183/09031936.00080312](https://doi.org/10.1183/09031936.00080312)
. ; Society AT (2002). “ATS statement: guidelines for the six-minute
walk test.” *American Journal of Respiratory and Critical Care
Medicine*, **166**(1), 111–117.
[doi:10.1164/ajrccm.166.1.at1102](https://doi.org/10.1164/ajrccm.166.1.at1102)
. ; for Chronic Obstructive Lung Disease (GOLD) GI (2025). “Global
strategy for the diagnosis, management, and prevention of COPD.” Online
report; no DOI assigned, <https://goldcopd.org/2025-gold-report/>.

## Examples

``` r
df <- data.frame(FEV1 = c(3.2, 2.1, 1.5), FVC = c(4.0, 3.0, 2.5))
spirometry_markers(df)
#> spirometry_markers(): reading input 'df' — 3 rows × 2 variables
#> spirometry_markers(): col_map (2 columns — 2 inferred from data)
#>   fev1              ->  'FEV1'    (inferred)
#>   fvc               ->  'FVC'    (inferred)
#> spirometry_markers(): computing markers:
#>   ratio_pre/post  [FEV1/FVC]
#>   copd_flag_fixed [ratio < 0.70]
#>   obstruction_lln [LLN-based]
#>   fev1_pp/fvc_pp  [% predicted]
#>   gold_grade      [GOLD severity]
#> spirometry_markers(): results: ratio_pre 3/3, ratio_post 0/3, copd_flag_fixed 3/3, obstruction_lln 0/3, fev1_pp 0/3, fvc_pp 0/3, fev1_z 0/3, fvc_z 0/3, ratio_z 0/3, gold_grade 0/3, bdr_fev1 0/3, bdr_fvc 0/3
#> # A tibble: 3 × 12
#>   ratio_pre ratio_post copd_flag_fixed obstruction_lln fev1_pp fvc_pp fev1_z
#>       <dbl>      <dbl> <lgl>           <lgl>             <dbl>  <dbl>  <dbl>
#> 1       0.8         NA FALSE           NA                   NA     NA     NA
#> 2       0.7         NA FALSE           NA                   NA     NA     NA
#> 3       0.6         NA TRUE            NA                   NA     NA     NA
#> # ℹ 5 more variables: fvc_z <dbl>, ratio_z <dbl>, gold_grade <chr>,
#> #   bdr_fev1 <dbl>, bdr_fvc <dbl>
```
