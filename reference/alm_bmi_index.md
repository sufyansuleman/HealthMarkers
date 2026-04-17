# Appendicular Lean Mass to BMI Index

Calculates the ratio of appendicular lean mass (ALM) to body mass index
(BMI), and flags low muscle mass based on FNIH Sarcopenia Project
cut-points.

## Usage

``` r
alm_bmi_index(
  data,
  col_map = NULL,
  na_action = c("keep", "omit", "error", "ignore", "warn"),
  verbose = TRUE
)
```

## Arguments

- data:

  A data.frame or tibble with ALM, BMI, and sex columns.

- col_map:

  Named list with:

  - alm: appendicular lean mass column name (kg)

  - bmi: body mass index column name (kg/m^2)

  - sex: sex column name ("Male"/"Female" or m/f; case-insensitive)

- na_action:

  One of c("keep","omit","error","ignore","warn").

- verbose:

  Logical; if TRUE (default), emits progress messages.

## Value

A tibble with:

- alm_bmi_ratio (numeric)

- low_muscle_mass (logical; TRUE if below sex-specific cut-point; NA if
  sex unknown or ratio NA)

## Details

ALM/BMI reflects muscle mass relative to body size. FNIH cut-points:

- Men: ALM/BMI \< 0.789

- Women: ALM/BMI \< 0.512

ALM should be in kilograms and BMI in kg/m^2.

## References

McLean RR, Shardell MD, Alley DE, et al. (2014). “Criteria for
clinically relevant weakness and low lean mass: FNIH Sarcopenia
Project.” *Journal of Gerontology A: Biological Sciences and Medical
Sciences*, **69**(5), 576–583.
[doi:10.1093/gerona/glu012](https://doi.org/10.1093/gerona/glu012) . ;
Studenski SA, Peters KW, Alley DE, et al. (2014). “The FNIH Sarcopenia
Project: Rationale, Study Description, Conference Recommendations, and
Final Estimates.” *Journal of Gerontology A: Biological Sciences and
Medical Sciences*, **69**(5), 564–570.
[doi:10.1093/gerona/glu010](https://doi.org/10.1093/gerona/glu010) .

## Examples

``` r
df <- data.frame(ALM_kg = c(7.2, 5.8, 6.5), BMI = c(24, 28, 22),
                 Sex = c("male", "female", "male"))
alm_bmi_index(df)
#> alm_bmi_index(): reading input 'df' — 3 rows × 3 variables
#> alm_bmi_index(): col_map (3 columns — 3 inferred from data)
#>   alm               ->  'ALM_kg'    (inferred)
#>   bmi               ->  'BMI'    (inferred)
#>   sex               ->  'Sex'    (inferred)
#> alm_bmi_index(): computing markers:
#>   alm_bmi_ratio    [ALM / BMI]
#>   low_muscle_mass  [ratio < sex-specific FNIH cut-point]
#> alm_bmi_index(): results: alm_bmi_ratio 3/3, low_muscle_mass 3/3
#> # A tibble: 3 × 2
#>   alm_bmi_ratio low_muscle_mass
#>           <dbl> <lgl>          
#> 1         0.3   TRUE           
#> 2         0.207 TRUE           
#> 3         0.295 TRUE           
```
