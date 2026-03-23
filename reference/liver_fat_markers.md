# Liver fat surrogates: HSI and NAFLD Liver Fat Score

Computes:

- HSI = 8 \* (ALT/AST) + BMI + 2 (if female) + 2 (if diabetes)

- NAFLD-LFS = -2.89 + 1.18*MetS + 0.45*Type2DM + 0.15*Insulin_u +
  0.04*AST - 0.94\*(AST/ALT)

## Usage

``` r
liver_fat_markers(
  data,
  col_map = list(ALT = "ALT", AST = "AST", BMI = "BMI", sex = "sex", diabetes =
    "diabetes", MetS = "MetS", insulin = "insulin", I0 = "I0", waist = "waist", TG =
    "TG", HDL_c = "HDL_c", sbp = "sbp", bp_sys = "bp_sys", bp_treated = "bp_treated",
    glucose = "glucose", G0 = "G0"),
  na_action = c("keep", "omit", "error", "ignore", "warn"),
  na_warn_prop = 0.2,
  check_extreme = FALSE,
  extreme_action = c("warn", "cap", "error", "ignore", "NA"),
  extreme_rules = NULL,
  verbose = FALSE
)
```

## Arguments

- data:

  Data frame with needed columns (see col_map).

- col_map:

  Named list mapping:

  - Required for HSI: ALT, AST, BMI

  - Optional direct inputs: sex, diabetes, MetS, insulin

  - Optional to derive MetS or insulin: I0, waist, TG, HDL_c, sbp,
    bp_sys, bp_treated, glucose, G0

- na_action:

  One of c("keep","omit","error","ignore","warn").

- na_warn_prop:

  Proportion in \\\[0,1\]\\ for high-missingness warnings when na_action
  = "warn". Default 0.2.

- check_extreme:

  Logical; if TRUE, scan selected inputs for plausible ranges.

- extreme_action:

  One of c("warn","cap","error","ignore","NA") controlling how extremes
  are handled.

- extreme_rules:

  Optional named list of c(min,max) to override defaults.

- verbose:

  Logical; if TRUE, prints progress.

## Value

A tibble with columns HSI and NAFLD_LFS.

## Details

Assumptions/units:

- ALT, AST in U/L; BMI in kg/m^2; I0 in pmol/L (converted to muU/mL via
  /6).

- MetS is taken directly if provided; otherwise derived via NCEP-ATP III
  when sufficient inputs exist.

- Type2DM is taken from `diabetes` (logical or 0/1).

## References

Lee J, Kim D, Kim HJ, Lee CH, Yang JI, Kim W, Kim YJ, Yoon J, Cho S,
Sung M, Lee H (2010). “Hepatic steatosis index: a simple screening tool
reflecting nonalcoholic fatty liver disease.” *Digestive and Liver
Disease*, **42**(7), 503–508.
[doi:10.1016/j.dld.2009.08.002](https://doi.org/10.1016/j.dld.2009.08.002)
. Kotronen A, Peltonen M, Hakkarainen A, Sevastianova K, Bergholm R,
Johansson LM, Lundbom N, Rissanen A, Ridderstrale M, Groop L,
Orho-Melander M, Yki-Järvinen H (2009). “Prediction of non-alcoholic
fatty liver disease and liver fat using metabolic and genetic factors.”
*Gastroenterology*, **137**(3), 865–872.
[doi:10.1053/j.gastro.2009.06.005](https://doi.org/10.1053/j.gastro.2009.06.005)
.

## Examples

``` r
df <- data.frame(ALT=20, AST=25, BMI=27, sex="female", diabetes=FALSE, I0=60)
liver_fat_markers(
  df,
  col_map = list(ALT="ALT", AST="AST", BMI="BMI",
                 sex="sex", diabetes="diabetes", I0="I0")
)
#> # A tibble: 1 × 2
#>     HSI NAFLD_LFS
#>   <dbl>     <dbl>
#> 1  35.4        NA
```
