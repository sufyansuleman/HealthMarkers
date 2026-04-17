# Calculate pulmonary function markers (FEV1/FVC, z-scores, percent predicted, LLN, etc.)

Uses the `rspiro` reference equations to compute predicted normals,
z-scores, percent predicted and lower limits of normal (LLN) for FEV1,
FVC, and the FEV1/FVC ratio.

## Usage

``` r
pulmo_markers(
  data,
  col_map = NULL,
  equation = c("GLI", "GLIgl", "NHANES3"),
  na_action = c("keep", "omit", "error"),
  na_warn_prop = 0.2,
  verbose = TRUE
)
```

## Arguments

- data:

  A data.frame or tibble with columns:

  - `age` (numeric): years

  - `sex` (character or numeric): "male"/"female" (case-insensitive) or
    codes 1/2 or 0/1

  - `height` (numeric): cm or m (auto-detected)

  - `ethnicity` (character): e.g. "Caucasian", "African-American", "NE
    Asian", "SE Asian", "Other/Mixed"

  - `fev1` (numeric): observed FEV1 in L

  - `fvc` (numeric): observed FVC in L

- col_map:

  Optional named list mapping canonical keys (`age`, `sex`, `height`,
  `ethnicity`, `fev1`, `fvc`) to actual column names in `data`. If
  `NULL`, column names are inferred automatically.

- equation:

  One of `c("GLI","GLIgl","NHANES3")` (see `rspiro` for details). GLIgl
  ignores ethnicity.

- na_action:

  One of `c("keep","omit","error")` for handling missing values in
  required inputs. Default "keep".

- na_warn_prop:

  Proportion \\\[0,1\]\\ to trigger high-missingness warnings on
  required inputs. Default 0.2.

- verbose:

  Logical; if `TRUE` prints progress messages and a completion summary.

## Value

A tibble with columns:

- `fev1_pred`, `fev1_z`, `fev1_pctpred`, `fev1_LLN`

- `fvc_pred`, `fvc_z`, `fvc_pctpred`, `fvc_LLN`

- `fev1_fvc_ratio`, `fev1_fvc_pred`, `fev1_fvc_z`, `fev1_fvc_pctpred`,
  `fev1_fvc_LLN`

## Details

Inputs are validated, missingness is handled via `na_action`, and
heights are auto-detected as cm when any height \> 3; otherwise
interpreted as metres (no automatic unit conversion beyond that
heuristic, preserving prior behavior).

## References

Quanjer PH, Stanojevic S, Cole TJ, Baur X, Hall GL, Culver BH, et al.
(2012). “Multi-ethnic reference values for spirometry for the 3–95-yr
age range: the global lung function 2012 equations.” *European
Respiratory Journal*, **40**, 1324–1343.
[doi:10.1183/09031936.00080312](https://doi.org/10.1183/09031936.00080312)
. Hankinson JL, Odencrantz JR, Fedan KB (1999). “Spirometric reference
values from a sample of the general U.S. population.” *American Journal
of Respiratory and Critical Care Medicine*, **159**, 179–187.
[doi:10.1164/ajrccm.159.1.9712108](https://doi.org/10.1164/ajrccm.159.1.9712108)
. Bowerman SD, Quanjer PH, others (2023). “GLI-Global update.” *European
Respiratory Journal*, **61**, 2201632. DOI not resolvable in Crossref as
of 2026-03-16.

## See also

rspiro

## Examples

``` r
if (requireNamespace("rspiro", quietly = TRUE)) {
  df <- data.frame(
    age = c(40, 55), sex = c("male", "female"),
    height = c(175, 162), ethnicity = c("Caucasian", "Caucasian"),
    fev1 = c(3.5, 2.4), fvc = c(4.4, 3.1)
  )
  pulmo_markers(df)
}
#> pulmo_markers(): reading input 'df' — 2 rows × 6 variables
#> pulmo_markers(): preparing inputs [GLI]
#> pulmo_markers(): col_map (6 columns — 6 inferred from data)
#>   age               ->  'age'    (inferred)
#>   sex               ->  'sex'    (inferred)
#>   height            ->  'height'    (inferred)
#>   ethnicity         ->  'ethnicity'    (inferred)
#>   fev1              ->  'fev1'    (inferred)
#>   fvc               ->  'fvc'    (inferred)
#> pulmo_markers(): computing markers:
#>   fev1_pred, fev1_z, fev1_pctpred, fev1_LLN [age, height, sex, ethnicity, fev1]
#>   fvc_pred, fvc_z, fvc_pctpred, fvc_LLN [age, height, sex, ethnicity, fvc]
#>   fev1_fvc_ratio, fev1_fvc_pred, fev1_fvc_z, fev1_fvc_pctpred, fev1_fvc_LLN [fev1, fvc]
#> pulmo_markers(): converting height from cm to m
#> pulmo_markers(): results: fev1_pred 2/2, fev1_z 2/2, fev1_pctpred 2/2, fev1_LLN 2/2, fvc_pred 2/2, fvc_z 2/2, fvc_pctpred 2/2, fvc_LLN 2/2, fev1_fvc_ratio 2/2, fev1_fvc_pred 2/2, fev1_fvc_z 2/2, fev1_fvc_pctpred 2/2, fev1_fvc_LLN 0/2
#> # A tibble: 2 × 13
#>   fev1_pred fev1_z fev1_pctpred fev1_LLN fvc_pred  fvc_z fvc_pctpred fvc_LLN
#>       <dbl>  <dbl>        <dbl>    <dbl>    <dbl>  <dbl>       <dbl>   <dbl>
#> 1      4.08 -1.13          85.8     3.23     5.05 -1.04         87.0    4.02
#> 2      2.62 -0.595         91.7     2.01     3.29 -0.410        94.2    2.54
#> # ℹ 5 more variables: fev1_fvc_ratio <dbl>, fev1_fvc_pred <dbl>,
#> #   fev1_fvc_z <dbl>, fev1_fvc_pctpred <dbl>, fev1_fvc_LLN <dbl>
```
