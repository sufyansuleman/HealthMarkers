# Compute tracer/DXA-based insulin sensitivity indices

Uses stable isotope tracer infusion rates and DXA-measured fat mass to
compute peripheral and adipose insulin sensitivity and related metrics.

## Usage

``` r
tracer_dxa_is(
  data,
  col_map = NULL,
  normalize = NULL,
  na_action = c("keep", "omit", "error"),
  na_warn_prop = 0.2,
  verbose = TRUE
)
```

## Arguments

- data:

  A data.frame or tibble containing raw measurements.

- col_map:

  Named list with entries (depending on mode): Adipose-only required: -
  I0: fasting insulin (pmol/L) - rate_glycerol, rate_palmitate: tracer
  rates (mumol/min) - fat_mass, weight, bmi: body composition - HDL_c:
  HDL cholesterol (mmol/L) Full mode additionally requires: - G0, G30,
  G120: glucose (mmol/L) - I30, I120: insulin (pmol/L) - TG:
  triglycerides (mmol/L) - FFA: free fatty acids (mmol/L)

- normalize:

  Ignored (kept for backward compatibility).

- na_action:

  One of c("keep","omit","error") for NA handling on required inputs.
  Default "keep".

- na_warn_prop:

  Proportion \\\[0,1\]\\ to trigger high-missingness warnings on
  required inputs. Default 0.2.

- verbose:

  Logical; if TRUE, prints progress messages and a completion summary.

## Value

- Adipose-only tibble columns: LIRI_inv, Lipo_inv, ATIRI_inv

- Full-mode tibble columns: I_AUC, FFA_AUC, tracer_palmitate_SI,
  tracer_glycerol_SI, LIRI_inv, Lipo_inv, ATIRI_inv

## Details

Modes:

- Adipose-only indices when only adipose-related keys are mapped (no
  OGTT glucose/insulin time series)

- Full indices otherwise

Expected units:

- Glucose: mmol/L (internally converted to mg/dL when needed)

- Insulin: pmol/L (internally converted to muU/mL via /6)

- TG: mmol/L (to mg/dL via \*88.57); HDL-c: mmol/L (to mg/dL via
  \*38.67)

- Tracer rates: mumol/min

- Fat mass, weight: kg; BMI: kg/m^2

## References

Groop LC, Bonadonna RC, Simonson DC, et al. (1989). “Different Effects
of Insulin and Oral Hypoglycemic Agents on Glucose and Lipid Metabolism
in Type II Diabetes.” *Journal of Clinical Investigation*, **84**(2),
578–585. [doi:10.1172/JCI114142](https://doi.org/10.1172/JCI114142) . ;
Steele R (1959). “Influences of Glucose Loading and of Injected Insulin
on Hepatic Glucose Output.” *Annals of the New York Academy of
Sciences*, **82**(2), 420–430.
[doi:10.1111/j.1749-6632.1959.tb44923.x](https://doi.org/10.1111/j.1749-6632.1959.tb44923.x)
. ; Boston RC, Stefanovski D, Moate PJ, Sumner AE, Watanabe RM, Bergman
RN (2003). “MINMOD Millennium: A Computer Program to Calculate Glucose
Effectiveness and Insulin Sensitivity from the Frequently Sampled
Intravenous Glucose Tolerance Test.” *Diabetes Technology &
Therapeutics*, **5**(6), 1003–1015.
[doi:10.1089/152091503322641060](https://doi.org/10.1089/152091503322641060)
. ; Roden M, Price TB, Perseghin G, et al. (1996). “Mechanism of Free
Fatty Acid-Induced Insulin Resistance in Humans.” *Journal of Clinical
Investigation*, **97**(12), 2859–2865.
[doi:10.1172/JCI118742](https://doi.org/10.1172/JCI118742) . ;
Gastaldelli A, Ferrannini E, Miyazaki Y, Matsuda M, DeFronzo RA (2004).
“Beta-Cell Dysfunction and Glucose Intolerance: Results from the San
Antonio Metabolism Study.” *Diabetologia*, **47**(1), 31–39.
[doi:10.1007/s00125-003-1263-9](https://doi.org/10.1007/s00125-003-1263-9)
. ; Karpe F, Dickmann JR, Frayn KN (2011). “Fatty Acids, Obesity, and
Insulin Resistance: Time for a Reevaluation.” *Diabetes*, **60**(10),
2441–2449. [doi:10.2337/db11-0425](https://doi.org/10.2337/db11-0425) .
; Petersen KF, Dufour S, Savage DB, et al. (2007). “The Role of Skeletal
Muscle Insulin Resistance in the Pathogenesis of the Metabolic
Syndrome.” *Proceedings of the National Academy of Sciences*,
**104**(31), 12587–12594.
[doi:10.1073/pnas.0705408104](https://doi.org/10.1073/pnas.0705408104) .
; Santomauro AT, Boden G, Silva ME, et al. (1999). “Overnight Lowering
of Free Fatty Acids with Acipimox Improves Insulin Resistance and
Glucose Tolerance in Obese Diabetic and Nondiabetic Subjects.”
*Diabetes*, **48**(9), 1836–1841.
[doi:10.2337/diabetes.48.9.1836](https://doi.org/10.2337/diabetes.48.9.1836)
.

## Examples

``` r
df <- data.frame(
  I0 = c(60, 75), rate_glycerol = c(2.1, 2.8), rate_palmitate = c(1.8, 2.3),
  fat_mass = c(18, 24), weight = c(72, 85), BMI = c(24, 29),
  HDL_c = c(1.3, 1.1)
)
col_map <- list(I0="I0", rate_glycerol="rate_glycerol",
                rate_palmitate="rate_palmitate", fat_mass="fat_mass",
                weight="weight", bmi="BMI", HDL_c="HDL_c")
tracer_dxa_is(df, col_map = col_map)
#> tracer_dxa_is(): reading input 'df' — 2 rows × 7 variables
#> tracer_dxa_is(): preparing inputs
#> tracer_dxa_is(): col_map (7 columns — 7 specified)
#>   I0                ->  'I0'
#>   rate_glycerol     ->  'rate_glycerol'
#>   rate_palmitate    ->  'rate_palmitate'
#>   fat_mass          ->  'fat_mass'
#>   weight            ->  'weight'
#>   bmi               ->  'BMI'
#>   HDL_c             ->  'HDL_c'
#> tracer_dxa_is(): computing markers:
#>   LIRI_inv, Lipo_inv, ATIRI_inv
#> tracer_dxa_is(): adipose-only indices
#> tracer_dxa_is(): results: LIRI_inv 2/2, Lipo_inv 2/2, ATIRI_inv 2/2
#> # A tibble: 2 × 3
#>   LIRI_inv Lipo_inv ATIRI_inv
#>      <dbl>    <dbl>     <dbl>
#> 1    -1.01      -21     -18  
#> 2    -1.13      -35     -28.7
```
