# Calculate OGTT-based insulin sensitivity indices

Given glucose & insulin at 0, 30, 120 min (plus weight, BMI, age, sex),
computes:

- Isi_120

- Cederholm_index

- Gutt_index

- Avignon_Si0

- Avignon_Si120

- Avignon_Sim

- Modified_stumvoll

- Stumvoll_Demographics

- Matsuda_AUC

- Matsuda_ISI

- BigttSi

- Ifc_inv

- HIRI_inv

- Belfiore_isi_gly

## Usage

``` r
ogtt_is(
  data,
  col_map,
  normalize = "none",
  verbose = FALSE,
  na_action = c("keep", "omit", "error"),
  na_warn_prop = 0.2,
  check_extreme = FALSE,
  extreme_limit = 1000,
  extreme_action = c("warn", "cap", "error", "ignore", "NA")
)
```

## Arguments

- data:

  A data.frame or tibble containing at least the columns mapped by
  `col_map`.

- col_map:

  Named list mapping:

  - G0, G30, G120 -\> glucose at 0, 30, 120 min (mmol/L)

  - I0, I30, I120 -\> insulin at 0, 30, 120 min (pmol/L)

  - weight -\> body weight (kg)

  - bmi -\> body-mass index (kg/m^2)

  - age -\> age (years)

  - sex -\> sex (1 = male, 2 = female)

- normalize:

  One of c("none","z","inverse","range","robust") used by
  normalize_vec().

- verbose:

  Logical; if TRUE, prints progress messages via hm_inform().

- na_action:

  One of c("keep","omit","error") for missing/non-finite required
  inputs. Default "keep".

- na_warn_prop:

  Proportion (0-1) for high-missingness diagnostics (debug). Default
  0.2.

- check_extreme:

  Logical; if TRUE, scan outputs for \|value\| \> extreme_limit. Default
  FALSE.

- extreme_limit:

  Positive numeric magnitude threshold for extremes. Default 1e3.

- extreme_action:

  One of c("warn","cap","error","ignore","NA") when extremes detected.
  Default "warn".

## Value

A tibble with the OGTT-based index columns listed above.

## Details

Units assumed:

- OGTT glucose in mmol/L (internally converted to mg/dL via \*18 for
  select indices)

- OGTT insulin in pmol/L (internally converted to muU/mL via /6 for
  select indices)

- weight in kg; BMI in kg/m^2; age in years; sex coded 1 = male, 2 =
  female

Notes

- Conversions mirror existing implementation to preserve outputs. Some
  formulas intentionally use unconverted inputs (as in prior code).

- Logs are safe: log(x) becomes NA when x \<= 0 or non-finite.

## References

Matsuda M, DeFronzo RA (1999). “Insulin Sensitivity Indices Obtained
from Oral Glucose Tolerance Testing: Comparison with the Minimal Model
Assessment.” *Diabetes Care*, **22**(9), 1462–1470.
[doi:10.2337/diacare.22.9.1462](https://doi.org/10.2337/diacare.22.9.1462)
. ; Gutt M, Davis CL, Spitzer SB, et al. (2000). “Validation of the
Insulin Sensitivity Index (ISI\\\_{0,120}\\) Derived from Oral Glucose
Tolerance Testing.” *Diabetes Research and Clinical Practice*,
**47**(3), 177–184.
[doi:10.1016/S0168-8227(99)00116-3](https://doi.org/10.1016/S0168-8227%2899%2900116-3)
. ; Stumvoll M, Mitrakou A, Pimenta W, et al. (2000). “Use of the Oral
Glucose Tolerance Test to Assess Insulin Release and Sensitivity.”
*Diabetes Care*, **23**(3), 295–301.
[doi:10.2337/diacare.23.3.295](https://doi.org/10.2337/diacare.23.3.295)
. ; Hansen T, Drivsholm T, Urhammer SA, Palacios RR, et al. (2007). “The
BIGTT Test.” *Diabetes Care*, **30**(2), 257–262.
[doi:10.2337/dc06-1240](https://doi.org/10.2337/dc06-1240) . ; Avignon
A, Charles M, Rabasa-Lhoret R, et al. (1999). “Assessment of Insulin
Sensitivity from Oral Glucose Tolerance Test in Normal Subjects and in
Insulin-Resistant Patients.” *International Journal of Obesity*,
**23**(5), 512–517.
[doi:10.1038/sj.ijo.0800864](https://doi.org/10.1038/sj.ijo.0800864) . ;
Belfiore F, Iannello S, Volpicelli G (1998). “Insulin Sensitivity
Indices Calculated from Basal and OGTT-Related Insulin and Glucose
Levels.” *Molecular Genetics and Metabolism*, **63**(2), 134–141.
[doi:10.1006/mgme.1997.2658](https://doi.org/10.1006/mgme.1997.2658) . ;
Matthews DR, Hosker JP, Rudenski AS, Naylor BA, Treacher DF, Turner RC
(1985). “Homeostasis Model Assessment: Insulin Resistance and Beta-Cell
Function from Fasting Plasma Glucose and Insulin Concentrations in Man.”
*Diabetologia*, **28**(7), 412–419.
[doi:10.1007/BF00280883](https://doi.org/10.1007/BF00280883) . ; Suleman
S, Madsen AL, Ängquist LH, Schubert M, Linneberg A, Loos RJF, Hansen T,
Grarup N (2024). “Genetic Underpinnings of Fasting and Oral
Glucose-stimulated Based Insulin Sensitivity Indices.” *The Journal of
Clinical Endocrinology & Metabolism*, **109**(11), 2754–2763.
[doi:10.1210/clinem/dgae275](https://doi.org/10.1210/clinem/dgae275) .

## See also

[`fasting_is()`](https://sufyansuleman.github.io/HealthMarkers/reference/fasting_is.md),
[`normalize_vec()`](https://sufyansuleman.github.io/HealthMarkers/reference/normalize_vec.md)

## Examples

``` r
df <- tibble::tibble(
  G0 = 5.5, I0 = 60,
  G30 = 7.8, I30 = 90,
  G120 = 6.2, I120 = 50,
  weight = 70, bmi = 24, age = 30, sex = 1
)
ogtt_is(
  df,
  col_map = list(
    G0 = "G0", I0 = "I0",
    G30 = "G30", I30 = "I30",
    G120 = "G120", I120 = "I120",
    weight = "weight", bmi = "bmi",
    age = "age", sex = "sex"
  ),
  normalize = "none",
  verbose = TRUE
)
#> ogtt_is(): preparing inputs
#> ogtt_is(): column map: G0 -> 'G0', I0 -> 'I0', G30 -> 'G30', I30 -> 'I30', G120 -> 'G120', I120 -> 'I120', weight -> 'weight', bmi -> 'bmi', age -> 'age', sex -> 'sex'
#> ogtt_is(): results: Isi_120 1/1, Cederholm_index 1/1, Gutt_index 1/1, Avignon_Si0 1/1, Avignon_Si120 1/1, Avignon_Sim 1/1, Modified_stumvoll 1/1, Stumvoll_Demographics 1/1, Matsuda_AUC 1/1, Matsuda_ISI 1/1, BigttSi 1/1, Ifc_inv 1/1, HIRI_inv 1/1, Belfiore_isi_gly 1/1
#> # A tibble: 1 × 14
#>   Isi_120 Cederholm_index Gutt_index Avignon_Si0 Avignon_Si120 Avignon_Sim
#>     <dbl>           <dbl>      <dbl>       <dbl>         <dbl>       <dbl>
#> 1    10.8            1.10       2.67        9.62          10.2        9.93
#> # ℹ 8 more variables: Modified_stumvoll <dbl>, Stumvoll_Demographics <dbl>,
#> #   Matsuda_AUC <dbl>, Matsuda_ISI <dbl>, BigttSi <dbl>, Ifc_inv <dbl>,
#> #   HIRI_inv <dbl>, Belfiore_isi_gly <dbl>
```
