# Calculate saliva-based stress & glycemic markers

Computes:

- log_cortisol_wake (log-transformed waking cortisol)

- CAR_AUC (Cortisol Awakening Response, trapezoidal AUC over 0-60 min by
  default)

- log_amylase (log-transformed salivary alpha-amylase)

- saliva_glucose (raw salivary glucose)

## Usage

``` r
saliva_markers(
  data,
  col_map = list(cort1 = "saliva_cort1", cort2 = "saliva_cort2", cort3 = "saliva_cort3",
    amylase = "saliva_amylase", glucose = "saliva_glucose"),
  verbose = FALSE,
  na_action = c("keep", "omit", "error"),
  na_warn_prop = 0.2,
  check_extreme = FALSE,
  extreme_action = c("warn", "cap", "error", "ignore"),
  extreme_rules = NULL,
  times = c(0, 30, 60)
)
```

## Arguments

- data:

  A data.frame or tibble with salivary markers.

- col_map:

  Named list mapping required inputs. Defaults assume same names:

  - cort1 -\> "saliva_cort1" (nmol/L at wake)

  - cort2 -\> "saliva_cort2" (nmol/L ~30 min)

  - cort3 -\> "saliva_cort3" (nmol/L ~60 min)

  - amylase -\> "saliva_amylase" (U/mL)

  - glucose -\> "saliva_glucose" (mg/dL)

- verbose:

  Logical; if `TRUE`, prints progress messages via hm_inform().

- na_action:

  One of `c("keep","omit","error")` for required-input NA handling.
  Default "keep".

- na_warn_prop:

  Proportion \\\[0,1\]\\ to trigger high-missingness diagnostics
  (debug). Default 0.2.

- check_extreme:

  Logical; if TRUE, scan inputs for extreme values. Default FALSE.

- extreme_action:

  One of `c("warn","cap","error","ignore")` when extremes detected.
  Default "warn".

- extreme_rules:

  Optional named list of c(min,max) bounds. If NULL, broad defaults are
  used (keyed by mapped column names).

- times:

  Numeric vector of sampling times (minutes) for CAR AUC. Must align
  with cort1/2/3. Default c(0,30,60).

## Value

A tibble with columns:

- `log_cortisol_wake`

- `CAR_AUC`

- `log_amylase`

- `saliva_glucose`

## Details

Inputs are validated, missingness handled via `na_action`, logs made
safe (\<= 0 -\> NA), and optional extremes scan/cap is available.

## References

Original derivations Pruessner JC, Kirschbaum C, Meinlschmid G,
Hellhammer DH. Two formulas for computation of the area under the curve
represent measures of total hormone concentration versus time-dependent
change. Psychoneuroendocrinology. 2003;28(7):916-931.
[doi:10.1016/S0306-4530(02)00108-7](https://doi.org/10.1016/S0306-4530%2802%2900108-7)
(AUC hormone measures) Kirschbaum C, Hellhammer DH. Salivary cortisol in
psychoneuroendocrine research: recent developments and applications.
Psychoneuroendocrinology. 1994;19(4):313-333.
[doi:10.1016/0306-4530(94)90013-2](https://doi.org/10.1016/0306-4530%2894%2990013-2)
(Salivary cortisol methods)

Validation and applications Clow A, Thorn L, Evans P, Hucklebridge F.
The awakening cortisol response: methodological issues and significance.
Stress. 2004;7(1):29-37.
[doi:10.1080/10253890410001667205](https://doi.org/10.1080/10253890410001667205)
(Cortisol awakening response) Nater UM, Rohleder N. Salivary
alpha-amylase as a non-invasive biomarker for the sympathetic nervous
system: current state of research. Psychoneuroendocrinology.
2009;34(4):486-496.
[doi:10.1016/j.psyneuen.2009.01.014](https://doi.org/10.1016/j.psyneuen.2009.01.014)
(Salivary alpha-amylase marker) Scales WE, Freeman EW, McCoy NL, Klerman
EB. Salivary glucose as a measure of blood glucose: correlations and
applications. Diabetes Care. 1987;10(4):414-418.
[doi:10.2337/diacare.10.4.414](https://doi.org/10.2337/diacare.10.4.414)
(Salivary glucose application)

## Examples

``` r
df <- tibble::tibble(
  saliva_cort1    = 12.5,
  saliva_cort2    = 18.0,
  saliva_cort3    = 16.2,
  saliva_amylase  = 85,
  saliva_glucose  = 4.2
)
saliva_markers(df)  # uses default col_map
#> # A tibble: 1 × 4
#>   log_cortisol_wake CAR_AUC log_amylase saliva_glucose
#>               <dbl>   <dbl>       <dbl>          <dbl>
#> 1              2.53    970.        4.44            4.2
```
