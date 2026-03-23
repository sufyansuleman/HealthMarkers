# Metabolic Syndrome Severity Score (MetSSS)

Computes a continuous metabolic syndrome severity z-score using sex- and
race-specific standardized components and coefficients (factor-loading
style).

Behavior note:

- Parameters are selected using ONLY the first row's (race, sex) key
  (for backward compatibility). A warning is issued if multiple keys
  present.

Required columns (no unit conversion performed):

- waist (cm), bp_sys (mmHg), bp_dia (mmHg)

- TG, HDL_c, glucose (mmol/L)

- sex (1=male, 2=female)

- race (one of "NHW","NHB","HW","HA" or accepted synonyms)

## Usage

``` r
metss(
  data,
  params = list(NHW_M = list(intercept = -2.344, waist = c(mean = 94, sd = 12.4, coef =
    0.846), TG = c(mean = 1.5, sd = 0.6, coef = 0.701), HDL = c(mean = 1.1, sd = 0.3,
    coef = -0.663), glucose = c(mean = 5.3, sd = 0.6, coef = 0.658), MAP = c(mean = 97,
    sd = 11, coef = 0.466))),
  verbose = FALSE,
  na_action = c("keep", "omit", "error", "ignore", "warn"),
  na_warn_prop = 0.2,
  check_extreme = FALSE,
  extreme_action = c("warn", "cap", "error", "ignore", "NA"),
  extreme_rules = NULL,
  diagnostics = TRUE
)
```

## Arguments

- data:

  data.frame / tibble.

- params:

  Named list keyed by "RACE_SEX" (e.g. "NHW_M"). Each element:
  list(intercept, waist, TG, HDL, glucose, MAP) where each component
  (except intercept) is a named numeric vector c(mean=, sd=, coef=).

- verbose:

  Logical; print progress.

- na_action:

  One of c("keep","omit","error","ignore","warn") for required-input
  NAs. Default "keep".

- na_warn_prop:

  Proportion (0-1) above which high-missingness warning fires when
  na_action='warn'. Default 0.2.

- check_extreme:

  Logical; scan for extreme values if TRUE.

- extreme_action:

  One of c("warn","cap","error","ignore","NA") when extremes detected.
  Default "warn".

- extreme_rules:

  Optional named list of c(min,max) for inputs (waist, bp_sys, bp_dia,
  TG, HDL_c, glucose).

- diagnostics:

  Logical; if TRUE (default) emit value/range diagnostic warnings
  (negative, out-of-range checks). Set FALSE to suppress these (e.g., in
  tests when also using check_extreme).

## Value

tibble with one numeric column: MetSSS

## Details

Calculate Metabolic Syndrome Severity Score (MetSSS)

## References

Gurka MJ, Lilly CL, Oliver MN, DeBoer MD (2014). “An examination of sex
and racial/ethnic differences in the metabolic syndrome among adults: A
confirmatory factor analysis and a resulting continuous severity score.”
*Metabolism*, **63**(2), 218–225.
[doi:10.1016/j.metabol.2013.10.006](https://doi.org/10.1016/j.metabol.2013.10.006)
. DeBoer MD, Gurka MJ, Woo JG, Morrison JA (2015). “Severity of
metabolic syndrome and its association with risk for type 2 diabetes and
cardiovascular disease.” *Diabetologia*, **58**(12), 2745–2752.
[doi:10.1007/s00125-015-3759-5](https://doi.org/10.1007/s00125-015-3759-5)
. DeBoer MD, Filipp SL, Gurka MJ (2017). “Independent associations
between metabolic syndrome severity and future coronary heart disease by
sex and race.” *Journal of the American College of Cardiology*,
**69**(9), 1204–1205.
[doi:10.1016/j.jacc.2016.10.088](https://doi.org/10.1016/j.jacc.2016.10.088)
. Gurka MJ, Filipp SL, Pearson TA, DeBoer MD (2018). “Assessing Baseline
and Temporal Changes in Cardiometabolic Risk Using Metabolic Syndrome
Severity and Common Risk Scores.” *Journal of the American Heart
Association*, **7**(16), e009754.
[doi:10.1161/JAHA.118.009754](https://doi.org/10.1161/JAHA.118.009754) .

## Examples

``` r
df <- data.frame(
  waist = 95, bp_sys = 120, bp_dia = 80, TG = 1.5, HDL_c = 1.2,
  glucose = 5.5, sex = 1, race = "NHW", age = 45
)
metss(df)
#> # A tibble: 1 × 1
#>   MetSSS
#>    <dbl>
#> 1  -2.43
```
