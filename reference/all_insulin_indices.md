# Compute insulin sensitivity/resistance panels (fasting, OGTT, adipose, tracer/DXA)

Compute insulin sensitivity/resistance panels (fasting, OGTT, adipose,
tracer/DXA)

## Usage

``` r
all_insulin_indices(
  data,
  col_map = NULL,
  normalize = c("none", "z", "inverse", "range", "robust"),
  mode = c("both", "IS", "IR"),
  verbose = TRUE,
  na_action = c("keep", "omit", "error")
)
```

## Arguments

- data:

  A data.frame or tibble of raw measurements.

- col_map:

  Named list with keys
  G0,I0,G30,I30,G120,I120,TG,HDL_c,FFA,waist,weight,bmi,age,sex,rate_palmitate,rate_glycerol,fat_mass.

- normalize:

  One of c("none","z","inverse","range","robust").

- mode:

  One of c("IS","IR","both"). "IR" returns only inverted IR, "IS" only
  the original IS, "both" returns both with IR\_ prefix.

- verbose:

  Logical.

- na_action:

  One of c("keep","omit","error"); forwarded to underlying calculators
  (HM-CS v2).

## Value

A tibble of IS (and/or IR\_) columns.

## Note

For scholarly references to specific indices (e.g., HOMA-IR, QUICKI,
Raynaud, Belfiore, tracer-derived indices, adiposity-related IS
metrics), consult the individual function help pages (e.g. ?fasting_is,
?ogtt_is, ?adipo_is, ?tracer_dxa_is). Citations are intentionally not
duplicated here.

## References

Aggregator wrapper. See underlying function help pages for full
references: fasting_is(), ogtt_is(), adipo_is(), tracer_dxa_is().
Suleman S, Madsen AL, Ängquist LH, Schubert M, Linneberg A, Loos RJF,
Hansen T, Grarup N (2024). “Genetic Underpinnings of Fasting and Oral
Glucose-stimulated Based Insulin Sensitivity Indices.” *The Journal of
Clinical Endocrinology & Metabolism*, **109**(11), 2754–2763.
[doi:10.1210/clinem/dgae275](https://doi.org/10.1210/clinem/dgae275) .

## Examples

``` r
df <- data.frame(
  G0 = 5.2, I0 = 60, G30 = 7.5, I30 = 90, G120 = 6.2, I120 = 80,
  TG = 1.5, HDL_c = 1.3, FFA = 0.3, waist = 85, weight = 70, bmi = 24,
  age = 40, sex = "M", rate_palmitate = 0.1, rate_glycerol = 0.2, fat_mass = 20
)
all_insulin_indices(df, col_map = list(
  G0="G0", I0="I0", G30="G30", I30="I30", G120="G120", I120="I120",
  TG="TG", HDL_c="HDL_c", FFA="FFA", waist="waist", weight="weight",
  bmi="bmi", age="age", sex="sex", rate_palmitate="rate_palmitate",
  rate_glycerol="rate_glycerol", fat_mass="fat_mass"
), normalize = "none", mode = "IS", verbose = FALSE, na_action = "keep")
#> # A tibble: 1 × 41
#>   Fasting_inv Raynaud HOMA_IR_inv  FIRI QUICKI Belfiore_basal Ig_ratio_basal
#>         <dbl>   <dbl>       <dbl> <dbl>  <dbl>          <dbl>          <dbl>
#> 1         -10       4       -41.6  37.4  0.146        0.00213         -0.107
#> # ℹ 34 more variables: Isi_basal <dbl>, Bennett <dbl>, HOMA_IR_rev_inv <dbl>,
#> #   Isi_120 <dbl>, Cederholm_index <dbl>, Gutt_index <dbl>, Avignon_Si0 <dbl>,
#> #   Avignon_Si120 <dbl>, Avignon_Sim <dbl>, Modified_stumvoll <dbl>,
#> #   Stumvoll_Demographics <dbl>, Matsuda_AUC <dbl>, Matsuda_ISI <dbl>,
#> #   BigttSi <dbl>, Ifc_inv <dbl>, HIRI_inv <dbl>, Belfiore_isi_gly <dbl>,
#> #   Revised_QUICKI <dbl>, VAI_Men_inv <dbl>, VAI_Women_inv <dbl>,
#> #   TG_HDL_C_inv <dbl>, TyG_inv <dbl>, LAP_Men_inv <dbl>, …
```
