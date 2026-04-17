# OGTT Insulin Sensitivity

## Scope

Compute OGTT-based insulin sensitivity indices (Matsuda, Cederholm,
Gutt, Stumvoll variants, BIGTT, HIRI_inv, Belfiore, etc.) from
glucose/insulin at 0/30/120 minutes plus weight, BMI, age, and sex.
Includes validation, missingness policies, and optional normalization of
the returned indices.

## When to use this

- You have OGTT glucose (mmol/L) and insulin (pmol/L) at 0, 30, 120
  minutes, plus weight, BMI, age, and sex, and need multiple published
  insulin sensitivity indices.
- You want consistent NA handling and optional normalization for
  modeling pipelines.

## What you need (inputs & options)

| Argument     | Purpose / Options                                  | Notes                                                                                                                                   |
|--------------|----------------------------------------------------|-----------------------------------------------------------------------------------------------------------------------------------------|
| data         | Data frame/tibble with OGTT labs and demographics  | Columns mapped via `col_map`                                                                                                            |
| col_map      | Named list mapping required keys                   | G0/G30/G120 (glucose mmol/L); I0/I30/I120 (insulin pmol/L); weight (kg); bmi (kg/m^2); age (years); sex (1=male, 2=female; 0/1 coerced) |
| normalize    | Output normalization method                        | “none”, “z”, “inverse”, “range”, “robust” (default “none”)                                                                              |
| na_action    | Missing-data policy for required inputs            | “keep” (default), “omit”, “error”                                                                                                       |
| na_warn_prop | Proportion to trigger high-missingness diagnostics | Default 0.2 (emitted in debug/verbose)                                                                                                  |
| verbose      | Emit progress and completion summaries             | Default FALSE                                                                                                                           |

**Units:** Inputs must already be in mmol/L (glucose) and pmol/L
(insulin); weight kg; BMI kg/m^2; age years; sex coded 1/2 (0/1
coerced). The function internally converts glucose to mg/dL (x18) and
insulin to muU/mL (/6) where specific formulas require it.

## Handling and expectations

- Validation: required columns must exist; non-numeric inputs are
  coerced with warnings; non-finite values become NA.
- Missingness: `keep` propagates NA; `omit` drops rows with any required
  NA; `error` aborts if required inputs contain NA (before computation).
- High-missing diagnostics: `na_warn_prop` controls debug/verbose
  messages on missingness; enable `verbose = TRUE` to see them.
- Unit handling: inputs are expected in mmol/L (glucose) and pmol/L
  (insulin); internally converted to mg/dL (\*18) and muU/mL (/6) where
  formulas need them.
- Normalization (optional): applied via
  [`normalize_vec()`](https://sufyansuleman.github.io/HealthMarkers/reference/normalize_vec.md);
  use “none” for raw interpretable values.
- Sex coding: expects 1 = male, 2 = female; 0 is treated as female after
  coercion; other strings become NA.

## Outputs

- Returns a tibble with: Isi_120, Cederholm_index, Gutt_index,
  Avignon_Si0, Avignon_Si120, Avignon_Sim, Modified_stumvoll,
  Stumvoll_Demographics, Matsuda_AUC, Matsuda_ISI, BigttSi, Ifc_inv,
  HIRI_inv, Belfiore_isi_gly.
- NA indicates missing/invalid inputs, zero/negative arguments to logs,
  or values blanked by normalization after NA inputs.

## Defaults and validation details

- Output screening happens before normalization; outputs are then
  normalized if `normalize != "none"`.
- High-missingness diagnostics are only printed when verbose/debug
  logging is on.
- Zero/negative inputs to log-based terms become NA by design (safe log
  guard).

## Worked example 1: Basic usage (raw outputs)

``` r
library(HealthMarkers)
library(tibble)

df <- tibble::tibble(
  G0 = 5.5, I0 = 60,
  G30 = 7.8, I30 = 90,
  G120 = 6.2, I120 = 50,
  weight = 70, bmi = 24, age = 30, sex = 1
)

ogtt_is(
  data = df,
  col_map = list(
    G0 = "G0", I0 = "I0",
    G30 = "G30", I30 = "I30",
    G120 = "G120", I120 = "I120",
    weight = "weight", bmi = "bmi",
    age = "age", sex = "sex"
  ),
  normalize = "none",
  na_action = "keep"
)
#> # A tibble: 1 × 14
#>   Isi_120 Cederholm_index Gutt_index Avignon_Si0 Avignon_Si120 Avignon_Sim
#>     <dbl>           <dbl>      <dbl>       <dbl>         <dbl>       <dbl>
#> 1    10.8            1.10       2.67        9.62          10.2        9.93
#> # ℹ 8 more variables: Modified_stumvoll <dbl>, Stumvoll_Demographics <dbl>,
#> #   Matsuda_AUC <dbl>, Matsuda_ISI <dbl>, BigttSi <dbl>, Ifc_inv <dbl>,
#> #   HIRI_inv <dbl>, Belfiore_isi_gly <dbl>
```

*Interpretation:* Returns all indices with NA only where inputs are
missing or non-finite.

## Worked example 2: Normalization

``` r
ogtt_is(
  data = df,
  col_map = list(
    G0 = "G0", I0 = "I0",
    G30 = "G30", I30 = "I30",
    G120 = "G120", I120 = "I120",
    weight = "weight", bmi = "bmi",
    age = "age", sex = "sex"
  ),
  normalize = "z",
  na_action = "keep",
  verbose = TRUE
)
#> # A tibble: 1 × 14
#>   Isi_120 Cederholm_index Gutt_index Avignon_Si0 Avignon_Si120 Avignon_Sim
#>     <dbl>           <dbl>      <dbl>       <dbl>         <dbl>       <dbl>
#> 1       0               0          0           0             0           0
#> # ℹ 8 more variables: Modified_stumvoll <dbl>, Stumvoll_Demographics <dbl>,
#> #   Matsuda_AUC <dbl>, Matsuda_ISI <dbl>, BigttSi <dbl>, Ifc_inv <dbl>,
#> #   HIRI_inv <dbl>, Belfiore_isi_gly <dbl>
```

*Interpretation:* Outputs are z-scaled; verbose logs summarize
column-map resolution and result counts.

## Troubleshooting & common pitfalls

- Missing YAML: ensure this header stays at the top; Markdown before it
  will break knitting.
- Missing columns: every required `col_map` key must exist in `data` and
  be mapped.
- Units: inputs must be mmol/L (glucose) and pmol/L (insulin); only
  internal conversions are applied for formulas.
- Sex coding: expects 1/2; 0/1 are coerced; other strings become NA and
  propagate.
- All NA outputs: usually missing required inputs, zero/negative values
  passed to logs, or NA produced by normalization after NA inputs.

## Verbose diagnostics

``` r
old_opt <- options(healthmarkers.verbose = "inform")
ogtt_is(
  data = df,
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
#> ogtt_is(): reading input 'df' — 1 rows × 10 variables
#> ogtt_is(): col_map (10 columns — 10 specified)
#>   G0                ->  'G0'
#>   I0                ->  'I0'
#>   G30               ->  'G30'
#>   I30               ->  'I30'
#>   G120              ->  'G120'
#>   I120              ->  'I120'
#>   weight            ->  'weight'
#>   bmi               ->  'bmi'
#>   age               ->  'age'
#>   sex               ->  'sex'
#> ogtt_is(): computing markers:
#>   Isi_120                [G120, I120]
#>   Cederholm_index        [G0, G120, I0, I120, weight]
#>   Gutt_index             [G0, G120, I0, I120, weight]
#>   Avignon_Si0            [G0, I0, weight]
#>   Avignon_Si120          [G120, I120, weight]
#>   Avignon_Sim            [Si0, Si120]
#>   Modified_stumvoll      [I0, I120, G120]
#>   Stumvoll_Demog.        [bmi, I120, age]
#>   Matsuda_AUC            [G0, I0, G_AUC, I_AUC]
#>   Matsuda_ISI            [G0, I0, G_mean, I_mean]
#>   BigttSi                [I0, I30, I120, G0, G30, G120, sex, bmi]
#>   Ifc_inv                [I0, I120]
#>   HIRI_inv               [G0, G30, I0, I30]
#>   Belfiore_isi_gly       [I_AUC, G_AUC]
#> ogtt_is(): results: Isi_120 1/1, Cederholm_index 1/1, Gutt_index 1/1, Avignon_Si0 1/1, Avignon_Si120 1/1, Avignon_Sim 1/1, Modified_stumvoll 1/1, Stumvoll_Demographics 1/1, Matsuda_AUC 1/1, Matsuda_ISI 1/1, BigttSi 1/1, Ifc_inv 1/1, HIRI_inv 1/1, Belfiore_isi_gly 1/1
#> # A tibble: 1 × 14
#>   Isi_120 Cederholm_index Gutt_index Avignon_Si0 Avignon_Si120 Avignon_Sim
#>     <dbl>           <dbl>      <dbl>       <dbl>         <dbl>       <dbl>
#> 1    10.8            1.10       2.67        9.62          10.2        9.93
#> # ℹ 8 more variables: Modified_stumvoll <dbl>, Stumvoll_Demographics <dbl>,
#> #   Matsuda_AUC <dbl>, Matsuda_ISI <dbl>, BigttSi <dbl>, Ifc_inv <dbl>,
#> #   HIRI_inv <dbl>, Belfiore_isi_gly <dbl>
options(old_opt)
```

## Tips for best results

- Keep `normalize = "none"` for clinical interpretation; use “z” or
  “robust” only when scaling is needed for modeling.
- Use `na_action = "omit"` for modeling-ready datasets; `keep` for
  exploratory review.
- Inspect distributions of glucose and insulin values to catch unit
  errors before running.

## Validation notes

- Glucose values are multiplied by 18 and insulin divided by 6 inside
  formulas that require mg/dL or muU/mL.
- Logs are safe: nonpositive arguments become NA.
- Normalization occurs after computing raw indices; disable to mirror
  published formulas.

## See also

- Function documentation:
  [`?ogtt_is`](https://sufyansuleman.github.io/HealthMarkers/reference/ogtt_is.md),
  [`?normalize_vec`](https://sufyansuleman.github.io/HealthMarkers/reference/normalize_vec.md)
- Related vignettes: fasting_is, health_markers, glycemic_markers
