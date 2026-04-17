# Saliva Markers

## Scope

Compute saliva-based stress and glycemic markers: safe-log waking
cortisol, cortisol awakening response AUC (trapezoidal over 3 samples),
safe-log salivary alpha-amylase, and pass-through salivary glucose.
Includes mapping validation, missingness policies, optional extreme
screening/capping on inputs, and safe handling of non-positive values
before logs.

## When to use this

- You have three salivary cortisol measures around wake (e.g., 0/30/60
  minutes) and want CAR AUC and log cortisol.
- You also collect salivary alpha-amylase (stress marker) and/or
  salivary glucose and need cleaned, log-safe outputs.
- You want explicit NA policies, optional extreme truncation, and
  flexible sampling times for AUC.

## What you need (inputs & options)

| Argument     | Purpose / Options                                     | Notes                                                  |
|--------------|-------------------------------------------------------|--------------------------------------------------------|
| data         | Data frame/tibble with salivary measures              | Columns mapped via `col_map`                           |
| col_map      | Named list mapping required inputs                    | cort1, cort2, cort3 (cortisol); amylase; glucose       |
| na_action    | Missing-data policy for required inputs               | “keep” (default), “omit”, “error”                      |
| na_warn_prop | Proportion threshold for high-missingness diagnostics | Default 0.2 (shown in debug/verbose)                   |
| times        | Sampling times (minutes) for CAR AUC                  | Numeric length 3, non-decreasing; default c(0, 30, 60) |
| verbose      | Emit progress and completion summaries                | Default FALSE                                          |

**Required columns (col_map):** cort1, cort2, cort3 (cortisol nmol/L),
amylase (U/mL), glucose (mg/dL). Mapped columns must exist.

**Units:** No conversion is performed. Supply cortisol in nmol/L,
alpha-amylase in U/mL, salivary glucose in mg/dL.

## Handling and expectations

- Validation & coercion: required columns must exist; non-numeric are
  coerced with warnings when NAs are introduced; non-finite become NA.
- Missingness: `keep` propagates NA; `omit` drops rows with any required
  NA; `error` aborts if required inputs contain NA.
- High-missing diagnostics: controlled by `na_warn_prop`; shown when
  `verbose` (or debug) is enabled.
- Safe logs: cortisol and amylase are log-transformed after setting
  non-positive to NA.
- CAR AUC: trapezoidal over the three cortisol values using `times`; any
  non-finite cortisol in the triplet yields NA for that row.
- Times: must be numeric length 3, non-decreasing; customize for your
  protocol.
- No unit conversion: ensure assays are in consistent units before
  calling.

## Defaults and validation details

- NA diagnostics appear only with verbose/debug logging; set
  `verbose = TRUE` during QC.
- Empty result: if `na_action = "omit"` drops all rows, returns a
  zero-row tibble with expected columns.

## Outputs

- Returns a tibble (rows follow `na_action`) with: log_cortisol_wake,
  CAR_AUC, log_amylase, saliva_glucose.
- NA indicates missing/invalid inputs, non-positive values before log,
  NA in any cortisol for CAR_AUC, or values dropped via `na_action`.

## Worked example 1: Standard times, keep NAs

``` r
library(HealthMarkers)
library(tibble)

df <- tibble::tibble(
  saliva_cort1 = c(12.5, 9.0, NA),
  saliva_cort2 = c(18.0, 11.0, 7.5),
  saliva_cort3 = c(16.2, 10.5, 6.8),
  saliva_amylase = c(85, 120, 95),
  saliva_glucose = c(4.2, 5.0, 4.8)
)

saliva_markers(
  data = df,
  col_map = list(
    cort1 = "saliva_cort1",
    cort2 = "saliva_cort2",
    cort3 = "saliva_cort3",
    amylase = "saliva_amylase",
    glucose = "saliva_glucose"
  ),
  na_action = "keep",
  verbose = TRUE
)
#> # A tibble: 3 × 4
#>   log_cortisol_wake CAR_AUC log_amylase saliva_glucose
#>               <dbl>   <dbl>       <dbl>          <dbl>
#> 1              2.53    970.        4.44            4.2
#> 2              2.20    622.        4.79            5  
#> 3             NA        NA         4.55            4.8
```

*Interpretation:* CAR_AUC and log cortisol/amylase compute where inputs
are present and positive; the third row yields NA for log_cortisol_wake
and CAR_AUC due to missing cort1.

## Worked example 2: Custom times, drop incomplete

``` r
df2 <- tibble::tibble(
  c1 = c(14, 0, 2200),    # zero and extreme cortisol; pre-filter
  c2 = c(19, 12, 2100),
  c3 = c(17, 11, 2050),
  amy = c(90, 70, 60000),  # extreme amylase; pre-filter
  glu = c(4.5, 5.1, 7.0)
)

# Pre-filter implausible values before calling
df2$c1[df2$c1 > 2000] <- NA
df2$amy[df2$amy > 50000] <- NA

saliva_markers(
  data = df2,
  col_map = list(cort1 = "c1", cort2 = "c2", cort3 = "c3", amylase = "amy", glucose = "glu"),
  times = c(0, 20, 50),
  na_action = "omit",
  verbose = TRUE
)
#> # A tibble: 2 × 4
#>   log_cortisol_wake CAR_AUC log_amylase saliva_glucose
#>               <dbl>   <dbl>       <dbl>          <dbl>
#> 1              2.64     870        4.50            4.5
#> 2             NA        465        4.25            5.1
```

*Interpretation:* Rows with required NAs are dropped; CAR_AUC uses the
custom times (0,20,50).

## Troubleshooting & common pitfalls

- Missing columns: ensure every required `col_map` key is mapped to an
  existing column.
- Non-positive cortisol/amylase: become NA before logging; expect NA
  outputs if values are 0 or negative.
- Times validation: must be numeric length 3 and non-decreasing;
  otherwise the function aborts.
- All NA outputs: typically due to missing cortisol triplets,
  non-positive cortisol/amylase, or aggressive `na_action = "omit"`.

## Verbose diagnostics

Enable verbose output to inspect column mapping, row counts, and result
summaries during QC:

``` r
old_opt <- options(healthmarkers.verbose = "inform")
saliva_markers(
  tibble::tibble(
    saliva_cort1   = 12, saliva_cort2   = 20, saliva_cort3   = 14,
    saliva_amylase = 150, saliva_glucose = 5.5
  ),
  verbose = TRUE
)
#> saliva_markers(): reading input 'data' — 1 rows × 5 variables
#> saliva_markers(): col_map (5 columns — 5 inferred from data)
#>   cort1             ->  'saliva_cort1'    (inferred)
#>   cort2             ->  'saliva_cort2'    (inferred)
#>   cort3             ->  'saliva_cort3'    (inferred)
#>   amylase           ->  'saliva_amylase'    (inferred)
#>   glucose           ->  'saliva_glucose'    (inferred)
#> saliva_markers(): optional inputs
#>   present:  cort1, cort2, cort3, amylase, glucose
#> saliva_markers(): computing markers:
#>   log_cortisol_wake    [cort1]
#>   CAR_AUC              [cort1, cort2, cort3]
#>   log_amylase          [amylase]
#>   saliva_glucose       [glucose]
#> saliva_markers(): results: log_cortisol_wake 1/1, CAR_AUC 1/1, log_amylase 1/1, saliva_glucose 1/1
#> # A tibble: 1 × 4
#>   log_cortisol_wake CAR_AUC log_amylase saliva_glucose
#>               <dbl>   <dbl>       <dbl>          <dbl>
#> 1              2.48     990        5.01            5.5
options(old_opt)
```

## Column recognition

Run `hm_col_report(your_data)` to check which analyte columns are
auto-detected before building your `col_map`. See the [Multi-Biobank
Compatibility](https://sufyansuleman.github.io/HealthMarkers/articles/multi_biobank.md)
article for recognised synonyms across major biobanks.

``` r
hm_col_report(your_data)
```

## Tips for best results

- Confirm assay units upfront; no conversions are applied.
- Set `na_action = "omit"` for modeling datasets; `keep` is better for
  exploratory review.
- Provide accurate sampling times in minutes for CAR AUC if your
  protocol differs from 0/30/60.

## Validation notes

- CAR_AUC is trapezoidal:
  $\sum_{i = 1}^{2}\frac{c_{i} + c_{i + 1}}{2} \times \left( t_{i + 1} - t_{i} \right)$
  using the three cortisol values and the `times` gaps.
- Safe logs: $\log(x)$ is only taken when $x > 0$ and finite; otherwise
  NA.

## See also

- Function docs:
  [`?saliva_markers`](https://sufyansuleman.github.io/HealthMarkers/reference/saliva_markers.md)
- Related vignettes: health_markers, health_summary, stress-related
  markers (e.g., inflammatory_markers)
