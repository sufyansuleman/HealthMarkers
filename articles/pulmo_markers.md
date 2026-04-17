# Pulmonary Markers

## Scope

Compute spirometry-based pulmonary markers using GLI/GLIgl/NHANES3
reference equations from `rspiro`: predicted normals, z-scores,
percent-predicted, LLN for FEV1, FVC, and the FEV1/FVC ratio. Handles
missing-data policies, basic height unit detection, and verbose
diagnostics.

## When to use this

- You need standardized pulmonary markers (predicted, z,
  percent-predicted, LLN) for FEV1, FVC, and FEV1/FVC using established
  reference equations.
- Your dataset has age/sex/height/ethnicity plus measured FEV1 and FVC
  (liters).
- You want controlled handling of missingness and clear warnings on
  implausible inputs (e.g., unmapped sex/ethnicity, zero/negative
  heights).

## What you need (inputs & options)

| Argument     | Purpose / Options                                                        | Notes                         |
|--------------|--------------------------------------------------------------------------|-------------------------------|
| data         | Data frame/tibble with spirometry fields                                 | Required columns listed below |
| equation     | “GLI”, “GLIgl” (ignores ethnicity), or “NHANES3”                         | Default GLI                   |
| na_action    | keep (propagate NA), omit (drop rows), error (abort if required missing) | Default keep                  |
| na_warn_prop | Proportion to trigger high-missingness warnings (default 0.2)            | Applies to required inputs    |
| verbose      | TRUE/FALSE for progress and completion summaries                         | Default FALSE                 |

**Required columns** (names must match exactly): age (years), sex
(“male”/“female” or 1/2 or 0/1), height (cm or m; auto-detected),
ethnicity (text), fev1 (L), fvc (L). No col_map is needed; use these
names.

**Units:** FEV1/FVC in liters. Height auto-detected as cm if any value
\> 3; otherwise treated as meters. No other unit conversions are
performed.

## Handling and expectations

- Validation & coercion: required columns are checked;
  age/height/fev1/fvc coerced to numeric with warnings if NAs are
  introduced.
- Missingness: keep = propagate NA; omit = drop rows with any required
  NA; error = abort if any required NA.
- Height handling: any height \> 3 is treated as cm and divided by 100;
  non-positive heights warn and yield NA outputs.
- Sex/Ethnicity mapping: case-insensitive mapping; unmapped values warn
  and become NA (sex) or Other/Mixed (ethnicity code 5).
- Division by zero/non-finite: ratios set to NA when the denominator is
  zero or non-finite.
- Dependency: requires `rspiro`; aborts if missing or if the chosen
  equation is unsupported.

## Outputs

Tibble (rows follow na_action) with: - fev1_pred, fev1_z, fev1_pctpred,
fev1_LLN - fvc_pred, fvc_z, fvc_pctpred, fvc_LLN - fev1_fvc_ratio,
fev1_fvc_pred, fev1_fvc_z, fev1_fvc_pctpred, fev1_fvc_LLN (LLN currently
NA placeholder) Values are numeric; NA where inputs are missing/invalid
or division-by-zero occurred.

## Worked example 1: GLI (default), keep NAs

``` r
library(HealthMarkers)
library(tibble)

df <- tibble::tibble(
  age = c(45, 62, 30),
  sex = c("male", "female", "female"),
  height = c(178, 162, 1.70),  # mixed cm/m accepted (auto-detect)
  ethnicity = c("Caucasian", "African-American", "Other"),
  fev1 = c(2.8, 1.4, 3.0),
  fvc  = c(3.5, 2.1, 3.8)
)

pulmo_markers(
  data = df,
  equation = "GLI",
  na_action = "keep",
  verbose = TRUE
)
#> # A tibble: 3 × 13
#>   fev1_pred    fev1_z fev1_pctpred fev1_LLN fvc_pred   fvc_z fvc_pctpred fvc_LLN
#>       <dbl>     <dbl>        <dbl>    <dbl>    <dbl>   <dbl>       <dbl>   <dbl>
#> 1  4.09         -2.42         68.5 3.22     5.14     -2.51e0        68.0 4.06e+0
#> 2  2.09         -1.95         67.0 1.51     2.64     -1.24e0        79.6 1.93e+0
#> 3  0.000188 512779.      1592155.  0.000151 0.000115  5.42e4   3303233.  9.31e-5
#> # ℹ 5 more variables: fev1_fvc_ratio <dbl>, fev1_fvc_pred <dbl>,
#> #   fev1_fvc_z <dbl>, fev1_fvc_pctpred <dbl>, fev1_fvc_LLN <dbl>
```

*Interpretation:* Mixed height units are handled; missingness (none
here) would propagate. Outputs include predicted, z, percent-predicted,
and ratios.

## Worked example 2: GLIgl, drop incomplete rows

``` r
df2 <- tibble::tibble(
  age = c(45, 62, 30),
  sex = c(1, 2, 2),
  height = c(178, NA, 170),
  ethnicity = c("Caucasian", "African-American", "Other"),
  fev1 = c(2.8, 1.4, 3.0),
  fvc  = c(3.5, 2.1, 3.8)
)

pulmo_markers(
  data = df2,
  equation = "GLIgl",
  na_action = "omit",
  verbose = TRUE
)
#> # A tibble: 2 × 13
#>   fev1_pred fev1_z fev1_pctpred fev1_LLN fvc_pred  fvc_z fvc_pctpred fvc_LLN
#>       <dbl>  <dbl>        <dbl>    <dbl>    <dbl>  <dbl>       <dbl>   <dbl>
#> 1      3.87 -1.89          72.3     2.94     4.81 -1.91         72.8    3.68
#> 2      3.37 -0.786         89.1     2.59     3.98 -0.330        95.4    3.08
#> # ℹ 5 more variables: fev1_fvc_ratio <dbl>, fev1_fvc_pred <dbl>,
#> #   fev1_fvc_z <dbl>, fev1_fvc_pctpred <dbl>, fev1_fvc_LLN <dbl>
```

*Interpretation:* Row 2 is dropped due to missing height. Ethnicity is
ignored for GLIgl. Outputs are returned only for complete cases.

## Troubleshooting & common pitfalls

- Missing package: install `rspiro` if absent.
- Unsupported equation: ensure equation is GLI, GLIgl, or NHANES3.
- Unit issues: heights must be cm or m; volumes in liters. Convert
  inches or other units upstream.
- Unmapped sex/ethnicity: check spelling/casing; unmapped values warn
  and become NA/Other.
- High missingness: heed `na_warn_prop` warnings; consider
  `na_action = "omit"` for modeling.
- All NA outputs: likely missing required fields, zero/negative heights,
  or failed mapping.

## Verbose diagnostics

``` r
if (requireNamespace("rspiro", quietly = TRUE)) {
  old_opt <- options(healthmarkers.verbose = "inform")
  pulmo_markers(
    data = df,
    equation = "GLI",
    verbose = TRUE
  )
  options(old_opt)
}
#> pulmo_markers(): reading input 'df' — 3 rows × 6 variables
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
#> pulmo_markers(): results: fev1_pred 3/3, fev1_z 3/3, fev1_pctpred 3/3, fev1_LLN 3/3, fvc_pred 3/3, fvc_z 3/3, fvc_pctpred 3/3, fvc_LLN 3/3, fev1_fvc_ratio 3/3, fev1_fvc_pred 3/3, fev1_fvc_z 3/3, fev1_fvc_pctpred 3/3, fev1_fvc_LLN 0/3
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

- Validate units before running; store heights in cm or m and volumes in
  liters.
- Use `verbose = TRUE` during development/QC to see omissions and NA
  counts.
- For modeling, prefer `na_action = "omit"` to avoid NA propagation.
- Spot-check a few rows: `fev1_pctpred` should be
  `100 * fev1 / fev1_pred` when finite; `fev1_fvc_ratio` should match
  `fev1 / fvc`.
- Pre-screen data with
  [`summary()`](https://rdrr.io/r/base/summary.html) to catch extremes
  before calling the function.

## Validation notes

- Expect ratios ~0.6–0.9 in adults; percent-predicted near 100% in
  healthy subjects.
- If many z-scores are extreme, re-check age/height/sex/ethnicity coding
  and units.
- If output is all NA, verify required columns and `rspiro`
  availability.

## See also

- Related vignettes: inflammatory_markers, metabolic_markers,
  nutrient_markers.
- Function docs:
  [`pulmo_markers()`](https://sufyansuleman.github.io/HealthMarkers/reference/pulmo_markers.md)
  and `rspiro` reference equation documentation.
