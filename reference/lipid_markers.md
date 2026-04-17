# Calculate lipid-panel markers, Visceral Adiposity Index (VAI), Lipid Accumulation Product (LAP), and TyG-BMI index

Given total cholesterol, HDL, TG (and optionally LDL, ApoB/ApoA1, waist,
BMI, glucose), computes:

- `non_HDL_c`, `remnant_c`

- `ratio_TC_HDL`, `ratio_TG_HDL`, `ratio_LDL_HDL`

- `ApoB_ApoA1`

- `VAI_Men`, `VAI_Women`

- `LAP_Men`, `LAP_Women`

- `TyG_BMI`

## Usage

``` r
lipid_markers(
  data,
  col_map = NULL,
  na_action = c("keep", "omit", "error", "ignore", "warn"),
  na_warn_prop = 0.2,
  verbose = TRUE
)
```

## Arguments

- data:

  A `data.frame` or `tibble` containing your lipid (and optional
  anthropometry/glucose) data.

- col_map:

  Named list mapping:

  - `TC` -\> total cholesterol

  - `HDL_c` -\> HDL-C

  - `TG` -\> triglycerides

  - `LDL_c` -\> (optional) LDL-C; if absent, estimated via Friedewald

  - `ApoB`, `ApoA1` -\> (optional) apolipoproteins

  - `waist` -\> (optional) waist circumference (cm)

  - `BMI` -\> (optional) body mass index (kg/m^2)

  - `glucose` -\> (optional) fasting glucose (mmol/L); used for TyG_BMI

- na_action:

  One of `c("keep","omit","error","ignore","warn")`.

  - keep/ignore: compute and propagate NA in outputs

  - omit: drop rows with NA in required inputs (TC, HDL_c, TG)

  - error: abort if any required input contains NA

  - warn: like keep, but emit missingness warnings

- na_warn_prop:

  Proportion (0-1) threshold for high-missingness warnings when
  `na_action = "warn"`. Default 0.2.

- verbose:

  Logical; if `TRUE` (default), prints step-by-step progress including
  column mapping, optional input availability, pre-computation notes,
  physiological range information (informational only, values are not
  altered), the list of markers being computed with their inputs, and a
  per-column results summary.

## Value

A tibble with computed lipid markers. Required outputs (always present):
`non_HDL_c`, `remnant_c`, `ratio_TC_HDL`, `ratio_TG_HDL`,
`ratio_LDL_HDL`, `ApoB_ApoA1`. Optional outputs (present when inputs
available): `VAI_Men`, `VAI_Women` (waist + BMI required); `LAP_Men`,
`LAP_Women` (waist required); `TyG_BMI` (BMI + glucose required). If an
ID column is detected in `data` (e.g. `id`, `IID`, `participant_id`), it
is prepended as the first output column.

## Details

Pre-computation (one level deep):

- If `BMI` is absent but `weight` (kg) and `height` (m or cm) are
  present, BMI is computed automatically.

- If `glucose` is absent but `G0` is present (or vice versa), the alias
  is derived automatically.

- If `LDL_c` is absent, it is always estimated via Friedewald (TC -
  HDL - TG/5). An informational message is emitted when
  `verbose = TRUE`.

## References

Friedewald WT, Levy RI, Fredrickson DS (1972). “Estimation of the
concentration of LDL cholesterol in plasma, without use of preparative
ultracentrifuge.” *Clinical Chemistry*, **18**(6), 499–502.
[doi:10.1093/clinchem/18.6.499](https://doi.org/10.1093/clinchem/18.6.499)
. Amato MC, Giordano C, Galia M, Criscimanna A, Vitabile S, Midiri M,
Galluzzo A (2010). “Visceral Adiposity Index: A Reliable Indicator of
Visceral Fat Function Associated with Cardiometabolic Risk.” *Diabetes
Care*, **33**(4), 920–922.
[doi:10.2337/dc09-1825](https://doi.org/10.2337/dc09-1825) . Kahn HS
(2005). “The Lipid Accumulation Product Performs Better than Body Mass
Index as an Indicator of Cardiovascular Risk in Women.” *BMC
Cardiovascular Disorders*, **5**(1), 26.
[doi:10.1186/1471-2261-5-26](https://doi.org/10.1186/1471-2261-5-26) .
Er L, Wu S, Chou H, Hsu L, Teng M, Sun Y, Ko Y (2016). “Triglyceride
Glucose-Body Mass Index Is a Simple and Clinically Useful Surrogate
Marker for Insulin Resistance in Nondiabetic Individuals.” *PLOS ONE*,
**11**(3), e0149731.
[doi:10.1371/journal.pone.0149731](https://doi.org/10.1371/journal.pone.0149731)
. Khamseh ME, Malek M, Abbasi R, Taheri E, others (2021). “Triglyceride
Glucose Index and Related Parameters (Triglyceride Glucose-Body Mass
Index and Triglyceride Glucose-Waist Circumference) Identify
Nonalcoholic Fatty Liver and Liver Fibrosis in Individuals with
Overweight/Obesity.” *Metabolic Syndrome and Related Disorders*,
**19**(3), 167–173.
[doi:10.1089/met.2020.0109](https://doi.org/10.1089/met.2020.0109) .

## Examples

``` r
df <- data.frame(TC = c(5.2, 6.1), HDL_c = c(1.3, 1.1), TG = c(1.8, 2.3),
                 LDL_c = c(3.2, 4.1), waist = c(88, 95), BMI = c(26, 29))
# Full verbose output (default)
lipid_markers(df)
#> lipid_markers(): reading input 'df' — 2 rows × 6 variables
#> lipid_markers(): col_map (6 columns — 6 inferred from data)
#>   TC                ->  'TC'    (inferred)
#>   HDL_c             ->  'HDL_c'    (inferred)
#>   TG                ->  'TG'    (inferred)
#>   LDL_c             ->  'LDL_c'    (inferred)
#>   waist             ->  'waist'    (inferred)
#>   BMI               ->  'BMI'    (inferred)
#> lipid_markers(): pre-computation: glucose cannot be derived -- provide: G0
#> lipid_markers(): optional inputs
#>   present:  waist, BMI
#>   missing:  ApoB, ApoA1, glucose
#>   indices -> NA:
#>   ApoB_ApoA1 -> NA  [missing: ApoB, ApoA1]
#>   TyG_BMI -> NA  [missing: glucose]
#>   derivable:
#>   glucose: provide G0 to enable
#> lipid_markers(): range note (informational, values not altered):
#>   TG: 2 value(s) outside plausible range
#> lipid_markers(): computing markers:
#>   non_HDL_c     [TC, HDL_c]
#>   remnant_c     [TC, HDL_c, LDL_c]
#>   ratio_TC_HDL  [TC, HDL_c]
#>   ratio_TG_HDL  [TG, HDL_c]
#>   ratio_LDL_HDL [LDL_c, HDL_c]
#>   ApoB_ApoA1    NA [ApoB/ApoA1 missing]
#>   VAI_Men/Women [waist, BMI, TG, HDL_c]
#>   LAP_Men/Women [waist, TG]
#>   TyG_BMI       NA [BMI/glucose missing]
#> lipid_markers(): results: non_HDL_c 2/2, remnant_c 2/2, ratio_TC_HDL 2/2, ratio_TG_HDL 2/2, ratio_LDL_HDL 2/2, ApoB_ApoA1 0/2, VAI_Men 2/2, VAI_Women 2/2, LAP_Men 2/2, LAP_Women 2/2
#> # A tibble: 2 × 10
#>   non_HDL_c remnant_c ratio_TC_HDL ratio_TG_HDL ratio_LDL_HDL ApoB_ApoA1 VAI_Men
#>       <dbl>     <dbl>        <dbl>        <dbl>         <dbl>      <dbl>   <dbl>
#> 1       3.9       0.7         4            1.38          2.46         NA    1.75
#> 2       5         0.9         5.55         2.09          3.73         NA    2.68
#> # ℹ 3 more variables: VAI_Women <dbl>, LAP_Men <dbl>, LAP_Women <dbl>
# Suppress messaging for batch use
lipid_markers(df, verbose = FALSE)
#> # A tibble: 2 × 10
#>   non_HDL_c remnant_c ratio_TC_HDL ratio_TG_HDL ratio_LDL_HDL ApoB_ApoA1 VAI_Men
#>       <dbl>     <dbl>        <dbl>        <dbl>         <dbl>      <dbl>   <dbl>
#> 1       3.9       0.7         4            1.38          2.46         NA    1.75
#> 2       5         0.9         5.55         2.09          3.73         NA    2.68
#> # ℹ 3 more variables: VAI_Women <dbl>, LAP_Men <dbl>, LAP_Women <dbl>
# Pre-compute BMI from weight and height
df2 <- data.frame(TC = 5.2, HDL_c = 1.3, TG = 1.8, weight = 70, height = 175)
lipid_markers(df2, verbose = FALSE)
#> # A tibble: 1 × 6
#>   non_HDL_c remnant_c ratio_TC_HDL ratio_TG_HDL ratio_LDL_HDL ApoB_ApoA1
#>       <dbl>     <dbl>        <dbl>        <dbl>         <dbl>      <dbl>
#> 1       3.9     0.360            4         1.38          2.72         NA
```
