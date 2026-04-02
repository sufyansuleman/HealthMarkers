# Compute liver-related indices (FLI, NFS, APRI, FIB-4, BARD, ALBI, MELD-XI) with validation and diagnostics

Given routine labs and anthropometry, computes:

- FLI - Fatty Liver Index (Bedogni et al. 2006)

- NFS - NAFLD Fibrosis Score (Angulo et al. 2007)

- APRI - AST-to-Platelet Ratio Index

- FIB4 - Fibrosis-4 Index

- BARD - BMI-AST/ALT-Diabetes score

- ALBI - Albumin-Bilirubin score

- MELD_XI - MELD excluding INR

## Usage

``` r
liver_markers(
  data,
  col_map = list(BMI = "BMI", waist = "waist", TG = "TG", GGT = "GGT", age = "age", AST =
    "AST", ALT = "ALT", platelets = "platelets", albumin = "albumin", diabetes =
    "diabetes", bilirubin = "bilirubin", creatinine = "creatinine"),
  verbose = FALSE,
  na_action = c("keep", "omit", "error", "ignore", "warn"),
  na_warn_prop = 0.2,
  check_extreme = FALSE,
  extreme_action = c("warn", "cap", "error", "ignore", "NA"),
  extreme_rules = NULL
)
```

## Arguments

- data:

  A data.frame or tibble containing your liver and anthropometry data.

- col_map:

  Named list mapping these keys -\> column names in `data`:

  - `BMI` (kg/m^2), `waist` (cm), `TG` (mg/dL), `GGT` (U/L),

  - `age` (years), `AST` (U/L), `ALT` (U/L), `platelets` (10^9/L),

  - `albumin` (g/L), `diabetes` (0/1 or logical),

  - `bilirubin` (mg/dL), `creatinine` (mg/dL).

- verbose:

  Logical; if TRUE, prints stepwise messages and a final summary.
  Default FALSE.

- na_action:

  One of c("keep","omit","error") controlling missing-data policy.
  Default "keep" (preserves prior behavior).

  - "keep": leave NAs; they propagate to outputs.

  - "omit": drop rows with NA in any required input.

  - "error": abort if any required input contains NA.

- na_warn_prop:

  Numeric in \\\[0,1\]\\; per-variable threshold for high-missingness
  warnings. Default 0.2.

- check_extreme:

  Logical; if TRUE, scan inputs for out-of-range values (see
  `extreme_rules`). Default FALSE.

- extreme_action:

  One of c("warn","cap","error","ignore") when extremes are detected
  (only used if `check_extreme = TRUE`).

  - "warn": only warn (default), "cap": truncate to allowed range,
    "error": abort, "ignore": do nothing.

- extreme_rules:

  Optional named list of c(min,max) ranges for keys in `col_map`. If
  NULL, broad defaults are used.

## Value

A tibble with one column per marker: `FLI`, `NFS`, `APRI`, `FIB4`,
`BARD`, `ALBI`, `MELD_XI`.

## Details

Enhancements:

- Robust input validation (columns present, types) with informative
  errors.

- Configurable NA policy and optional extreme-value scanning/capping.

- Data-quality warnings (high missingness, non-positive logs, zero
  denominators).

- Verbose stepwise progress and completion summary.

Units (no automatic conversion):

- BMI: kg/m^2; Waist: cm; TG: mg/dL; GGT/AST/ALT: U/L; Platelets:
  10^9/L; Albumin: g/L; Bilirubin: mg/dL; Creatinine: mg/dL.

- ALBI uses bilirubin in mumol/L internally (converted as bilirubin
  (mg/dL) \* 17.1).

Formulas

- FLI = logistic(0.953*ln(TG) + 0.139*BMI + 0.718*ln(GGT) +
  0.053*waist - 15.745) \* 100

- NFS = -1.675 + 0.037*age + 0.094*BMI + 1.13*diabetes +
  0.99*(AST/ALT) - 0.013*platelets - 0.66*albumin

- APRI = (AST / 40) / platelets \* 100; assumes AST upper limit of
  normal = 40 U/L

- FIB-4 = (age \* AST) / (platelets \* sqrt(ALT))

- BARD = 1 if BMI\>=28, +1 if AST/ALT\>=0.8, +1 if diabetes present; sum
  in 0,1,2,3

- ALBI = 0.66*log10(bilirubin (mumol/L)) - 0.0852*albumin (g/L)

- MELD-XI = 5.11*ln(bilirubin (mg/dL)) + 11.76*ln(creatinine (mg/dL)) +
  9.44

## References

Bedogni G, Bellentani S, Miglioli L, others (2006). “The Fatty Liver
Index: a simple and accurate predictor of hepatic steatosis in the
general population.” *BMC Gastroenterology*, **6**, 33.
[doi:10.1186/1471-230X-6-33](https://doi.org/10.1186/1471-230X-6-33) .
Angulo P, Hui JM, Marchesini G, others (2007). “The NAFLD fibrosis
score: a noninvasive system that identifies liver fibrosis in patients
with NAFLD.” *Hepatology*, **45**(4), 846–854.
[doi:10.1002/hep.21496](https://doi.org/10.1002/hep.21496) . Wai CT,
Greenson JK, Fontana RJ, others (2003). “A simple noninvasive index can
predict both significant fibrosis and cirrhosis in patients with chronic
hepatitis C.” *Hepatology*, **38**(2), 518–526.
[doi:10.1053/jhep.2003.50346](https://doi.org/10.1053/jhep.2003.50346) .
Sterling RK, Lissen E, Clumeck N, others (2006). “Development of a
simple noninvasive index to predict significant fibrosis in patients
with HIV/HCV coinfection (FIB-4).” *Hepatology*, **43**(6), 1317–1325.
[doi:10.1002/hep.21178](https://doi.org/10.1002/hep.21178) . Harrison
SA, Oliver D, Arnold HL, others (2008). “Development and validation of a
simple NAFLD clinical scoring system for identifying patients without
advanced disease.” *Gut*, **57**(10), 1441–1447.
[doi:10.1136/gut.2007.146019](https://doi.org/10.1136/gut.2007.146019) .
Johnson PJ, Berhane S, Kagebayashi C, others (2015). “Assessment of
liver function in patients with hepatocellular carcinoma: the ALBI
grade.” *Journal of Clinical Oncology*, **33**(6), 550–558.
[doi:10.1200/JCO.2014.57.9151](https://doi.org/10.1200/JCO.2014.57.9151)
. Heuman DM, Abou-Assi SG, Habib A, others (2006). “MELD-XI: A rational
approach to "sickest first" liver transplantation in cirrhotic patients
requiring anticoagulant therapy.” *Liver Transplantation*, **13**(1),
30–37. [doi:10.1002/lt.20906](https://doi.org/10.1002/lt.20906) .

## See also

[`inflammatory_markers()`](https://sufyansuleman.github.io/HealthMarkers/reference/inflammatory_markers.md),
[`kidney_failure_risk()`](https://sufyansuleman.github.io/HealthMarkers/reference/kidney_failure_risk.md),
[`iAge()`](https://sufyansuleman.github.io/HealthMarkers/reference/iAge.md)

## Examples

``` r
library(tibble)
df <- tibble(
  BMI           = 24,
  waist         = 80,
  TG = 150, # mg/dL
  GGT           = 30,
  age           = 45,
  AST           = 25,
  ALT           = 20,
  platelets     = 250, # 10^9/L
  albumin       = 42,  # g/L
  diabetes      = FALSE,
  bilirubin     = 1.0, # mg/dL
  creatinine    = 0.9  # mg/dL
)
liver_markers(df, verbose = TRUE)
#> liver_markers(): preparing inputs
#> liver_markers(): column map: BMI -> 'BMI', waist -> 'waist', TG -> 'TG', GGT -> 'GGT', age -> 'age', AST -> 'AST', ALT -> 'ALT', platelets -> 'platelets', albumin -> 'albumin', diabetes -> 'diabetes', bilirubin -> 'bilirubin', creatinine -> 'creatinine'
#> liver_markers(): results: FLI 1/1, NFS 1/1, APRI 1/1, FIB4 1/1, BARD 1/1, ALBI 1/1, MELD_XI 1/1
#> # A tibble: 1 × 7
#>     FLI   NFS  APRI  FIB4  BARD  ALBI MELD_XI
#>   <dbl> <dbl> <dbl> <dbl> <int> <dbl>   <dbl>
#> 1  27.9 -27.5  0.25  1.01     1 -2.76    8.20

# \donttest{
# With extreme-value capping and diagnostics
liver_markers(
  df,
  check_extreme = TRUE,
  extreme_action = "cap",
  verbose = TRUE
)
#> liver_markers(): preparing inputs
#> liver_markers(): column map: BMI -> 'BMI', waist -> 'waist', TG -> 'TG', GGT -> 'GGT', age -> 'age', AST -> 'AST', ALT -> 'ALT', platelets -> 'platelets', albumin -> 'albumin', diabetes -> 'diabetes', bilirubin -> 'bilirubin', creatinine -> 'creatinine'
#> liver_markers(): results: FLI 1/1, NFS 1/1, APRI 1/1, FIB4 1/1, BARD 1/1, ALBI 1/1, MELD_XI 1/1
#> # A tibble: 1 × 7
#>     FLI   NFS  APRI  FIB4  BARD  ALBI MELD_XI
#>   <dbl> <dbl> <dbl> <dbl> <int> <dbl>   <dbl>
#> 1  27.9 -27.5  0.25  1.01     1 -2.76    8.20
# }
```
