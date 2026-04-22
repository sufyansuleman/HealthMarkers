# Liver Markers

## Scope

Compute liver-related indices (FLI, NFS, APRI, FIB-4, BARD, ALBI,
MELD-XI) with configurable handling for missing data and extreme values.

## When to use

- You have routine liver labs and anthropometry (BMI, waist, TG, GGT,
  AST, ALT, platelets, albumin, bilirubin, creatinine, diabetes flag)
  and want standard steatosis/fibrosis scores.
- You need built-in NA policy controls, numeric coercion, and optional
  extreme-value scanning/capping.
- You want MELD-XI, ALBI, and common non-invasive fibrosis indices in
  one call for screening or modeling.

## Inputs

- `data`: data.frame/tibble with required inputs.
- `col_map`: named list mapping required keys to columns (units assumed,
  no conversion): BMI (kg/m^2), waist (cm), TG (mg/dL), GGT (U/L), age
  (years), AST (U/L), ALT (U/L), platelets (10^9/L), albumin (g/L),
  diabetes (0/1 or logical), bilirubin (mg/dL), creatinine (mg/dL).
- `na_action`: `keep` (default) propagates NA; `omit` drops rows missing
  required inputs; `error` aborts; `ignore` treated as keep; `warn`
  keeps but issues high-missingness warnings via `na_warn_prop`.
- Numeric coercion: mapped columns (except `diabetes`) are coerced to
  numeric; non-finite values become NA before policies. `diabetes` is
  coerced to integer 0/1 with a warning if values are outside
  {0,1,TRUE,FALSE}.

## Quick start

``` r
library(HealthMarkers)
library(tibble)

liver <- tibble::tibble(
  BMI           = c(24, 30, 33),
  waist         = c(80, 100, 110),
  triglycerides = c(150, 260, 90),   # mg/dL
  GGT           = c(30, 55, 18),
  age           = c(45, 58, 67),
  AST           = c(25, 60, 42),
  ALT           = c(20, 45, 28),
  platelets     = c(250, 190, 210),  # 10^9/L
  albumin       = c(42, 38, 40),     # g/L
  diabetes      = c(FALSE, TRUE, 1),
  bilirubin     = c(1.0, 0.9, 1.3),  # mg/dL
  creatinine    = c(0.9, 1.1, 1.0)   # mg/dL
)

liver_markers(
  data = liver,
  col_map = list(
    BMI = "BMI", waist = "waist", TG = "triglycerides", GGT = "GGT", age = "age",
    AST = "AST", ALT = "ALT", platelets = "platelets", albumin = "albumin", diabetes = "diabetes",
    bilirubin = "bilirubin", creatinine = "creatinine"
  ),
  na_action = "keep"
)
#> # A tibble: 3 × 7
#>     FLI   NFS  APRI  FIB4  BARD  ALBI MELD_XI
#>   <dbl> <dbl> <dbl> <dbl> <int> <dbl>   <dbl>
#> 1  27.9 -27.5 0.25   1.01     1 -2.76    8.20
#> 2  87.0 -21.8 0.789  2.73     3 -2.45   10.0 
#> 3  73.8 -22.6 0.5    2.53     3 -2.52   10.8
```

## Non-standard column names

When your data use different column names, only supply `col_map` entries
for keys that differ. The remaining keys are matched automatically via a
built-in synonym dictionary (case-insensitive, common abbreviations:
`bili` → `bilirubin`, `creat` → `creatinine`, `platelet_count` →
`platelets`, etc.).

``` r
df_ns <- tibble::tibble(
  bmi_kgm2      = 27,
  waist_cm      = 92,
  trig_mgdl     = 180,   # mg/dL
  ggt           = 40,
  age_years     = 52,
  AST           = 32,
  ALT           = 28,
  PLT           = 220,   # platelets, 10^9/L
  serum_albumin = 40,    # g/L
  dm            = 1L,
  bili          = 0.8,   # mg/dL
  creat         = 1.0    # mg/dL
)

liver_markers(
  data    = df_ns,
  col_map = list(
    BMI        = "bmi_kgm2",
    waist      = "waist_cm",
    TG         = "trig_mgdl",
    age        = "age_years",
    diabetes   = "dm"
  ),
  verbose = FALSE
)
#> # A tibble: 1 × 7
#>     FLI   NFS  APRI  FIB4  BARD  ALBI MELD_XI
#>   <dbl> <dbl> <dbl> <dbl> <int> <dbl>   <dbl>
#> 1  61.8 -24.2 0.364  1.43     2 -2.66    8.30
```

`GGT`, `AST`, `ALT`, `PLT` (→ `platelets`), `serum_albumin` (→
`albumin`), `bili` (→ `bilirubin`), and `creat` (→ `creatinine`) were
all matched automatically; only the five renamed keys required explicit
`col_map` entries.

## Handling missingness

- Missing values: `keep/ignore` propagate NA; `omit` drops rows missing
  required inputs; `warn` keeps but logs high-missingness by column;
  `error` aborts on missing required inputs.
- Non-finite values become NA before any policy is applied.

## Outputs

- Returns a tibble with: `FLI`, `NFS`, `APRI`, `FIB4`, `BARD`, `ALBI`,
  `MELD_XI`.
- Bilirubin is converted to umol/L internally for ALBI; other inputs use
  provided units.
- Zero or nonpositive platelets, TG, GGT, bilirubin, creatinine, or
  negative ALT trigger warnings and yield NA in affected indices.

## Verbose diagnostics

``` r
old_opt <- options(healthmarkers.verbose = "inform")
liver_markers(
  data = liver,
  col_map = list(
    BMI = "BMI", waist = "waist", TG = "triglycerides", GGT = "GGT", age = "age",
    AST = "AST", ALT = "ALT", platelets = "platelets", albumin = "albumin", diabetes = "diabetes",
    bilirubin = "bilirubin", creatinine = "creatinine"
  ),
  verbose = TRUE
)
#> liver_markers(): reading input 'liver' — 3 rows × 12 variables
#> liver_markers(): col_map (12 columns — 12 specified)
#>   BMI               ->  'BMI'
#>   waist             ->  'waist'
#>   TG                ->  'triglycerides'
#>   GGT               ->  'GGT'
#>   age               ->  'age'
#>   AST               ->  'AST'
#>   ALT               ->  'ALT'
#>   platelets         ->  'platelets'
#>   albumin           ->  'albumin'
#>   diabetes          ->  'diabetes'
#>   bilirubin         ->  'bilirubin'
#>   creatinine        ->  'creatinine'
#> liver_markers(): optional inputs
#>   present:  BMI, waist, TG, GGT, age, AST, ALT, platelets, albumin, diabetes, bilirubin, creatinine
#> liver_markers(): computing markers:
#>   FLI        [BMI, waist, TG, GGT]
#>   NFS        [age, BMI, diabetes, AST, ALT, platelets, albumin]
#>   APRI       [AST, platelets]
#>   FIB4       [age, AST, platelets, ALT]
#>   BARD       [BMI, AST, ALT, diabetes]
#>   ALBI       [bilirubin, albumin]
#>   MELD_XI    [bilirubin, creatinine]
#> liver_markers(): results: FLI 3/3, NFS 3/3, APRI 3/3, FIB4 3/3, BARD 3/3, ALBI 3/3, MELD_XI 3/3
#> # A tibble: 3 × 7
#>     FLI   NFS  APRI  FIB4  BARD  ALBI MELD_XI
#>   <dbl> <dbl> <dbl> <dbl> <int> <dbl>   <dbl>
#> 1  27.9 -27.5 0.25   1.01     1 -2.76    8.20
#> 2  87.0 -21.8 0.789  2.73     3 -2.45   10.0 
#> 3  73.8 -22.6 0.5    2.53     3 -2.52   10.8
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

## Tips

- Confirm triglycerides are in mg/dL (not mmol/L) and albumin in g/L.
- For modeling-ready outputs, prefer `na_action = "omit"`; use `keep` or
  `warn` when inspecting data quality.
