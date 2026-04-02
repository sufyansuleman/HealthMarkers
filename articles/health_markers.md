# Health marker aggregators

## Scope

Guide to the three aggregators: -
[`all_insulin_indices()`](https://sufyansuleman.github.io/HealthMarkers/reference/all_insulin_indices.md):
fasting/OGTT/adipose/tracer insulin sensitivity/resistance panels. -
[`metabolic_markers()`](https://sufyansuleman.github.io/HealthMarkers/reference/metabolic_markers.md):
selectable bundle (insulin, adiposity_sds, cardio, lipid, liver,
glycemic, MetS) appended to your data. -
[`all_health_markers()`](https://sufyansuleman.github.io/HealthMarkers/reference/all_health_markers.md):
registry-driven dispatcher across most marker groups with optional
column inference and insulin inclusion. All wrappers use guards to skip
missing inputs/optional deps instead of crashing when `verbose = FALSE`.

## When to use

- You want one call to run multiple marker groups, with optional insulin
  panels.
- You need column inference by default but can supply a `col_map` for
  nonstandard names.
- You prefer graceful skipping (no hard errors) when inputs or optional
  packages are absent.

## Inputs and requirements

- `data`: data.frame/tibble with columns required by the groups you
  select.
- `col_map`: optional mapping; recommended for insulin/lipid groups or
  nonstandard names.
- `which`: groups to compute.
  [`metabolic_markers()`](https://sufyansuleman.github.io/HealthMarkers/reference/metabolic_markers.md)
  supports `insulin`, `adiposity_sds`, `cardio`, `lipid`, `liver`,
  `glycemic`, `mets`.
  [`all_health_markers()`](https://sufyansuleman.github.io/HealthMarkers/reference/all_health_markers.md)
  covers a larger registry (lipid, renal, inflammatory, liver, obesity,
  insulin panels, etc.).
- `include_insulin`: whether to add insulin panels in
  [`all_health_markers()`](https://sufyansuleman.github.io/HealthMarkers/reference/all_health_markers.md);
  requires glucose/insulin columns.
- `na_action`: `keep`/`omit`/`error` (plus `ignore`/`warn` where
  supported) forwarded to underlying helpers.
- Optional dependencies: some groups rely on suggested packages (e.g.,
  CVD risk backends). Missing deps are skipped with NA outputs.

## Load packages and data

Use the packaged simulated dataset for runnable examples. Swap
`sim_small` with your data frame.

``` r
library(HealthMarkers)

sim_path <- system.file("extdata", "simulated_hm_data.rds", package = "HealthMarkers")
sim <- readRDS(sim_path)
sim_small <- dplyr::slice_head(sim, n = 30)
```

## Column map (when you need it)

Most groups infer columns automatically. Provide a `col_map` when your
names differ or when using insulin panels.

### Typical keys for these examples

- `TG`, `HDL_c`, `LDL_c`, `TC`: lipid inputs (mmol/L)
- `BMI`: body mass index (kg/m^2)
- `G0`, `I0`: fasting glucose (mmol/L) and insulin (pmol/L) for insulin
  panels
- `ALT`, `AST`: liver enzymes (U/L)
- `age`, `sex`: demographics used by several groups

``` r
col_map <- list(
  TG = "TG",
  HDL_c = "HDL_c",
  LDL_c = "LDL_c",
  TC = "TC",
  BMI = "BMI",
  G0 = "G0",
  I0 = "I0",
  ALT = "ALT",
  AST = "AST",
  age = "age",
  sex = "sex"
)
```

## Core calculation (targeted metabolic bundle)

Run
[`metabolic_markers()`](https://sufyansuleman.github.io/HealthMarkers/reference/metabolic_markers.md)
on a small subset, focusing on glycemic, lipid, and liver groups
(insulin panel excluded to keep inputs minimal). Show only new columns.

``` r
mm <- metabolic_markers(
  data = sim_small,
  col_map = col_map,
  which = c("glycemic", "lipid", "liver"),
  normalize = "none",
  mode = "both",
  verbose = FALSE,
  na_action = "keep"
)

mm_new <- setdiff(names(mm), names(sim_small))
head(dplyr::select(mm, dplyr::all_of(mm_new)))
#>   non_HDL_c  remnant_c ratio_TC_HDL ratio_TG_HDL ratio_LDL_HDL ApoB_ApoA1
#> 1  2.805635 -0.1707940     2.828547    0.6934188      1.939860  0.4725922
#> 2  4.411394  1.8086884     3.946180    1.4946281      1.738235  0.6374380
#> 3  3.306616  0.9604294     3.702865    0.9518317      1.917799  0.3916076
#> 4  3.719980 -0.1980635     4.916000    2.0409466      4.124501  0.8039860
#> 5  4.025115  0.7886167     4.085650    1.4183511      2.481097  0.5155962
#> 6  3.879587  0.6680692     3.971257    0.6777942      2.459603  0.8798657
#>         FLI       NFS      APRI      FIB4 BARD      ALBI  MELD_XI    SPISE
#> 1  1.379285 -26.54137 0.5822226 2.8827295    2 -2.132707 76.60588 6.538621
#> 2  2.638468 -25.01258 0.4325298 1.3397544    1 -1.662751 77.83057 7.040211
#> 3  1.643102 -27.32148 0.2685047 1.2561533    2 -2.078360 72.61716 6.738432
#> 4 28.874626 -27.27550 0.3315235 0.9452263    2 -2.093965 74.95934 4.125126
#> 5  9.050782 -30.47121 0.4758078 1.1140914    1 -2.463606 73.71918 6.245599
#> 6  8.854861 -26.97225 0.2044272 0.9347936    1 -2.213821 77.93558 5.338716
#>      METS_IR prediabetes HOMA_CP LAR ASI TyG_index
#> 1   175.4422           0      NA  NA  NA  8.390992
#> 2   163.2440           0      NA  NA  NA  9.133717
#> 3   342.1412           0      NA  NA  NA  8.423506
#> 4 -1869.7322           0      NA  NA  NA  9.073867
#> 5   270.7383           0      NA  NA  NA  8.947845
#> 6   347.0725           0      NA  NA  NA  8.365320
```

Interpretation: outputs include lipid ratios, TyG-based metrics, and
liver-derived markers. Rows are retained because `na_action = "keep"`
propagates missing inputs to `NA` outputs.

## Comprehensive run (registry dispatcher)

[`all_health_markers()`](https://sufyansuleman.github.io/HealthMarkers/reference/all_health_markers.md)
can auto-infer columns or use your `col_map`. Here we request a
multi-panel set and skip insulin for minimal inputs.

``` r
hm <- all_health_markers(
  data = sim_small,
  col_map = col_map,
  which = c("glycemic", "lipid", "renal", "inflammatory"),
  include_insulin = FALSE,
  normalize = "none",
  mode = "both",
  verbose = TRUE,
  na_action = "keep"
)

hm_new <- setdiff(names(hm), names(sim_small))
head(dplyr::select(hm, dplyr::all_of(hm_new)))
#>      SPISE    METS_IR prediabetes HOMA_CP LAR ASI TyG_index non_HDL_c
#> 1 6.538621   175.4422           0      NA  NA  NA  8.390992  2.805635
#> 2 7.040211   163.2440           0      NA  NA  NA  9.133717  4.411394
#> 3 6.738432   342.1412           0      NA  NA  NA  8.423506  3.306616
#> 4 4.125126 -1869.7322           0      NA  NA  NA  9.073867  3.719980
#> 5 6.245599   270.7383           0      NA  NA  NA  8.947845  4.025115
#> 6 5.338716   347.0725           0      NA  NA  NA  8.365320  3.879587
#>    remnant_c ratio_TC_HDL ratio_TG_HDL ratio_LDL_HDL ApoB_ApoA1
#> 1 -0.1707940     2.828547    0.6934188      1.939860  0.4725922
#> 2  1.8086884     3.946180    1.4946281      1.738235  0.6374380
#> 3  0.9604294     3.702865    0.9518317      1.917799  0.3916076
#> 4 -0.1980635     4.916000    2.0409466      4.124501  0.8039860
#> 5  0.7886167     4.085650    1.4183511      2.481097  0.5155962
#> 6  0.6680692     3.971257    0.6777942      2.459603  0.8798657
```

Verbose mode reports inferred/user mappings and which groups were
computed or skipped. Skipped groups leave the input untouched.

## Insulin panel example (with graceful skips)

Include insulin indices alongside a small panel. Missing
OGTT/adipose/tracer inputs are skipped safely; fasting indices compute
if `G0`/`I0` are present.

``` r
col_map_insulin <- list(
  G0   = "G0",
  I0   = "I0",
  TG   = "TG",
  HDL_c = "HDL_c",
  BMI  = "BMI",
  age  = "age",
  sex  = "sex"
)

ins <- all_health_markers(
  data = sim_small,
  col_map = col_map_insulin,
  which = c("glycemic", "lipid"),
  include_insulin = TRUE,
  normalize = "none",
  mode = "both",
  verbose = TRUE,
  na_action = "keep"
)

ins_new <- setdiff(names(ins), names(sim_small))
head(dplyr::select(ins, dplyr::all_of(ins_new)))
#>   Fasting_inv  Raynaud HOMA_IR_inv     FIRI    QUICKI Belfiore_basal
#> 1  -13.982032 2.860814   -58.12619 52.31357 0.1393508    0.001528072
#> 2  -16.037564 2.494144   -66.61505 59.95355 0.1367531    0.001333477
#> 3  -15.703667 2.547176   -61.62037 55.45833 0.1382264    0.001441485
#> 4   -6.747394 5.928215   -30.47178 27.42460 0.1531319    0.002912840
#> 5  -13.626809 2.935390   -56.85119 51.16607 0.1397829    0.001562315
#> 6  -16.971980 2.356826   -82.67276 74.40548 0.1328303    0.001074612
#>   Ig_ratio_basal Isi_basal    Bennett HOMA_IR_rev_inv IR_Fasting_inv IR_Raynaud
#> 1    -0.14948108  7.646200 0.08353409       -3.229233    -0.07152036  0.3495508
#> 2    -0.17160184  6.671832 0.07941994       -3.700836    -0.06235361  0.4009391
#> 3    -0.17786699  7.212622 0.08104295       -3.423354    -0.06367939  0.3925917
#> 4    -0.06640363 14.585444 0.11334634       -1.692877    -0.14820537  0.1686849
#> 5    -0.14516647  7.817681 0.08429105       -3.158399    -0.07338475  0.3406702
#> 6    -0.15485316  5.375948 0.07519132       -4.592931    -0.05892065  0.4242995
#>   IR_HOMA_IR_inv    IR_FIRI IR_QUICKI IR_Belfiore_basal IR_Ig_ratio_basal
#> 1    -0.01720395 0.01911550  7.176132          654.4196         -6.689810
#> 2    -0.01501162 0.01667958  7.312446          749.9193         -5.827443
#> 3    -0.01622840 0.01803156  7.234508          693.7292         -5.622179
#> 4    -0.03281725 0.03646361  6.530316          343.3075        -15.059418
#> 5    -0.01758978 0.01954420  7.153952          640.0758         -6.888643
#> 6    -0.01209588 0.01343987  7.528405          930.5685         -6.457731
#>   IR_Isi_basal IR_Bennett IR_HOMA_IR_rev_inv    SPISE    METS_IR prediabetes
#> 1   0.13078393  11.971160         -0.3096711 6.538621   175.4422           0
#> 2   0.14988387  12.591296         -0.2702092 7.040211   163.2440           0
#> 3   0.13864583  12.339136         -0.2921112 6.738432   342.1412           0
#> 4   0.06856151   8.822517         -0.5907105 4.125126 -1869.7322           0
#> 5   0.12791517  11.863655         -0.3166161 6.245599   270.7383           0
#> 6   0.18601371  13.299407         -0.2177259 5.338716   347.0725           0
#>   HOMA_CP LAR ASI TyG_index non_HDL_c  remnant_c ratio_TC_HDL ratio_TG_HDL
#> 1      NA  NA  NA  8.390992  2.805635 -0.1707940     2.828547    0.6934188
#> 2      NA  NA  NA  9.133717  4.411394  1.8086884     3.946180    1.4946281
#> 3      NA  NA  NA  8.423506  3.306616  0.9604294     3.702865    0.9518317
#> 4      NA  NA  NA  9.073867  3.719980 -0.1980635     4.916000    2.0409466
#> 5      NA  NA  NA  8.947845  4.025115  0.7886167     4.085650    1.4183511
#> 6      NA  NA  NA  8.365320  3.879587  0.6680692     3.971257    0.6777942
#>   ratio_LDL_HDL ApoB_ApoA1
#> 1      1.939860  0.4725922
#> 2      1.738235  0.6374380
#> 3      1.917799  0.3916076
#> 4      4.124501  0.8039860
#> 5      2.481097  0.5155962
#> 6      2.459603  0.8798657
```

The verbose summary will note which insulin sub-panels ran (fasting) and
which were skipped due to missing OGTT/adipose/tracer inputs.

## Missing data and row handling

`na_action` controls row retention. Demonstrate with a tiny slice
containing a missing lipid value.

``` r
demo <- sim_small[1:6, c("TG", "HDL_c", "BMI", "ALT", "AST")]
demo$TG[2] <- NA

mm_keep <- metabolic_markers(
  data = demo,
  col_map = col_map,
  which = c("glycemic", "lipid", "liver"),
  na_action = "keep",
  verbose = FALSE
)

mm_omit <- metabolic_markers(
  data = demo,
  col_map = col_map,
  which = c("glycemic", "lipid", "liver"),
  na_action = "omit",
  verbose = FALSE
)

list(
  keep_rows = nrow(mm_keep),
  omit_rows = nrow(mm_omit)
)
#> $keep_rows
#> [1] 6
#> 
#> $omit_rows
#> [1] 6
```

`keep` propagates missing inputs to `NA` markers; `omit` drops rows with
required-field NAs for the selected groups.

## Expectations

- Required inputs for chosen groups must be present/mapped; missing
  required columns abort or skip depending on the underlying helper.
- `na_action` chooses whether missing inputs lead to retained rows
  (`keep`), dropped rows (`omit`), or immediate errors (`error`).
- `normalize` and `mode` affect insulin outputs; they are ignored by
  groups that do not use them.
- [`all_health_markers()`](https://sufyansuleman.github.io/HealthMarkers/reference/all_health_markers.md)
  infers columns when `col_map` is missing/NULL; inference fails if
  required keys cannot be found.
- Verbose mode summarizes mappings plus computed/skipped groups so you
  can audit the run.

## Verbose diagnostics

Set `verbose = TRUE` (and `healthmarkers.verbose = "inform"`) to surface
three structured messages on each call: preparing inputs, a column
mapping summary, and a group execution summary.

``` r
old_opt <- options(healthmarkers.verbose = "inform")
df_v <- data.frame(TC = 200, HDL_c = 50, TG = 150, LDL_c = 120, BMI = 25)
col_v <- list(TC = "TC", HDL_c = "HDL_c", TG = "TG", LDL_c = "LDL_c", BMI = "BMI")
all_health_markers(
  df_v, col_v,
  which = c("lipid"),
  include_insulin = FALSE,
  verbose = TRUE
)
#> all_health_markers(): preparing inputs
#> Column mapping summary: TC->TC (user), HDL_c->HDL_c (user), TG->TG (user), LDL_c->LDL_c (user), BMI->BMI (user)
#> health_markers(): calling lipid
#> lipid_markers(): preparing inputs
#> lipid_markers(): column map: TC -> 'TC', HDL_c -> 'HDL_c', TG -> 'TG'
#> lipid_markers(): results: non_HDL_c 1/1, remnant_c 1/1, ratio_TC_HDL 1/1, ratio_TG_HDL 1/1, ratio_LDL_HDL 1/1, ApoB_ApoA1 0/1
#> all_health_markers(): summary - computed: lipid
#>    TC HDL_c  TG LDL_c BMI non_HDL_c remnant_c ratio_TC_HDL ratio_TG_HDL
#> 1 200    50 150   120  25       150        30            4            3
#>   ratio_LDL_HDL ApoB_ApoA1
#> 1           2.4         NA
options(old_opt)
```

## Tips

- Start narrow: pick a few groups with `metabolic_markers(which = ...)`
  before running the full registry.
- Supply an explicit `col_map` when your column names differ, especially
  for insulin and lipid inputs.
- Set `include_insulin = TRUE` in
  [`all_health_markers()`](https://sufyansuleman.github.io/HealthMarkers/reference/all_health_markers.md)
  only when glucose/insulin columns are available (G0/I0 or OGTT inputs
  mapped).
- Use `na_action = "error"` for strict pipelines; `omit` for dropping
  incomplete rows; `keep` for exploratory runs.
- After computing, use
  [`dplyr::select()`](https://dplyr.tidyverse.org/reference/select.html)
  to keep only the markers you care about; outputs can be wide.
