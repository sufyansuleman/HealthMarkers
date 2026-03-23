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
#> 1  4.472486  0.4091206     5.952950    1.0716354      4.499879  0.5855594
#> 2  3.338557  0.2301283     4.340045    1.8606939      3.109814  1.0015916
#> 3  4.154120  1.4407099     3.679324    0.7970881      1.750095  0.7404117
#> 4  4.112558  1.1387383     5.058502    1.5935284      2.934731  0.6027401
#> 5  3.324661  0.2533190     3.088419    1.0120564      1.929294  0.4739219
#> 6  3.572372 -0.2791537     3.564597    1.5878714      2.765001  0.5349333
#>          FLI       NFS       APRI      FIB4 BARD      ALBI  MELD_XI    SPISE
#> 1  0.7354445 -29.97639 0.09705562 0.4318256    0 -2.321598 68.92211 8.297275
#> 2 21.2612910 -29.87016 0.54252783 1.0360319    2 -2.339372 77.84573 4.177562
#> 3  3.9602290 -27.69066 0.64589943 3.1763837    2 -2.340113 75.79015 6.351097
#> 4 12.3587058 -24.12105 0.28700967 1.6094822    2 -1.801828 79.35300 5.963605
#> 5  1.1742516 -29.25494 0.51546032 2.1591873    1 -2.597017 69.15888 7.139034
#> 6  4.7813115 -24.57211 0.39259013 2.2820065    2 -2.232632 72.80311 4.529109
#>   METS_IR prediabetes HOMA_CP LAR ASI TyG_index
#> 1      NA           0      NA  NA  NA        NA
#> 2      NA           0      NA  NA  NA        NA
#> 3      NA           0      NA  NA  NA        NA
#> 4      NA           0      NA  NA  NA        NA
#> 5      NA           0      NA  NA  NA        NA
#> 6      NA           0      NA  NA  NA        NA
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
#>      SPISE METS_IR prediabetes HOMA_CP LAR ASI TyG_index non_HDL_c  remnant_c
#> 1 8.297275      NA           0      NA  NA  NA        NA  4.472486  0.4091206
#> 2 4.177562      NA           0      NA  NA  NA        NA  3.338557  0.2301283
#> 3 6.351097      NA           0      NA  NA  NA        NA  4.154120  1.4407099
#> 4 5.963605      NA           0      NA  NA  NA        NA  4.112558  1.1387383
#> 5 7.139034      NA           0      NA  NA  NA        NA  3.324661  0.2533190
#> 6 4.529109      NA           0      NA  NA  NA        NA  3.572372 -0.2791537
#>   ratio_TC_HDL ratio_TG_HDL ratio_LDL_HDL ApoB_ApoA1
#> 1     5.952950    1.0716354      4.499879  0.5855594
#> 2     4.340045    1.8606939      3.109814  1.0015916
#> 3     3.679324    0.7970881      1.750095  0.7404117
#> 4     5.058502    1.5935284      2.934731  0.6027401
#> 5     3.088419    1.0120564      1.929294  0.4739219
#> 6     3.564597    1.5878714      2.765001  0.5349333
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
#> 1  -19.518266 2.049362   -78.63227 70.76904 0.1337203    0.001129799
#> 2  -13.404093 2.984163   -61.62912 55.46621 0.1382237    0.001441280
#> 3  -15.753592 2.539103   -65.91276 59.32148 0.1369516    0.001347675
#> 4   -9.251371 4.323683   -36.45137 32.80623 0.1490425    0.002435591
#> 5  -17.033794 2.348273   -83.07725 74.76953 0.1327442    0.001069382
#> 6  -14.438290 2.770411   -67.34974 60.61476 0.1365483    0.001318940
#>   Ig_ratio_basal Isi_basal    Bennett HOMA_IR_rev_inv IR_Fasting_inv IR_Raynaud
#> 1     -0.2153273  5.652189 0.07467301       -4.368459    -0.05123406  0.4879567
#> 2     -0.1295706  7.211598 0.08304933       -3.423840    -0.07460408  0.3351023
#> 3     -0.1673429  6.742920 0.07980676       -3.661820    -0.06347758  0.3938398
#> 4     -0.1043556 12.192805 0.10022567       -2.025076    -0.10809208  0.2312843
#> 5     -0.1552237  5.349773 0.07507500       -4.615403    -0.05870683  0.4258448
#> 6     -0.1375666  6.599052 0.08048688       -3.741652    -0.06926028  0.3609573
#>   IR_HOMA_IR_inv    IR_FIRI IR_QUICKI IR_Belfiore_basal IR_Ig_ratio_basal
#> 1    -0.01271743 0.01413047  7.478297          885.1130         -4.644092
#> 2    -0.01622609 0.01802899  7.234650          693.8277         -7.717802
#> 3    -0.01517157 0.01685730  7.301847          742.0185         -5.975755
#> 4    -0.02743381 0.03048201  6.709494          410.5779         -9.582617
#> 5    -0.01203699 0.01337443  7.533286          935.1191         -6.442314
#> 6    -0.01484787 0.01649763  7.323414          758.1845         -7.269204
#>   IR_Isi_basal IR_Bennett IR_HOMA_IR_rev_inv    SPISE METS_IR prediabetes
#> 1   0.17692260  13.391719         -0.2289137 8.297275      NA           0
#> 2   0.13866553  12.041036         -0.2920697 4.177562      NA           0
#> 3   0.14830370  12.530266         -0.2730883 6.351097      NA           0
#> 4   0.08201558   9.977484         -0.4938086 5.963605      NA           0
#> 5   0.18692382  13.320013         -0.2166658 7.139034      NA           0
#> 6   0.15153691  12.424386         -0.2672616 4.529109      NA           0
#>   HOMA_CP LAR ASI TyG_index non_HDL_c  remnant_c ratio_TC_HDL ratio_TG_HDL
#> 1      NA  NA  NA        NA  4.472486  0.4091206     5.952950    1.0716354
#> 2      NA  NA  NA        NA  3.338557  0.2301283     4.340045    1.8606939
#> 3      NA  NA  NA        NA  4.154120  1.4407099     3.679324    0.7970881
#> 4      NA  NA  NA        NA  4.112558  1.1387383     5.058502    1.5935284
#> 5      NA  NA  NA        NA  3.324661  0.2533190     3.088419    1.0120564
#> 6      NA  NA  NA        NA  3.572372 -0.2791537     3.564597    1.5878714
#>   ratio_LDL_HDL ApoB_ApoA1
#> 1      4.499879  0.5855594
#> 2      3.109814  1.0015916
#> 3      1.750095  0.7404117
#> 4      2.934731  0.6027401
#> 5      1.929294  0.4739219
#> 6      2.765001  0.5349333
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
