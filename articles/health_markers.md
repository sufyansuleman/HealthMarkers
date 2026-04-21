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
#>   non_HDL_c remnant_c ratio_TC_HDL ratio_TG_HDL ratio_LDL_HDL ApoB_ApoA1
#> 1      5.14      1.43     4.953846    2.4230769      2.853846  0.6899225
#> 2      4.58      0.33     4.754098    0.5901639      3.483607  0.4120879
#> 3      5.70      0.49     5.523810    0.8492063      4.134921  0.9924812
#> 4      2.97      0.42     3.020408    0.6258503      1.734694  1.0826446
#> 5      3.12      0.75     2.962264    1.0314465      1.490566  0.6736111
#> 6      4.00      0.58     3.380952    0.7619048      2.035714  0.4509804
#>     VAI_Men VAI_Women LAP_Men LAP_Women  TyG_BMI       FLI       NFS      APRI
#> 1 3.0404154  4.614469 105.525   127.575 297.9208 5.5614542 -22.36501 0.2140411
#> 2 0.7834761  1.190929  24.984    30.024 236.1715 2.9074145 -22.49108 0.3308824
#> 3 0.9912081  1.509719  19.260    26.750 230.1380 1.5564455 -30.82639 0.7000000
#> 4 0.8153524  1.242666  24.012    30.452 225.5786 0.7301821 -26.95540 0.5615385
#> 5 1.0959268  1.673274   9.840    21.320 215.1741 0.4444250 -22.01252 0.2272727
#> 6 0.7377906  1.116715  22.528    31.488 317.6446 6.1582809 -28.18628 0.1707650
#>        FIB4 BARD      ALBI  MELD_XI    SPISE  METS_IR prediabetes HOMA_CP LAR
#> 1 0.5779393    1 -1.587680 68.33445 4.849288 302.5694           0      NA  NA
#> 2 0.5623053    2 -1.505498 72.49500 7.113989 352.8944           1      NA  NA
#> 3 1.1033166    1 -2.718063 71.93274 7.510664 298.4802           0      NA  NA
#> 4 1.6544453    2 -2.057258 71.36352 8.292019 188.0396           0      NA  NA
#> 5 0.7601567    0 -1.459018 74.67420 8.381122 134.0136           0      NA  NA
#> 6 0.7069672    1 -1.995310 73.23331 5.086907 179.8589           0      NA  NA
#>   ASI TyG_index
#> 1  NA  9.310025
#> 2  NA  7.951901
#> 3  NA  8.523629
#> 4  NA  8.609869
#> 5  NA  8.928386
#> 6  NA  8.678815
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
#> ── all_health_markers(): column mapping ───────────────────────────────────────────────────────
#>  Data: 30 rows × 519 columns
#> 
#>  User-supplied (11):
#>    ✔ TG                       -> TG
#>    ✔ HDL_c                    -> HDL_c
#>    ✔ LDL_c                    -> LDL_c
#>    ✔ TC                       -> TC
#>    ✔ BMI                      -> BMI
#>    ✔ G0                       -> G0
#>    ✔ I0                       -> I0
#>    ✔ ALT                      -> ALT
#>    ✔ AST                      -> AST
#>    ✔ age                      -> age
#>    ✔ sex                      -> sex
#>  ───────────────────────────────────────────────────────────────────────────────────────
#> ── all_health_markers(): group summary ───────────────────────────────────────────────────────
#>  Computed (4):
#>    ✔ glycemic
#>    ✔ lipid
#>    ✔ renal
#>    ✔ inflammatory
#>  ───────────────────────────────────────────────────────────────────────────────────────

hm_new <- setdiff(names(hm), names(sim_small))
head(dplyr::select(hm, dplyr::all_of(hm_new)))
#>         whr       MAP BUN_Cr_ratio      VLDL non_HDL remnant_c         AIP
#> 1 0.8842011  85.00000   0.05193687 1.4318182    5.14      1.43  0.38436720
#> 2 1.0020101  90.66667   0.12525849 0.3272727    4.58      0.33 -0.22902733
#> 3 0.8058252  81.66667   0.10722892 0.4863636    5.70      0.49 -0.07098677
#> 4 0.9324463  84.33333   0.08955954 0.4181818    2.97      0.42 -0.20352951
#> 5 0.7634409 116.33333   0.09718640 0.7454545    3.12      0.75  0.01344672
#> 6 0.8515464  85.33333   0.14898305 0.5818182    4.00      0.58 -0.11809931
#>      CRI_I   CRI_II HDL_TG_ratio LDL_HDL_ratio      NLR      dNLR       PLR
#> 1 4.953846 2.853846    0.4126984      2.853846 2.502912 1.5340688 154.57914
#> 2 4.754098 3.483607    1.6944444      3.483607 1.583486 1.3036254  93.57798
#> 3 5.523810 4.134921    1.1775701      4.134921 1.892516 0.9910573  87.89553
#> 4 3.020408 1.734694    1.5978261      1.734694 2.299722 1.4876293 226.00834
#> 5 2.962264 1.490566    0.9695122      1.490566 2.580645 2.2602230  84.04075
#> 6 3.380952 2.035714    1.3125000      2.035714 2.733387 1.9553265 293.03443
#>         SII      SIRI  Mg_Zn_den Cu_Zn_den Cort_DHEAS_den    T_E2_den
#> 1  730.8502 1.7395236 0.06681250  1.225000       66.87657 0.133514690
#> 2  323.0312 0.7094018 0.06606667  1.213333       35.59113 0.116924779
#> 3  331.1904 0.8762351 0.10210000  1.610000       52.46440 0.141312384
#> 4  747.4096 0.6002274 0.07342857  1.357143       56.78392 0.159005705
#> 5  510.9677 0.9290323 0.05715385  1.484615       34.27673 0.007779773
#> 6 1000.4195 0.7844820 0.05635294  1.241176       41.55556 0.002063599
#>   TSH_fT4_den Tyr_Phe_Ratio KIM1_gCr NGAL_gCr   NAG_gCr L_FABP_gCr IL18_gCr
#> 1  0.10418719     1.1428571 1.602041 16.32653  5.000000  16.122449 123.7755
#> 2  0.08628743     1.8000000 2.583333 21.56250 11.458333   1.562500 153.9583
#> 3  0.04641026     0.6438356 1.773810 55.35714  4.285714  10.952381 217.2619
#> 4  0.14133803     1.0882353 2.603604 33.60360 10.360360   7.207207 117.7477
#> 5  0.25781095     0.9062500 1.436782 44.02299  5.977011   9.770115 194.7126
#> 6  0.09473684     1.3750000 1.800000 28.18182  9.363636   1.000000 112.7273
#>      SPISE  METS_IR prediabetes HOMA_CP LAR ASI TyG_index non_HDL_c
#> 1 4.849288 302.5694           0      NA  NA  NA  9.310025      5.14
#> 2 7.113989 352.8944           1      NA  NA  NA  7.951901      4.58
#> 3 7.510664 298.4802           0      NA  NA  NA  8.523629      5.70
#> 4 8.292019 188.0396           0      NA  NA  NA  8.609869      2.97
#> 5 8.381122 134.0136           0      NA  NA  NA  8.928386      3.12
#> 6 5.086907 179.8589           0      NA  NA  NA  8.678815      4.00
#>   ratio_TC_HDL ratio_TG_HDL ratio_LDL_HDL ApoB_ApoA1   VAI_Men VAI_Women
#> 1     4.953846    2.4230769      2.853846  0.6899225 3.0404154  4.614469
#> 2     4.754098    0.5901639      3.483607  0.4120879 0.7834761  1.190929
#> 3     5.523810    0.8492063      4.134921  0.9924812 0.9912081  1.509719
#> 4     3.020408    0.6258503      1.734694  1.0826446 0.8153524  1.242666
#> 5     2.962264    1.0314465      1.490566  0.6736111 1.0959268  1.673274
#> 6     3.380952    0.7619048      2.035714  0.4509804 0.7377906  1.116715
#>   LAP_Men LAP_Women  TyG_BMI   eGFR_cr eGFR_cys eGFR_combined FE_Urea
#> 1 105.525   127.575 297.9208 0.4725555 89.82553      6.696628      NA
#> 2  24.984    30.024 236.1715 0.5818192 43.32180      4.949108      NA
#> 3  19.260    26.750 230.1380 0.5165652 80.61015      6.504759      NA
#> 4  24.012    30.452 225.5786 0.6102493 93.72321      7.710013      NA
#> 5   9.840    21.320 215.1741 0.2392153 37.79522      3.155730      NA
#> 6  22.528    31.488 317.6446 0.3701522 57.57784      4.915629      NA
#>   Beta2Micro      LMR      NER      PIV       CLR        CAR       PCR mGPS
#> 1       1.99 2.717986 11.70297 507.9409 1.8845950 0.10439883  82.02247    0
#> 2       1.88 4.866071 10.65432 144.7180 0.6009174 0.03628809 155.72519    0
#> 3       1.47 4.300216 12.11576 153.3411 0.5524862 0.02235772 159.09091    0
#> 4       1.54 5.509579 13.38866 195.0739 1.2378303 0.04178404 182.58427    0
#> 5       1.20 6.544444 13.24619 183.9484 1.2393888 0.08295455  67.80822    0
#> 6       1.75 4.351916 12.59779 287.1204 2.8422738 0.08179724 103.09859    0
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
#> ── all_health_markers(): column mapping ───────────────────────────────────────────────────────
#>  Data: 30 rows × 519 columns
#> 
#>  User-supplied (7):
#>    ✔ G0                       -> G0
#>    ✔ I0                       -> I0
#>    ✔ TG                       -> TG
#>    ✔ HDL_c                    -> HDL_c
#>    ✔ BMI                      -> BMI
#>    ✔ age                      -> age
#>    ✔ sex                      -> sex
#>  ───────────────────────────────────────────────────────────────────────────────────────
#> ── all_health_markers(): group summary ───────────────────────────────────────────────────────
#>  Computed (3):
#>    ✔ insulin_panel
#>    ✔ glycemic
#>    ✔ lipid
#>  ───────────────────────────────────────────────────────────────────────────────────────

ins_new <- setdiff(names(ins), names(sim_small))
head(dplyr::select(ins, dplyr::all_of(ins_new)))
#>         whr       MAP BUN_Cr_ratio      VLDL non_HDL remnant_c         AIP
#> 1 0.8842011  85.00000   0.05193687 1.4318182    5.14      1.43  0.38436720
#> 2 1.0020101  90.66667   0.12525849 0.3272727    4.58      0.33 -0.22902733
#> 3 0.8058252  81.66667   0.10722892 0.4863636    5.70      0.49 -0.07098677
#> 4 0.9324463  84.33333   0.08955954 0.4181818    2.97      0.42 -0.20352951
#> 5 0.7634409 116.33333   0.09718640 0.7454545    3.12      0.75  0.01344672
#> 6 0.8515464  85.33333   0.14898305 0.5818182    4.00      0.58 -0.11809931
#>      CRI_I   CRI_II HDL_TG_ratio LDL_HDL_ratio      NLR      dNLR       PLR
#> 1 4.953846 2.853846    0.4126984      2.853846 2.502912 1.5340688 154.57914
#> 2 4.754098 3.483607    1.6944444      3.483607 1.583486 1.3036254  93.57798
#> 3 5.523810 4.134921    1.1775701      4.134921 1.892516 0.9910573  87.89553
#> 4 3.020408 1.734694    1.5978261      1.734694 2.299722 1.4876293 226.00834
#> 5 2.962264 1.490566    0.9695122      1.490566 2.580645 2.2602230  84.04075
#> 6 3.380952 2.035714    1.3125000      2.035714 2.733387 1.9553265 293.03443
#>         SII      SIRI  Mg_Zn_den Cu_Zn_den Cort_DHEAS_den    T_E2_den
#> 1  730.8502 1.7395236 0.06681250  1.225000       66.87657 0.133514690
#> 2  323.0312 0.7094018 0.06606667  1.213333       35.59113 0.116924779
#> 3  331.1904 0.8762351 0.10210000  1.610000       52.46440 0.141312384
#> 4  747.4096 0.6002274 0.07342857  1.357143       56.78392 0.159005705
#> 5  510.9677 0.9290323 0.05715385  1.484615       34.27673 0.007779773
#> 6 1000.4195 0.7844820 0.05635294  1.241176       41.55556 0.002063599
#>   TSH_fT4_den Tyr_Phe_Ratio KIM1_gCr NGAL_gCr   NAG_gCr L_FABP_gCr IL18_gCr
#> 1  0.10418719     1.1428571 1.602041 16.32653  5.000000  16.122449 123.7755
#> 2  0.08628743     1.8000000 2.583333 21.56250 11.458333   1.562500 153.9583
#> 3  0.04641026     0.6438356 1.773810 55.35714  4.285714  10.952381 217.2619
#> 4  0.14133803     1.0882353 2.603604 33.60360 10.360360   7.207207 117.7477
#> 5  0.25781095     0.9062500 1.436782 44.02299  5.977011   9.770115 194.7126
#> 6  0.09473684     1.3750000 1.800000 28.18182  9.363636   1.000000 112.7273
#>   Fasting_inv  Raynaud HOMA_IR_inv     FIRI    QUICKI Belfiore_basal
#> 1  -11.366667 3.519062   -57.10613 51.39552 0.1396955    0.001555345
#> 2   -4.250000 9.411765   -18.87000 16.98300 0.1652595    0.004699524
#> 3   -9.766667 4.095563   -42.11387 37.90248 0.1459025    0.002108455
#> 4   -8.433333 4.743083   -34.47547 31.02792 0.1502909    0.002575003
#> 5  -14.550000 2.749141   -44.34840 39.91356 0.1448102    0.002002325
#> 6   -7.883333 5.073996   -28.69533 25.82580 0.1545535    0.003092887
#>   Ig_ratio_basal Isi_basal    Bennett HOMA_IR_rev_inv   Isi_120 Cederholm_index
#> 1    -0.10055438   7.78278 0.08701970       -3.172563  4.520129     -0.56104336
#> 2    -0.04254254  23.55296 0.15010822       -1.048333 11.402795      1.68716261
#> 3    -0.10066653  10.55340 0.09591293       -2.339659  8.883204      1.20576626
#> 4    -0.09168660  12.89162 0.10372520       -1.915304  5.823433     -0.05089198
#> 5    -0.21216098  10.02166 0.08833253       -2.463800 13.770463      0.90805754
#> 6    -0.09625560  15.48839 0.10993532       -1.594185 10.794054     -0.03418896
#>   Gutt_index Avignon_Si0 Avignon_Si120 Avignon_Sim Modified_stumvoll
#> 1   1.884723    5.167848      3.001414    4.084631        0.08790964
#> 2   3.306261   15.908791      7.701989   11.805390        0.11377198
#> 3   2.645181    8.277176      6.967219    7.622197        0.10235116
#> 4   2.447943    9.878632      4.462401    7.170517        0.10121880
#> 5   3.261458   10.881278     14.951643   12.916460        0.09910122
#> 6   3.151113   11.434764      7.969032    9.701898        0.10729388
#>   Stumvoll_Demographics Matsuda_AUC Matsuda_ISI   BigttSi     Ifc_inv  HIRI_inv
#> 1            0.08159114  0.04015632    5.389644  5.641586 -0.33563401 -28.95984
#> 2            0.10318358  0.09730413   13.563597  9.675939 -0.66933653 -15.06799
#> 3            0.11109956  0.05206414    7.123916  9.024814 -0.08187877 -25.88895
#> 4            0.10736040  0.04704409    6.715156  9.047209 -0.57585809 -34.80576
#> 5            0.10352132  0.04857617    6.771880 13.404216  0.57345684 -29.52850
#> 6            0.06098938  0.07491055   10.244073  9.125401 -0.12876317 -17.30208
#>   Belfiore_isi_gly Revised_QUICKI VAI_Men_inv VAI_Women_inv TG_HDL_C_inv
#> 1     4.143841e-08      0.3378936  -3.0404154     -4.614469    -5.549830
#> 2     8.039832e-08      0.4438001  -0.7834761     -1.190929    -1.351715
#> 3     5.137065e-08      0.3484546  -0.9912081     -1.509719    -1.945027
#> 4     3.433466e-08      0.3688395  -0.8153524     -1.242666    -1.433451
#> 5     4.709091e-08      0.3936017  -1.0959268     -1.673274    -2.362431
#> 6     7.246191e-08      0.4069380  -0.7377906     -1.116715    -1.745071
#>     TyG_inv LAP_Men_inv LAP_Women_inv McAuley_index Adipo_inv Belfiore_inv_FFA
#> 1 -9.665790    -105.525      -127.575      4.921992 -8.058967       -0.2207757
#> 2 -8.066312     -24.984       -30.024     10.244069 -1.793500       -0.7159477
#> 3 -8.433222     -19.260       -26.750      7.177225 -7.637533       -0.2315476
#> 4 -8.228836     -24.012       -30.452      7.836831 -5.591300       -0.3034303
#> 5 -8.513343      -9.840       -21.320      5.623300 -5.063400       -0.3298479
#> 6 -8.443005     -22.528       -31.488      7.209093 -3.500200       -0.4444247
#>     I_AUC FFA_AUC tracer_palmitate_SI tracer_glycerol_SI  LIRI_inv  Lipo_inv
#> 1 2856.00   85.08          0.13475138         0.13259669 -1.153177 -27.28000
#> 2 1615.25   50.64          0.21412500         0.10768750 -1.027444  -7.32275
#> 3 2755.50   93.84          0.11590426         0.22792553 -1.161611 -41.85017
#> 4 3591.50   79.56          0.13943662         0.12690141 -1.177375 -22.79530
#> 5 3507.25   41.76          0.06589443         0.10252199 -1.295167 -50.86680
#> 6 2200.75   53.28          0.03127536         0.04565217 -1.209682 -12.41625
#>    ATIRI_inv IR_Fasting_inv IR_Raynaud IR_HOMA_IR_inv    IR_FIRI IR_QUICKI
#> 1 -27.723300    -0.08797654  0.2841667    -0.01751125 0.01945695  7.158427
#> 2 -14.560500    -0.23529412  0.1062500    -0.05299417 0.05888241  6.051089
#> 3 -21.281567    -0.10238908  0.2441667    -0.02374515 0.02638350  6.853892
#> 4 -25.047000    -0.11857708  0.2108333    -0.02900613 0.03222904  6.653763
#> 5 -32.693850    -0.06872852  0.3637500    -0.02254873 0.02505414  6.905592
#> 6  -8.506117    -0.12684989  0.1970833    -0.03484887 0.03872097  6.470250
#>   IR_Belfiore_basal IR_Ig_ratio_basal IR_Isi_basal IR_Bennett
#> 1          642.9440         -9.944868    0.1284888  11.491651
#> 2          212.7875        -23.505882    0.0424575   6.661861
#> 3          474.2810         -9.933788    0.0947562  10.426123
#> 4          388.3490        -10.906719    0.0775698   9.640858
#> 5          499.4195         -4.713402    0.0997839  11.320857
#> 6          323.3225        -10.389006    0.0645645   9.096258
#>   IR_HOMA_IR_rev_inv IR_Isi_120 IR_Cederholm_index IR_Gutt_index IR_Avignon_Si0
#> 1         -0.3152026  0.2212326         -1.7823934     0.5305820     0.19350413
#> 2         -0.9538951  0.0876978          0.5927111     0.3024564     0.06285833
#> 3         -0.4274127  0.1125720          0.8293481     0.3780459     0.12081415
#> 4         -0.5221104  0.1717200        -19.6494632     0.4085063     0.10122859
#> 5         -0.4058771  0.0726192          1.1012518     0.3066113     0.09190097
#> 6         -0.6272797  0.0926436        -29.2492058     0.3173482     0.08745262
#>   IR_Avignon_Si120 IR_Avignon_Sim IR_Modified_stumvoll IR_Stumvoll_Demographics
#> 1       0.33317630     0.24482015            11.375317                12.256233
#> 2       0.12983659     0.08470707             8.789510                 9.691464
#> 3       0.14352930     0.13119577             9.770285                 9.000936
#> 4       0.22409460     0.13945996             9.879588                 9.314421
#> 5       0.06688228     0.07742059            10.090693                 9.659846
#> 6       0.12548576     0.10307262             9.320196                16.396297
#>   IR_Matsuda_AUC IR_Matsuda_ISI IR_BigttSi IR_Ifc_inv IR_HIRI_inv
#> 1       24.90268     0.18554100 0.17725513  -2.979436 -0.03453058
#> 2       10.27706     0.07372675 0.10334914  -1.494017 -0.06636583
#> 3       19.20708     0.14037223 0.11080561 -12.213178 -0.03862652
#> 4       21.25666     0.14891687 0.11053133  -1.736539 -0.02873088
#> 5       20.58622     0.14766948 0.07460339   1.743810 -0.03386558
#> 6       13.34925     0.09761742 0.10958423  -7.766196 -0.05779652
#>   IR_Belfiore_isi_gly IR_Revised_QUICKI IR_VAI_Men_inv IR_VAI_Women_inv
#> 1            24132201          2.959512     -0.3289024       -0.2167097
#> 2            12438072          2.253267     -1.2763631       -0.8396808
#> 3            19466368          2.869814     -1.0088699       -0.6623748
#> 4            29125090          2.711206     -1.2264636       -0.8047215
#> 5            21235522          2.540640     -0.9124697       -0.5976307
#> 6            13800354          2.457377     -1.3553982       -0.8954833
#>   IR_TG_HDL_C_inv IR_TyG_inv IR_LAP_Men_inv IR_LAP_Women_inv IR_McAuley_index
#> 1      -0.1801857 -0.1034577   -0.009476427     -0.007838526       0.20316976
#> 2      -0.7398009 -0.1239724   -0.040025616     -0.033306688       0.09761746
#> 3      -0.5141316 -0.1185786   -0.051921080     -0.037383178       0.13932962
#> 4      -0.6976170 -0.1215239   -0.041645844     -0.032838566       0.12760260
#> 5      -0.4232927 -0.1174627   -0.101626016     -0.046904315       0.17783152
#> 6      -0.5730425 -0.1184412   -0.044389205     -0.031758130       0.13871371
#>   IR_Adipo_inv IR_Belfiore_inv_FFA     IR_I_AUC IR_FFA_AUC
#> 1   -0.1240854           -4.529483 0.0003501401 0.01175364
#> 2   -0.5575690           -1.396750 0.0006190992 0.01974724
#> 3   -0.1309323           -4.318767 0.0003629105 0.01065644
#> 4   -0.1788493           -3.295650 0.0002784352 0.01256913
#> 5   -0.1974958           -3.031700 0.0002851237 0.02394636
#> 6   -0.2856980           -2.250100 0.0004543905 0.01876877
#>   IR_tracer_palmitate_SI IR_tracer_glycerol_SI IR_LIRI_inv IR_Lipo_inv
#> 1               7.421074              7.541667  -0.8671696 -0.03665689
#> 2               4.670169              9.286129  -0.9732892 -0.13656072
#> 3               8.627811              4.387398  -0.8608735 -0.02389477
#> 4               7.171717              7.880133  -0.8493467 -0.04386869
#> 5              15.175790              9.754005  -0.7721014 -0.01965919
#> 6              31.974050             21.904762  -0.8266637 -0.08053962
#>   IR_ATIRI_inv    SPISE  METS_IR prediabetes HOMA_CP LAR ASI TyG_index
#> 1  -0.03607074 4.849288 302.5694           0      NA  NA  NA  9.310025
#> 2  -0.06867896 7.113989 352.8944           1      NA  NA  NA  7.951901
#> 3  -0.04698902 7.510664 298.4802           0      NA  NA  NA  8.523629
#> 4  -0.03992494 8.292019 188.0396           0      NA  NA  NA  8.609869
#> 5  -0.03058679 8.381122 134.0136           0      NA  NA  NA  8.928386
#> 6  -0.11756246 5.086907 179.8589           0      NA  NA  NA  8.678815
#>   non_HDL_c ratio_TC_HDL ratio_TG_HDL ratio_LDL_HDL ApoB_ApoA1   VAI_Men
#> 1      5.14     4.953846    2.4230769      2.853846  0.6899225 3.0404154
#> 2      4.58     4.754098    0.5901639      3.483607  0.4120879 0.7834761
#> 3      5.70     5.523810    0.8492063      4.134921  0.9924812 0.9912081
#> 4      2.97     3.020408    0.6258503      1.734694  1.0826446 0.8153524
#> 5      3.12     2.962264    1.0314465      1.490566  0.6736111 1.0959268
#> 6      4.00     3.380952    0.7619048      2.035714  0.4509804 0.7377906
#>   VAI_Women LAP_Men LAP_Women  TyG_BMI
#> 1  4.614469 105.525   127.575 297.9208
#> 2  1.190929  24.984    30.024 236.1715
#> 3  1.509719  19.260    26.750 230.1380
#> 4  1.242666  24.012    30.452 225.5786
#> 5  1.673274   9.840    21.320 215.1741
#> 6  1.116715  22.528    31.488 317.6446
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
#> all_health_markers(): mapping summary -- 13 key(s) mapped
#> ── all_health_markers(): column mapping ───────────────────────────────────────────────────────
#>  Data: 1 row × 5 columns
#> 
#>  User-supplied (5):
#>    ✔ TC                       -> TC
#>    ✔ HDL_c                    -> HDL_c
#>    ✔ TG                       -> TG
#>    ✔ LDL_c                    -> LDL_c
#>    ✔ BMI                      -> BMI
#>  ───────────────────────────────────────────────────────────────────────────────────────
#> health_markers(): calling lipid
#> lipid_markers(): reading input 'data' — 1 rows × 13 variables
#> lipid_markers(): col_map (5 columns — 5 inferred from data)
#>   TC                ->  'TC'    (inferred)
#>   HDL_c             ->  'HDL_c'    (inferred)
#>   TG                ->  'TG'    (inferred)
#>   LDL_c             ->  'LDL_c'    (inferred)
#>   BMI               ->  'BMI'    (inferred)
#> lipid_markers(): pre-computation: glucose cannot be derived -- provide: G0
#> lipid_markers(): optional inputs
#>   present:  BMI
#>   missing:  ApoB, ApoA1, waist, glucose
#>   indices -> NA:
#>   ApoB_ApoA1 -> NA  [missing: ApoB, ApoA1]
#>   VAI -> NA  [missing: waist]
#>   LAP -> NA  [missing: waist]
#>   TyG_BMI -> NA  [missing: glucose]
#>   derivable:
#>   glucose: provide G0 to enable
#> lipid_markers(): computing markers:
#>   non_HDL_c     [TC, HDL_c]
#>   remnant_c     [TC, HDL_c, LDL_c]
#>   ratio_TC_HDL  [TC, HDL_c]
#>   ratio_TG_HDL  [TG, HDL_c]
#>   ratio_LDL_HDL [LDL_c, HDL_c]
#>   ApoB_ApoA1    NA [ApoB/ApoA1 missing]
#>   VAI_Men/Women NA [waist/BMI missing]
#>   LAP_Men/Women NA [waist missing]
#>   TyG_BMI       NA [BMI/glucose missing]
#> lipid_markers(): results: non_HDL_c 1/1, remnant_c 1/1, ratio_TC_HDL 1/1, ratio_TG_HDL 1/1, ratio_LDL_HDL 1/1, ApoB_ApoA1 0/1
#> all_health_markers(): computed: lipid
#> ── all_health_markers(): group summary ───────────────────────────────────────────────────────
#>  Computed (1):
#>    ✔ lipid
#>  ───────────────────────────────────────────────────────────────────────────────────────
#>    TC HDL_c  TG LDL_c BMI     VLDL non_HDL remnant_c       AIP CRI_I CRI_II
#> 1 200    50 150   120  25 68.18182     150        30 0.4771213     4    2.4
#>   HDL_TG_ratio LDL_HDL_ratio non_HDL_c ratio_TC_HDL ratio_TG_HDL ratio_LDL_HDL
#> 1    0.3333333           2.4       150            4            3           2.4
#>   ApoB_ApoA1
#> 1         NA
options(old_opt)
```

## Column recognition

Run `hm_col_report(your_data)` to check which analyte columns are
auto-detected across all marker categories, and get a ready-to-paste
`col_map` template for any that are not. See the [Multi-Biobank
Compatibility](https://sufyansuleman.github.io/HealthMarkers/articles/articles/multi_biobank.md)
article for recognised synonyms across 10 major biobanks.

``` r
hm_col_report(your_data)
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
