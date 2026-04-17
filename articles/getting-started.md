# Getting Started with HealthMarkers

## Overview

Load the bundled simulated dataset, pick which markers to compute, and
view only the outputs you care about. All examples use shipped data;
swap in your data when ready.

## When to use

- You want a quick tour of core helpers and the
  [`all_health_markers()`](https://sufyansuleman.github.io/HealthMarkers/reference/all_health_markers.md)
  orchestrator.
- You need to see column inference vs explicit mapping for your data.
- You prefer ready-to-run code snippets (no optional dependencies
  required for these examples).

## What the package offers

- Many clinical/metabolic markers: glycemic, lipid, renal, inflammatory,
  liver, anthropometric/obesity, atherogenic indices, frailty/Charlson,
  sweat/urine panels, micronutrients, pulmonary indices, and more.
- Single-purpose helpers (e.g.,
  [`cvd_marker_aip()`](https://sufyansuleman.github.io/HealthMarkers/reference/cvd_marker_aip.md),
  [`sweat_markers()`](https://sufyansuleman.github.io/HealthMarkers/reference/sweat_markers.md),
  [`fasting_is()`](https://sufyansuleman.github.io/HealthMarkers/reference/fasting_is.md))
  plus
  [`all_health_markers()`](https://sufyansuleman.github.io/HealthMarkers/reference/all_health_markers.md)
  to run multiple groups at once.
- Column mapping diagnostics with
  [`hm_col_report()`](https://sufyansuleman.github.io/HealthMarkers/reference/hm_col_report.md)
  and override via `col_map` when your labels differ.
- Light missing-data utilities
  ([`impute_missing()`](https://sufyansuleman.github.io/HealthMarkers/reference/impute_missing.md))
  to prep data before computing markers.

## Quick starts

- Targeted helper: `cvd_marker_aip(sim_small)` or
  `renal_markers(sim_small)`.
- Multiple groups:
  `all_health_markers(sim_small, which = c("glycemic", "lipid"))`.
- Everything: `all_health_markers(sim_small, which = "all")` (wide
  output; filter afterward).
- Insulin sensitivity/resistance:
  [`fasting_is()`](https://sufyansuleman.github.io/HealthMarkers/reference/fasting_is.md),
  [`ogtt_is()`](https://sufyansuleman.github.io/HealthMarkers/reference/ogtt_is.md),
  [`adipo_is()`](https://sufyansuleman.github.io/HealthMarkers/reference/adipo_is.md),
  or include insulin groups in
  [`all_health_markers()`](https://sufyansuleman.github.io/HealthMarkers/reference/all_health_markers.md)
  with a `col_map` for glucose/insulin.

## Common helpers (at a glance)

- `all_health_markers(data, which = ...)` — run multiple groups or all
  markers at once.
- `cvd_marker_aip(data, col_map = list(TG = "TG", HDL_c = "HDL_c"))` —
  atherogenic index of plasma.
- `renal_markers(data)` — renal and eGFR-related outputs.
- `glycemic_markers(data)` — glycemic metrics; pair with
  [`fasting_is()`](https://sufyansuleman.github.io/HealthMarkers/reference/fasting_is.md)
  /
  [`ogtt_is()`](https://sufyansuleman.github.io/HealthMarkers/reference/ogtt_is.md)
  /
  [`adipo_is()`](https://sufyansuleman.github.io/HealthMarkers/reference/adipo_is.md)
  for insulin indices.
- `sweat_markers(data)` and `urine_markers(data)` — alternate biofluid
  panels.

## Load the package and simulated data

Inputs: the package plus the bundled RDS file at
`inst/extdata/simulated_hm_data.rds`.

Outputs: two data frames—`sim` (full) and `sim_small` (first 30 rows for
fast examples).

``` r
if (!requireNamespace("HealthMarkers", quietly = TRUE)) {
  if (requireNamespace("pkgload", quietly = TRUE)) {
    pkgload::load_all("..")
  } else {
    stop("Install HealthMarkers (or pkgload for development) before knitting.", call. = FALSE)
  }
}
library(HealthMarkers)

sim_path <- system.file("extdata", "simulated_hm_data.rds", package = "HealthMarkers")
sim <- readRDS(sim_path)

# Work with a small subset using slice_head
sim_small <- dplyr::slice_head(sim, n = 30)

dim(sim_small)
#> [1]  30 519
```

## Minimal example: broad panels

Input: `sim_small` with inferred columns.

What it does:
[`all_health_markers()`](https://sufyansuleman.github.io/HealthMarkers/reference/all_health_markers.md)
auto-detects columns and computes the requested groups.

Output: a tibble of computed markers. Below we print only the new
columns.

``` r
hm_out <- all_health_markers(
  data = sim_small,
  which = c("glycemic", "lipid", "renal", "inflammatory"),
  verbose = FALSE
)

hm_new_cols <- setdiff(names(hm_out), names(sim_small))
head(dplyr::select(hm_out, dplyr::all_of(hm_new_cols)))
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
#>         SII      SIRI sodium_potassium_ratio  Mg_Zn_den Cu_Zn_den
#> 1  730.8502 1.7395236               33.25688 0.06681250  1.225000
#> 2  323.0312 0.7094018               36.55352 0.06606667  1.213333
#> 3  331.1904 0.8762351               39.16667 0.10210000  1.610000
#> 4  747.4096 0.6002274               28.63158 0.07342857  1.357143
#> 5  510.9677 0.9290323               37.63736 0.05715385  1.484615
#> 6 1000.4195 0.7844820               34.33875 0.05635294  1.241176
#>   Cort_DHEAS_den    T_E2_den TSH_fT4_den Tyr_Phe_Ratio KIM1_gCr NGAL_gCr
#> 1       66.87657 0.133514690  0.10418719     1.1428571 1.602041 16.32653
#> 2       35.59113 0.116924779  0.08628743     1.8000000 2.583333 21.56250
#> 3       52.46440 0.141312384  0.04641026     0.6438356 1.773810 55.35714
#> 4       56.78392 0.159005705  0.14133803     1.0882353 2.603604 33.60360
#> 5       34.27673 0.007779773  0.25781095     0.9062500 1.436782 44.02299
#> 6       41.55556 0.002063599  0.09473684     1.3750000 1.800000 28.18182
#>     NAG_gCr L_FABP_gCr IL18_gCr Fasting_inv  Raynaud HOMA_IR_inv     FIRI
#> 1  5.000000  16.122449 123.7755  -11.366667 3.519062   -57.10613 51.39552
#> 2 11.458333   1.562500 153.9583   -4.250000 9.411765   -18.87000 16.98300
#> 3  4.285714  10.952381 217.2619   -9.766667 4.095563   -42.11387 37.90248
#> 4 10.360360   7.207207 117.7477   -8.433333 4.743083   -34.47547 31.02792
#> 5  5.977011   9.770115 194.7126  -14.550000 2.749141   -44.34840 39.91356
#> 6  9.363636   1.000000 112.7273   -7.883333 5.073996   -28.69533 25.82580
#>      QUICKI Belfiore_basal Ig_ratio_basal Isi_basal    Bennett HOMA_IR_rev_inv
#> 1 0.1396955    0.001555345    -0.10055438   7.78278 0.08701970       -3.172563
#> 2 0.1652595    0.004699524    -0.04254254  23.55296 0.15010822       -1.048333
#> 3 0.1459025    0.002108455    -0.10066653  10.55340 0.09591293       -2.339659
#> 4 0.1502909    0.002575003    -0.09168660  12.89162 0.10372520       -1.915304
#> 5 0.1448102    0.002002325    -0.21216098  10.02166 0.08833253       -2.463800
#> 6 0.1545535    0.003092887    -0.09625560  15.48839 0.10993532       -1.594185
#>     Isi_120 Cederholm_index Gutt_index Avignon_Si0 Avignon_Si120 Avignon_Sim
#> 1  4.520129     -0.56104336   1.884723    5.167848      3.001414    4.084631
#> 2 11.402795      1.68716261   3.306261   15.908791      7.701989   11.805390
#> 3  8.883204      1.20576626   2.645181    8.277176      6.967219    7.622197
#> 4  5.823433     -0.05089198   2.447943    9.878632      4.462401    7.170517
#> 5 13.770463      0.90805754   3.261458   10.881278     14.951643   12.916460
#> 6 10.794054     -0.03418896   3.151113   11.434764      7.969032    9.701898
#>   Modified_stumvoll Stumvoll_Demographics Matsuda_AUC Matsuda_ISI   BigttSi
#> 1        0.08790964            0.08159114  0.04015632    5.389644  5.641586
#> 2        0.11377198            0.10318358  0.09730413   13.563597  9.675939
#> 3        0.10235116            0.11109956  0.05206414    7.123916  9.024814
#> 4        0.10121880            0.10736040  0.04704409    6.715156  9.047209
#> 5        0.09910122            0.10352132  0.04857617    6.771880 13.404216
#> 6        0.10729388            0.06098938  0.07491055   10.244073  9.125401
#>       Ifc_inv  HIRI_inv Belfiore_isi_gly Revised_QUICKI VAI_Men_inv
#> 1 -0.33563401 -28.95984     4.143841e-08      0.3378936  -3.0404154
#> 2 -0.66933653 -15.06799     8.039832e-08      0.4438001  -0.7834761
#> 3 -0.08187877 -25.88895     5.137065e-08      0.3484546  -0.9912081
#> 4 -0.57585809 -34.80576     3.433466e-08      0.3688395  -0.8153524
#> 5  0.57345684 -29.52850     4.709091e-08      0.3936017  -1.0959268
#> 6 -0.12876317 -17.30208     7.246191e-08      0.4069380  -0.7377906
#>   VAI_Women_inv TG_HDL_C_inv   TyG_inv LAP_Men_inv LAP_Women_inv McAuley_index
#> 1     -4.614469    -5.549830 -9.665790    -105.525      -127.575      4.921992
#> 2     -1.190929    -1.351715 -8.066312     -24.984       -30.024     10.244069
#> 3     -1.509719    -1.945027 -8.433222     -19.260       -26.750      7.177225
#> 4     -1.242666    -1.433451 -8.228836     -24.012       -30.452      7.836831
#> 5     -1.673274    -2.362431 -8.513343      -9.840       -21.320      5.623300
#> 6     -1.116715    -1.745071 -8.443005     -22.528       -31.488      7.209093
#>   Adipo_inv Belfiore_inv_FFA   I_AUC FFA_AUC tracer_palmitate_SI
#> 1 -8.058967       -0.2207757 2856.00   85.08          0.13475138
#> 2 -1.793500       -0.7159477 1615.25   50.64          0.21412500
#> 3 -7.637533       -0.2315476 2755.50   93.84          0.11590426
#> 4 -5.591300       -0.3034303 3591.50   79.56          0.13943662
#> 5 -5.063400       -0.3298479 3507.25   41.76          0.06589443
#> 6 -3.500200       -0.4444247 2200.75   53.28          0.03127536
#>   tracer_glycerol_SI  LIRI_inv  Lipo_inv  ATIRI_inv IR_Fasting_inv IR_Raynaud
#> 1         0.13259669 -1.153177 -27.28000 -27.723300    -0.08797654  0.2841667
#> 2         0.10768750 -1.027444  -7.32275 -14.560500    -0.23529412  0.1062500
#> 3         0.22792553 -1.161611 -41.85017 -21.281567    -0.10238908  0.2441667
#> 4         0.12690141 -1.177375 -22.79530 -25.047000    -0.11857708  0.2108333
#> 5         0.10252199 -1.295167 -50.86680 -32.693850    -0.06872852  0.3637500
#> 6         0.04565217 -1.209682 -12.41625  -8.506117    -0.12684989  0.1970833
#>   IR_HOMA_IR_inv    IR_FIRI IR_QUICKI IR_Belfiore_basal IR_Ig_ratio_basal
#> 1    -0.01751125 0.01945695  7.158427          642.9440         -9.944868
#> 2    -0.05299417 0.05888241  6.051089          212.7875        -23.505882
#> 3    -0.02374515 0.02638350  6.853892          474.2810         -9.933788
#> 4    -0.02900613 0.03222904  6.653763          388.3490        -10.906719
#> 5    -0.02254873 0.02505414  6.905592          499.4195         -4.713402
#> 6    -0.03484887 0.03872097  6.470250          323.3225        -10.389006
#>   IR_Isi_basal IR_Bennett IR_HOMA_IR_rev_inv IR_Isi_120 IR_Cederholm_index
#> 1    0.1284888  11.491651         -0.3152026  0.2212326         -1.7823934
#> 2    0.0424575   6.661861         -0.9538951  0.0876978          0.5927111
#> 3    0.0947562  10.426123         -0.4274127  0.1125720          0.8293481
#> 4    0.0775698   9.640858         -0.5221104  0.1717200        -19.6494632
#> 5    0.0997839  11.320857         -0.4058771  0.0726192          1.1012518
#> 6    0.0645645   9.096258         -0.6272797  0.0926436        -29.2492058
#>   IR_Gutt_index IR_Avignon_Si0 IR_Avignon_Si120 IR_Avignon_Sim
#> 1     0.5305820     0.19350413       0.33317630     0.24482015
#> 2     0.3024564     0.06285833       0.12983659     0.08470707
#> 3     0.3780459     0.12081415       0.14352930     0.13119577
#> 4     0.4085063     0.10122859       0.22409460     0.13945996
#> 5     0.3066113     0.09190097       0.06688228     0.07742059
#> 6     0.3173482     0.08745262       0.12548576     0.10307262
#>   IR_Modified_stumvoll IR_Stumvoll_Demographics IR_Matsuda_AUC IR_Matsuda_ISI
#> 1            11.375317                12.256233       24.90268     0.18554100
#> 2             8.789510                 9.691464       10.27706     0.07372675
#> 3             9.770285                 9.000936       19.20708     0.14037223
#> 4             9.879588                 9.314421       21.25666     0.14891687
#> 5            10.090693                 9.659846       20.58622     0.14766948
#> 6             9.320196                16.396297       13.34925     0.09761742
#>   IR_BigttSi IR_Ifc_inv IR_HIRI_inv IR_Belfiore_isi_gly IR_Revised_QUICKI
#> 1 0.17725513  -2.979436 -0.03453058            24132201          2.959512
#> 2 0.10334914  -1.494017 -0.06636583            12438072          2.253267
#> 3 0.11080561 -12.213178 -0.03862652            19466368          2.869814
#> 4 0.11053133  -1.736539 -0.02873088            29125090          2.711206
#> 5 0.07460339   1.743810 -0.03386558            21235522          2.540640
#> 6 0.10958423  -7.766196 -0.05779652            13800354          2.457377
#>   IR_VAI_Men_inv IR_VAI_Women_inv IR_TG_HDL_C_inv IR_TyG_inv IR_LAP_Men_inv
#> 1     -0.3289024       -0.2167097      -0.1801857 -0.1034577   -0.009476427
#> 2     -1.2763631       -0.8396808      -0.7398009 -0.1239724   -0.040025616
#> 3     -1.0088699       -0.6623748      -0.5141316 -0.1185786   -0.051921080
#> 4     -1.2264636       -0.8047215      -0.6976170 -0.1215239   -0.041645844
#> 5     -0.9124697       -0.5976307      -0.4232927 -0.1174627   -0.101626016
#> 6     -1.3553982       -0.8954833      -0.5730425 -0.1184412   -0.044389205
#>   IR_LAP_Women_inv IR_McAuley_index IR_Adipo_inv IR_Belfiore_inv_FFA
#> 1     -0.007838526       0.20316976   -0.1240854           -4.529483
#> 2     -0.033306688       0.09761746   -0.5575690           -1.396750
#> 3     -0.037383178       0.13932962   -0.1309323           -4.318767
#> 4     -0.032838566       0.12760260   -0.1788493           -3.295650
#> 5     -0.046904315       0.17783152   -0.1974958           -3.031700
#> 6     -0.031758130       0.13871371   -0.2856980           -2.250100
#>       IR_I_AUC IR_FFA_AUC IR_tracer_palmitate_SI IR_tracer_glycerol_SI
#> 1 0.0003501401 0.01175364               7.421074              7.541667
#> 2 0.0006190992 0.01974724               4.670169              9.286129
#> 3 0.0003629105 0.01065644               8.627811              4.387398
#> 4 0.0002784352 0.01256913               7.171717              7.880133
#> 5 0.0002851237 0.02394636              15.175790              9.754005
#> 6 0.0004543905 0.01876877              31.974050             21.904762
#>   IR_LIRI_inv IR_Lipo_inv IR_ATIRI_inv    SPISE  METS_IR prediabetes HOMA_CP
#> 1  -0.8671696 -0.03665689  -0.03607074 4.849288 302.5694           0      NA
#> 2  -0.9732892 -0.13656072  -0.06867896 7.113989 352.8944           1      NA
#> 3  -0.8608735 -0.02389477  -0.04698902 7.510664 298.4802           0      NA
#> 4  -0.8493467 -0.04386869  -0.03992494 8.292019 188.0396           0      NA
#> 5  -0.7721014 -0.01965919  -0.03058679 8.381122 134.0136           0      NA
#> 6  -0.8266637 -0.08053962  -0.11756246 5.086907 179.8589           0      NA
#>   LAR ASI TyG_index non_HDL_c ratio_TC_HDL ratio_TG_HDL ratio_LDL_HDL
#> 1  NA  NA  9.310025      5.14     4.953846    2.4230769      2.853846
#> 2  NA  NA  7.951901      4.58     4.754098    0.5901639      3.483607
#> 3  NA  NA  8.523629      5.70     5.523810    0.8492063      4.134921
#> 4  NA  NA  8.609869      2.97     3.020408    0.6258503      1.734694
#> 5  NA  NA  8.928386      3.12     2.962264    1.0314465      1.490566
#> 6  NA  NA  8.678815      4.00     3.380952    0.7619048      2.035714
#>   ApoB_ApoA1   VAI_Men VAI_Women LAP_Men LAP_Women  TyG_BMI   eGFR_cr eGFR_cys
#> 1  0.6899225 3.0404154  4.614469 105.525   127.575 297.9208 0.4725555 89.82553
#> 2  0.4120879 0.7834761  1.190929  24.984    30.024 236.1715 0.5818192 43.32180
#> 3  0.9924812 0.9912081  1.509719  19.260    26.750 230.1380 0.5165652 80.61015
#> 4  1.0826446 0.8153524  1.242666  24.012    30.452 225.5786 0.6102493 93.72321
#> 5  0.6736111 1.0959268  1.673274   9.840    21.320 215.1741 0.2392153 37.79522
#> 6  0.4509804 0.7377906  1.116715  22.528    31.488 317.6446 0.3701522 57.57784
#>   eGFR_combined FE_Urea Beta2Micro      LMR      NER      PIV       CLR
#> 1      6.696628      NA       1.99 2.717986 11.70297 507.9409 1.8845950
#> 2      4.949108      NA       1.88 4.866071 10.65432 144.7180 0.6009174
#> 3      6.504759      NA       1.47 4.300216 12.11576 153.3411 0.5524862
#> 4      7.710013      NA       1.54 5.509579 13.38866 195.0739 1.2378303
#> 5      3.155730      NA       1.20 6.544444 13.24619 183.9484 1.2393888
#> 6      4.915629      NA       1.75 4.351916 12.59779 287.1204 2.8422738
#>          CAR       PCR mGPS
#> 1 0.10439883  82.02247    0
#> 2 0.03628809 155.72519    0
#> 3 0.02235772 159.09091    0
#> 4 0.04178404 182.58427    0
#> 5 0.08295455  67.80822    0
#> 6 0.08179724 103.09859    0

# New columns added (names only):
head(hm_new_cols)
#> [1] "whr"          "MAP"          "BUN_Cr_ratio" "VLDL"         "non_HDL"     
#> [6] "remnant_c"
```

## Data readiness: columns at a glance

Most functions infer columns automatically. If your names differ,
provide a `col_map`. Typical defaults:

| Panel                                     | Typical columns                                  | Notes                                                                                                                                                        |
|-------------------------------------------|--------------------------------------------------|--------------------------------------------------------------------------------------------------------------------------------------------------------------|
| Glycemic                                  | `FPG`, `insulin_fasting`, `HbA1c`                | Uses fasting and HbA1c where available                                                                                                                       |
| Lipid                                     | `TG`, `HDL_c`, `LDL_c`, `TC`                     | Supports TG/HDL aliasing for AIP                                                                                                                             |
| Renal                                     | `creatinine`, `uacr`, `age`, `sex`               | Infers eGFR and kidney markers                                                                                                                               |
| Blood pressure (used by CVD risk helpers) | `SBP`, `DBP`                                     | Inputs to CVD risk functions; not a standalone [`all_health_markers()`](https://sufyansuleman.github.io/HealthMarkers/reference/all_health_markers.md) group |
| Anthropometrics                           | `height`, `weight`, `BMI`, `waist_circumference` | Supports z-score and obesity indices                                                                                                                         |

### The easiest way: `hm_col_report()`

Call
[`hm_col_report()`](https://sufyansuleman.github.io/HealthMarkers/reference/hm_col_report.md)
**before running any computation** to see a full column-mapping report
for your data:

``` r
# See which columns are matched and how — no computation performed
hm_col_report(my_data)

# Show all 258 keys, including unmatched ones
hm_col_report(my_data, show_unmatched = TRUE)

# Capture the matched col_map to pass directly to any function
cm <- hm_col_report(my_data, verbose = FALSE)
all_health_markers(my_data, col_map = cm)
```

The report shows: - **key** — the internal name all functions use -
**data_column** — the matched column in your data - **how matched** —
`exact`, `case-insensitive`, `col contains synonym`, or
`synonym contains col` - A ready-to-paste `col_map` template for any
keys not found

Column inference is usually enough; if your data use different labels,
provide a `col_map` override.

## Column inference vs explicit mapping

Input: the same data but with explicit column aliases.

What it does: provides TG/HDL names to compute AIP when your columns
differ.

Output: a tibble with the AIP result.

``` r
lipid_cols <- list(TG = "TG", HDL_c = "HDL_c")

aip <- cvd_marker_aip(sim_small, col_map = lipid_cols, na_action = "keep")
aip_new <- setdiff(names(aip), names(sim_small))
head(dplyr::select(aip, dplyr::all_of(aip_new)))
#> # A tibble: 6 × 2
#>   model   value
#>   <chr>   <dbl>
#> 1 AIP    0.384 
#> 2 AIP   -0.229 
#> 3 AIP   -0.0710
#> 4 AIP   -0.204 
#> 5 AIP    0.0134
#> 6 AIP   -0.118
```

## Handling missingness

Most functions accept `na_action`. This shows how outputs differ when
required inputs are missing.

``` r
missing_demo <- sim_small[1:5, c("TG", "HDL_c")]
missing_demo$TG[2] <- NA

# Keep rows (produces NA where inputs are missing)
cvd_marker_aip(missing_demo, col_map = lipid_cols, na_action = "keep")
#> # A tibble: 5 × 2
#>   model   value
#>   <chr>   <dbl>
#> 1 AIP    0.384 
#> 2 AIP   NA     
#> 3 AIP   -0.0710
#> 4 AIP   -0.204 
#> 5 AIP    0.0134

# Drop rows with required NA
cvd_marker_aip(missing_demo, col_map = lipid_cols, na_action = "omit")
#> # A tibble: 4 × 2
#>   model   value
#>   <chr>   <dbl>
#> 1 AIP    0.384 
#> 2 AIP   -0.0710
#> 3 AIP   -0.204 
#> 4 AIP    0.0134
```

> **`na_action` aliases:** `"ignore"` is a backward-compatible alias for
> `"keep"` (same behavior; retained so older code continues to work).
> `"warn"` is also an alias for `"keep"` that additionally emits a
> missingness warning. If you are reading another function’s help page
> and see `"ignore"` listed first in the choices, it behaves identically
> to `"keep"`.

If you want to impute before computing markers:

``` r
imputed <- impute_missing(sim_small, method = "mice", m = 1, maxit = 3)
head(imputed)
```

### Imputation options (what happens under the hood)

- [`impute_missing()`](https://sufyansuleman.github.io/HealthMarkers/reference/impute_missing.md)
  is a preprocessing helper; marker functions do not impute internally.
  Run it first, then call calculators.
- Methods:
  - `method = "mice"`: multiple imputation by chained equations via
    **mice**; control `m` (imputations) and `maxit` (iterations).
  - `method = "missForest"`: nonparametric random-forest imputation via
    **missForest**; good for mixed data types.
  - `method = "mean" | "median" | "constant"`: simple deterministic
    fills for numeric columns.
- Output: a data frame with imputed values replacing missing entries;
  then pass it to
  [`all_health_markers()`](https://sufyansuleman.github.io/HealthMarkers/reference/all_health_markers.md)
  or any helper (e.g.,
  [`cvd_marker_aip()`](https://sufyansuleman.github.io/HealthMarkers/reference/cvd_marker_aip.md),
  [`renal_markers()`](https://sufyansuleman.github.io/HealthMarkers/reference/renal_markers.md)).

## Sweat markers

Compute sweat markers; uses auto-inferred column names and verbose NA
diagnostics.

``` r
sweat <- sweat_markers(sim_small, verbose = FALSE)
sweat_new <- setdiff(names(sweat), names(sim_small))
head(dplyr::select(sweat, dplyr::all_of(sweat_new)))
#> # A tibble: 6 × 2
#>   Na_K_ratio sweat_rate
#>        <dbl>      <dbl>
#> 1       5.63    0.00347
#> 2       8.22    0.0199 
#> 3       3.49    0.00640
#> 4       7.12    0.00572
#> 5       5.74    0.00194
#> 6       7.96    0.00710
```

## Verbose mode: skipped optional markers

When a function supports optional inputs (columns that extend outputs
when present), set `verbose = TRUE` to see which optional markers were
not computed because their columns were absent from the data. This helps
you distinguish “column not found” from “marker intentionally absent”.

``` r
# Provide only required columns; optional columns are absent
glyc_min <- sim_small[, c("HDL_c", "TG", "BMI")]

# verbose = TRUE lists which optional markers were skipped
glyc_out <- glycemic_markers(glyc_min, verbose = TRUE)
```

The informational message lists all optional keys (glucose, HbA1c,
C_peptide, G0, I0, leptin, adiponectin) that had no matching column, so
the caller knows which derived metrics (e.g., prediabetes flag, HOMA_CP,
LAR) will be `NA` in the output. Once you add those columns to your
data, the message disappears and the metrics are computed automatically.

## Common functions

### Cardio-metabolic

Input: `sim_small`; groups: glycemic, lipid, renal, inflammatory.

Output: a tibble with those markers (first rows shown).

``` r
cardio_panel <- all_health_markers(
  data = sim_small,
  which = c("glycemic", "lipid", "renal", "inflammatory"),
  verbose = FALSE
)

cardio_new <- setdiff(names(cardio_panel), names(sim_small))
head(dplyr::select(cardio_panel, dplyr::all_of(cardio_new)))
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
#>         SII      SIRI sodium_potassium_ratio  Mg_Zn_den Cu_Zn_den
#> 1  730.8502 1.7395236               33.25688 0.06681250  1.225000
#> 2  323.0312 0.7094018               36.55352 0.06606667  1.213333
#> 3  331.1904 0.8762351               39.16667 0.10210000  1.610000
#> 4  747.4096 0.6002274               28.63158 0.07342857  1.357143
#> 5  510.9677 0.9290323               37.63736 0.05715385  1.484615
#> 6 1000.4195 0.7844820               34.33875 0.05635294  1.241176
#>   Cort_DHEAS_den    T_E2_den TSH_fT4_den Tyr_Phe_Ratio KIM1_gCr NGAL_gCr
#> 1       66.87657 0.133514690  0.10418719     1.1428571 1.602041 16.32653
#> 2       35.59113 0.116924779  0.08628743     1.8000000 2.583333 21.56250
#> 3       52.46440 0.141312384  0.04641026     0.6438356 1.773810 55.35714
#> 4       56.78392 0.159005705  0.14133803     1.0882353 2.603604 33.60360
#> 5       34.27673 0.007779773  0.25781095     0.9062500 1.436782 44.02299
#> 6       41.55556 0.002063599  0.09473684     1.3750000 1.800000 28.18182
#>     NAG_gCr L_FABP_gCr IL18_gCr Fasting_inv  Raynaud HOMA_IR_inv     FIRI
#> 1  5.000000  16.122449 123.7755  -11.366667 3.519062   -57.10613 51.39552
#> 2 11.458333   1.562500 153.9583   -4.250000 9.411765   -18.87000 16.98300
#> 3  4.285714  10.952381 217.2619   -9.766667 4.095563   -42.11387 37.90248
#> 4 10.360360   7.207207 117.7477   -8.433333 4.743083   -34.47547 31.02792
#> 5  5.977011   9.770115 194.7126  -14.550000 2.749141   -44.34840 39.91356
#> 6  9.363636   1.000000 112.7273   -7.883333 5.073996   -28.69533 25.82580
#>      QUICKI Belfiore_basal Ig_ratio_basal Isi_basal    Bennett HOMA_IR_rev_inv
#> 1 0.1396955    0.001555345    -0.10055438   7.78278 0.08701970       -3.172563
#> 2 0.1652595    0.004699524    -0.04254254  23.55296 0.15010822       -1.048333
#> 3 0.1459025    0.002108455    -0.10066653  10.55340 0.09591293       -2.339659
#> 4 0.1502909    0.002575003    -0.09168660  12.89162 0.10372520       -1.915304
#> 5 0.1448102    0.002002325    -0.21216098  10.02166 0.08833253       -2.463800
#> 6 0.1545535    0.003092887    -0.09625560  15.48839 0.10993532       -1.594185
#>     Isi_120 Cederholm_index Gutt_index Avignon_Si0 Avignon_Si120 Avignon_Sim
#> 1  4.520129     -0.56104336   1.884723    5.167848      3.001414    4.084631
#> 2 11.402795      1.68716261   3.306261   15.908791      7.701989   11.805390
#> 3  8.883204      1.20576626   2.645181    8.277176      6.967219    7.622197
#> 4  5.823433     -0.05089198   2.447943    9.878632      4.462401    7.170517
#> 5 13.770463      0.90805754   3.261458   10.881278     14.951643   12.916460
#> 6 10.794054     -0.03418896   3.151113   11.434764      7.969032    9.701898
#>   Modified_stumvoll Stumvoll_Demographics Matsuda_AUC Matsuda_ISI   BigttSi
#> 1        0.08790964            0.08159114  0.04015632    5.389644  5.641586
#> 2        0.11377198            0.10318358  0.09730413   13.563597  9.675939
#> 3        0.10235116            0.11109956  0.05206414    7.123916  9.024814
#> 4        0.10121880            0.10736040  0.04704409    6.715156  9.047209
#> 5        0.09910122            0.10352132  0.04857617    6.771880 13.404216
#> 6        0.10729388            0.06098938  0.07491055   10.244073  9.125401
#>       Ifc_inv  HIRI_inv Belfiore_isi_gly Revised_QUICKI VAI_Men_inv
#> 1 -0.33563401 -28.95984     4.143841e-08      0.3378936  -3.0404154
#> 2 -0.66933653 -15.06799     8.039832e-08      0.4438001  -0.7834761
#> 3 -0.08187877 -25.88895     5.137065e-08      0.3484546  -0.9912081
#> 4 -0.57585809 -34.80576     3.433466e-08      0.3688395  -0.8153524
#> 5  0.57345684 -29.52850     4.709091e-08      0.3936017  -1.0959268
#> 6 -0.12876317 -17.30208     7.246191e-08      0.4069380  -0.7377906
#>   VAI_Women_inv TG_HDL_C_inv   TyG_inv LAP_Men_inv LAP_Women_inv McAuley_index
#> 1     -4.614469    -5.549830 -9.665790    -105.525      -127.575      4.921992
#> 2     -1.190929    -1.351715 -8.066312     -24.984       -30.024     10.244069
#> 3     -1.509719    -1.945027 -8.433222     -19.260       -26.750      7.177225
#> 4     -1.242666    -1.433451 -8.228836     -24.012       -30.452      7.836831
#> 5     -1.673274    -2.362431 -8.513343      -9.840       -21.320      5.623300
#> 6     -1.116715    -1.745071 -8.443005     -22.528       -31.488      7.209093
#>   Adipo_inv Belfiore_inv_FFA   I_AUC FFA_AUC tracer_palmitate_SI
#> 1 -8.058967       -0.2207757 2856.00   85.08          0.13475138
#> 2 -1.793500       -0.7159477 1615.25   50.64          0.21412500
#> 3 -7.637533       -0.2315476 2755.50   93.84          0.11590426
#> 4 -5.591300       -0.3034303 3591.50   79.56          0.13943662
#> 5 -5.063400       -0.3298479 3507.25   41.76          0.06589443
#> 6 -3.500200       -0.4444247 2200.75   53.28          0.03127536
#>   tracer_glycerol_SI  LIRI_inv  Lipo_inv  ATIRI_inv IR_Fasting_inv IR_Raynaud
#> 1         0.13259669 -1.153177 -27.28000 -27.723300    -0.08797654  0.2841667
#> 2         0.10768750 -1.027444  -7.32275 -14.560500    -0.23529412  0.1062500
#> 3         0.22792553 -1.161611 -41.85017 -21.281567    -0.10238908  0.2441667
#> 4         0.12690141 -1.177375 -22.79530 -25.047000    -0.11857708  0.2108333
#> 5         0.10252199 -1.295167 -50.86680 -32.693850    -0.06872852  0.3637500
#> 6         0.04565217 -1.209682 -12.41625  -8.506117    -0.12684989  0.1970833
#>   IR_HOMA_IR_inv    IR_FIRI IR_QUICKI IR_Belfiore_basal IR_Ig_ratio_basal
#> 1    -0.01751125 0.01945695  7.158427          642.9440         -9.944868
#> 2    -0.05299417 0.05888241  6.051089          212.7875        -23.505882
#> 3    -0.02374515 0.02638350  6.853892          474.2810         -9.933788
#> 4    -0.02900613 0.03222904  6.653763          388.3490        -10.906719
#> 5    -0.02254873 0.02505414  6.905592          499.4195         -4.713402
#> 6    -0.03484887 0.03872097  6.470250          323.3225        -10.389006
#>   IR_Isi_basal IR_Bennett IR_HOMA_IR_rev_inv IR_Isi_120 IR_Cederholm_index
#> 1    0.1284888  11.491651         -0.3152026  0.2212326         -1.7823934
#> 2    0.0424575   6.661861         -0.9538951  0.0876978          0.5927111
#> 3    0.0947562  10.426123         -0.4274127  0.1125720          0.8293481
#> 4    0.0775698   9.640858         -0.5221104  0.1717200        -19.6494632
#> 5    0.0997839  11.320857         -0.4058771  0.0726192          1.1012518
#> 6    0.0645645   9.096258         -0.6272797  0.0926436        -29.2492058
#>   IR_Gutt_index IR_Avignon_Si0 IR_Avignon_Si120 IR_Avignon_Sim
#> 1     0.5305820     0.19350413       0.33317630     0.24482015
#> 2     0.3024564     0.06285833       0.12983659     0.08470707
#> 3     0.3780459     0.12081415       0.14352930     0.13119577
#> 4     0.4085063     0.10122859       0.22409460     0.13945996
#> 5     0.3066113     0.09190097       0.06688228     0.07742059
#> 6     0.3173482     0.08745262       0.12548576     0.10307262
#>   IR_Modified_stumvoll IR_Stumvoll_Demographics IR_Matsuda_AUC IR_Matsuda_ISI
#> 1            11.375317                12.256233       24.90268     0.18554100
#> 2             8.789510                 9.691464       10.27706     0.07372675
#> 3             9.770285                 9.000936       19.20708     0.14037223
#> 4             9.879588                 9.314421       21.25666     0.14891687
#> 5            10.090693                 9.659846       20.58622     0.14766948
#> 6             9.320196                16.396297       13.34925     0.09761742
#>   IR_BigttSi IR_Ifc_inv IR_HIRI_inv IR_Belfiore_isi_gly IR_Revised_QUICKI
#> 1 0.17725513  -2.979436 -0.03453058            24132201          2.959512
#> 2 0.10334914  -1.494017 -0.06636583            12438072          2.253267
#> 3 0.11080561 -12.213178 -0.03862652            19466368          2.869814
#> 4 0.11053133  -1.736539 -0.02873088            29125090          2.711206
#> 5 0.07460339   1.743810 -0.03386558            21235522          2.540640
#> 6 0.10958423  -7.766196 -0.05779652            13800354          2.457377
#>   IR_VAI_Men_inv IR_VAI_Women_inv IR_TG_HDL_C_inv IR_TyG_inv IR_LAP_Men_inv
#> 1     -0.3289024       -0.2167097      -0.1801857 -0.1034577   -0.009476427
#> 2     -1.2763631       -0.8396808      -0.7398009 -0.1239724   -0.040025616
#> 3     -1.0088699       -0.6623748      -0.5141316 -0.1185786   -0.051921080
#> 4     -1.2264636       -0.8047215      -0.6976170 -0.1215239   -0.041645844
#> 5     -0.9124697       -0.5976307      -0.4232927 -0.1174627   -0.101626016
#> 6     -1.3553982       -0.8954833      -0.5730425 -0.1184412   -0.044389205
#>   IR_LAP_Women_inv IR_McAuley_index IR_Adipo_inv IR_Belfiore_inv_FFA
#> 1     -0.007838526       0.20316976   -0.1240854           -4.529483
#> 2     -0.033306688       0.09761746   -0.5575690           -1.396750
#> 3     -0.037383178       0.13932962   -0.1309323           -4.318767
#> 4     -0.032838566       0.12760260   -0.1788493           -3.295650
#> 5     -0.046904315       0.17783152   -0.1974958           -3.031700
#> 6     -0.031758130       0.13871371   -0.2856980           -2.250100
#>       IR_I_AUC IR_FFA_AUC IR_tracer_palmitate_SI IR_tracer_glycerol_SI
#> 1 0.0003501401 0.01175364               7.421074              7.541667
#> 2 0.0006190992 0.01974724               4.670169              9.286129
#> 3 0.0003629105 0.01065644               8.627811              4.387398
#> 4 0.0002784352 0.01256913               7.171717              7.880133
#> 5 0.0002851237 0.02394636              15.175790              9.754005
#> 6 0.0004543905 0.01876877              31.974050             21.904762
#>   IR_LIRI_inv IR_Lipo_inv IR_ATIRI_inv    SPISE  METS_IR prediabetes HOMA_CP
#> 1  -0.8671696 -0.03665689  -0.03607074 4.849288 302.5694           0      NA
#> 2  -0.9732892 -0.13656072  -0.06867896 7.113989 352.8944           1      NA
#> 3  -0.8608735 -0.02389477  -0.04698902 7.510664 298.4802           0      NA
#> 4  -0.8493467 -0.04386869  -0.03992494 8.292019 188.0396           0      NA
#> 5  -0.7721014 -0.01965919  -0.03058679 8.381122 134.0136           0      NA
#> 6  -0.8266637 -0.08053962  -0.11756246 5.086907 179.8589           0      NA
#>   LAR ASI TyG_index non_HDL_c ratio_TC_HDL ratio_TG_HDL ratio_LDL_HDL
#> 1  NA  NA  9.310025      5.14     4.953846    2.4230769      2.853846
#> 2  NA  NA  7.951901      4.58     4.754098    0.5901639      3.483607
#> 3  NA  NA  8.523629      5.70     5.523810    0.8492063      4.134921
#> 4  NA  NA  8.609869      2.97     3.020408    0.6258503      1.734694
#> 5  NA  NA  8.928386      3.12     2.962264    1.0314465      1.490566
#> 6  NA  NA  8.678815      4.00     3.380952    0.7619048      2.035714
#>   ApoB_ApoA1   VAI_Men VAI_Women LAP_Men LAP_Women  TyG_BMI   eGFR_cr eGFR_cys
#> 1  0.6899225 3.0404154  4.614469 105.525   127.575 297.9208 0.4725555 89.82553
#> 2  0.4120879 0.7834761  1.190929  24.984    30.024 236.1715 0.5818192 43.32180
#> 3  0.9924812 0.9912081  1.509719  19.260    26.750 230.1380 0.5165652 80.61015
#> 4  1.0826446 0.8153524  1.242666  24.012    30.452 225.5786 0.6102493 93.72321
#> 5  0.6736111 1.0959268  1.673274   9.840    21.320 215.1741 0.2392153 37.79522
#> 6  0.4509804 0.7377906  1.116715  22.528    31.488 317.6446 0.3701522 57.57784
#>   eGFR_combined FE_Urea Beta2Micro      LMR      NER      PIV       CLR
#> 1      6.696628      NA       1.99 2.717986 11.70297 507.9409 1.8845950
#> 2      4.949108      NA       1.88 4.866071 10.65432 144.7180 0.6009174
#> 3      6.504759      NA       1.47 4.300216 12.11576 153.3411 0.5524862
#> 4      7.710013      NA       1.54 5.509579 13.38866 195.0739 1.2378303
#> 5      3.155730      NA       1.20 6.544444 13.24619 183.9484 1.2393888
#> 6      4.915629      NA       1.75 4.351916 12.59779 287.1204 2.8422738
#>          CAR       PCR mGPS
#> 1 0.10439883  82.02247    0
#> 2 0.03628809 155.72519    0
#> 3 0.02235772 159.09091    0
#> 4 0.04178404 182.58427    0
#> 5 0.08295455  67.80822    0
#> 6 0.08179724 103.09859    0
```

### Urine markers

Input: `sim_small`; output: urine markers with rows containing required
inputs.

``` r
urine <- urine_markers(sim_small, na_action = "omit")
urine_new <- setdiff(names(urine), names(sim_small))
head(dplyr::select(urine, dplyr::all_of(urine_new)))
#> # A tibble: 6 × 11
#>   albuminuria_stage microalbuminuria  UPCR U_Na_K_ratio NGAL_per_gCr
#>   <fct>             <fct>            <dbl>        <dbl>        <dbl>
#> 1 A3                normal            694.        1.45         1633.
#> 2 A3                normal            958.        1.8          2156.
#> 3 A3                normal           1905.        0.598        5536.
#> 4 A3                normal           3559.        2.12         3360.
#> 5 A3                normal           3575.        1.23         4402.
#> 6 A3                normal           2873.        2.59         2818.
#> # ℹ 6 more variables: KIM1_per_gCr <dbl>, NAG_per_gCr <dbl>,
#> #   Beta2Micro_per_gCr <dbl>, A1Micro_per_gCr <dbl>, IL18_per_gCr <dbl>,
#> #   L_FABP_per_gCr <dbl>
```

### All markers (wide; optional)

To compute every available group on your data (can produce many
columns), set `which = "all"`. Here we keep it example-only to avoid
long output:

``` r
all_out <- all_health_markers(data = sim_small, which = "all", verbose = FALSE)
head(setdiff(names(all_out), names(sim_small)))
```

## Export-ready output

Select columns and write to a CSV (disabled during vignette build):

``` r
export_cols <- c("aip", "egfr_ckd_epi", "homa_ir")
out_path <- tempfile("healthmarkers_export_", fileext = ".csv")
hm_out_subset <- dplyr::select(cardio_panel, dplyr::any_of(export_cols))
utils::write.csv(hm_out_subset, out_path, row.names = FALSE)
out_path
```

## Next steps

- Explore focused vignettes for insulin sensitivity, cardio-renal
  panels, and frailty.
- See individual help pages (e.g.,
  [`?fasting_is`](https://sufyansuleman.github.io/HealthMarkers/reference/fasting_is.md),
  [`?cvd_risk`](https://sufyansuleman.github.io/HealthMarkers/reference/cvd_risk.md),
  [`?frailty_index`](https://sufyansuleman.github.io/HealthMarkers/reference/frailty_index.md))
  for detailed arguments and references.

> **All 46 domain vignettes** are available on the package website —
> only 12 core vignettes are bundled with the CRAN package.  
> Browse the full collection at
> <https://sufyansuleman.github.io/HealthMarkers/articles/>
