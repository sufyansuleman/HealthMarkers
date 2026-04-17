# Compute all available HealthMarkers categories

Compute all available HealthMarkers categories

## Usage

``` r
all_health_markers(
  data,
  col_map,
  which = "all",
  include_insulin = TRUE,
  normalize = c("none", "z", "inverse", "range", "robust"),
  mode = c("both", "IS", "IR"),
  verbose = TRUE,
  na_action = c("keep", "omit", "error")
)
```

## Arguments

- data:

  A data.frame or tibble.

- col_map:

  Named list for column mapping forwarded to underlying functions. If
  `col_map` is `NULL` or an empty list, `all_health_markers()` calls
  [`hm_col_report()`](https://sufyansuleman.github.io/HealthMarkers/reference/hm_col_report.md)
  once at the top level to guess a column map from common synonyms (for
  example `TG` vs `triglycerides`, `BMI` vs `bmi`, `HDL_c` vs `HDL`).
  The inferred `col_map` is then reused for all groups that require it,
  and an error is thrown if required keys (e.g. `TG`, `HDL_c`, `LDL_c`,
  `TC`, `BMI`, `age`, `sex`) cannot be inferred.

- which:

  "all" or a vector of registry keys (see Details).

- include_insulin:

  Logical; include all_insulin_indices() first.

- normalize:

  One of c("none","z","inverse","range","robust").

- mode:

  One of c("both","IS","IR") passed to insulin indices.

- verbose:

  Logical.

- na_action:

  One of c("keep","omit","error"); forwarded to underlying calculators
  (HM-CS v2).

## Value

Data frame with original columns plus many derived markers.

## Details

Common group names for `which` include:

- `"lipid"`, `"liver"`, `"glycemic"`, `"mets"`, `"oxidative"`

- `"bone"`, `"allostatic_load"`, `"nutrient"`, `"vitamin"`,
  `"vitamin_d_status"`

- `"renal"`, `"ckd_stage"`, `"kidney_kfre"`

- `"frailty_index"`, `"charlson"`, `"sarc_f"`

- `"nfl"`, `"iAge"`, `"calcium_corrected"`, `"kyn_trp"`

## Note

For academic / clinical references tied to each derived marker or index,
consult the help pages of the source functions (e.g. ?allostatic_load,
?bone_markers, ?vitamin_markers, ?inflammatory_markers, etc.). This
aggregator provides integration only and does not restate citations.

## References

Aggregator wrapper. See underlying function help pages for full
references across categories included by `which`.

## Examples

``` r
df <- data.frame(
  TC = 200, HDL_c = 50, TG = 150, LDL_c = 120,
  ALT = 30, AST = 20, BMI = 25
)
all_health_markers(df, col_map = list(), which = c("lipid","liver"),
                   include_insulin = FALSE, normalize = "none", mode = "both",
                   verbose = FALSE, na_action = "keep")
#>    TC HDL_c  TG LDL_c ALT AST BMI     VLDL non_HDL remnant_c       AIP CRI_I
#> 1 200    50 150   120  30  20  25 68.18182     150        30 0.4771213     4
#>   CRI_II HDL_TG_ratio LDL_HDL_ratio non_HDL_c ratio_TC_HDL ratio_TG_HDL
#> 1    2.4    0.3333333           2.4       150            4            3
#>   ratio_LDL_HDL ApoB_ApoA1 FLI NFS APRI FIB4 BARD ALBI MELD_XI
#> 1           2.4         NA  NA  NA   NA   NA    0   NA      NA
```
