# Compute a suite of hormone ratio markers with QA and verbose summaries

Ratios computed:

- FAI = (total_testosterone / SHBG) \* 100

- LH_FSH = LH / FSH

- E2_P = estradiol / progesterone

- T3_T4 = free_T3 / free_T4

- ARR = aldosterone / renin

- Ins_Glu = insulin / glucagon

- GH_IGF1 = GH / IGF1

- PRL_T = prolactin / total_testosterone

- CAR_slope = (cortisol_30 - cortisol_0) / 30

## Usage

``` r
hormone_markers(
  data,
  col_map,
  na_action = c("ignore", "warn", "error", "keep", "omit"),
  na_warn_prop = 0.2,
  check_extreme = FALSE,
  extreme_action = c("warn", "cap", "error", "ignore", "NA"),
  extreme_rules = NULL,
  verbose = FALSE
)
```

## Arguments

- data:

  Data frame or tibble with mapped hormone inputs.

- col_map:

  Named list mapping the required keys to column names:
  total_testosterone, SHBG, LH, FSH, estradiol, progesterone, free_T3,
  free_T4, aldosterone, renin, insulin, glucagon, GH, IGF1, prolactin,
  cortisol_0, cortisol_30.

- na_action:

  One of "ignore","warn","error","keep","omit". HM-CS: keep == ignore;
  omit drops rows with any NA in used inputs.

- na_warn_prop:

  Proportion in \\\[0,1\]\\ for high-missingness warnings when
  na_action="warn". Default 0.2.

- check_extreme:

  Logical; if TRUE, scan inputs for out-of-range values (see
  extreme_rules). Default FALSE.

- extreme_action:

  One of "warn","cap","error","ignore","NA" when check_extreme=TRUE.
  "cap" truncates to range; "NA" sets out-of-range to NA.

- extreme_rules:

  Optional list of c(min,max) per key to override defaults.

- verbose:

  Logical; print progress and completion summary.

## Value

Tibble with the nine ratio markers.

## Examples

``` r
df <- data.frame(
  TT = c(15, 12), SHBG = c(40, 35), LH = c(5, 6), FSH = c(4, 5),
  E2 = c(100, 120), Prog = c(0.5, 0.6), fT3 = c(4.5, 4.2),
  fT4 = c(15, 14), Aldo = c(200, 180), Renin = c(10, 12),
  Ins = c(60, 70), Gluc = c(8, 9), GH = c(1.2, 1.0),
  IGF1 = c(180, 160), Prl = c(10, 12), Cort0 = c(400, 380),
  Cort30 = c(600, 580)
)
col_map <- list(
  total_testosterone = "TT", SHBG = "SHBG", LH = "LH", FSH = "FSH",
  estradiol = "E2", progesterone = "Prog", free_T3 = "fT3",
  free_T4 = "fT4", aldosterone = "Aldo", renin = "Renin",
  insulin = "Ins", glucagon = "Gluc", GH = "GH", IGF1 = "IGF1",
  prolactin = "Prl", cortisol_0 = "Cort0", cortisol_30 = "Cort30"
)
hormone_markers(df, col_map = col_map)
#> # A tibble: 2 × 10
#>     FAI LH_FSH  E2_P  E2_T T3_T4   ARR Ins_Glu GH_IGF1 PRL_T CAR_slope
#>   <dbl>  <dbl> <dbl> <dbl> <dbl> <dbl>   <dbl>   <dbl> <dbl>     <dbl>
#> 1  37.5   1.25   200  6.67   0.3    20    7.5  0.00667 0.667      6.67
#> 2  34.3   1.2    200 10      0.3    15    7.78 0.00625 1          6.67
```
