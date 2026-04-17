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
  col_map = NULL,
  na_action = c("keep", "omit", "error", "warn", "ignore"),
  na_warn_prop = 0.2,
  verbose = TRUE
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

  One of `c("keep","omit","error","warn","ignore")`. `"keep"`/`"ignore"`
  leave NAs; `"omit"` drops rows with any NA in used inputs; `"error"`
  aborts; `"warn"` also warns about high missingness.

- na_warn_prop:

  Proportion in \\\[0,1\]\\ for high-missingness warnings when
  `na_action = "warn"`. Default 0.2.

- verbose:

  Logical; if `TRUE` (default), prints column mapping, input
  availability, inference notes, physiological range information
  (informational only, values not altered), computing markers, and a
  per-column results summary.

## Value

Tibble with one column per computable ratio. If an ID column is detected
in `data` (e.g. `id`, `IID`, `participant_id`), it is prepended as the
first output column.

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
#> hormone_markers(): reading input 'df' — 2 rows × 17 variables
#> hormone_markers(): skipping 1 ratio(s) with unmapped inputs: TSH_fT4
#> hormone_markers(): col_map (15 columns — 15 specified)
#>   total_testosterone->  'TT'
#>   SHBG              ->  'SHBG'
#>   LH                ->  'LH'
#>   FSH               ->  'FSH'
#>   estradiol         ->  'E2'
#>   progesterone      ->  'Prog'
#>   free_T3           ->  'fT3'
#>   free_T4           ->  'fT4'
#>   aldosterone       ->  'Aldo'
#>   renin             ->  'Renin'
#>   insulin           ->  'Ins'
#>   IGF1              ->  'IGF1'
#>   prolactin         ->  'Prl'
#>   cortisol_0        ->  'Cort0'
#>   cortisol_30       ->  'Cort30'
#> hormone_markers(): optional inputs
#>   present:  total_testosterone, SHBG, LH, FSH, estradiol, progesterone, free_T3, free_T4, aldosterone, renin, insulin, glucagon, GH, IGF1, prolactin, cortisol_0, cortisol_30
#>   missing:  TSH
#>   ratios skipped (missing inputs): TSH_fT4
#> hormone_markers(): computing markers:
#>   FAI          [total_testosterone, SHBG]
#>   LH_FSH       [LH, FSH]
#>   E2_P         [estradiol, progesterone]
#>   E2_T         [estradiol, total_testosterone]
#>   T3_T4        [free_T3, free_T4]
#>   TSH_fT4      NA [missing: TSH]
#>   ARR          [aldosterone, renin]
#>   Ins_Glu      [insulin, glucagon]
#>   GH_IGF1      [GH, IGF1]
#>   PRL_T        [prolactin, total_testosterone]
#>   CAR_slope    [cortisol_0, cortisol_30]
#> hormone_markers(): results: FAI 2/2, LH_FSH 2/2, E2_P 2/2, E2_T 2/2, T3_T4 2/2, ARR 2/2, Ins_Glu 2/2, GH_IGF1 2/2, PRL_T 2/2, CAR_slope 2/2
#> # A tibble: 2 × 10
#>     FAI LH_FSH  E2_P  E2_T T3_T4   ARR Ins_Glu GH_IGF1 PRL_T CAR_slope
#>   <dbl>  <dbl> <dbl> <dbl> <dbl> <dbl>   <dbl>   <dbl> <dbl>     <dbl>
#> 1  37.5   1.25   200  6.67   0.3    20    7.5  0.00667 0.667      6.67
#> 2  34.3   1.2    200 10      0.3    15    7.78 0.00625 1          6.67
```
