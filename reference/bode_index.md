# BODE Index (BMI, Obstruction, Dyspnea, Exercise capacity)

Computes the BODE index (0-10) using FEV1 % predicted, 6-minute walk
distance (6MWD), mMRC dyspnea scale, and BMI. Higher scores indicate
worse prognosis in COPD.

## Usage

``` r
bode_index(
  data,
  col_map = list(fev1_pct = "FEV1pct", sixmwd = "Walk_m", mmrc = "mMRC", bmi = "BMI",
    fev1 = NULL, fev1_pred = NULL, fev1_pp = NULL),
  na_action = c("keep", "omit", "error", "ignore", "warn"),
  check_extreme = FALSE,
  extreme_action = c("warn", "cap", "error", "ignore", "NA"),
  extreme_rules = NULL,
  verbose = FALSE
)
```

## Arguments

- data:

  data.frame/tibble with required columns.

- col_map:

  named list with keys: fev1_pct OR (fev1 and fev1_pred) OR fev1_pp;
  plus sixmwd, mmrc, bmi. Example minimal: list(fev1_pct="FEV1pct",
  sixmwd="Walk_m", mmrc="mMRC", bmi="BMI") Example derive:
  list(fev1="FEV1", fev1_pred="FEV1_pred", sixmwd="Walk_m", mmrc="mMRC",
  bmi="BMI") Example from spirometry_markers: list(fev1_pp="fev1_pp",
  sixmwd="Walk_m", mmrc="mMRC", bmi="BMI")

- na_action:

  one of c("keep","omit","error","ignore","warn").

- check_extreme:

  logical; if TRUE scan for extreme values.

- extreme_action:

  one of c("warn","cap","error","ignore","NA").

- extreme_rules:

  named list of bounds (c(lo,hi)) for fev1_pct,sixmwd,mmrc,bmi.
  Defaults: fev1_pct c(10,140), sixmwd c(50,800), mmrc c(0,4), bmi
  c(10,60).

- verbose:

  logical; TRUE emits messages.

## Value

tibble with bode_index (integer). NA if any required input missing
(unless omitted).

## Details

Scoring components: FEV1 % predicted: \>=65 = 0; 50-64 = 1; 36-49 = 2;
\<=35 = 3 6MWD (meters): \>=350 = 0; 250-349 = 1; 150-249 = 2; \<=149 =
3 mMRC dyspnea: 0-1 = 0; 2 = 1; 3 = 2; 4 = 3 BMI: \>21 = 0; \<=21 = 1

## References

Celli BR, Cote CG, Marin JM, Casanova C, Montes de Oca M, Mendez R,
Pinto-Plata V, Cabral HJ (2004). “The BODE Index in Chronic Obstructive
Pulmonary Disease.” *The New England Journal of Medicine*, **350**,
1005–1012.
[doi:10.1056/NEJMoa021322](https://doi.org/10.1056/NEJMoa021322) .

## Examples

``` r
df <- data.frame(FEV1pct = c(68, 45, 30), Walk_m = c(400, 280, 140),
                 mMRC = c(1, 2, 3), BMI = c(24, 19, 18))
bode_index(df)
#> # A tibble: 3 × 6
#>   bode_index fev1_pct fev1_score walk_score mmrc_score bmi_score
#>        <int>    <dbl>      <int>      <int>      <int>     <int>
#> 1          0       68          0          0          0         0
#> 2          5       45          2          1          1         1
#> 3          9       30          3          3          2         1
```
