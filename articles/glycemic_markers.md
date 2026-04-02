# Glycemic and metabolic markers

## Scope

Compute SPISE, METS_IR, prediabetes/diabetes flags (HbA1c), HOMA_CP,
LAR, ASI, and TyG_index from fasting labs and anthropometry. Required
inputs: HDL_c (mmol/L), TG (mmol/L), BMI (kg/m^2); optional
glucose/HbA1c/C-peptide/insulin/leptin/adiponectin if present. Uses the
packaged simulated data for runnable examples.

## When to use

- You have fasting lipids and BMI and want quick insulin
  resistance/sensitivity proxies.
- You can supply glucose/HbA1c/C-peptide/insulin/adipokines when
  available to enrich outputs.
- You need explicit NA handling and optional extreme-value screening.

## Inputs and requirements

- Required (mapped via `col_map`): `HDL_c` (mmol/L), `TG` (mmol/L),
  `BMI` (kg/m^2).
- Optional: `glucose`, `HbA1c`, `C_peptide`, `G0`, `I0`, `leptin`,
  `adiponectin`.
- Unit notes: TyG converts TG to mg/dL (x88.57) and glucose to mg/dL
  (x18); HOMA_CP uses `(G0 * (C_peptide/6)) / 22.5`.
- `na_action`: `keep`/`ignore`/`warn` retain rows with NA outputs;
  `omit` drops rows with missing used inputs; `error` aborts.
- `check_extreme`: when TRUE, scans inputs with default broad ranges;
  `extreme_action` = `warn`/`cap`/`NA`/`error`/`ignore`.

## Load packages and data

Use a small subset of the simulated data (30-50 rows). Swap `sim_small`
with your data frame in practice.

``` r
library(HealthMarkers)

sim_path <- system.file("extdata", "simulated_hm_data.rds", package = "HealthMarkers")
sim <- readRDS(sim_path)
sim_small <- dplyr::slice_head(sim, n = 30)
```

## Column map (required)

Map required and optional inputs to your column names; left-hand keys
are fixed.

### What each input represents

- `HDL_c`: HDL cholesterol (mmol/L) required
- `TG`: triglycerides (mmol/L) required
- `BMI`: body mass index (kg/m^2) required
- `glucose`: fasting glucose (mmol/L)
- `HbA1c`: hemoglobin A1c (mmol/mol)
- `C_peptide`: C-peptide (pmol/L)
- `G0`: fasting glucose (mmol/L) for HOMA_CP
- `I0`: fasting insulin (pmol/L) for ASI
- `leptin`: leptin (ng/mL)
- `adiponectin`: adiponectin (ng/mL)

``` r
col_map <- list(
  HDL_c = "HDL_c",
  TG = "TG",
  BMI = "BMI",
  glucose = "glucose",
  HbA1c = "HbA1c",
  C_peptide = "C_peptide",
  G0 = "G0",
  I0 = "I0",
  leptin = "leptin",
  adiponectin = "adiponectin"
)
```

## Core calculation

Compute all markers and show only newly created columns.

``` r
gm <- glycemic_markers(
  data = sim_small,
  col_map = col_map,
  na_action = "keep",
  check_extreme = FALSE,
  verbose = FALSE
)

gm_new <- setdiff(names(gm), names(sim_small))
head(dplyr::select(gm, dplyr::all_of(gm_new)))
#> # A tibble: 6 × 7
#>   SPISE METS_IR prediabetes HOMA_CP   LAR   ASI TyG_index
#>   <dbl>   <dbl>       <int>   <dbl> <dbl> <dbl>     <dbl>
#> 1  6.54    175.           0      NA    NA    NA      8.39
#> 2  7.04    163.           0      NA    NA    NA      9.13
#> 3  6.74    342.           0      NA    NA    NA      8.42
#> 4  4.13  -1870.           0      NA    NA    NA      9.07
#> 5  6.25    271.           0      NA    NA    NA      8.95
#> 6  5.34    347.           0      NA    NA    NA      8.37
```

Interpretation: SPISE and METS_IR summarize insulin
sensitivity/resistance; TyG_index rises with higher TG/glucose; HOMA_CP
approximates insulin resistance via C-peptide and glucose; LAR and ASI
relate adipokines to insulin; HbA1c flags populate
`prediabetes`/`diabetes` when available. Rows are retained here because
`na_action = "keep"`.

## Inputs and units (quick reminder)

- Required: `HDL_c` (mmol/L), `TG` (mmol/L), `BMI` (kg/m^2).
- Optional if present/mapped: `glucose`, `HbA1c`, `C_peptide`, `G0`,
  `I0`, `leptin`, `adiponectin`.
- TyG converts TG to mg/dL (88.57) and glucose to mg/dL (18) internally.
- HOMA_CP uses `(G0 * (C_peptide/6)) / 22.5`; verify your units for
  C-peptide.
- Missingness: `na_action` controls row handling
  (`keep`/`omit`/`error`/`warn`/`ignore`).
- Extreme screening: `check_extreme = TRUE` can warn/cap/NA/error using
  default broad ranges; override via `extreme_rules`.

## Missing data and extremes

Show row handling and extreme screening on a tiny slice.

``` r
demo <- sim_small[1:6, c("HDL_c", "TG", "BMI")]
demo$glucose <- c(5.5, 6.2, 7.0, 5.8, 6.0, 5.4)
demo$HbA1c <- c(42, 39, 41, 45, 44, 40)
demo$C_peptide <- c(300, 450, 500, 400, 380, 360)
demo$G0 <- demo$glucose
demo$I0 <- c(60, 90, 120, 80, 75, 70)
demo$leptin <- c(10, 12, 15, 11, 9, 13)
demo$adiponectin <- c(8, 7, 6, 9, 10, 7)
demo$TG[3] <- 30       # extreme TG
demo$HbA1c[2] <- NA    # missing HbA1c

gm_keep <- glycemic_markers(
  data = demo,
  col_map = col_map,
  na_action = "keep",
  check_extreme = TRUE,
  extreme_action = "cap",
  verbose = FALSE
)

gm_omit <- glycemic_markers(
  data = demo,
  col_map = col_map,
  na_action = "omit",
  check_extreme = TRUE,
  extreme_action = "cap",
  verbose = FALSE
)

list(
  keep_rows = nrow(gm_keep),
  omit_rows = nrow(gm_omit),
  capped_preview = head(dplyr::select(gm_keep, dplyr::all_of(gm_new)))
)
#> $keep_rows
#> [1] 6
#> 
#> $omit_rows
#> [1] 5
#> 
#> $capped_preview
#> # A tibble: 6 × 7
#>   SPISE METS_IR prediabetes HOMA_CP   LAR    ASI TyG_index
#>   <dbl>   <dbl>       <int>   <dbl> <dbl>  <dbl>     <dbl>
#> 1  6.54    179.           1    12.2  1.25 0.133       8.45
#> 2  7.04    173.          NA    20.7  1.71 0.0778      9.31
#> 3  3.82    504.           0    25.9  2.5  0.05       11.6 
#> 4  4.13  -1886.           1    17.2  1.22 0.112       9.10
#> 5  6.25    284.           1    16.9  0.9  0.133       9.09
#> 6  5.34    332.           0    14.4  1.86 0.1         8.25
```

`na_action = "keep"` preserves all rows (missing inputs flow to `NA`
outputs); `na_action = "omit"` drops rows with missing/invalid used
inputs. `extreme_action = "cap"` trims out-of-range values (e.g., TG =
30) before computation.

## Expectations

- Required inputs must be present/mapped; missing required columns
  abort.
- `na_action` governs whether missing/invalid inputs drop rows, warn, or
  error.
- `check_extreme`/`extreme_action` can cap, blank, warn, or abort on
  out-of-range inputs.
- Optional columns drive optional markers; absent inputs yield `NA` in
  their markers.

## Verbose diagnostics

Set `verbose = TRUE` (and `healthmarkers.verbose = "inform"`) to surface
three structured messages on each call: preparing inputs, the column
map, and a results summary.

``` r
old_opt <- options(healthmarkers.verbose = "inform")
df_v <- data.frame(HDL_c = 1.0, TG = 1.3, BMI = 24, glucose = 5.6)
glycemic_markers(
  df_v,
  col_map = list(HDL_c = "HDL_c", TG = "TG", BMI = "BMI", glucose = "glucose"),
  verbose = TRUE
)
#> glycemic_markers(): preparing inputs
#> glycemic_markers(): column map: HDL_c -> 'HDL_c', TG -> 'TG', BMI -> 'BMI'
#> glycemic_markers(): optional markers not computed (columns absent): HbA1c, C_peptide, G0, I0, leptin, adiponectin
#> glycemic_markers(): results: SPISE 1/1, METS_IR 0/1, prediabetes 0/1, diabetes 0/1, HOMA_CP 0/1, LAR 0/1, ASI 0/1, TyG_index 1/1
#> # A tibble: 1 × 8
#>   SPISE METS_IR prediabetes diabetes HOMA_CP   LAR   ASI TyG_index
#>   <dbl>   <dbl>       <int>    <int>   <dbl> <dbl> <dbl>     <dbl>
#> 1  8.10      NA          NA       NA      NA    NA    NA      8.67
options(old_opt)
```

## Tips

- Start with fasting lipids and BMI; add
  glucose/HbA1c/C-peptide/insulin/adipokines when available for richer
  markers.
- Convert units beforehand if your data are not in the expected
  mmol/L/pmol/L/ng/mL units.
- For stricter pipelines, use `na_action = "omit"` or `"error"`;
  keep/warn for exploratory work.
- Turn on `check_extreme = TRUE` with `extreme_action = "cap"` when
  working with noisy or simulated inputs.
- See
  [`?glycemic_markers`](https://sufyansuleman.github.io/HealthMarkers/reference/glycemic_markers.md)
  for full argument details and marker definitions.
