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
- You need explicit NA handling.

## Inputs and requirements

- Required (mapped via `col_map`): `HDL_c` (mmol/L), `TG` (mmol/L),
  `BMI` (kg/m^2).
- Optional: `glucose`, `HbA1c`, `C_peptide`, `G0`, `I0`, `leptin`,
  `adiponectin`.
- Unit notes: TyG converts TG to mg/dL (x88.57) and glucose to mg/dL
  (x18); HOMA_CP uses `(G0 * (C_peptide/6)) / 22.5`.
- `na_action`: `keep`/`ignore`/`warn` retain rows with NA outputs;
  `omit` drops rows with missing used inputs; `error` aborts.

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
  data     = sim_small,
  col_map  = col_map,
  na_action = "keep",
  verbose  = FALSE
)

gm_new <- setdiff(names(gm), names(sim_small))
head(dplyr::select(gm, dplyr::all_of(gm_new)))
#> # A tibble: 6 × 7
#>   SPISE METS_IR prediabetes HOMA_CP   LAR   ASI TyG_index
#>   <dbl>   <dbl>       <int>   <dbl> <dbl> <dbl>     <dbl>
#> 1  4.85    336.           0      NA    NA    NA      9.67
#> 2  7.11    369.           1      NA    NA    NA      8.07
#> 3  7.51    289.           0      NA    NA    NA      8.43
#> 4  8.29    164.           0      NA    NA    NA      8.23
#> 5  8.38    116.           0      NA    NA    NA      8.51
#> 6  5.09    165.           0      NA    NA    NA      8.44
```

Interpretation: SPISE and METS_IR summarize insulin
sensitivity/resistance; TyG_index rises with higher TG/glucose; HOMA_CP
approximates insulin resistance via C-peptide and glucose; LAR and ASI
relate adipokines to insulin; HbA1c flags populate
`prediabetes`/`diabetes` when available. Rows are retained here because
`na_action = "keep"`.

## Inputs and units (quick reminder)

- Required: `HDL_c` (mmol/L), `TG` (mmol/L), `BMI` (kg/m^2).
  Auto-derived: BMI from `weight` (kg) + `height` (m/cm) if BMI absent.
- Optional if present/mapped: `glucose`, `HbA1c`, `C_peptide`, `G0`,
  `I0`, `leptin`, `adiponectin`. Auto-derived: `glucose` from `G0` (or
  vice versa) if one is absent.
- TyG converts TG to mg/dL (88.57) and glucose to mg/dL (18) internally.
- HOMA_CP uses `(G0 * (C_peptide/6)) / 22.5`; verify your units for
  C-peptide.
- Missingness: `na_action` controls row handling
  (`keep`/`omit`/`error`/`warn`/`ignore`).

## Pre-computation example

Providing `weight` and `height` instead of `BMI` works automatically:

``` r
df_no_bmi <- data.frame(
  HDL_c   = c(1.0, 1.2),
  TG      = c(1.3, 1.5),
  weight  = c(70, 85),    # kg
  height  = c(175, 180),  # cm
  glucose = c(5.5, 6.0)
)
gm_precomp <- glycemic_markers(df_no_bmi, verbose = FALSE)
gm_precomp$SPISE  # computed via auto-derived BMI
#> [1] 8.649504 7.229832
```

Similarly, providing `G0` without `glucose` enables METS_IR and
TyG_index via alias:

``` r
df_g0_only <- data.frame(
  HDL_c = 1.2, TG = 1.3, BMI = 24, G0 = 5.5
)
gm_alias <- glycemic_markers(df_g0_only, verbose = FALSE)
c(METS_IR = gm_alias$METS_IR, TyG_index = gm_alias$TyG_index)
#>   METS_IR TyG_index 
#> 330.35250   8.64813
```

## Non-standard column names

When your data frame uses different column names (e.g. `pglu0` instead
of `G0`), pass a `col_map` that redirects them. The function
materialises the alias and runs pre-computation normally.

``` r
# Data has 'pglu0' instead of 'G0'
df_mapped <- data.frame(
  HDL_c = 1.2, TG = 1.3, BMI = 24.0, pglu0 = 5.5
)
gm_mapped <- glycemic_markers(
  df_mapped,
  col_map = list(G0 = "pglu0"),   # only need to specify what differs
  verbose = FALSE
)
c(METS_IR = gm_mapped$METS_IR, TyG_index = gm_mapped$TyG_index)
#>   METS_IR TyG_index 
#> 330.35250   8.64813
```

Similarly, map `weight` / `height` to differently-named anthropometric
columns and BMI is derived automatically:

``` r
df_wh <- data.frame(
  HDL_c = 1.1, TG = 1.4, wt_kg = 80, ht_cm = 175, glucose = 5.8
)
gm_wh <- glycemic_markers(
  df_wh,
  col_map = list(weight = "wt_kg", height = "ht_cm"),
  verbose = FALSE
)
gm_wh$SPISE   # BMI was derived from wt_kg / ht_cm
#> [1] 7.254687
```

## Partial col_map and dictionary inference

You only need to specify the keys that *differ* from dictionary
defaults. For any key you omit,
[`glycemic_markers()`](https://sufyansuleman.github.io/HealthMarkers/reference/glycemic_markers.md)
auto-infers the mapping from common synonym patterns (e.g. `hdlc`,
`hdl_cholesterol`, `hdl.c` all resolve to `HDL_c`). Provide a partial
`col_map` with just the non-standard entries:

``` r
# Column names match common synonyms — no col_map needed for those
df_syn <- data.frame(
  hdlc = 1.0, trig = 1.3, bmi = 24.0, pglu0 = 5.5
)
gm_syn <- glycemic_markers(
  df_syn,
  col_map = list(G0 = "pglu0"),   # only override the non-obvious one
  verbose = FALSE
)
c(SPISE = gm_syn$SPISE, METS_IR = gm_syn$METS_IR, TyG_index = gm_syn$TyG_index)
#>     SPISE   METS_IR TyG_index 
#>   8.10289        NA   8.64813
```

## Missing data handling

Show row handling on a small slice.

``` r
demo <- sim_small[1:6, c("HDL_c", "TG", "BMI")]
demo$glucose   <- c(5.5, 6.2, 7.0, 5.8, 6.0, 5.4)
demo$HbA1c     <- c(42, NA, 41, 45, 44, 40)
demo$C_peptide <- c(300, 450, 500, 400, 380, 360)
demo$G0        <- demo$glucose
demo$I0        <- c(60, 90, 120, 80, 75, 70)
demo$leptin    <- c(10, 12, 15, 11, 9, 13)
demo$adiponectin <- c(8, 7, 6, 9, 10, 7)

cm2 <- list(
  HDL_c = "HDL_c", TG = "TG", BMI = "BMI",
  glucose = "glucose", HbA1c = "HbA1c",
  C_peptide = "C_peptide", G0 = "G0", I0 = "I0",
  leptin = "leptin", adiponectin = "adiponectin"
)

gm_keep <- glycemic_markers(data = demo, col_map = cm2, na_action = "keep",  verbose = FALSE)
gm_omit <- glycemic_markers(data = demo, col_map = cm2, na_action = "omit",  verbose = FALSE)

list(keep_rows = nrow(gm_keep), omit_rows = nrow(gm_omit))
#> $keep_rows
#> [1] 6
#> 
#> $omit_rows
#> [1] 5
```

`na_action = "keep"` preserves all rows (missing inputs flow to `NA`
outputs); `na_action = "omit"` drops rows with missing/invalid used
inputs.

## Expectations

- Required inputs must be present/mapped or derivable (BMI from
  weight/height); missing required columns abort.
- `na_action` governs whether missing/invalid inputs drop rows, warn, or
  error.
- Optional columns drive optional markers; absent inputs yield `NA` in
  their markers.
- Range checks are informational only — values outside physiological
  plausibility ranges are noted in `verbose` output but not altered.

## Verbose diagnostics

Set `verbose = TRUE` (the default) to surface five structured messages:
column mapping, optional inputs (what is present/missing and which
indices they affect), pre-computation actions, the full list of markers
being computed with their inputs, and a per-column results summary.

``` r
df_v <- data.frame(HDL_c = 1.0, TG = 1.3, BMI = 24, glucose = 5.6)
glycem_out <- glycemic_markers(
  df_v,
  col_map = list(HDL_c = "HDL_c", TG = "TG", BMI = "BMI", glucose = "glucose"),
  verbose = TRUE
)
#> glycemic_markers(): reading input 'df_v' — 1 rows × 4 variables
#> glycemic_markers(): col_map (5 columns — 4 specified, 1 inferred from data)
#>   HDL_c             ->  'HDL_c'
#>   TG                ->  'TG'
#>   BMI               ->  'BMI'
#>   glucose           ->  'glucose'
#>   G0                ->  'glucose'    (inferred)
#> glycemic_markers(): optional inputs
#>   present:  glucose, G0
#>   missing:  HbA1c, C_peptide, I0, leptin, adiponectin
#>   indices -> NA:
#>   prediabetes -> NA  [missing: HbA1c]
#>   diabetes -> NA  [missing: HbA1c]
#>   HOMA_CP -> NA  [missing: C_peptide]
#>   LAR -> NA  [missing: leptin, adiponectin]
#>   ASI -> NA  [missing: adiponectin, I0]
#> glycemic_markers(): computing markers:
#>   SPISE           [HDL_c, TG, BMI]
#>   METS_IR         [glucose, TG, BMI, HDL_c]
#>   prediabetes     NA [HbA1c missing]
#>   diabetes        NA [HbA1c missing]
#>   HOMA_CP         NA [C_peptide/G0 missing]
#>   LAR             NA [leptin/adiponectin missing]
#>   ASI             NA [adiponectin/I0 missing]
#>   TyG_index       [TG, glucose]
#> glycemic_markers(): results: SPISE 1/1, METS_IR 0/1, prediabetes 0/1, diabetes 0/1, HOMA_CP 0/1, LAR 0/1, ASI 0/1, TyG_index 1/1
```

## Column recognition

Run `hm_col_report(your_data)` to check which analyte columns are
auto-detected before building your `col_map`. See the [Multi-Biobank
Compatibility](https://sufyansuleman.github.io/HealthMarkers/articles/articles/multi_biobank.md)
article for recognised synonyms across major biobanks.

``` r
hm_col_report(your_data)
```

## Tips

- Start with fasting lipids and BMI (or weight/height); add
  glucose/HbA1c/C-peptide/insulin/adipokines for richer markers.
- Provide `weight` and `height` instead of BMI when BMI is not directly
  available — it is computed automatically.
- Convert units beforehand if your data are not in the expected
  mmol/L/pmol/L/ng/mL units.
- For stricter pipelines, use `na_action = "omit"` or `"error"`;
  keep/warn for exploratory work.
- Use `verbose = TRUE` (default) for detailed output messages and
  diagnostics.
- See
  [`?glycemic_markers`](https://sufyansuleman.github.io/HealthMarkers/reference/glycemic_markers.md)
  for full argument details and marker definitions.
