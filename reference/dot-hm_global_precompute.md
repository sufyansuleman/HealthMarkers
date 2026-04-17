# Derive "Tier 0" variables before any marker function runs.

Computes the following variables **only when they are absent** from
`data`:

- BMI:

  from `weight` (kg) and `height` (m or cm) via `weight / height_m^2`

- glucose / G0:

  bidirectional alias: whichever is absent is filled from the other

- insulin / I0:

  bidirectional alias (pmol/L \<-\> muU/mL unchanged – same unit
  assumed)

- eGFR:

  CKD-EPI 2009 creatinine equation from `creatinine`, `age`, `sex` (and
  optionally `race`). Written as `eGFR` only; downstream functions that
  need `eGFR_cr` receive it via col_map.

- UACR:

  `urine_albumin` / `urine_creatinine` (both in mg/mmol or the ratio
  already in mg/g – no unit conversion; caller's responsibility)

- LDL_c:

  Friedewald: `TC - HDL_c - TG/2.2` (mmol/L); skipped if TG \> 4.5

## Usage

``` r
.hm_global_precompute(data, col_map = NULL, verbose = FALSE)
```

## Arguments

- data:

  data.frame after col_map materialization.

- col_map:

  Named list of key -\> column mappings (used only to resolve the
  physical column names when materialization has NOT been run yet).

- verbose:

  Logical; emit one-line summary per derived variable.

## Value

A list:

- data:

  data.frame with new columns appended.

- precomputed:

  Character vector of variable names that were derived.

## Details

col_map keys are respected: if the user has mapped e.g.\\
`creatinine -> "Cr_serum"`, the materialized `data[["creatinine"]]`
column (placed there by
[`.hm_build_col_map()`](https://sufyansuleman.github.io/HealthMarkers/reference/dot-hm_build_col_map.md))
is used transparently.
