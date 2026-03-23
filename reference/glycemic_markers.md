# Calculate glycemic-, C-peptide-, and additional metabolic markers

Given fasting labs and anthropometry, computes:

- SPISE (Single-Point Insulin Sensitivity Estimator)

- METS_IR (Metabolic Score for Insulin Resistance)

- prediabetes flag (HbA1c \>= 42 mmol/mol)

- diabetes flag (HbA1c \>= 48 mmol/mol)

- HOMA_CP (C-peptide-based HOMA-IR variant; operational formula, see
  notes)

- LAR (Leptin/Adiponectin Ratio)

- ASI (Adiponectin Sensitivity Index; adiponectin/insulin)

- TyG_index (Triglyceride-Glucose Index)

## Usage

``` r
glycemic_markers(
  data,
  col_map = NULL,
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

  A data.frame or tibble containing at least:

  - HDL_c (mmol/L), TG (mmol/L), BMI (kg/m^2) Optional if present:
    glucose (mmol/L), HbA1c (mmol/mol), C_peptide (pmol/L), G0 (mmol/L),
    I0 (pmol/L), leptin (ng/mL), adiponectin (ng/mL).

- col_map:

  Optional named list mapping keys to column names in `data`. Required
  keys: `HDL_c`, `TG`, `BMI`. Optional keys (if present will be used):
  `glucose`, `HbA1c`, `C_peptide`, `G0`, `I0`, `leptin`, `adiponectin`.
  If `NULL` (default), the function uses columns named exactly as the
  keys.

- na_action:

  One of `c("ignore","warn","error","keep","omit")`. HM-CS aliases:
  `keep` == `ignore`; `omit` drops rows with any NA/non-finite in used
  inputs before computing markers. Default "ignore".

- na_warn_prop:

  Proportion (0-1) threshold for high-missingness warnings among used
  columns when `na_action = "warn"`. Default 0.2.

- check_extreme:

  Logical; if `TRUE`, scan selected input variables for values outside
  plausible ranges defined in `extreme_rules`. Default `FALSE`.

- extreme_action:

  One of `c("warn","cap","error","ignore","NA")` controlling how to
  handle extremes when `check_extreme = TRUE`. If "cap", values are
  truncated to the allowed range; if "NA", out-of-range values become
  NA. Default "warn".

- extreme_rules:

  Named list of numeric length-2 ranges for inputs to scan when
  `check_extreme = TRUE`. Defaults are broad medical plausibility
  ranges:

  - HDL_c: c(0.1, 5), TG: c(0.1, 20), BMI: c(10, 80),

  - glucose: c(2, 30), HbA1c: c(20, 200), C_peptide: c(0, 5000),

  - G0: c(2, 30), I0: c(0, 3000), leptin: c(0, 200), adiponectin: c(0,
    300). Only variables present in `data` are checked.

- verbose:

  Logical; if `TRUE`, prints progress and a completion summary. Default
  FALSE.

## Value

A tibble with columns:

- SPISE, METS_IR, prediabetes, diabetes, HOMA_CP, LAR, ASI, TyG_index

## Details

Assumed units (no automatic conversion of inputs except where noted):

- HDL_c, TG: mmol/L (TyG internally converts TG to mg/dL via 88.57)

- BMI: kg/m^2

- glucose, G0: mmol/L (TyG internally converts glucose to mg/dL via 18)

- HbA1c: mmol/mol

- C_peptide, I0: pmol/L (HOMA_CP uses I-like conversion factor 6 as in
  insulin muU/mL; see notes)

- leptin, adiponectin: ng/mL

Quality controls and options:

- Input validation ensures required variables exist and are
  numeric-coercible.

- Non-numeric inputs are coerced to numeric with a warning (NAs
  introduced reported).

- Missing or non-finite inputs are handled via `na_action`.

- Logs and divisions are computed safely (non-positive arguments yield
  NA).

- Optional detection/handling of extreme input values via
  `check_extreme` and `extreme_action`.

- Verbose mode prints step-by-step progress and a completion summary.

Notes on HOMA_CP:

- This function retains the package's existing operational formula:
  HOMA_CP = (G0 (mmol/L) \* (C_peptide (pmol/L) / 6)) / 22.5 which
  mirrors HOMA-IR's structure using a 6 pmol/muU scaling used for
  insulin. Users should verify unit conventions for their datasets;
  alternative C-peptide HOMA implementations exist (e.g., HOMA2-CP).

## References

Paulmichl K, Hatunic M, Höbaus C, et al. (2016). “Modification and
Validation of the Triglyceride-to–HDL Cholesterol Ratio as a Surrogate
of Insulin Sensitivity in White Juveniles and Adults without Diabetes
Mellitus: The Single Point Insulin Sensitivity Estimator (SPISE).”
*Clinical Chemistry*, **62**(9), 1211–1219.
[doi:10.1373/clinchem.2016.257436](https://doi.org/10.1373/clinchem.2016.257436)
. ; Bello-Chavolla OY, Almeda-Valdes P, García-Sánchez A, et al. (2018).
“METS-IR, a novel score to evaluate insulin sensitivity, is predictive
of visceral adiposity and incident type 2 diabetes.” *European Journal
of Endocrinology*, **178**(5), 533–544.
[doi:10.1530/EJE-17-0883](https://doi.org/10.1530/EJE-17-0883) . ;
Frühbeck G, Catalan V, Rodríguez A, Ramón Sánchez-Recalde Á, Becerril S,
Sánchez-González Á, Baena N, Valentí-Azcárate F, Burrell MA, Salvador J
(2019). “Adiponectin-leptin Ratio is a Functional Biomarker of Adipose
Tissue Inflammation.” *Nutrients*, **11**(2), 454.
[doi:10.3390/nu11020454](https://doi.org/10.3390/nu11020454) . ;
Matthews DR, Hosker JP, Rudenski AS, Naylor BA, Treacher DF, Turner RC
(1985). “Homeostasis Model Assessment: Insulin Resistance and Beta-Cell
Function from Fasting Plasma Glucose and Insulin Concentrations in Man.”
*Diabetologia*, **28**(7), 412–419.
[doi:10.1007/BF00280883](https://doi.org/10.1007/BF00280883) . ;
Simental-Mendía LE, Rodríguez-Morán M, Guerrero-Romero F (2008). “The
Product of Fasting Glucose and Triglycerides as Surrogate for
Identifying Insulin Resistance.” *Metabolic Syndrome and Related
Disorders*, **6**(4), 299–304.
[doi:10.1089/met.2008.0034](https://doi.org/10.1089/met.2008.0034) . ;
Furler SM, Gan SK, Poynten AM, Chisholm DJ, Campbell LV, Kriketos AD
(2006). “Relationship of Adiponectin with Insulin Sensitivity in Humans,
Independent of Lipid Availability.” *Obesity*, **14**(2), 228–234.
[doi:10.1038/oby.2006.29](https://doi.org/10.1038/oby.2006.29) .

## Examples

``` r
df <- tibble::tibble(
  HDL_c       = c(1.0, 1.3),
  TG          = c(1.3, 2.0),
  BMI         = c(24, 30),
  glucose     = c(5.6, 7.1),
  HbA1c       = c(44, 38),
  C_peptide   = c(300, 500),
  G0          = c(5.5, 6.2),
  I0          = c(60, 120),
  leptin      = c(10, 20),
  adiponectin = c(8, 5)
)
# Quiet defaults
glycemic_markers(df)
#> # A tibble: 2 × 8
#>   SPISE METS_IR prediabetes diabetes HOMA_CP   LAR    ASI TyG_index
#>   <dbl>   <dbl>       <int>    <int>   <dbl> <dbl>  <dbl>     <dbl>
#> 1  8.10     NA            1        0    12.2  1.25 0.133       8.67
#> 2  5.79    318.           0        0    23.0  4    0.0417      9.33
# Warn on missingness and scan for extremes with capping
glycemic_markers(df,
  na_action = "warn", na_warn_prop = 0.1,
  check_extreme = TRUE, extreme_action = "cap",
  verbose = TRUE
)
#> # A tibble: 2 × 8
#>   SPISE METS_IR prediabetes diabetes HOMA_CP   LAR    ASI TyG_index
#>   <dbl>   <dbl>       <int>    <int>   <dbl> <dbl>  <dbl>     <dbl>
#> 1  8.10     NA            1        0    12.2  1.25 0.133       8.67
#> 2  5.79    318.           0        0    23.0  4    0.0417      9.33
```
