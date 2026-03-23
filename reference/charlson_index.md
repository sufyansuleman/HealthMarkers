# Charlson Comorbidity Index (CCI)

Computes the Charlson Comorbidity Index by summing weighted
comorbidities.

## Usage

``` r
charlson_index(
  data,
  col_map = list(mi = "mi", chf = "chf", pvd = "pvd", stroke = "stroke", dementia =
    "dementia", copd = "copd", rheum = "rheum", ulcer = "ulcer", mild_liver =
    "mild_liver", diabetes = "diabetes", diab_comp = "diab_comp", hemiplegia =
    "hemiplegia", renal = "renal", cancer = "cancer", leukemia = "leukemia", lymphoma =
    "lymphoma", sev_liver = "sev_liver", metastatic_cancer = "metastatic_cancer", hiv =
    "hiv"),
  verbose = FALSE,
  na_action = c("keep", "omit", "error", "ignore", "warn"),
  check_extreme = FALSE,
  extreme_action = c("warn", "cap", "error", "ignore", "NA"),
  extreme_rules = NULL
)
```

## Arguments

- data:

  A data.frame or tibble with binary indicators (0/1) for each
  comorbidity.

- col_map:

  Named list mapping keys to columns in `data`: mi, chf, pvd, stroke,
  dementia, copd, rheum, ulcer, mild_liver, diabetes, diab_comp,
  hemiplegia, renal, cancer, leukemia, lymphoma, sev_liver,
  metastatic_cancer, hiv.

- verbose:

  Logical; if TRUE, emits progress messages.

- na_action:

  One of c("keep","omit","error","ignore","warn").

- check_extreme:

  Logical; if TRUE, scan inputs for plausible ranges (binary 0-1).

- extreme_action:

  One of c("warn","cap","error","ignore","NA").

- extreme_rules:

  Optional overrides (applied per key); default bounds c(0,1) for all.

## Value

A tibble with one column: charlson_index (integer total score; NA if any
required input is NA and na_action != "omit").

## Details

The Charlson Index predicts 10-year mortality by summing weighted
comorbidities. We implement the canonical 19-condition, weight scheme:

- 1 point: myocardial infarction, congestive heart failure, peripheral
  vascular disease, cerebrovascular disease (stroke), dementia, chronic
  pulmonary disease (COPD), rheumatologic disease, peptic ulcer disease

- 2 points: hemiplegia/paraplegia, moderate/severe renal disease, any
  malignancy (non-metastatic), leukemia, lymphoma, diabetes with
  complications

- 3 points: moderate/severe liver disease

- 6 points: metastatic solid tumor, AIDS/HIV

To avoid double counting paired conditions, the following use the
maximum applicable weight:

- Diabetes: max(1 \* diabetes without complications, 2 \* diabetes with
  complications)

- Liver disease: max(1 \* mild liver disease, 3 \* moderate/severe liver
  disease)

- Cancer: max(2 \* non-metastatic solid tumor, 6 \* metastatic solid
  tumor)

Age points are not included here and can be added separately if needed.

## Examples

``` r
patient <- tibble::tibble(
  mi=0, chf=0, pvd=0, stroke=0, dementia=0, copd=0, rheum=0, ulcer=0,
  mild_liver=0, diabetes=0, diab_comp=1, hemiplegia=0, renal=1,
  cancer=0, leukemia=0, lymphoma=0, sev_liver=0, metastatic_cancer=0, hiv=0
)
charlson_index(
  patient,
  col_map = as.list(stats::setNames(names(patient), names(patient)))
)
#> # A tibble: 1 × 1
#>   charlson_index
#>            <int>
#> 1              4
```
