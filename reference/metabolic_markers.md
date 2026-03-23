# Aggregate selected metabolic marker groups

Aggregate selected metabolic marker groups

## Usage

``` r
metabolic_markers(
  data,
  col_map,
  which = c("insulin", "adiposity_sds", "cardio", "lipid", "liver", "glycemic", "mets"),
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

  Named list for column mapping forwarded to underlying functions.

- which:

  Character vector of groups to compute:
  c("insulin","adiposity_sds","cardio","lipid","liver","glycemic","mets").

- normalize:

  One of c("none","z","inverse","range","robust").

- mode:

  One of c("both","IS","IR").

- verbose:

  Logical.

- na_action:

  One of c("keep","omit","error"); forwarded to underlying calculators
  (HM-CS v2).

## Value

Data frame with original columns plus derived markers.

## Note

For references supporting liver, lipid, glycemic, MetS, adiposity and
other domain-specific indices, see each underlying function's
documentation (e.g. ?liver_markers, ?lipid_markers, ?glycemic_markers,
?metss, ?adiposity_sds). This wrapper omits repeated reference listings
to avoid redundancy.

## References

Aggregator wrapper. See underlying function help pages for full
references: all_insulin_indices(), lipid_markers(), liver_markers(),
glycemic_markers(), metss().

## Examples

``` r
df <- data.frame(
  TC = 200, HDL_c = 50, TG = 150, LDL_c = 120,
  ALT = 30, AST = 20, BMI = 25
)
metabolic_markers(df, col_map = list(), which = c("lipid","liver"),
                  normalize = "none", mode = "both", verbose = FALSE, na_action = "keep")
#>    TC HDL_c  TG LDL_c ALT AST BMI non_HDL_c remnant_c ratio_TC_HDL ratio_TG_HDL
#> 1 200    50 150   120  30  20  25       150        30            4            3
#>   ratio_LDL_HDL ApoB_ApoA1 triglycerides
#> 1           2.4         NA           150
```
