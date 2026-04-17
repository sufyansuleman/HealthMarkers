# SARC-F Sarcopenia Screening Score

Computes the SARC-F questionnaire score, a quick screening tool for
sarcopenia risk.

## Usage

``` r
sarc_f_score(
  data,
  col_map = NULL,
  na_action = c("keep", "omit", "error", "ignore", "warn"),
  verbose = TRUE
)
```

## Arguments

- data:

  A data.frame or tibble with SARC-F questionnaire responses.

- col_map:

  Named list mapping the five SARC-F components to columns: strength,
  walking, chair, stairs, falls.

- na_action:

  One of c("keep","omit","error","ignore","warn").

- verbose:

  Logical; if TRUE (default), emits progress messages.

## Value

A tibble with:

- sarc_f_score (numeric 0-10; NA if any component is NA)

- sarc_f_high_risk (logical; TRUE if score \>= 4, NA if score is NA)

## Details

SARC-F has 5 items: Strength, Assistance in walking, Rise from a chair,
Climb stairs, and Falls. Each item is scored 0 (no difficulty) to 2
(high difficulty). Total SARC-F score ranges 0-10. A score \>= 4
indicates high risk of sarcopenia and suggests further assessment.

## References

Malmstrom TK, Morley JE (2013). “SARC-F: a simple questionnaire to
rapidly diagnose sarcopenia.” *Journal of the American Medical Directors
Association*, **14**(8), 531–532.
[doi:10.1016/j.jamda.2013.05.018](https://doi.org/10.1016/j.jamda.2013.05.018)
. ; Malmstrom TK, Miller DK, Simonsick EM, Ferrucci L, Morley JE (2015).
“SARC-F: a symptom score to predict persons with sarcopenia at risk for
poor functional outcomes.” *Journal of Cachexia, Sarcopenia and Muscle*,
**7**(1), 28–36.
[doi:10.1002/jcsm.12048](https://doi.org/10.1002/jcsm.12048) .

## Examples

``` r
df <- data.frame(Strength = c(1, 2, 0), Walking = c(0, 1, 2),
                 Chair = c(1, 1, 2), Stairs = c(0, 2, 2), Falls = c(0, 1, 1))
sarc_f_score(df)
#> sarc_f_score(): reading input 'df' — 3 rows × 5 variables
#> sarc_f_score(): col_map (5 columns — 5 inferred from data)
#>   strength          ->  'Strength'    (inferred)
#>   walking           ->  'Walking'    (inferred)
#>   chair             ->  'Chair'    (inferred)
#>   stairs            ->  'Stairs'    (inferred)
#>   falls             ->  'Falls'    (inferred)
#> sarc_f_score(): computing markers:
#>   sarc_f_score      [0-10 sum]
#>   sarc_f_high_risk  [score >= 4]
#> sarc_f_score(): results: sarc_f_score 3/3, sarc_f_high_risk 3/3
#> # A tibble: 3 × 2
#>   sarc_f_score sarc_f_high_risk
#>          <dbl> <lgl>           
#> 1            2 FALSE           
#> 2            7 TRUE            
#> 3            7 TRUE            
```
