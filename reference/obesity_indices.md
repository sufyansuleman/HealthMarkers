# Compute anthropometric obesity & adiposity indices

Calculates a comprehensive set of body shape and adiposity indices:

- BMI and WHO BMI categories

- Waist-to-hip ratio (WHR) and optional WHR adjusted for BMI (WHRadjBMI)

- Waist-to-height ratio (WHtR)

- Abdominal Volume Index (AVI)

- Body Adiposity Index (BAI)

- A Body Shape Index (ABSI)

- Body Roundness Index (BRI)

- Conicity Index (CI)

- (Optional) Relative Fat Mass (RFM)

## Usage

``` r
obesity_indices(
  data,
  weight,
  height,
  waist,
  hip,
  sex = NULL,
  weight_unit = c("kg", "lb"),
  height_unit = c("cm", "m"),
  adjust_WHR = FALSE,
  include_RFM = FALSE,
  na_action = c("keep", "omit", "error"),
  na_warn_prop = 0.2,
  verbose = TRUE
)
```

## Arguments

- data:

  A data.frame or tibble containing the input columns.

- weight:

  Unquoted column name for weight.

- height:

  Unquoted column name for height.

- waist:

  Unquoted column name for waist circumference.

- hip:

  Unquoted column name for hip circumference.

- sex:

  (Optional) Unquoted column name for sex, coded 0=male, 1=female;
  required if include_RFM=TRUE.

- weight_unit:

  One of c("kg","lb"); if "lb", converts weight to kg by \*0.45359237.

- height_unit:

  One of c("cm","m"); if "cm", converts height to metres by /100.

- adjust_WHR:

  Logical; if TRUE, adds a column WHRadjBMI as residuals from WHR ~ BMI.

- include_RFM:

  Logical; if TRUE, computes Relative Fat Mass (requires sex column).

- na_action:

  One of c("keep","omit","error") for handling NA in required inputs.
  Default "keep".

- na_warn_prop:

  Proportion \\\[0,1\]\\ to trigger high-missingness warnings for
  required inputs. Default 0.2.

- verbose:

  Logical; if TRUE, prints column mapping and computing messages.

## Value

A tibble with only the computed indices (slim output):

- weight_kg, height_m (unit-normalised intermediates),

- BMI, BMI_cat,

- WHR, WHRadjBMI (if `adjust_WHR = TRUE`),

- waist_to_height_ratio, waist_to_BMI_ratio, weight_to_height_ratio,

- AVI, BAI, ABSI, BRI, CI,

- RFM (if `include_RFM = TRUE`).

## Details

Units assumed (no automatic conversion beyond the specified
weight/height options):

- weight: kg (or lb if weight_unit = "lb")

- height: m (or cm if height_unit = "cm")

- waist, hip: cm

- sex: 0 = male, 1 = female (only required if include_RFM = TRUE)

## References

Quetelet A (1842). *A Treatise on Man, and the Development of his
Faculties*. William and Robert Chambers, Edinburgh. Historical
monograph; no DOI assigned,
<https://archive.org/search?query=A%20Treatise%20on%20Man%20Quetelet>.
WHO Expert Committee (1995). “Physical Status: The Use and
Interpretation of Anthropometry. WHO Technical Report Series 854.” World
Health Organization, Geneva. No DOI for this WHO technical report; see
URL, <https://www.who.int/publications/i/item/9241208546>.
Guerrero-Romero F, Rodríguez-Morán M (2003). “Abdominal volume index. An
anthropometric index of central obesity.” *Archives of Medical
Research*, **34**(6), 428–432.
[doi:10.1016/S0188-4409(03)00073-0](https://doi.org/10.1016/S0188-4409%2803%2900073-0)
. Bergman RN, Stefanovski D, Buchanan TA, others (2011). “A better index
of body adiposity.” *Obesity (Silver Spring)*, **19**(5), 1083–1089.
[doi:10.1038/oby.2011.38](https://doi.org/10.1038/oby.2011.38) .
Krakauer NY, Krakauer JC (2012). “A new body shape index predicts
mortality hazard independently of BMI.” *PLoS One*, **7**(7), e39504.
[doi:10.1371/journal.pone.0039504](https://doi.org/10.1371/journal.pone.0039504)
. Thomas DM, Bredlau C, Bosy-Westphal A, others (2013). “Relationships
between body roundness with body fat and visceral adipose tissue
emerging from a new geometrical model.” *Obesity (Silver Spring)*,
**21**(11), 2264–2271.
[doi:10.1002/oby.20408](https://doi.org/10.1002/oby.20408) . Valdez R
(1991). “A simple model-based index of abdominal adiposity.” *Journal of
Clinical Epidemiology*, **44**(9), 955–956.
[doi:10.1016/0895-4356(91)90059-I](https://doi.org/10.1016/0895-4356%2891%2990059-I)
. Woolcott OO, Bergman RN (2018). “Relative fat mass (RFM) as a new
estimator of whole-body fat percentage.” *Scientific Reports*, **8**,
10980.
[doi:10.1038/s41598-018-29362-1](https://doi.org/10.1038/s41598-018-29362-1)
. Calle EE, Thun MJ, Petrelli JM, Rodriguez C, Heath Jr. CW (1999).
“Body-mass index and mortality in a prospective cohort of U.S. adults.”
*New England Journal of Medicine*, **341**(15), 1097–1105.
[doi:10.1056/NEJM199910073411501](https://doi.org/10.1056/NEJM199910073411501)
. Freedman DS, Thornton JC, Pi-Sunyer FX, others (2012). “The body
adiposity index is not a more accurate measure of adiposity than BMI,
waist circumference, or hip circumference.” *Obesity (Silver Spring)*,
**20**(12), 2438–2444.
[doi:10.1038/oby.2012.81](https://doi.org/10.1038/oby.2012.81) . He S,
Chen X (2013). “Could the new body shape index predict the new onset of
diabetes mellitus in the Chinese population?” *PLoS One*, **8**(1),
e50573.
[doi:10.1371/journal.pone.0050573](https://doi.org/10.1371/journal.pone.0050573)
. Maessen MF, Eijsvogels TM, Verheggen RJ, others (2014). “Entering a
new era of body indices: the feasibility of ABSI and BRI to identify
cardiovascular health status.” *PLoS One*, **9**(9), e107212.
[doi:10.1371/journal.pone.0107212](https://doi.org/10.1371/journal.pone.0107212)
.

## Examples

``` r
library(tibble)
df <- tibble(
  wt     = c(70, 80),  # kg
  ht     = c(175, 165),# cm
  waist  = c(80, 90),  # cm
  hip    = c(100, 95), # cm
  sex    = c(0, 1)
)
obesity_indices(
  df,
  weight       = wt,
  height       = ht,
  waist        = waist,
  hip          = hip,
  sex          = sex,
  weight_unit  = "kg",
  height_unit  = "cm",
  adjust_WHR   = TRUE,
  include_RFM  = TRUE,
  verbose      = TRUE
)
#> obesity_indices(): reading input 'df' — 2 rows × 5 variables
#> obesity_indices(): col_map: weight -> 'wt', height -> 'ht', waist -> 'waist', hip -> 'hip', sex -> 'sex'
#> obesity_indices(): computing markers:
#>   BMI, BMI_cat, WHR, WHtR, AVI, BAI, ABSI, BRI, CI, WHRadjBMI, RFM
#> obesity_indices(): results: weight_kg 2/2, height_m 2/2, BMI 2/2, BMI_cat 2/2, WHR 2/2, WHRadjBMI 2/2, waist_to_height_ratio 2/2, waist_to_BMI_ratio 2/2, weight_to_height_ratio 2/2, AVI 2/2, BAI 2/2, ABSI 2/2, BRI 2/2, CI 2/2, RFM 2/2
#> # A tibble: 2 × 15
#>   weight_kg height_m   BMI BMI_cat         WHR WHRadjBMI waist_to_height_ratio
#>       <dbl>    <dbl> <dbl> <chr>         <dbl>     <dbl>                 <dbl>
#> 1        70     1.75  22.9 Normal weight 0.8           0                  45.7
#> 2        80     1.65  29.4 Overweight    0.947         0                  54.5
#> # ℹ 8 more variables: waist_to_BMI_ratio <dbl>, weight_to_height_ratio <dbl>,
#> #   AVI <dbl>, BAI <dbl>, ABSI <dbl>, BRI <dbl>, CI <dbl>, RFM <dbl>
```
