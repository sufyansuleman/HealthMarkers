# Calculate lipid-panel markers, Visceral Adiposity Index (VAI), Lipid Accumulation Product (LAP), and TyG-BMI index

Given total cholesterol, HDL, TG (and optionally LDL, ApoB/ApoA1, waist,
BMI, glucose), computes:

- `non_HDL_c`, `remnant_c`

- `ratio_TC_HDL`, `ratio_TG_HDL`, `ratio_LDL_HDL`

- `ApoB_ApoA1`

- `VAI_Men`, `VAI_Women`

- `LAP_Men`, `LAP_Women`

- `TyG_BMI`

## Usage

``` r
lipid_markers(
  data,
  col_map = list(TC = "TC", HDL_c = "HDL_c", TG = "TG", LDL_c = "LDL_c", ApoB = "ApoB",
    ApoA1 = "ApoA1", waist = NULL, BMI = NULL),
  na_action = c("keep", "omit", "error", "ignore", "warn"),
  check_extreme = FALSE,
  extreme_action = c("warn", "cap", "error", "ignore", "NA"),
  extreme_rules = NULL,
  verbose = FALSE
)
```

## Arguments

- data:

  A `data.frame` or `tibble` containing your lipid (and optional
  anthropometry/glucose) data.

- col_map:

  Named list mapping:

  - `TC` -\> total cholesterol

  - `HDL_c` -\> HDL-C

  - `TG` -\> triglycerides

  - `LDL_c` -\> (optional) LDL-C; if missing, estimated via Friedewald

  - `ApoB`, `ApoA1` -\> (optional) apolipoproteins

  - `waist` -\> (optional) waist circumference (cm)

  - `BMI` -\> (optional) body mass index (kg/m^2)

- na_action:

  One of c("keep","omit","error","ignore","warn").

  - keep/ignore: compute and propagate NA in outputs

  - omit: drop rows with NA in required inputs (TC, HDL_c, TG)

  - error: abort if any required input contains NA

  - warn: like keep, but emit missingness warnings

- check_extreme:

  Logical; if TRUE, scan inputs for out-of-range values.

- extreme_action:

  One of c("warn","cap","error","ignore","NA") controlling how extremes
  are handled when check_extreme=TRUE. "cap" truncates to range; "NA"
  sets flagged values to NA.

- extreme_rules:

  Optional named list of c(min,max) per key to override defaults.

- verbose:

  Logical; if `TRUE`, prints messages about computing markers.

## Value

A tibble with:

- `non_HDL_c`, `remnant_c`

- `ratio_TC_HDL`, `ratio_TG_HDL`, `ratio_LDL_HDL`

- `ApoB_ApoA1`

- `VAI_Men`, `VAI_Women`

- `LAP_Men`, `LAP_Women`

- `TyG_BMI`

## References

Friedewald WT, Levy RI, Fredrickson DS (1972). “Estimation of the
concentration of LDL cholesterol in plasma, without use of preparative
ultracentrifuge.” *Clinical Chemistry*, **18**(6), 499–502.
[doi:10.1093/clinchem/18.6.499](https://doi.org/10.1093/clinchem/18.6.499)
. Amato MC, Giordano C, Galia M, Criscimanna A, Vitabile S, Midiri M,
Galluzzo A (2010). “Visceral Adiposity Index: A Reliable Indicator of
Visceral Fat Function Associated with Cardiometabolic Risk.” *Diabetes
Care*, **33**(4), 920–922.
[doi:10.2337/dc09-1825](https://doi.org/10.2337/dc09-1825) . Kahn HS
(2005). “The Lipid Accumulation Product Performs Better than Body Mass
Index as an Indicator of Cardiovascular Risk in Women.” *BMC
Cardiovascular Disorders*, **5**(1), 26.
[doi:10.1186/1471-2261-5-26](https://doi.org/10.1186/1471-2261-5-26) .
Er L, Wu S, Chou H, Hsu L, Teng M, Sun Y, Ko Y (2016). “Triglyceride
Glucose-Body Mass Index Is a Simple and Clinically Useful Surrogate
Marker for Insulin Resistance in Nondiabetic Individuals.” *PLOS ONE*,
**11**(3), e0149731.
[doi:10.1371/journal.pone.0149731](https://doi.org/10.1371/journal.pone.0149731)
. Khamseh ME, Malek M, Abbasi R, Taheri E, others (2021). “Triglyceride
Glucose Index and Related Parameters (Triglyceride Glucose-Body Mass
Index and Triglyceride Glucose-Waist Circumference) Identify
Nonalcoholic Fatty Liver and Liver Fibrosis in Individuals with
Overweight/Obesity.” *Metabolic Syndrome and Related Disorders*,
**19**(3), 167–173.
[doi:10.1089/met.2020.0109](https://doi.org/10.1089/met.2020.0109) .

## Examples

``` r
df <- data.frame(TC = c(5.2, 6.1), HDL_c = c(1.3, 1.1), TG = c(1.8, 2.3),
                 LDL_c = c(3.2, 4.1), WC = c(88, 95), BMI = c(26, 29),
                 Sex = c("female", "male"))
lipid_markers(df)
#> # A tibble: 2 × 6
#>   non_HDL_c remnant_c ratio_TC_HDL ratio_TG_HDL ratio_LDL_HDL ApoB_ApoA1
#>       <dbl>     <dbl>        <dbl>        <dbl>         <dbl>      <dbl>
#> 1       3.9       0.7         4            1.38          2.46         NA
#> 2       5         0.9         5.55         2.09          3.73         NA
```
