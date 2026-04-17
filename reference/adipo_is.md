# Adipose insulin sensitivity indices (QUICKI, VAI, LAP, TyG, TG/HDL, Belfiore)

Computes adipose-related insulin sensitivity/resistance indices from
fasting inputs. Expected input units (converted internally):

- Glucose G0 mmol/L -\> mg/dL (\* 18)

- Insulin I0 pmol/L -\> muU/mL (/ 6)

- TG mmol/L -\> mg/dL (\* 88.57)

- HDL mmol/L -\> mg/dL (\* 38.67)

Reported indices (higher magnitude of negative "\_inv" values implies
worse adipose IR):

- Revised_QUICKI = 1 / (log10(I0 (muU/mL)) + log10(G0 (mg/dL)) +
  log10(FFA (mmol/L)))

- VAI (sex-specific; inverted as VAI\_\*\_inv so larger negative =
  worse)

- TG_HDL_C_inv = -(TG/HDL) in mg/dL

- TyG_inv = -ln(TG (mg/dL) \* G0 (mg/dL) / 2)

- LAP (sex-specific; inverted)

- McAuley_index = exp(2.63 - 0.28 ln(I0 (muU/mL)) - 0.31 ln(TG
  (mmol/L)))

- Adipo_inv = -(FFA \* I0 (muU/mL))

- Belfiore_inv_FFA = - 2 / (I0 (muU/mL) \* FFA + 1)

## Usage

``` r
adipo_is(
  data,
  col_map = NULL,
  normalize = "none",
  na_action = c("keep", "omit", "error"),
  verbose = TRUE,
  ...
)
```

## Arguments

- data:

  Data frame or tibble with required columns mapped by `col_map`

- col_map:

  Named list mapping keys to columns: G0, I0, TG, HDL_c, FFA, waist, bmi

- normalize:

  One of c("none","z","inverse","range","robust"); default "none"

- na_action:

  One of c("keep","omit","error"); default "keep"

- verbose:

  Logical; if `TRUE` (default), prints column mapping, the list of
  indices being computed, and a per-column results summary.

- ...:

  Reserved

## Value

A tibble with columns: `Revised_QUICKI`, `VAI_Men_inv`, `VAI_Women_inv`,
`TG_HDL_C_inv`, `TyG_inv`, `LAP_Men_inv`, `LAP_Women_inv`,
`McAuley_index`, `Adipo_inv`, `Belfiore_inv_FFA`. If an ID column is
detected in `data` (e.g. `id`, `IID`, `participant_id`), it is prepended
as the first output column.

## References

Katz A, Nambi SS, Mather K, Baron AD, Follmann DA, Sullivan G, Quon MJ
(2000). “Quantitative Insulin Sensitivity Check Index: A Simple,
Accurate Method for Assessing Insulin Sensitivity in Humans.” *Journal
of Clinical Endocrinology & Metabolism*, **85**(7), 2402–2410.
[doi:10.1210/jcem.85.7.6661](https://doi.org/10.1210/jcem.85.7.6661) . ;
Amato MC, Giordano C, Galia M, Criscimanna A, Vitabile S, Midiri M,
Galluzzo A (2010). “Visceral Adiposity Index: A Reliable Indicator of
Visceral Fat Function Associated with Cardiometabolic Risk.” *Diabetes
Care*, **33**(4), 920–922.
[doi:10.2337/dc09-1825](https://doi.org/10.2337/dc09-1825) . ; Kahn HS
(2005). “The Lipid Accumulation Product Performs Better than Body Mass
Index as an Indicator of Cardiovascular Risk in Women.” *BMC
Cardiovascular Disorders*, **5**(1), 26.
[doi:10.1186/1471-2261-5-26](https://doi.org/10.1186/1471-2261-5-26) . ;
Guerrero-Romero F, Simental-Mendía LE, González-Ortiz M,
Martínez-Abundis E, Ramos-Zavala MG, Hernández-González SO,
Jacques-Camarena O, Rodríguez-Morán M (2010). “The Product of
Triglycerides and Glucose, a Simple Measure of Insulin Sensitivity.
Comparison with the Euglycemic-Hyperinsulinemic Clamp.” *Journal of
Clinical Endocrinology & Metabolism*, **95**(7), 3347–3351.
[doi:10.1210/jc.2010-0288](https://doi.org/10.1210/jc.2010-0288) . ;
Dobiášová M, Frohlich JJ (2001). “The Plasma Parameter Log(TG/HDL-C) as
an Atherogenic Index: Correlation with Lipoprotein Particle Size and
Esterification Rate in ApoB-Lipoprotein-Depleted Plasma.” *Clinical
Biochemistry*, **34**(7), 583–588.
[doi:10.1016/S0009-9120(01)00263-6](https://doi.org/10.1016/S0009-9120%2801%2900263-6)
. ; Belfiore F, Iannello S, Volpicelli G (1998). “Insulin Sensitivity
Indices Calculated from Basal and OGTT-Related Insulin and Glucose
Levels.” *Molecular Genetics and Metabolism*, **63**(2), 134–141.
[doi:10.1006/mgme.1997.2658](https://doi.org/10.1006/mgme.1997.2658) . ;
Raynaud E, Pérez-Martin A, Brun J, Benhaddad AA, Mercier J (1999).
“Fasting Plasma Insulin and Insulin Resistance Indices.” *Diabetes &
Metabolism*, **25**(6), 524–532. No DOI identified in Crossref/PubMed as
of 2026-03-16; see URL,
<https://pubmed.ncbi.nlm.nih.gov/?term=Fasting+Plasma+Insulin+and+Insulin+Resistance+Indices>.

## Examples

``` r
df <- tibble::tibble(
  G0 = c(5.2, 6.1),      # mmol/L
  I0 = c(60, 110),       # pmol/L
  TG = c(1.2, 1.8),      # mmol/L
  HDL_c = c(1.3, 1.0),   # mmol/L
  FFA = c(0.4, 0.6),     # mmol/L
  waist = c(85, 102),    # cm
  bmi = c(24, 31)        # kg/m^2
)
cm <- as.list(names(df)); names(cm) <- names(df)
out <- adipo_is(df, cm, verbose = FALSE, na_action = "keep")
```
