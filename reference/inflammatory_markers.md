# Compute inflammatory indices (classic, eosinophil, or both)

Panels:

- classic: NLR, PLR, LMR, dNLR, SII, SIRI, AISI, CRP_category

- eos: NLR, PLR, LMR, NER, SII, SIRI, PIV, CLR, CAR, PCR, mGPS, ESR (if
  mapped)

- both: union of classic and eos panels

## Usage

``` r
inflammatory_markers(
  data,
  col_map,
  panel = c("auto", "classic", "eos", "both"),
  na_action = c("keep", "omit", "error"),
  check_extreme = FALSE,
  extreme_action = c("warn", "cap", "error", "ignore", "NA"),
  verbose = FALSE
)
```

## Arguments

- data:

  data.frame or tibble

- col_map:

  named list mapping keys to column names in `data`. Keys: neutrophils,
  lymphocytes, monocytes, platelets, WBC, CRP, albumin, eosinophils,
  ESR.

- panel:

  one of c("auto","classic","eos","both"). "auto" uses presence of
  eosinophils key.

- na_action:

  one of c("keep","omit","error"). Default "keep" propagates NA in
  outputs where inputs are missing. "omit" drops rows with any NA in
  required inputs. "error" aborts if required inputs contain NA.

- check_extreme:

  logical; if TRUE, handle extremes per `extreme_action`

- extreme_action:

  one of c("warn","cap","error","ignore","NA")

- verbose:

  logical; if TRUE, prints progress messages via hm_inform

## Value

tibble with selected inflammatory indices

## Details

Derived markers:

- NLR = neutrophils / lymphocytes

- PLR = platelets / lymphocytes

- LMR = lymphocytes / monocytes

- dNLR = neutrophils / (WBC - neutrophils) when WBC available

- SII = platelets \* neutrophils / lymphocytes

- SIRI = neutrophils \* monocytes / lymphocytes

- AISI = neutrophils \* monocytes \* platelets / lymphocytes

- CRP_category: "low" (\<1 mg/L), "moderate" (1-3 mg/L), "high" (\>3
  mg/L) when CRP available

- Eosinophil-panel extras: NER = neutrophils / eosinophils; PIV =
  platelets*neutrophils*monocytes/lymphocytes; CLR = CRP/lymphocytes;
  CAR = CRP/albumin; PCR = platelets/CRP; mGPS (CRP, albumin); ESR
  passthrough.

## References

Zahorec R (2001). “Ratio of neutrophil to lymphocyte counts–rapid and
simple parameter of systemic inflammation and stress.” *Bratislavske
lekarske listy*, **102**(1), 5–14. No DOI identified; PMID: 11723675,
<https://pubmed.ncbi.nlm.nih.gov/11723675/>. ; Templeton AJ, others
(2014). “Prognostic role of neutrophil-to-lymphocyte ratio in solid
tumors: a systematic review and meta-analysis.” *Journal of the National
Cancer Institute*, **106**(6), dju124.
[doi:10.1093/jnci/dju124](https://doi.org/10.1093/jnci/dju124) . ; Hu B,
others (2014). “Systemic Immune-Inflammation Index Predicts Prognosis of
Patients after Curative Resection for Hepatocellular Carcinoma.”
*Clinical Cancer Research*, **20**(23), 6212–6222.
[doi:10.1158/1078-0432.CCR-14-0442](https://doi.org/10.1158/1078-0432.CCR-14-0442)
. ; Qi Q, others (2016). “A novel systemic inflammation response index
(SIRI) for predicting the survival of patients with pancreatic cancer
after chemotherapy.” *Cancer*, **122**(14), 2158–2167.
[doi:10.1002/cncr.30057](https://doi.org/10.1002/cncr.30057) . ;
Mammadova A, Naurzvai D, others (2025). “Association of systemic
immune-inflammation index and aggregate index of systemic inflammation
with clinical status in stable and exacerbated COPD: A single-center
retrospective study.” *Medicine*, **104**(39), e44589.
[doi:10.1097/MD.0000000000044589](https://doi.org/10.1097/MD.0000000000044589)
. ; Proctor MJ, others (2011). “An inflammation-based prognostic score
(mGPS) predicts cancer survival independent of tumour site: a Glasgow
Inflammation Outcome Study.” *British Journal of Cancer*, **104**(4),
726–734.
[doi:10.1038/sj.bjc.6606087](https://doi.org/10.1038/sj.bjc.6606087) . ;
Pearson TA, others (2003). “Markers of inflammation and cardiovascular
disease: a statement for healthcare professionals from the CDC and AHA.”
*Circulation*, **107**(3), 499–511.
[doi:10.1161/01.CIR.0000052939.59093.45](https://doi.org/10.1161/01.CIR.0000052939.59093.45)
.

## Examples

``` r
df <- data.frame(
  neutrophils = c(4, 2),
  lymphocytes = c(2, 0),
  monocytes   = c(0.5, 0.3),
  platelets   = c(200, 150),
  WBC         = c(7, 4.5),
  CRP         = c(2.5, 0.8),
  albumin     = c(40, 42),
  eosinophils = c(0.2, 0.1),
  ESR         = c(12, 15)
)
cm <- list(
  neutrophils = "neutrophils", lymphocytes = "lymphocytes", monocytes = "monocytes",
  platelets = "platelets", WBC = "WBC", CRP = "CRP", albumin = "albumin",
  eosinophils = "eosinophils", ESR = "ESR"
)
# Classic panel (no eosinophils key)
classic_cm <- cm; classic_cm$eosinophils <- NULL; classic_cm$ESR <- NULL
inflammatory_markers(df, classic_cm, panel = "classic", na_action = "keep")
#> Warning: inflammatory_markers(): zero denominators detected.
#> # A tibble: 2 × 8
#>     NLR   PLR   LMR  dNLR   SII  SIRI  AISI CRP_category
#>   <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <ord>       
#> 1     2   100     4  1.33   400     1   200 moderate    
#> 2   Inf   Inf     0  0.8    Inf   Inf   Inf low         
# Eosinophil panel
inflammatory_markers(df, cm, panel = "eos", na_action = "keep",
                     check_extreme = TRUE, extreme_action = "cap", verbose = TRUE)
#> inflammatory_markers(): preparing inputs
#> inflammatory_markers(): column map: neutrophils -> 'neutrophils', lymphocytes -> 'lymphocytes', monocytes -> 'monocytes', platelets -> 'platelets', CRP -> 'CRP'
#> Warning: inflammatory_markers(): capped out-of-range extreme input values.
#> inflammatory_markers(): results: NLR 2/2, PLR 2/2, LMR 2/2, NER 2/2, SII 2/2, SIRI 2/2, PIV 2/2, CLR 2/2, CAR 2/2, PCR 2/2, mGPS 2/2, ESR 2/2
#> # A tibble: 2 × 12
#>     NLR   PLR   LMR   NER   SII  SIRI   PIV   CLR    CAR   PCR  mGPS   ESR
#>   <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl>  <dbl> <dbl> <int> <dbl>
#> 1     2   100  4        4   400   1     200  1.25 0.0625   80      0    12
#> 2     2   150  3.33     2   300   0.6    90  0.8  0.0190  188.     0    15
```
