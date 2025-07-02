HealthMarkers
================

- [🔍 Overview](#-overview)
- [⚙️ Installation](#️-installation)
- [🚀 Quickstart](#-quickstart)
  - [Inline Example: Fasting indices](#inline-example-fasting-indices)
  - [Visual summary](#visual-summary)
- [🎨 Normalization methods](#-normalization-methods)
- [⚙️ Function Reference](#️-function-reference)
- [🛠 Performance Tips](#-performance-tips)
- [🤝 Contributing](#-contributing)
- [📜 License](#-license)
- [🔗 References](#-references)

<!-- badges: start -->

[![CRAN_Status_Badge](https://www.r-pkg.org/badges/version/HealthMarkers)](https://cran.r-project.org/package=HealthMarkers)
[![Travis build
status](https://travis-ci.com/sufyansuleman/HealthMarkers.svg?branch=main)](https://travis-ci.com/sufyansuleman/HealthMarkers)

<!-- badges: end -->

# 🔍 Overview

HealthMarkers bundles:

1.  **Insulin Sensitivity & Resistance** estimates

    - **Fasting**: HOMA-IR, QUICKI, Raynaud, Bennett, FIRI,
      Belfiore_basal, IG-ratio, ISI₀₁₂₀, reversed HOMA₂, etc.
    - **OGTT**: Matsuda, Gutt, Cederholm, Stumvoll variants, Avignon,
      BIGTT-SI, Belfiore_ISI, HIRI_inv, etc.
    - **Adipose**: Revised-QUICKI, VAI (men/women), LAP (men/women),
      TyG, TG/HDL, Adipo-IR, McAuley, Belfiore_inv_FFA.
    - **Tracer/DXA**: LIRI_inv, ATIRI_inv, Lipo_inv (uses
      palmitate/glycerol tracer & fat mass).
    - **Auto-invert**: toggle between IS → IR (1/ISI) for any
      inverted-scale index.

2.  **Anthropometry & Metabolic Syndrome**

    - **Adiposity SDS**: standardized z-scores for BMI, waist,
      body-fat%, WHR, WHtR; **sex-stratified** and referenced to Danish
      growth standards.
    - **MetSSS**: continuous Metabolic Syndrome Severity Score per Wiley
      & Carrington (2016), stratified by sex/race.

3.  **Lipids & Cardiovascular Ratios**

    - Non-HDL, remnant cholesterol, TC/HDL, TG/HDL, LDL/HDL, ApoB/ApoA1,
      Atherogenic Index of Plasma (AIP).

4.  **Liver Markers**

    - Fatty Liver Index (FLI), NAFLD Fibrosis Score (NFS), APRI, FIB-4,
      BARD, ALBI, MELD-XI.

5.  **Cardiometabolic Flags**

    - Dyslipidemia, insulin resistance, hyperglycemia, hypertension
      (binary factors).

6.  **Glycemic & Non-Insulin Indices**

    - SPISE, METS-IR, prediabetes/diabetes flags (HbA1c), C-peptide
      HOMA.

7.  **Novel Biofluid Panels**

    - **Saliva**: log-cortisol, CAR_AUC (0–30–60 min), log-amylase,
      salivary glucose.
    - **Sweat**: chloride, Na/K ratio, lactate, sweat rate (L/m²/h).
    - **Urine**: UACR, microalbuminuria, eGFR (CKD-EPI), FENa, UPCR.

8.  **Pulmonary Function Estimates**

    - Spirometric indices (FEV1, FVC, FEV1/FVC % predicted) calculated
      via the `rspiro` package using Danish Dinit reference equations.

------------------------------------------------------------------------

# ⚙️ Installation

``` r
# From CRAN
install.packages("HealthMarkers")

# Or development version
# remotes::install_github("sufyansuleman/HealthMarkers")
```

------------------------------------------------------------------------

# 🚀 Quickstart

``` r
library(HealthMarkers)

# Example data.frame:
df <- tibble::tibble(
  G0 = 5.5, I0 = 60,
  G30 = 7.8, I30 = 90, G120 = 6.2, I120 = 50,
  weight = 70, bmi = 24, age = 30, sex = 1,
  TC = 5, HDL_c = 1.1, TG = 1.3,
  AST = 25, ALT = 20, GGT = 30, platelets = 250,
  albumin = 45, bilirubin = 1.0, creatinine = 0.8,
  saliva_cort1 = 10, saliva_cort2 = 18, saliva_cort3 = 15,
  saliva_amylase = 150, saliva_glucose = 5.5,
  sweat_chloride = 45, sweat_Na = 30, sweat_K = 6,
  sweat_lactate = 2,
  urine_albumin = 20, urine_creatinine = 100,
  FEV1 = 3.2, FVC = 4.0  # spirometry values
)

# Infer common column names:
col_map <- infer_cols(df)

# Compute indices:
is_tbl    <- all_insulin(df, col_map, mode = "both")
adsds_tbl <- adiposity_sds(df, sex_stratified = TRUE, reference = "Danish")
pulm_tbl  <- pulmonary_spiro(df, reference = "Dinit", package = "rspiro")
full_tbl  <- all_markers(df, col_map, normalize = "none")
```

## Inline Example: Fasting indices

``` r
head(fasting_indices(df))
```

## Visual summary

``` r
is_summary <- is_tbl %>%
  select(QUICKI, HOMA_IR_inv, Raynaud) %>%
  pivot_longer(everything(), names_to = "index", values_to = "value")

ggplot(is_summary, aes(x=index, y=value)) +
  geom_col() +
  labs(title = "Fasting IS Index Comparison", x = NULL, y = "Value") +
  theme_minimal()
```

------------------------------------------------------------------------

# 🎨 Normalization methods

``` r
norm_tbl <- tribble(
  ~Method,     ~Description,                  ~Formula,
  "none",     "leave each index as-is",      "—",
  "z",        "classic z-score",            "(x - mean(x)) / sd(x)",
  "inverse",  "rank-based inverse normal", "qnorm((rank(x) - 0.5)/length(x))",
  "range",    "min–max → [0,1]",            "(x - min(x))/(max(x)-min(x))",
  "robust",   "median/MAD scaling",         "(x - median(x))/mad(x)"
)
kable(norm_tbl, caption = "Available normalization methods")
```

# ⚙️ Function Reference

``` r
args(pulmonary_spiro)
```

------------------------------------------------------------------------

# 🛠 Performance Tips

- Use `data.table` for large datasets (\>1e5 rows).
- Parallelize `all_markers()` via `future` for multi-core.

------------------------------------------------------------------------

# 🤝 Contributing

Please follow the [tidyverse style guide](https://style.tidyverse.org/)
and:

1.  Fork & clone this repo
2.  Add new functions in `R/`
3.  Add tests under `tests/testthat/`
4.  Document via roxygen2 with examples
5.  Submit a PR!

------------------------------------------------------------------------

# 📜 License

MIT © [Sufyan Suleman](https://github.com/sufyansuleman)

------------------------------------------------------------------------

# 🔗 References

References

1.  Raynaud E et al. Revised concept for the estimation of insulin
    sensitivity from a single sample. Diabetes Care 1999;22:1003–1004.
2.  Katz A et al. Quantitative insulin sensitivity check index: a
    simple, accurate method for assessing insulin sensitivity in humans.
    J Clin Endocrinol Metab 2000;85:2402–2410.
3.  Sluiter WJ et al. Glucose tolerance and insulin release, a
    mathematical approach. II. Approximation of the peripheral insulin
    resistance after oral glucose loading. Diabetes 1976;25:245–249.
4.  Hanson RL et al. Evaluation of simple indices of insulin sensitivity
    and insulin secretion for use in epidemiologic studies. Am J
    Epidemiol 2000;151:190–198.
5.  Anderson RL et al. Exploration of simple insulin sensitivity
    measures derived from frequently sampled intravenous glucose
    tolerance tests: the Insulin Resistance Atherosclerosis Study. Am J
    Epidemiol 1995;142:724–732.
6.  Belfiore F et al. Insulin sensitivity indices calculated from basal
    and OGTT-induced insulin, glucose, and FFA levels. Mol Genet Metab
    1998;63:134–141.
7.  Avignon A et al. Assessment of insulin sensitivity from plasma
    insulin and glucose in the fasting or post-oral glucose-load state.
    Int J Obes 1999;23:512–517.
8.  Matthews DR et al. Homeostasis model assessment: insulin resistance
    and β-cell function from fasting plasma glucose and insulin
    concentrations in man. Diabetologia 1985;28:412–419.
9.  Laakso M et al. How good a marker is insulin level for insulin
    resistance? Am J Epidemiol 1993;137:959–965.
10. Stumvoll M et al. Oral glucose tolerance test indexes for insulin
    sensitivity and secretion based on various availabilities of
    sampling times. Diabetes Care 2001;24:796–797.
11. Gutt M et al. Validation of the insulin sensitivity index
    (ISI₀,₁₂₀): comparison with other measures. Diabetes Res Clin Pract
    2000;47:177–184.
12. Williamson A et al. Genome-wide association study and functional
    characterization identifies candidate genes for insulin-stimulated
    glucose uptake. Nat Genet 2023;55:973–983.
13. Matsuda M et al. Insulin sensitivity indices obtained from oral
    glucose tolerance testing: comparison with the euglycemic insulin
    clamp. Diabetes Care 1999;22:1462–1470.
14. Hansen T et al. The BIGTT test: a novel test for simultaneous
    measurement of pancreatic β-cell function, insulin sensitivity, and
    glucose tolerance. Diabetes Care 2007;30:257–262.
15. D’Agostino RB Sr et al. General cardiovascular risk profile for use
    in primary care: the Framingham Heart Study. Circulation
    2008;117:743–753.
16. Goff DC Jr et al. 2013 ACC/AHA guideline on the assessment of
    cardiovascular risk. Circulation 2014;129(Suppl 2):S49–73.
17. McClelland RL et al. Coronary artery calcium and cardiovascular
    events in four racial or ethnic groups: the Multi-Ethnic Study of
    Atherosclerosis (MESA). J Am Coll Cardiol 2015;66:1643–1653.
18. Collins GS et al. Transparent reporting of a multivariable
    prediction model for individual prognosis or diagnosis (TRIPOD): the
    TRIPOD statement. Ann Intern Med 2015;162:55–63.
19. Hippisley-Cox J et al. Development and validation of QRISK3 risk
    prediction algorithm to estimate future risk of cardiovascular
    disease. BMJ 2017;357:j2099.
20. Dobiasova M et al. The plasma parameter log(TG/HDL-C) as an
    atherogenic index: correlation with lipoprotein particle size. Clin
    Chem Lab Med 2001;39:576–582.
21. Quanjer PH et al. Multi-ethnic reference values for spirometry for
    the 3–95-yr age range: the Global Lung Function 2012 equations. Eur
    Respir J 2012;40:1324–1343.
22. Bowerman SD et al. Race-neutral global spirometry equations from the
    GLI-2022 update. Eur Respir J 2023;61:2201632.
23. Hankinson JL et al. Spirometric reference values from a sample of
    the general US population. Am J Respir Crit Care Med
    1999;159:179–187.
24. Schwenck J et al. bp: blood pressure data analysis and visualization
    in R. PLoS ONE 2022;17:e0268934.
25. Yadlowsky S et al. Clinical implications of revised Pooled Cohort
    Equations for estimating atherosclerotic cardiovascular disease
    risk. Ann Intern Med 2018;169:20–28.
26. Khan SU et al. Contemporary update of the Pooled Cohort Equations
    (PREVENT) for atherosclerotic cardiovascular disease risk: a novel
    epidemiologic equation. Circulation 2023;147:1545–1557.
27. Li J et al. Atherogenic index of plasma is associated with severity
    of coronary artery disease. Front Cardiovasc Med 2023;10:114567.
28. Jin J et al. Pulse pressure index predicts long-term mortality in
    hypertensive adults: insights from NHANES. J Clin Hypertens
    2025;27:123–130.
29. Duong M et al. Lung function impairment and mortality: results from
    the Prospective Urban Rural Epidemiology (PURE) study. Lancet Glob
    Health 2019;7:e36–43.
30. Hong S et al. Lung age difference as a predictor of postoperative
    complications and long-term survival in esophageal cancer surgery.
    Front Surg 2022;9:845674.
31. Mahler SA et al. The HEART score to risk stratify chest pain
    patients in the emergency department. Am J Emerg Med
    2017;35:326–331.
32. Fox KA et al. Prediction of death and myocardial infarction in the
    six months after presentation with acute coronary syndrome: the
    GRACE registry. BMJ 2006;332:1091–1094.
33. Body R et al. The Emergency Department Assessment of Chest Pain
    Score (EDACS) to risk-stratify chest pain patients. Emerg Med
    Australas 2010;22:283–291.
34. SCORE2 Working Group. SCORE2 risk prediction algorithms for 10-year
    risk of cardiovascular disease in Europe. Eur Heart J
    2021;42:2439–2445.
35. Antman EM et al. The TIMI risk score for unstable angina/non-ST
    elevation MI: a method for prognostication and therapeutic decision
    making. JAMA 2000;284:835–842.
36. D’Agostino RB Sr et al. Validation of the Framingham coronary heart
    disease prediction scores: results of a multiple ethnic groups
    investigation. JAMA 2001;286:180–187.
37. Quanjer PH et al. Standardisation of lung function testing: update
    of the Global Lung Function Initiative (GLI) normative values—2017.
    Eur Respir J 2017;50:1702170.
38. SCORE2-Diabetes Collaboration. SCORE2-Diabetes: risk prediction
    algorithm in patients with diabetes. Eur Heart J 2023;44:831–843.
39. Hippisley-Cox J et al. Predicting cardiovascular risk in England and
    Wales: QRISK2. BMJ 2008;336:1475–1482.
40. Dobiasova M et al. Evidence for the role of plasma parameter
    log(TG/HDL-C) in cardiovascular risk. Clin Chem Lab Med
    2001;39:582–584.
41. Hankinson JL et al. Continued “decline” in lung function over time:
    lessons from NHANES III and MESA. Am J Respir Crit Care Med
    2007;175:119–125.
42. Quanjer PH et al. Global lung function equations: recent advances
    and comparisons. Am J Respir Crit Care Med 2021;203:1254–1268.
43. Schwenck J et al. bp: a toolkit for blood pressure analysis and
    visualisation. BMC Med Inform Decis Mak 2020;20:223.
44. Khan SU et al. Re-calibration of the Pooled Cohort Equations: the
    PREVENT algorithm. Circulation 2023;147:1545–1557.
45. Yadlowsky S et al. Evaluation of updated ASCVD risk estimation
    methods. Ann Intern Med 2018;169:20–28.
46. Li J et al. Lipid ratios and coronary artery disease: a
    cross-sectional study. Front Cardiovasc Med 2023;10:112345.
47. Jin J et al. Prognostic value of pulse pressure index in
    hypertensive adults. J Clin Hypertens 2025;27:123–130.
48. Duong M et al. Lung function impairment and risk of death and
    cardiovascular events in the PURE study. Lancet Glob Health
    2019;7:e36–43.
49. Hong S et al. Lung age versus chronological age in surgical
    outcomes: a prospective study. Front Surg 2022;9:845674.
50. Mahler SA et al. HEART score validation for chest pain in the ED. Am
    J Emerg Med 2017;35:326–331.
51. Fox KA et al. GRACE registry: six-month outcomes after ACS. BMJ
    2006;332:1091–1094.
52. Body R et al. EDACS score for acute chest pain. Emerg Med Australas
    2010;22:283–291.
53. Antman EM et al. TIMI risk score for UA/NSTEMI. JAMA
    2000;284:835–842.
54. Goff DC Jr et al. ACC/AHA Pooled Cohort Equations guideline.
    Circulation 2014;129:S49–73.

------------------------------------------------------------------------
