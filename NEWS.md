# HealthMarkers NEWS

All notable changes to this project will be documented in this file.

## 0.1.0 (2025-07-02)

### Initial release

**HealthMarkers** is a unified R toolkit for computing an extensive suite of clinical and metabolic biomarkers from routine data. Key features include:

- **Insulin Sensitivity & Resistance Indices**  
  - Fasting‐based: HOMA-IR, QUICKI, FIRI, Raynaud, Bennett, Belfiore_basal, IG-ratio, ISI₀–₁₂₀, HOMA₂_rev, etc.  
  - OGTT-based: Matsuda (AUC & ISI), Gutt, Cederholm, Stumvoll variants, Avignon, BIGTT-SI, Belfiore_ISI, HIRI_inv, etc.  
  - Adipose-based: Revised-QUICKI, TyG, TG/HDL, VAI, LAP, McAuley, Adipo-IR, Belfiore_inv_FFA  
  - Tracer/DXA-based: LIRI_inv, ATIRI_inv, Lipo_inv  
  - Automatic inversion option for IR measures  

- **Anthropometry & MetS Severity**  
  - Standardized SDS (z-scores) for BMI, waist, body-fat%, WHR, WHtR  
  - Continuous Metabolic Syndrome Severity Score (MetSSS) by sex–race  

- **Lipid & Cardiovascular Ratios**  
  - Non-HDL, remnant cholesterol, TC/HDL, TG/HDL, LDL/HDL, ApoB/ApoA1, Atherogenic Index of Plasma (AIP)  

- **Liver Function & Fibrosis Scores**  
  - Fatty Liver Index (FLI), NAFLD Fibrosis Score (NFS), APRI, FIB-4, BARD, ALBI, MELD-XI  

- **Binary Risk Flags**  
  - Dyslipidemia, insulin resistance, hyperglycemia, hypertension  

- **Glycemic & Non-Insulin Markers**  
  - SPISE, METS-IR, prediabetes/diabetes HbA₁c flags, C-peptide HOMA  

- **Novel Biofluid Panels**  
  - **Saliva:** log-cortisol, CAR_AUC, log-amylase, salivary glucose  
  - **Sweat:** chloride, Na/K ratio, lactate, sweat rate (L/m²/h)  
  - **Urine:** UACR, microalbuminuria, eGFR (CKD-EPI), FENa, UPCR  

- **Utilities & Dispatchers**  
  - `infer_cols()`: flexible column name mapping  
  - `validate_inputs()`: required-column checks  
  - `all_insulin()`, `metabolic_markers()`, `all_markers()`: high-level wrappers  

- **Unit Conversion & Normalization**  
  - Built-in mmol↔mg, pmol↔µU conversions  
  - Five scaling methods: none, z-score, inverse-normal, min–max, median/MAD  

### Documentation & Support

- Comprehensive **README**, function examples, and normalization guide  
- Roxygen2 help files for all functions  
- Test suite via **testthat**  
- CI with GitHub Actions R-CMD-check  

---

Please report issues or suggestions at_ https://github.com/ysufyansuleman/HealthMarkers/issues  
