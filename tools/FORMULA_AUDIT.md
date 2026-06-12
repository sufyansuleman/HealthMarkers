# HealthMarkers — formula & unit audit log

Systematic review of every marker-calculation script: each formula is checked
against its original publication, and the unit conversions are traced from the
documented inputs through to the formula. Tests are reviewed alongside the code.
This file is the working record; confirmed fixes are summarised in `NEWS.md` at
release time.

**Legend:** ✅ correct · 🔧 fixed · ⚠️ flagged / needs decision · ⏳ not yet reviewed

---

## Reviewed

### `fasting_is.R` — 🔧 fixed (v0.1.4)
- `HOMA_IR_inv` 🔧 used mg/dL glucose with ÷22.5 → 18× too large. Fixed to raw mmol/L (Matthews 1985).
- `FIRI` 🔧 used mg/dL glucose with ÷25 → 18× too large. Fixed to raw mmol/L (Duncan 1995).
- `QUICKI` 🔧 used natural log → switched to log10 (Katz 2000). Units already correct.
- `HOMA_IR_rev_inv` ✅ correct standard HOMA-IR via mg/dL ÷405 (now equals `HOMA_IR_inv`).
- `Raynaud`, `Fasting_inv` ✅. `Belfiore_basal`, `Bennett`, `Ig_ratio_basal`, `Isi_basal` — proxy/approx forms, left as-is.
- Tests updated to lock corrected values. All pass.

### `ogtt_is.R` — 🔧 fixed (v0.1.4)
- `Cederholm_index` 🔧 pre-converted glucose to mg/dL then applied the formula's own ×180 (mmol→mg) factor → double conversion, non-linear distortion. Fixed to raw mmol/L (Cederholm & Wibell 1990).
- `Gutt_index` ✅ correctly uses mg/dL (no ×180 term).
- `Matsuda_ISI` ✅ (mg/dL + µU/mL), `Modified_stumvoll` / `Stumvoll_Demographics` ✅ (raw pmol/L + mmol/L), `BigttSi` ✅ (raw units, male=1 coding).
- `Avignon_Si0/Si120` ⚠️ structure plausible but original units (mmol/L vs mg/dL) not yet verified against Avignon 1999.
- `Matsuda_AUC`, `Ifc_inv`, `HIRI_inv`, `Belfiore_isi_gly`, `Isi_120` — documented non-standard composites.
- Test added to lock Cederholm value. All pass.

### `adipo_is.R` — ✅ correct
- `Revised_QUICKI` ✅ (log10, FFA mmol/L, Perseghin), `VAI` ✅ (mmol/L constants, Amato 2010), `LAP` ✅ (mmol/L, Kahn 2005), `TyG` ✅ (mg/dL ln, Guerrero-Romero), `McAuley` ✅ (µU/mL + mmol/L TG).

### `tracer_dxa_is.R` — ⚠️ documented caveats
- LIRI coefficients attributed to Gastaldelli but primary source unverified (already flagged in docstring). Conversions fine. Tracer SI are simple ratios (Steele equation not implemented — documented).

---

### `glycemic_markers.R` — 🔧 fixed
- `SPISE` 🔧 (Paulmichl 2016) defined for **mg/dL** HDL-c & TG; code used raw mmol/L. Fixed: HDL×38.67, TG×88.57. (Validated cutoff ~6.6 only valid in mg/dL.)
- `METS_IR` 🔧 (Bello-Chavolla 2018) defined for **mg/dL** glucose/TG/HDL; code used raw mmol/L. Fixed: glucose×18, TG×88.57, HDL×38.67.
- `TyG_index` ✅ already converts (mg/dL). `LAR`, `ASI` ✅ ratios. prediabetes/diabetes HbA1c cut-offs (42/48 mmol/mol) ✅ WHO/IDF.
- `HOMA_CP` ⚠️ non-standard C-peptide HOMA (operational), already documented.
- Tests updated for SPISE/METS_IR mg/dL values; denom-zero case retriggered via HDL_mgdl=1.

## Supporting/utility scripts — checked for unit-conversion code
- `.hm_global_precompute` (utils_helpers.R) — derives missing vars (BMI, glucose/G0 & insulin/I0 aliases, eGFR, UACR, LDL_c Friedewald, VLDL, WHR, MAP, non_HDL, remnant_c). All kept in canonical units; **no glucose/TG/HDL→mg/dL conversion**. ✅ Confirms per-formula conversions don't double-convert.
- `utils_infer-cols.R` — name-only column inference; **never converts values**. Canonical units are caller's responsibility. ✅
- ⚠️ **OPEN (renal batch):** `.hm_global_precompute` eGFR uses CKD-EPI 2009 (κ=0.9/0.7, α=−0.411/−0.329) which requires creatinine in **mg/dL**. Verify the package's canonical creatinine unit; if µmol/L, this is a ~88× error.

### `lipid_markers.R` — ✅ correct
- VAI ✅ (mmol/L, Amato 2010), LAP ✅ (mmol/L, Kahn 2005), TyG_BMI ✅ (converts TG×88.57, glucose×18), non_HDL_c/remnant_c ✅ (mmol/L), ratios ✅. No change.

### `atherogenic_indices.R` — 🔧 docs fixed (formula OK)
- AIP = log10(TG/HDL) ✅ correct for **mmol/L** (Dobiasova 2004), but AIP is NOT scale-invariant (TG×88.57 vs HDL×38.67 → mg/dL shifts AIP by +0.36). Docstring wrongly said "all indices unitless" and example used mg/dL values → fixed docs/example to mmol/L; corrected unit note; removed stale `check_extreme`/`extreme_action` mention. No formula change.
- CRI_I (TC/HDL), CRI_II (LDL/HDL) ✅ scale-invariant. Tests unit-agnostic, unchanged.

### `liver_markers.R` — ✅ correct (conventional US units)
- FLI (Bedogni 2006), NFS (−0.66 g/dL→−0.066 g/L correctly adjusted, Angulo 2007), APRI (ULN AST=40, Wai 2003), FIB-4 (Sterling 2006), BARD (AST/ALT≥0.8 = 2pts, Harrison 2008), ALBI (bili×17.1→µmol/L, Johnson 2015), MELD-XI (Heuman 2007) — all ✅.
- ⚠️ NOTE: uses **mg/dL** for TG/bilirubin/creatinine and g/L albumin (documented), unlike the mmol/L metabolic functions. Self-consistent & documented; cross-function unit mismatch is a caller footgun, not a formula bug.

### `liver_fat_markers.R` — 🔧 fixed
- `NAFLD_LFS` 🔧 Kotronen (2009) codes **type-2 diabetes as yes=2/no=0** (contribution 0.45×2=0.90); code used `0.45*dm2(0/1)` → half value for diabetics. Fixed to `0.45*(2*dm2)`. Docstring + test updated.
- `HSI` ✅ (Lee 2010), MetS derivation ✅ (documented simplified NCEP-ATP III, mmol/L), insulin units ✅ (mU/L; I0/6 fallback).

### `renal_markers.R` — 🔧 fixed
- `eGFR_cr` (CKD-EPI 2009) ✅ correct for **mg/dL** creatinine (documented) — this RESOLVES the global-precompute eGFR flag (both use mg/dL constants consistently; canonical creatinine unit = mg/dL).
- `eGFR_cys` (Inker 2012 cystatin) ✅. `BUN_Cr_ratio`, `FE_Urea` ✅.
- `eGFR_combined` 🔧 THREE wrong constants vs Inker 2012 (verified NKF/NEJM): reused creatinine-only α (−0.411/−0.329) → fixed to combined α (−0.207 M / −0.248 F); female multiplier 1.008 → **0.969**; Black multiplier 1.145 → **1.08**. Docstring updated. (Test only checks column names.)

### `kidney_kfre.R` — 🔧 MAJOR fix (full rewrite)
- `kidney_failure_risk()` linear predictor was wrong vs Tangri 2011 4-var KFRE (verified official sources). Old: `0.220*log(age) − 0.556*log(eGFR) + 0.451*log(UACR) + 0.391*male`, S0 0.934/0.881 — log-transformed age/eGFR (should be /10, /5 linear), flipped age sign, no centering, wrong male coef, baseline survivals matching no calibration.
- Rewrote to: `LP = −0.2201*(age/10 − 7.036) + 0.2467*(male − 0.5642) − 0.5567*(eGFR/5 − 7.222) + 0.4510*(ln(ACR_mmol) − 5.137)`, S0 **0.9832 (2y) / 0.9365 (5y)** (non-NA calibration, per user). ACR input kept as mg/g, converted internally to mg/mmol (/8.84). Docstring updated; regression test added. **User decisions: non-NA calibration + mg/g input.**

### `ckd_stage.R` — 🔧 fixed
- G-stages (G1–G5) ✅ and A-stages (A1–A3, mg/g) ✅ per KDIGO 2012.
- `KDIGO_risk` heatmap 🔧 two cells wrong: G1/G2+A3 returned "Moderate" → **High**; G3a+A3 returned "High" → **Very High**. Rewrote map to match KDIGO 2012 exactly. Tests pass.

### `obesity_metrics.R` — 🔧 fixed
- `BRI` 🔧 Thomas (2013) eccentricity ratio is `(WC/(2π))/(0.5·height) = WC/(π·height)`; code used `WC/(2π·height)` (half) → near-zero/negative BRI for normal adults. Fixed denominator to `π·height_m`. Value test added.
- BMI/cat (WHO), WHR, WHtR, AVI (Guerrero-Romero 1999), BAI (Bergman 2011), ABSI (Krakauer 2012), CI (Valdez 1991), RFM (Woolcott 2018) all ✅.

### `alm_bmi_index.R` — ✅ correct
- ALM/BMI ratio; FNIH cut-points Men <0.789 / Women <0.512 (McLean 2014) ✅.

### `calc_sds.R`, `adiposity_sds.R`, `adiposity_sds_strat.R` — ✅ correct
- Pure SDS/z-score engines `(x − mean)/sd` from user-supplied reference stats; no embedded formula/units. ✅

### `metss.R` — ✅ correct (structure)
- MetSSS (Gurka 2014) = `intercept + Σ coef·z(component)`, MAP=(2·DBP+SBP)/3 ✅. Coefficients dimensionless so mmol/L means/sds are self-consistent. Embedded race/sex constants not independently re-verified vs Gurka supplement (plausible, documented).

### `cvd_risk.R` — 🔧 doc note (formulas in external pkgs)
- ASCVD/Stroke (PooledCohort PCE), QRISK3, RiskScorescvd are **wrappers** — math done by validated backends ✅. Cholesterol passed as mg/dL (backend requirement; cross-package convention differs from mmol/L metabolic fns — documented).
- `cvd_marker_aip` 🔧 added unit caveat (defined mmol/L by Dobiasova; mg/dL shifts +0.36) — mirrors atherogenic_indices. `cvd_marker_ldl_particle_number` = ApoB passthrough ✅.

### `metabolic_risk_features.R` — ✅ correct
- Heuristic flags with documented mmol/L lipid cutoffs (NHLBI 2011 pediatric), ADA prediabetes ranges (glucose 5.6–6.9 mmol/L, HbA1c 39–47 mmol/mol, exclusive bounds documented), BP/HOMA z-scores. Explicitly non-diagnostic. ✅

### `bone_markers.R` — ✅ correct
- OSTA = (weight − age)×0.2 (Woo 2002), ALMI = ALM/height², FMI = FM/height², BMD T-score = (BMD − ref_mean)/ref_sd (WHO). Optional turnover markers passthrough. ✅

### `frax_score.R` — ✅ (documented placeholder)
- Explicitly a non-validated educational placeholder ("does not implement the proprietary FRAX algorithm"); arbitrary demo constants, no published formula to verify. Clearly labelled not-for-clinical-use. No change.

### `inflammatory_markers.R` — ✅ correct
- All ratio indices unit-invariant: NLR, PLR, LMR, dNLR, SII (Hu 2014), SIRI (Qi 2016), AISI, PIV, NER, CLR, CAR, PCR. CRP_category (Pearson 2003 AHA/CDC, mg/L). mGPS (Proctor 2011: CRP>10 + albumin ≥/<35) ✅.

### `inflammatory_age.R` (iAge) — ✅ (documented proxy)
- Explicitly a simplified weighted-sum proxy (user weights), documented as NOT method-identical to Sayed 2021. No published formula to verify. No change.

### `nfl_marker.R` — ✅ (documented passthrough)
- Returns NfL value as-is; no formula. No change.

### `vitamin_d_status.R` — ✅ correct
- <20 Deficient / 20–29 Insufficient / ≥30 Sufficient ng/mL (Endocrine Society/Holick 2011); nmol/L→ng/mL ÷2.5 note correct.

### `vitamin_markers.R` — ✅ correct
- z-scores (user ref mean/sd), simple ratios (B12/Folate, Ferritin/TSat, Cort/DHEAS, T/E2, TSH/fT4, Toco/lipids, Mg/Zn, Cu/Zn), passthroughs. No embedded constants.

### `nutrient_markers.R` — ✅ correct
- AGR = alb/(TP−alb), Omega3Index = EPA+DHA, GlycatedAlbuminPct, Ca×Phosphate, AnionGap = (Na+K)−(Cl+HCO3), Tyr/Phe — all standard. Unit-mixing (BUN/Cr, Mg/Cr in mixed units) honestly self-documented in docstring.

### `hormone_markers.R` — ✅ correct
- FAI=(TT/SHBG)×100 (Sowers 2009), ARR=aldo/renin (Funder 2016), CAR_slope=(cort30−cort0)/30 (Clow 2004), plus simple ratios (LH/FSH, E2/P, E2/T, fT3/fT4, TSH/fT4, Ins/Glu, GH/IGF1, PRL/T). Inference heuristics (fT3, GH) flagged exploratory. No change.

### `urine_markers.R` — ✅ correct
- UACR = albumin(mg/L)×100/creatinine(mg/dL) → mg/g (correct unit conversion, documented). KDIGO A1<30/A2 30–300/A3>300, microalbuminuria 30–300. UPCR & tubular markers creatinine-normalized (per g Cr). No change.

### `bode_index.R` — ✅ correct
- FEV1% (≥65/≥50/≥36/<36), 6MWD (≥350/≥250/≥150/<150), mMRC (≤1/2/3/4), BMI (>21/≤21) cut-points all match Celli 2004.

### `spirometry_markers.R` — ✅ correct
- FEV1/FVC, fixed-ratio 0.70 COPD flag, GOLD grades (≥80/≥50/≥30/<30), BDR %change. GLI predicted/LLN/z via rspiro when available; non-clinical fallback explicitly labelled.

### `pulmo_markers.R` — ✅ correct
- Delegates predicted/z/LLN to validated rspiro (GLI/GLIgl/NHANES3); %predicted = 100×obs/pred; sex/ethnicity code mapping + cm→m height detection correct.

### `charlson_index.R` — ✅ correct
- All comorbidity weights (1/2/3/6) and max()-based de-dup for diabetes/liver/cancer match Charlson 1987.

### `sarc_f_score.R` — ✅ correct
- 5 items ×0–2, total 0–10, high-risk ≥4 (Malmstrom 2013).

### `frailty_index.R` — ✅ (wrapper)
- Thin wrapper around validated `di::di()` (Mitnitski/Rockwood deficit accumulation).

### `allostatic_load.R` — ✅ correct
- Count of biomarkers over **user-supplied** thresholds (Seeman 1997); no embedded constants.

### `psych_scores.R` — ✅ correct (all instruments)
- Cutoffs verified: PHQ-9 (5/10/15/20, Kroenke 2001), GAD-7 (5/10/15, Spitzer 2006), K6 (≥13), K10, GHQ-12 (likert/binary), WHO-5 (raw×4, <50, Topp 2015), ISI (8/15/22, Bastien 2001), MDQ (≥7+cluster+impairment), ASRS (Part A 3/3/3/4/4/4, ≥4 — WHO v1.1), BIS/SPQ key-driven, cognitive z-mean/PCA1, dx/med flags. All correct.

### Other fluids & misc — ✅ all correct
- `corrected_calcium.R` ✅ Payne 1973 (Ca + 0.8·(4−alb g/dL)); SI handling algebraically equals Ca + 0.02·(40−alb g/L).
- `kyn_trp_ratio.R` ✅ Kyn(nmol/L)/Trp(µmol/L) = conventional KTR×1000 scale (documented units).
- `oxidative_markers.R` ✅ GSH/GSSG redox ratio (same-unit, documented).
- `sweat_markers.R` ✅ Na/K ratio, mass-loss sweat rate (Δwt/duration/BSA, 1kg≈1L); passthroughs.
- `saliva_markers.R` ✅ ln(cortisol/amylase), CAR_AUC trapezoidal AUCg (Pruessner 2003), glucose passthrough.

---

## AUDIT COMPLETE — summary of fixes
**Bugs fixed (code):**
1. fasting_is: HOMA_IR_inv, FIRI (mmol/L), QUICKI (log10)
2. ogtt_is: Cederholm (mmol/L)
3. glycemic_markers: SPISE, METS_IR (→ mg/dL)
4. liver_fat_markers: NAFLD-LFS (T2DM coded 2)
5. renal_markers: eGFR_combined (Inker 2012 α + multipliers)
6. ckd_stage: KDIGO heatmap (2 cells)
7. kidney_kfre: full KFRE rewrite (Tangri 2011 non-NA)
8. obesity_metrics: BRI (π not 2π)

**Doc-only clarifications:** atherogenic_indices (AIP mmol/L), cvd_marker_aip (AIP unit note).
**Verified correct, no change:** ~30 other scripts (see entries above). Supporting/util scripts confirmed to do no hidden unit conversion.
