<!-- badges: start -->

[![CRAN
status](https://www.r-pkg.org/badges/version/HealthMarkers)](https://CRAN.R-project.org/package=HealthMarkers)
[![R-CMD-check](https://github.com/sufyansuleman/HealthMarkers/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/sufyansuleman/HealthMarkers/actions/workflows/R-CMD-check.yaml)
[![License:
MIT](https://img.shields.io/badge/License-MIT-yellow.svg)](https://opensource.org/licenses/MIT)
<!-- badges: end -->

# HealthMarkers

**HealthMarkers** is a comprehensive R toolkit for computing,
standardising, and summarising clinical and research biomarkers from
routine laboratory and phenotypic data. It provides over 50 specialist
functions covering over 290 biomarkers, including insulin sensitivity
indices, cardiovascular risk scores, inflammatory aging clocks, frailty
indices, psychiatric rating scales, alternate-biofluid panels, and much
more. All accessible through a unified dispatcher,
`all_health_markers()`.

> **Full documentation, function reference, and vignettes** are
> available at the package website:  
> <https://sufyansuleman.github.io/HealthMarkers/>

- **Covers a wide range of biomarkers.** A single `all_health_markers()`
  call returns glycaemic, lipid, liver, renal, pulmonary, inflammatory,
  hormonal, bone, psychiatric, and nutritional markers as one wide
  tibble.
- **Variable naming flexibility.** The built-in synonym dictionary
  covers naming conventions from 15+ cohorts and biobanks: UK Biobank,
  NHANES, HUNT, Tromsø, FinnGen, Estonian Biobank, LifeLines
  (Netherlands), Generation Scotland, All of Us (LOINC codes), Danish
  registers (NPU codes), and more. Column names from any of these
  systems are recognised automatically without any manual mapping.
- **Safe defaults.** NA handling, input validation, column-name
  inference, and range-capping are built in. Failed marker groups are
  skipped with a warning, never crashing your pipeline.
- **Reproducibility.** Explicit `col_map` arguments map *your* column
  names to expected internal keys, no silent renaming.
- **References.** Every function cites the primary paper, derived
  methods, and validated applications or reviews of original work with
  modifications. Full bibliography in `inst/REFERENCES.bib`. 46
  vignettes with worked clinical examples are included.

------------------------------------------------------------------------

## Installation

    # From CRAN
    install.packages("HealthMarkers")

    # Development version from GitHub
    remotes::install_github("sufyansuleman/HealthMarkers")

Optional packages unlock additional marker groups:

    install.packages(c("CVrisk", "rspiro", "PooledCohort", "QRISK3",
                       "RiskScorescvd", "di", "mice", "missForest"))

When optional packages are absent, their dependent groups are skipped
safely; running with `verbose = TRUE` shows which groups were computed
and which were skipped (and why) in the summary message.

------------------------------------------------------------------------

## Package overview

<table>
<colgroup>
<col style="width: 33%" />
<col style="width: 33%" />
<col style="width: 33%" />
</colgroup>
<thead>
<tr>
<th>Domain</th>
<th>Functions</th>
<th>Outputs</th>
</tr>
</thead>
<tbody>
<tr>
<td><strong>Insulin sensitivity</strong></td>
<td><code>fasting_is()</code>, <code>ogtt_is()</code>,
<code>adipo_is()</code>, <code>tracer_dxa_is()</code>,
<code>all_insulin_indices()</code></td>
<td>HOMA-IR, QUICKI, Matsuda, Stumvoll, Gutt, SPISE, LIRI, 40+
indices</td>
</tr>
<tr>
<td><strong>Glycaemic</strong></td>
<td><code>glycemic_markers()</code></td>
<td>TyG index, METS-IR, LAR, ASI, HOMA-CP, diabetes risk flags</td>
</tr>
<tr>
<td><strong>Lipid &amp; atherogenic</strong></td>
<td><code>lipid_markers()</code>, <code>atherogenic_indices()</code>,
<code>cvd_marker_aip()</code>,
<code>cvd_marker_ldl_particle_number()</code></td>
<td>TC/HDL, AIP, CRI-I/II, Castelli, LDL particle number</td>
</tr>
<tr>
<td><strong>Liver</strong></td>
<td><code>liver_markers()</code>, <code>liver_fat_markers()</code></td>
<td>FLI, NFS, FIB-4, APRI, BARD, ALBI, MELD-XI, HSI, LAP</td>
</tr>
<tr>
<td><strong>Metabolic syndrome</strong></td>
<td><code>metss()</code>, <code>metabolic_risk_features()</code>,
<code>allostatic_load()</code></td>
<td>MetS severity, component flags, allostatic load index</td>
</tr>
<tr>
<td><strong>Cardiovascular risk</strong></td>
<td><code>cvd_risk()</code>, <code>cvd_risk_ascvd()</code>,
<code>cvd_risk_qrisk3()</code>, <code>cvd_risk_scorescvd()</code>,
<code>cvd_risk_stroke()</code></td>
<td>ASCVD (PCE), QRISK3, SCORE2/SCORE2-OP, 10-yr stroke risk</td>
</tr>
<tr>
<td><strong>Renal / CKD</strong></td>
<td><code>kidney_failure_risk()</code>, <code>renal_markers()</code>,
<code>ckd_stage()</code>, <code>urine_markers()</code></td>
<td>KFRE 2-yr/5-yr, eGFR (CKD-EPI), CKD stage, UACR, FE-Urea</td>
</tr>
<tr>
<td><strong>Pulmonary</strong></td>
<td><code>pulmo_markers()</code>, <code>spirometry_markers()</code>,
<code>bode_index()</code></td>
<td>FEV1/FVC z-scores, GLI 2012 % predicted, BODE index</td>
</tr>
<tr>
<td><strong>Inflammatory</strong></td>
<td><code>inflammatory_markers()</code>, <code>iAge()</code></td>
<td>NLR, PLR, SII, LMR, iAge inflammatory clock</td>
</tr>
<tr>
<td><strong>Hormonal</strong></td>
<td><code>hormone_markers()</code></td>
<td>T/E2 ratio, TSH/fT4, cortisol/DHEA, LH/FSH, HOMA-B, FAI</td>
</tr>
<tr>
<td><strong>Body composition</strong></td>
<td><code>obesity_indices()</code>, <code>adiposity_sds()</code>,
<code>adiposity_sds_strat()</code>, <code>alm_bmi_index()</code></td>
<td>BMI, WHR, ABSI, BRI, BAI, sex/age-stratified SDS, ALM/BMI</td>
</tr>
<tr>
<td><strong>Bone</strong></td>
<td><code>bone_markers()</code>, <code>frax_score()</code></td>
<td>P1NP, osteocalcin, CTX, NTX, FRAX 10-yr fracture probability</td>
</tr>
<tr>
<td><strong>Frailty / comorbidity</strong></td>
<td><code>frailty_index()</code>, <code>charlson_index()</code>,
<code>sarc_f_score()</code></td>
<td>Rockwood deficit index, Charlson CCI, SARC-F</td>
</tr>
<tr>
<td><strong>Vitamins &amp; nutrients</strong></td>
<td><code>vitamin_markers()</code>, <code>vitamin_d_status()</code>,
<code>nutrient_markers()</code></td>
<td>Vitamin D status category, B12/folate ratio, ferritin
saturation</td>
</tr>
<tr>
<td><strong>Alternate biofluids</strong></td>
<td><code>saliva_markers()</code>, <code>sweat_markers()</code>,
<code>urine_markers()</code></td>
<td>Cortisol awakening response, sweat chloride, urinary ratios</td>
</tr>
<tr>
<td><strong>Neurological</strong></td>
<td><code>nfl_marker()</code>, <code>kyn_trp_ratio()</code>,
<code>corrected_calcium()</code></td>
<td>Age-adjusted NfL, kynurenine/tryptophan ratio, corrected
calcium</td>
</tr>
<tr>
<td><strong>Psychiatric</strong></td>
<td><code>psych_markers()</code></td>
<td>PHQ-9, GAD-7, ISI, GHQ-12, K10, K6, WHO-5, ASRS, BIS-11, SPQ</td>
</tr>
<tr>
<td><strong>Anthropometric SDS</strong></td>
<td><code>calc_sds()</code></td>
<td>Generic SDS z-score from any reference mean and SD</td>
</tr>
</tbody>
</table>

------------------------------------------------------------------------

## How to use HealthMarkers

### all\_health\_markers()\`: the dispatcher

**Use this when** you want to compute many marker groups in one call and
receive everything back as a single wide tibble.

    library(HealthMarkers)

    labs <- data.frame(
      age  = 52,  sex  = "M",
      G0   = 5.8, I0   = 14.2,
      TC   = 5.6, LDL_c = 3.4, HDL_c = 1.1, TG = 2.1,
      ALT  = 38,  AST  = 30,   BMI   = 30.1,
      SBP  = 138, DBP  = 88,   WC    = 98,
      eGFR = 74,  UACR = 18
    )

    results <- all_health_markers(
      data    = labs,
      which   = c("glycemic", "lipid", "liver", "renal", "kidney_kfre",
                  "mets", "obesity_metrics"),
      verbose = TRUE
    )

    # results is the original data + all computed columns as one wide tibble
    names(results)

The `which` argument accepts any of the following group keys:

    insulin_fasting     insulin_ogtt        insulin_adipose     insulin_tracer_dxa
    glycemic            lipid               atherogenic         cvd_aip
    cvd_risk            cvd_ldl_particles   cvd_ascvd           cvd_qrisk3
    cvd_scorescvd       cvd_stroke          liver               liver_fat
    mets
    metabolic_risk      pulmo               spirometry          bode
    saliva              sweat               urine               renal
    kidney_kfre         ckd_stage           nutrient            vitamin
    vitamin_d_status    hormone             inflammatory        iAge
    bone                frax                allostatic_load     oxidative
    frailty_index       charlson            sarc_f              psych
    nfl                 inflammatory_age    calcium_corrected   kyn_trp
    adiposity_sds       adiposity_sds_strat obesity_metrics     alm_bmi

Pass `which = "all"` to run every group (groups requiring unavailable
optional packages will be silently skipped).

### Individual functions:

**Use this when** you need fine-grained control, are working with
specialist data (e.g. OGTT time-series, DXA outputs, spirometry), or
want to inspect one marker family in detail.

------------------------------------------------------------------------

## Selected Functions

### Insulin sensitivity

**When to use:** fasting glucose + insulin data are available; OGTT data
with multiple time points; DXA body-composition data; tracer clamp data.

    # Fasting indices (HOMA-IR, QUICKI, Bennett, FIRI, ...)
    # Needs: G0 (fasting glucose mmol/L), I0 (fasting insulin mU/L)
    fasting_is(data, col_map = list(G0 = "glucose", I0 = "insulin"))

    # OGTT indices (Matsuda, Stumvoll, Gutt, Avignon, ...)
    # Needs: G0/G30/G60/G120 and I0/I30/I60/I120 (mmol/L and mU/L)
    ogtt_is(data, col_map = list(G0="G0", G30="G30", G60="G60", G120="G120",
                                  I0="I0", I30="I30", I60="I60", I120="I120"))

    # Adipose-tissue indices (LIRI, SPISE, VAI, LAP, ...)
    # Needs: BMI, WC, TG, HDL_c
    adipo_is(data, col_map = list(BMI="BMI", WC="WC", TG="TG", HDL_c="HDL_c"))

    # DXA / tracer-based indices
    # Needs: fat mass, lean mass, Ra (palmitate/glycerol rates)
    tracer_dxa_is(data, col_map = list(fat_mass="FM_kg", lean_mass="LM_kg"))

    # All insulin indices at once (fasting + OGTT + adipose + DXA)
    all_insulin_indices(data, col_map = list(...), normalize = "none",
                        mode = "both",  # "IS" = sensitivity only, "IR" = resistance only
                        na_action = "keep")

### Cardiovascular risk

**When to use:** primary prevention cohorts; assessing 10-year MACE
risk; comparing risk algorithms side-by-side.

    # ASCVD Pooled Cohort Equations (10-yr or 30-yr)
    # Needs: age, sex, race, total_chol, HDL_c, sbp, bp_treated, smoker, diabetes, bmi
    # Requires: PooledCohort package
    cvd_risk_ascvd(data, year = 10)

    # QRISK3 (UK population)
    # Requires: QRISK3 package
    cvd_risk_qrisk3(data)

    # SCORE2 / SCORE2-OP (European)
    # Requires: RiskScorescvd package
    cvd_risk_scorescvd(data)

    # 10-year stroke risk (Pooled Cohort)
    # Requires: PooledCohort package
    cvd_risk_stroke(data)

    # Atherogenic index of plasma (log TG/HDL)
    cvd_marker_aip(data, col_map = list(TG = "TG", HDL_c = "HDL_c"))

    # LDL particle number from ApoB
    cvd_marker_ldl_particle_number(data, col_map = list(ApoB = "ApoB"))

    # Run all CVD algorithms at once and pick one model
    cvd_risk(data, model = "ALL")   # or "ASCVD", "QRISK3", "Stroke", etc.

### Renal function

**When to use:** nephrology studies; CKD cohorts; monitoring
progression.

    # Kidney Failure Risk Equation (KFRE) 2-year and 5-year probability
    # Needs: age, sex, eGFR (CKD-EPI, mL/min/1.73m²), UACR (mg/g)
    kidney_failure_risk(data, col_map = list(age="age", sex="sex",
                                              eGFR="eGFR", UACR="UACR"))

    # eGFR, creatinine ratios, BUN/creatinine, FE-Urea, etc.
    renal_markers(data, col_map = list(creatinine="Creat", age="age", sex="sex"))

    # KDIGO CKD staging (G1–G5 × A1–A3)
    ckd_stage(data, col_map = list(eGFR="eGFR", UACR="UACR"))

    # Urine panel: protein/creatinine ratio, microalbumin, osmolality
    urine_markers(data, col_map = list(urine_creat="UCr", urine_protein="UPr"))

### Pulmonary function

**When to use:** respiratory epidemiology; COPD staging; lung-function
studies.

    # Individual spirometry z-scores and % predicted (GLI 2012)
    # Needs: FEV1, FVC; optionally age, height, sex, ethnicity
    # Requires: rspiro package
    spirometry_markers(data, col_map = list(fev1="FEV1", fvc="FVC",
                                             age="age", height="ht_cm", sex="sex"))

    # Simpler pulmonary ratios (FEV1/FVC, FEF25-75, etc.) no extra packages needed
    pulmo_markers(data)

    # BODE index for COPD prognosis
    # Needs: FEV1% predicted, 6-minute walk distance, mMRC dyspnoea score, BMI
    bode_index(data, col_map = list(fev1_pct="FEV1pct", sixmwd="Walk6m",
                                      mmrc="mMRC", bmi="BMI"))

### Psychiatric scores

**When to use:** mental health research; epidemiological surveys with
standardised questionnaires.

    # Score one or many scales from item columns
    # Supported: PHQ-9, GAD-7, K6, K10, GHQ-12, WHO-5, ISI, MDQ,
    #            ASRS, BIS-11, SPQ, cognitive composite
    #
    # col_map is a nested list keyed by instrument name.
    # Internal item keys use zero-padded names (phq9_01 ... phq9_09).
    psych_markers(
      data,
      col_map = list(
        phq9 = list(items = list(phq9_01 = "Q1", phq9_02 = "Q2", ...)),
        gad7 = list(items = list(gad7_01 = "G1", gad7_02 = "G2", ...))
      ),
      which   = c("phq9", "gad7", "k10")  # choose scales to score
    )

    # If your columns are already named phq9_01 ... phq9_09 etc., no col_map needed:
    phq9_score(data)
    gad7_score(data)
    k10_score(data)

### Body composition, anthropometric and SDS

**When to use:** paediatric cohorts (SDS); obesity epidemiology;
sarcopenia assessment.

    # Common obesity and adiposity indices
    # Needs: height, weight, WC, hip circumference; optionally age, sex
    obesity_indices(data)

    # SDS z-scores from user-supplied reference mean and SD
    calc_sds(x = data$BMI, mean_ref = 22.5, sd_ref = 3.8)

    # Sex-stratified SDS for multiple adiposity variables simultaneously
    adiposity_sds_strat(data, col_map = list(sex = "sex"),
                        var_cols = c("BMI","WC","WHR"),
                        ref_male = list(BMI = c(mean=25, sd=4)),
                        ref_female = list(BMI = c(mean=24, sd=3.8)))

    # Appendicular lean mass / BMI index (sarcopenia screening)
    alm_bmi_index(data, col_map = list(alm="ALM_kg", bmi="BMI", sex="Sex"))

### Inflammatory and aging markers

**When to use:** immunology studies; biological age estimation; chronic
disease research.

    # Blood count-derived inflammatory ratios
    # Needs: neutrophils, lymphocytes, monocytes, platelets
    inflammatory_markers(data, col_map = list(neut="NEUT", lymph="LYMPH",
                                               mono="MONO", plt="PLT"))

    # iAge inflammatory aging clock
    # Needs: a panel of inflammatory proteins (IL-6, CXCL9, etc.)
    iAge(data, col_map = list(IL6="IL6", CXCL9="CXCL9"))

### Alternate biofluids

**When to use:** stress research (saliva); cystic fibrosis / sweat
testing; nephrology urine panels.

    saliva_markers(data, col_map = list(cortisol_wake="C_wake",
                                         cortisol_30="C_30min"))
    sweat_markers(data,  col_map = list(sweat_chloride="Cl_mmol"))
    urine_markers(data,  col_map = list(urine_creat="UCr", urine_na="UNa"))

------------------------------------------------------------------------

## Column mapping and multi-biobank support

Every function accepts a `col_map` argument: a named list mapping
internal keys (what the function expects) to your actual column names
(what you have).

    # Internal key = "G0", your column is called "fasting_glucose_mmol"
    fasting_is(
      data    = my_data,
      col_map = list(G0 = "fasting_glucose_mmol", I0 = "insulin_uU_mL")
    )

### Multi-biobank automatic variable name OR column name recognition

The synonym dictionary recognises column names from 15+ major cohorts
and biobanks out of the box. The table below shows how the same analyte
is named across systems:

<table>
<colgroup>
<col style="width: 12%" />
<col style="width: 12%" />
<col style="width: 12%" />
<col style="width: 12%" />
<col style="width: 12%" />
<col style="width: 12%" />
<col style="width: 12%" />
<col style="width: 12%" />
</colgroup>
<thead>
<tr>
<th>Internal key</th>
<th>UK Biobank</th>
<th>NHANES</th>
<th>HUNT/Tromsø</th>
<th>FinnGen</th>
<th>Estonian BB</th>
<th>LifeLines (NL)</th>
<th>LOINC</th>
</tr>
</thead>
<tbody>
<tr>
<td><code>fasting_glucose</code></td>
<td><code>glucose_0_0</code></td>
<td><code>LBXGLU</code></td>
<td><code>fastende_blodsukker</code></td>
<td><code>paastoglukoosi</code></td>
<td><code>p_glukoos</code></td>
<td><code>nuchtere_glucose</code></td>
<td><code>LOINC_2345_7</code></td>
</tr>
<tr>
<td><code>total_cholesterol</code></td>
<td><code>cholesterol_0_0</code></td>
<td><code>LBXSCH</code></td>
<td><code>total_kolesterol</code></td>
<td><code>kokonaiskolesteroli</code></td>
<td><code>kogukolesterool</code></td>
<td><code>totaal_cholesterol</code></td>
<td><code>LOINC_2093_3</code></td>
</tr>
<tr>
<td><code>creatinine</code></td>
<td><code>creatinine_0_0</code></td>
<td><code>LBXSCR</code></td>
<td><code>kreatinin</code></td>
<td><code>kreatiniini</code></td>
<td><code>kreatiniin</code></td>
<td><code>creatinine</code></td>
<td><code>LOINC_2160_0</code></td>
</tr>
<tr>
<td><code>HbA1c</code></td>
<td><code>glycated_haemoglobin_hba1c_0_0</code></td>
<td><code>LBXGH</code></td>
<td><code>HbA1c</code></td>
<td><code>hemoglobiini_a1c</code></td>
<td><code>HbA1c</code></td>
<td><code>geglycosyleerd_hemoglobine</code></td>
<td><code>LOINC_4548_4</code></td>
</tr>
<tr>
<td><code>SBP</code></td>
<td><code>systolic_blood_pressure_0_0</code></td>
<td><code>BPXSY1</code></td>
<td><code>systolisk_blodtrykk</code></td>
<td><code>SBP</code></td>
<td><code>sbp</code></td>
<td><code>systolische_bloeddruk</code></td>
<td><code>LOINC_8480_6</code></td>
</tr>
<tr>
<td><code>vitaminD</code></td>
<td><code>vitamin_d_0_0</code></td>
<td><code>LBXVD2</code></td>
<td><code>d_vitamin</code></td>
<td><code>D_vitamiini</code></td>
<td><code>D_vitamiin</code></td>
<td><code>vitamine_D</code></td>
<td><code>LOINC_62292_8</code></td>
</tr>
<tr>
<td><code>ALT</code></td>
<td><code>alanine_aminotransferase_0_0</code></td>
<td><code>LBXSATSI</code></td>
<td><code>ALAT</code></td>
<td><code>alaniiniaminotransferaasi</code></td>
<td><code>ALAT</code></td>
<td><code>alanineaminotransferase</code></td>
<td><code>LOINC_1742_6</code></td>
</tr>
</tbody>
</table>

For **OMOP CDM / All of Us** data, concept codes in `LOINC_XXXX_X`
format are recognised for all major analytes. For **Nordic EHR /
register data**, Danish and Norwegian NPU codes (`NPU01994`, `NPU01567`,
etc.) are matched directly.

Generation Scotland-specific names (`SBP_mean`, `DBP_mean`,
`genetic_sex`, `ethnic_group`) and HUNT/Tromsø Norwegian-language terms
are also included.

### Recommended workflow for real datasets

**Call `hm_col_report()` first** to see which columns are auto-detected
and which need a manual mapping:

    library(HealthMarkers)
    hm_col_report(my_data)

This prints a report like:

    ── HealthMarkers column report ────────────────────────────────────────────
     Data: 40314 rows × 299 columns   |   Keys in dictionary: 258

     key                  data_column        how matched
     -------------------- ------------------ ------------------
     fasting_glucose      pglu0              exact  ✔
     TG                   trig               exact  ✔
     ALT                  alat               exact  ✔
     albumin              alb                exact  ✔
     vitaminD             vitd25             exact  ✔
     eGFR                 ─                  NOT FOUND ✘

     ✔ 187 keys matched   ✘ 71 keys not found

    ── col_map template for missing keys ──────────────────────────────────────
     col_map <- list(
       eGFR  = "from_your_data",   # fill in your column name
     )

**Copy the printed `col_map` template** and fill in your column names
for any unmatched keys:

    my_col_map <- list(
      eGFR = "GFR_ckdepi"
    )

Or capture the auto-detected mappings directly and merge:

    # Returns a named list of all matched key → column pairs
    cm <- hm_col_report(my_data, verbose = FALSE)

    # Add manual overrides for anything not matched
    cm$eGFR <- "GFR_ckdepi"

**Pass `col_map` to any function:**

    all_health_markers(
      data    = my_data,
      which   = c("insulin_fasting", "glycemic", "lipid", "liver"),
      col_map = cm
    )

`hm_col_report()` accepts two optional flags:

    hm_col_report(my_data, show_unmatched = TRUE)  # list every unmatched key
    hm_col_report(my_data, fuzzy = TRUE)           # add fuzzy matching as last resort

### Internal key reference

The most commonly needed internal keys are:

<table>
<colgroup>
<col style="width: 33%" />
<col style="width: 33%" />
<col style="width: 33%" />
</colgroup>
<thead>
<tr>
<th>Internal key</th>
<th>Meaning</th>
<th>Example column names</th>
</tr>
</thead>
<tbody>
<tr>
<td><code>G0</code></td>
<td>Fasting glucose (mmol/L)</td>
<td><code>pglu0</code>, <code>fasting_glucose</code>,
<code>gluc0</code>, <code>LBXGLU</code>,
<code>paastoglukoosi</code></td>
</tr>
<tr>
<td><code>I0</code></td>
<td>Fasting insulin (mU/L or pmol/L)</td>
<td><code>insu0</code>, <code>insulin0</code>,
<code>ins_fast</code></td>
</tr>
<tr>
<td><code>G30</code>, <code>G120</code></td>
<td>30-/120-min OGTT glucose</td>
<td><code>pglu30</code>, <code>pglu120</code></td>
</tr>
<tr>
<td><code>I30</code>, <code>I120</code></td>
<td>30-/120-min OGTT insulin</td>
<td><code>insu30</code>, <code>insu120</code></td>
</tr>
<tr>
<td><code>TG</code></td>
<td>Triglycerides (mmol/L)</td>
<td><code>trig</code>, <code>TryG</code>, <code>TAG</code>,
<code>triglyserider</code>, <code>triglyseridit</code>,
<code>LOINC_2571_8</code></td>
</tr>
<tr>
<td><code>HDL_c</code></td>
<td>HDL cholesterol</td>
<td><code>hdlc</code>, <code>HDL</code>, <code>hdl_chol</code>,
<code>hdl_kolesteroli</code>, <code>LOINC_2085_9</code></td>
</tr>
<tr>
<td><code>LDL_c</code></td>
<td>LDL cholesterol</td>
<td><code>ldl</code>, <code>LDL</code>, <code>ldl_chol</code>,
<code>ldl_kolesteroli</code>, <code>LOINC_13457_7</code></td>
</tr>
<tr>
<td><code>TC</code></td>
<td>Total cholesterol</td>
<td><code>chol</code>, <code>total_chol</code>,
<code>kokonaiskolesteroli</code>, <code>LOINC_2093_3</code></td>
</tr>
<tr>
<td><code>ALT</code></td>
<td>Alanine aminotransferase</td>
<td><code>alat</code>, <code>SGPT</code>, <code>GPT</code>,
<code>LBXSATSI</code>, <code>NPU03429</code>,
<code>LOINC_1742_6</code></td>
</tr>
<tr>
<td><code>albumin</code></td>
<td>Serum albumin</td>
<td><code>alb</code>, <code>Albumin</code>, <code>NPU04998</code>,
<code>albumiini</code>, <code>LOINC_1751_7</code></td>
</tr>
<tr>
<td><code>creatinine</code></td>
<td>Serum creatinine</td>
<td><code>crea</code>, <code>kreatinin</code>, <code>kreatiniini</code>,
<code>NPU01994</code>, <code>LOINC_2160_0</code></td>
</tr>
<tr>
<td><code>UACR</code></td>
<td>Urine albumin/creatinine ratio</td>
<td><code>ualbcrea</code>, <code>ACR</code></td>
</tr>
<tr>
<td><code>SBP</code> / <code>DBP</code></td>
<td>Systolic/diastolic BP</td>
<td><code>sysbp</code>, <code>diabp</code>,
<code>systolisk_blodtrykk</code>, <code>LOINC_8480_6</code></td>
</tr>
<tr>
<td><code>BMI</code></td>
<td>Body mass index</td>
<td><code>bmi</code>, <code>BMI_kgm2</code>, <code>painoindeksi</code>,
<code>LOINC_39156_5</code></td>
</tr>
<tr>
<td><code>waist</code></td>
<td>Waist circumference (cm)</td>
<td><code>waist_cm</code>, <code>WC</code>, <code>midjeomkrets</code>,
<code>tailleomtrek</code></td>
</tr>
<tr>
<td><code>vitaminD</code></td>
<td>25-OH vitamin D</td>
<td><code>vitd25</code>, <code>d_vitamin</code>,
<code>D_vitamiini</code>, <code>NPU10501</code>,
<code>LOINC_62292_8</code></td>
</tr>
<tr>
<td><code>HbA1c</code></td>
<td>Glycated haemoglobin</td>
<td><code>hba1c</code>, <code>HbA1c</code>,
<code>hemoglobiini_a1c</code>, <code>NPU27300</code>,
<code>LOINC_4548_4</code></td>
</tr>
<tr>
<td><code>WBC</code></td>
<td>White blood cells</td>
<td><code>leukocytes</code>, <code>leukocytter</code>,
<code>leukocyter</code>, <code>LOINC_6690_2</code></td>
</tr>
<tr>
<td><code>Hgb</code></td>
<td>Haemoglobin</td>
<td><code>hb</code>, <code>haemoglobin</code>,
<code>hemoglobiini</code>, <code>NPU03609</code>,
<code>LOINC_718_7</code></td>
</tr>
</tbody>
</table>

------------------------------------------------------------------------

## Handle missing data before computing

Missing values should generally be resolved before passing data to any
marker function. The package provides three main helpers:

- `impute_mice()`: multiple imputation using the `mice` package,
  suitable for inference and analyses where preserving uncertainty is
  important.
- `impute_missforest()`: random-forest imputation via `missForest`,
  useful when prediction quality is the priority.
- `impute_missing()`: fast deterministic column-wise imputation for
  numeric variables, good for quick exploratory work and simple
  pipelines. Use `mean` when missing values are roughly symmetric, or
  `median` when the data are skewed or contain outliers.

<!-- -->

    # Multiple imputation (mice) recommended when you want to preserve inference uncertainty
    completed <- impute_mice(my_data, m = 5, seed = 42)

    # Random-forest imputation (missForest) recommended for predictive filling of missing values
    completed <- impute_missforest(my_data)

    # Simple deterministic imputation for fast exploratory analysis
    completed <- impute_missing(my_data, method = "median")

`impute_missing()` supports several methods beyond `median`, including
`mean`, `zero`, and `constant`.

------------------------------------------------------------------------

## Verbose diagnostics

Set `verbose = TRUE` on any function to see progress messages about
which columns were mapped, which groups were computed, and which were
skipped (with reasons).:

    results <- all_health_markers(data = labs, which = c("lipid","liver"),
                                   verbose = TRUE)
    #> Column mapping summary: TC->TC (user), HDL_c->HDL_c (user) ...
    #> -> lipid
    #> -> liver
    #> all_health_markers(): summary - computed: lipid, liver | skipped/failed: none

Enable globally for an entire session:

    # levels: "none" (default), "inform" (progress only), "debug" (all internal steps)
    options(healthmarkers.verbose = "inform")

------------------------------------------------------------------------

## Further information

The **package website** contains the full function reference, rendered
vignettes, and a searchable article index:

- **Website:** <https://sufyansuleman.github.io/HealthMarkers/>
- **Function reference:**
  <https://sufyansuleman.github.io/HealthMarkers/reference/>
- **All articles / vignettes:**
  <https://sufyansuleman.github.io/HealthMarkers/articles/>

------------------------------------------------------------------------

## Vignettes

There are **47 vignettes** covering every marker domain. The 12 core
vignettes below are bundled with the package; the remaining 35 are
available exclusively on the package website (they are not built by CRAN
to keep installation fast).

**Bundled with the package** accessible via `browseVignettes()` or
`vignette()`:

    browseVignettes("HealthMarkers")

    vignette("getting-started",     package = "HealthMarkers")
    vignette("fasting_is",          package = "HealthMarkers")
    vignette("ogtt_is",             package = "HealthMarkers")
    vignette("glycemic_markers",    package = "HealthMarkers")
    vignette("lipid_markers",       package = "HealthMarkers")
    vignette("cvd_risk",            package = "HealthMarkers")
    vignette("liver_markers",       package = "HealthMarkers")
    vignette("frailty_index",       package = "HealthMarkers")
    vignette("inflammatory_markers",package = "HealthMarkers")
    vignette("obesity_indices",     package = "HealthMarkers")
    vignette("impute_missing",      package = "HealthMarkers")
    vignette("health_markers",      package = "HealthMarkers")

**All 47 vignettes** (including adipo\_is, tracer\_dxa\_is,
allostatic\_load, bone\_markers, psych\_markers, the new multi-biobank
guide, and 30 more) are rendered and searchable on the package website:

> <https://sufyansuleman.github.io/HealthMarkers/articles/>

------------------------------------------------------------------------

## Development status and validated publications

HealthMarkers is under active development. All indices are implemented
from their original, revised or verified published manuscripts. If you
notice an error in any index, please [open an
issue](https://github.com/sufyansuleman/HealthMarkers/issues) so it can
be corrected.

The insulin sensitivity and resistance indices have been independently
verified and are used in the following peer-reviewed publications:

- Suleman S, Ängquist L, Linneberg A, Hansen T, Grarup N. Exploring the
  genetic intersection between obesity-associated genetic variants and
  insulin sensitivity indices. *Sci Rep.* 2025;15:15761. [PMID
  40328835](https://pubmed.ncbi.nlm.nih.gov/40328835/)

- Suleman S, Huang Y, Jensen RT, Poggi AI, Christensen SB, Fraulund MM,
  Anderson LV, Stinson SE, Fonvig CE, Pedersen O, Holm JC, Hansen T,
  Grarup N. Adult-based Genetic Risk Scores for Insulin Resistance
  Associate With Cardiometabolic Traits in Children and Adolescents. *J
  Clin Endocrinol Metab.* 2024;110(9):2645–2654. [PMID
  39690980](https://pubmed.ncbi.nlm.nih.gov/39690980/)

- Suleman S, Madsen AL, Ängquist LH, Schubert M, Linneberg A, Loos RJF,
  Hansen T, Grarup N. Genetic Underpinnings of Fasting and Oral
  Glucose-stimulated Based Insulin Sensitivity Indices. *J Clin
  Endocrinol Metab.* 2024;109(11):2754–2763. [PMID
  38635292](https://pubmed.ncbi.nlm.nih.gov/38635292/)

- Williamson A, Norris DM, Yin X, et al. Genome-wide association study
  and functional characterisation identifies candidate genes for
  insulin-stimulated glucose uptake. *Nat Genet.* 2023;55(6):973–983.
  [PMID 37291194](https://pubmed.ncbi.nlm.nih.gov/37291194/)

------------------------------------------------------------------------

## Contributing

Issues and pull requests are welcome at
<https://github.com/sufyansuleman/HealthMarkers/issues>.

When contributing a new marker function please:

1.  Add a unit test in `tests/testthat/` with at least one numeric
    check.
2.  Add a `@references` entry in the roxygen block and cite the primary
    paper in `inst/REFERENCES.bib`.
3.  Register the function in the `all_health_markers()` dispatcher if it
    fits an existing domain.
4.  Add or update the relevant vignette in `vignettes/`.

------------------------------------------------------------------------

## Citation

    citation("HealthMarkers")

------------------------------------------------------------------------

## License

MIT Sufyan Suleman ([ORCID
0000-0001-6612-6915](https://orcid.org/0000-0001-6612-6915))

------------------------------------------------------------------------

## AI use disclaimer

OpenAI (ChatGPT) and Anthropic Claude were used during the development
of this package to assist with code refinement, debugging, and editing
of documentation content. All outputs were reviewed, verified, and
approved by the author.
