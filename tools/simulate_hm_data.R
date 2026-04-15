## tools/simulate_hm_data.R
## Generates inst/extdata/simulated_hm_data.rds from scratch.
## 100 realistic rows covering every variable required by every function
## in the HealthMarkers package.
##
## Run from package root:
##   Rscript tools/simulate_hm_data.R

set.seed(42L)
n <- 100L

# ============================================================
# Helper
# ============================================================
rn   <- function(mn, mx, digits = 2) round(runif(n, mn, mx), digits)
ri   <- function(mn, mx)             as.integer(round(runif(n, mn, mx)))
rb   <- function(p = 0.2)            as.integer(rbinom(n, 1, p))
rc   <- function(choices, probs = NULL) sample(choices, n, replace = TRUE, prob = probs)

# Sex: 1 = male, 2 = female
sex      <- rc(c(1L, 2L))
is_male  <- sex == 1L
is_female <- !is_male

# ============================================================
# 1  Demographics
# ============================================================
id        <- sprintf("P%03d", seq_len(n))
age       <- rn(25, 85, 1)
sex_c     <- ifelse(is_male, "M", "F")
race      <- rc(c("white","black","hispanic","asian","other"),
                c(0.65, 0.13, 0.11, 0.06, 0.05))
ethnicity <- rc(c("Caucasian","African-American","Hispanic","Asian","Other"),
                c(0.65, 0.13, 0.11, 0.06, 0.05))
ethnicity_qrisk <- rc(
  c("White","Indian","Pakistani","Bangladeshi","OtherAsian",
    "BlackCaribbean","BlackAfrican","Chinese","Other"),
  c(0.65,0.05,0.04,0.02,0.04,0.06,0.07,0.03,0.04)
)
townsend  <- rn(-6, 10, 2)

# ============================================================
# 2  Anthropometry
# ============================================================
height    <- round(ifelse(is_male, rnorm(n, 177, 7), rnorm(n, 163, 6)), 1)
height_m  <- round(height / 100, 3)
weight    <- round(ifelse(is_male, rnorm(n, 85, 15), rnorm(n, 70, 13)), 1)
weight[weight < 40] <- 40
BMI       <- round(weight / (height_m^2), 1)
bmi       <- BMI
waist     <- round(ifelse(is_male, rnorm(n, 95, 12), rnorm(n, 86, 11)), 1)
hip       <- round(ifelse(is_male, rnorm(n, 102, 10), rnorm(n, 103, 10)), 1)
fat_mass  <- round(ifelse(is_male, rnorm(n, 22, 6), rnorm(n, 28, 7)), 1)
fat_mass[fat_mass < 5]  <- 5
FM        <- fat_mass
ALM       <- round(ifelse(is_male, rnorm(n, 22, 3), rnorm(n, 16, 2.5)), 1)
ALM[ALM < 8] <- 8
body_surface_area <- round(0.007184 * (weight^0.425) * (height^0.725), 3)
weight_before <- weight
weight_after  <- round(weight - runif(n, 0, 1.5), 1)

# ============================================================
# 3  Glucose / Insulin (OGTT + tracer)
# ============================================================
G0       <- round(rnorm(n, 5.5, 1.0), 2); G0[G0 < 3.5] <- 3.5
I0       <- round(exp(rnorm(n, log(65), 0.6)), 1)
G30      <- round(G0 + runif(n, 1.5, 4.0), 2)
I30      <- round(I0 * runif(n, 2.5, 6.0), 1)
G60      <- round(G30 + runif(n, -0.5, 1.5), 2)
I60      <- round(I30 * runif(n, 0.8, 1.4), 1)
G120     <- round(G0 + runif(n, -0.5, 2.5), 2); G120[G120 < 3.5] <- 3.5
I120     <- round(I0 * runif(n, 0.5, 2.0), 1)
glucose  <- G0
insulin  <- I0
FFA      <- rn(0.20, 0.90, 3)
HbA1c    <- round(rnorm(n, 38, 7), 1)
HbA1c_ifcc <- HbA1c
z_HOMA   <- round(scale(log((G0 * I0) / 22.5))[, 1], 3)

rate_palmitate <- rn(1.0, 3.5, 3)
rate_glycerol  <- rn(1.5, 4.5, 3)

# ============================================================
# 4  Lipids
# ============================================================
TC       <- rn(3.8, 7.2, 2)
HDL_c    <- round(ifelse(is_male, rnorm(n, 1.25, 0.25), rnorm(n, 1.45, 0.25)), 2)
HDL_c[HDL_c < 0.6] <- 0.6
TG       <- round(exp(rnorm(n, log(1.6), 0.45)), 2)
LDL_c    <- round(TC - HDL_c - TG/2.2, 2)
LDL_c[LDL_c < 0.5] <- 0.5
ApoB     <- rn(0.60, 1.50, 2)
ApoA1    <- rn(1.00, 2.00, 2)
TG_mgdl  <- round(TG * 88.5, 1)
total_chol <- TC

# ============================================================
# 5  Liver
# ============================================================
ALT      <- ri(12, 100)
AST      <- ri(12, 80)
GGT      <- ri(15, 120)
ALP      <- ri(40, 150)
bilirubin <- rn(5, 25, 1)

# ============================================================
# 6  Renal
# ============================================================
creatinine   <- round(ifelse(is_male, rnorm(n, 82, 18), rnorm(n, 65, 15)), 1)
creatinine[creatinine < 40] <- 40
creat        <- creatinine
BUN          <- rn(3.5, 9.5, 2)     # mmol/L
urea_serum   <- BUN
cystatin_C   <- rn(0.60, 1.80, 2)
# eGFR via CKD-EPI (approx)
eGFR <- round(pmin(120, pmax(8, 141 *
  pmin(creatinine/88.4 / ifelse(is_male, 0.9, 0.7), 1)^
    ifelse(is_male, -0.411, -0.329) *
  pmax(creatinine/88.4 / ifelse(is_male, 0.9, 0.7), 1)^(-1.209) *
  (0.993^age) * ifelse(is_male, 1, 1.018)), 1))
UACR         <- round(exp(rnorm(n, log(12), 0.9)), 1)
urine_albumin    <- round(UACR * rn(0.8, 1.2, 2), 1)
urine_creatinine <- round(urine_albumin / UACR, 2)
u_albumin    <- urine_albumin
u_creatinine <- urine_creatinine
ACR          <- UACR
a1_micro     <- as.integer(UACR > 30)
urine_protein <- round(urine_albumin * rn(1.1, 2.5, 2), 1)
urine_Na      <- ri(40, 200)
urine_K       <- ri(20, 100)
KIM1         <- rn(0.5, 3.0, 2)
NGAL         <- rn(5, 50, 1)
NAG          <- rn(1, 15, 1)
L_FABP       <- rn(1, 20, 1)
beta2_micro  <- rn(0.5, 3.5, 2)

# ============================================================
# 7  Blood pressure / CVD risk factors
# ============================================================
sbp         <- ri(105, 175)
dbp         <- ri(60, 105)
bp_sys      <- sbp
bp_dia      <- dbp
bp_treated  <- as.logical(rb(0.35))
smoker      <- as.logical(rb(0.22))
smoke       <- as.integer(smoker)
diabetes    <- as.logical(rb(0.15))
diabetes1   <- rb(0.05)
diabetes2   <- rb(0.12)
hyperlipidaemia <- rb(0.30)
hypertension    <- as.integer(sbp >= 140 | bp_treated)
systolic_blood_pressure   <- sbp
std_systolic_blood_pressure <- round(scale(sbp)[, 1], 3)
bp_sys_z   <- std_systolic_blood_pressure
bp_dia_z   <- round(scale(dbp)[, 1], 3)
blood_pressure_treatment <- as.integer(bp_treated)
atrial_fibrillation       <- rb(0.08)
systemic_lupus_erythematosis <- rb(0.02)
migraine    <- rb(0.12)
severe_mental_illness <- rb(0.04)
erectile_disfunction  <- as.integer(is_male) * rb(0.15)
rheumatoid  <- rb(0.06)
rheumatoid_arthritis <- rheumatoid
atypical_antipsy     <- rb(0.04)
regular_steroid_tablets <- rb(0.07)
family.history        <- rb(0.25)
heart_attack_relative <- family.history
gender_qrisk   <- ifelse(is_male, 1L, 0L)
alcohol        <- rb(0.20)

# ============================================================
# 8  Pulmonary
# ============================================================
FVC      <- round(ifelse(is_male, rnorm(n, 4.5, 0.7), rnorm(n, 3.4, 0.5)), 2)
FVC[FVC < 1.5] <- 1.5
FEV1     <- round(FVC * runif(n, 0.65, 0.88), 2)
fvc      <- FVC
fev1     <- FEV1
FEV1FVC  <- round(FEV1 / FVC, 3)
FEV1pct  <- round(FEV1 / FVC * 100, 1)
fev1_pct <- FEV1pct
fvc_post <- round(FVC * runif(n, 1.00, 1.12), 2)
fev1_post <- round(FEV1 * runif(n, 1.00, 1.12), 2)
mmrc     <- ri(0, 4)
sixmwd   <- ri(200, 550)

# ============================================================
# 9  Bone
# ============================================================
BMD          <- rn(0.70, 1.30, 3)
bmd          <- BMD
BMD_ref_mean <- round(ifelse(is_male, 1.05, 0.98), 2)
BMD_ref_sd   <- 0.12
TBS          <- rn(1.10, 1.45, 3)
BSAP         <- rn(5, 35, 1)
PINP         <- rn(20, 100, 1)
CTX          <- rn(100, 700, 0)
Osteocalcin  <- rn(8, 40, 1)
PTH          <- rn(10, 75, 1)
prior_fracture  <- rb(0.15)
parent_fracture <- rb(0.20)
steroids        <- rb(0.08)
secondary_op    <- rb(0.05)

# ============================================================
# 10  Inflammatory / immune
# ============================================================
WBC          <- rn(3.5, 11.0, 2)
neutrophils  <- round(WBC * runif(n, 0.45, 0.70), 3)
lymphocytes  <- round(WBC * runif(n, 0.20, 0.40), 3)
monocytes    <- round(WBC * runif(n, 0.04, 0.10), 3)
eosinophils  <- round(WBC * runif(n, 0.01, 0.06), 3)
platelets    <- ri(150, 400)
CRP          <- round(exp(rnorm(n, log(1.8), 0.8)), 2)
albumin      <- rn(32, 50, 1)
ESR          <- ri(4, 55)
IL6          <- round(exp(rnorm(n, log(2.0), 0.7)), 2)
IL18         <- round(exp(rnorm(n, log(180), 0.5)), 1)
TNFa         <- round(exp(rnorm(n, log(3.0), 0.6)), 2)
amylase      <- ri(30, 110)

# ============================================================
# 11  Hormones
# ============================================================
total_testosterone <- round(
  ifelse(is_male, rnorm(n, 16, 4), rnorm(n, 1.4, 0.5)), 2)
total_testosterone[total_testosterone < 0.3] <- 0.3
testosterone <- total_testosterone
SHBG         <- rn(15, 80, 1)
LH           <- rn(1.5, 12, 2)
FSH          <- round(ifelse(is_male, rnorm(n, 4, 1.5), rnorm(n, 6, 3.5)), 2)
FSH[FSH < 1] <- 1
estradiol    <- round(ifelse(is_male, rnorm(n, 90, 25), rnorm(n, 200, 100)), 1)
estradiol[estradiol < 10] <- 10
progesterone <- round(ifelse(is_male, runif(n, 0.3, 1.0), runif(n, 0.3, 15)), 2)
free_T3      <- rn(3.5, 6.5, 2)
free_T4      <- rn(10, 22, 1)
FT4          <- free_T4
TSH          <- round(exp(rnorm(n, log(1.8), 0.55)), 3)
TSH[TSH < 0.05] <- 0.05
aldosterone  <- ri(50, 500)
renin        <- rn(3, 40, 1)
GH           <- rn(0.05, 5.0, 3)
IGF1         <- rn(80, 280, 1)
prolactin    <- rn(4, 30, 2)
glucagon     <- rn(5, 25, 2)
cortisol_0   <- ri(180, 550)
cortisol_30  <- round(cortisol_0 * runif(n, 1.3, 2.2), 1)
cort1        <- cortisol_0
cort2        <- cortisol_30
cort3        <- round(cortisol_0 * runif(n, 0.8, 1.2), 1)
Cortisol     <- cortisol_0
DHEAS        <- round(ifelse(is_male, rnorm(n, 6.5, 2), rnorm(n, 4.5, 2)), 2)
DHEAS[DHEAS < 0.5] <- 0.5

# ============================================================
# 12  Vitamins + nutrients
# ============================================================
VitD         <- rn(20, 120, 1)
vitd         <- VitD
vitaminD     <- VitD
VitD_ref_mean <- 55
VitD_ref_sd   <- 12
B12          <- ri(150, 700)
vitaminB12   <- B12
folate       <- rn(5, 35, 1)
Folate       <- folate
ferritin     <- ri(15, 280)
Ferritin     <- ferritin
transferrin_sat <- rn(15, 45, 1)
TSat         <- transferrin_sat
transferrin  <- rn(2.0, 3.5, 2)
iron         <- ri(7, 30)
total_protein <- rn(60, 82, 1)
Retinol      <- rn(0.60, 1.50, 3)
Retinol_ref_mean <- 0.90
Retinol_ref_sd   <- 0.18
Tocopherol   <- rn(18, 55, 1)
Total_lipids <- rn(2.5, 6.0, 2)
PIVKA_II     <- rn(1, 12, 1)
VitC         <- rn(25, 100, 1)
Homocysteine <- round(exp(rnorm(n, log(10), 0.35)), 1)
MMA          <- rn(0.10, 0.60, 3)
Magnesium    <- rn(0.70, 1.10, 3)
magnesium    <- Magnesium
Mg           <- Magnesium
Zinc         <- ri(9, 20)
zinc         <- Zinc
Copper       <- rn(10, 22, 1)
copper       <- Copper
EPA          <- rn(1.0, 5.0, 2)
DHA          <- rn(2.0, 8.5, 2)
glycated_albumin <- rn(10, 22, 1)
uric_acid    <- ri(200, 500)
phosphate    <- rn(0.85, 1.60, 2)
calcium      <- rn(2.10, 2.65, 3)
Na           <- ri(135, 148)
K            <- rn(3.5, 5.2, 2)
Cl           <- ri(96, 108)
HCO3         <- ri(22, 30)
Tyr          <- ri(40, 120)
Phe          <- ri(40, 100)
corrected_calcium <- round(calcium + 0.02 * (40 - albumin), 3)

# ============================================================
# 13  Oxidative stress
# ============================================================
GSH          <- rn(600, 1800, 0)
GSSG         <- rn(5, 25, 1)

# ============================================================
# 14  Neurological / kynurenine / NfL
# ============================================================
nfl          <- round(exp(rnorm(n, log(10), 0.55)), 2)  # key is "nfl" (lower)
kynurenine   <- ri(2000, 4500)                          # nmol/L (key = "kynurenine")
tryptophan   <- ri(40, 90)                              # umol/L (key = "tryptophan")
kyn_trp_ratio <- round(kynurenine / (tryptophan * 1000) * 1000, 4)
Kyn_nM       <- kynurenine
Trp_uM       <- tryptophan

# ============================================================
# 15  Saliva / sweat
# ============================================================
saliva_cort1   <- rn(8, 30, 1)
saliva_cort2   <- round(saliva_cort1 * runif(n, 1.2, 2.0), 1)
saliva_cort3   <- round(saliva_cort1 * runif(n, 0.6, 1.0), 1)
saliva_amylase <- ri(30, 200)
saliva_glucose <- rn(3.0, 8.0, 2)
sweat_chloride <- ri(20, 60)
sweat_Na       <- ri(30, 80)
sweat_K        <- rn(3, 15, 1)
sweat_lactate  <- rn(5, 30, 1)
duration       <- ri(30, 90)

# ============================================================
# 16  MetS flag and lipid ratio helpers
# ============================================================
MetS <- as.integer(
  (waist > ifelse(is_male, 102, 88)) +
  (TG >= 1.7) +
  (HDL_c < ifelse(is_male, 1.03, 1.29)) +
  (sbp >= 130 | dbp >= 85) +
  (G0 >= 5.6) >= 3
)

# ============================================================
# 17  Spirometry helpers
# ============================================================
FEV1FVC_cat <- ifelse(FEV1FVC < 0.70, "obstruction", "normal")

# ============================================================
# 18  Charlson comorbidities (binary 0/1)
# ============================================================
mi              <- rb(0.12)
chf             <- rb(0.09)
pvd             <- rb(0.07)
stroke          <- rb(0.06)
dementia        <- rb(0.05)
copd            <- rb(0.10)
COPD            <- copd
rheum           <- rheumatoid
ulcer           <- rb(0.05)
mild_liver      <- rb(0.06)
diab_comp       <- as.integer(diabetes) * rb(0.40)
hemiplegia      <- rb(0.03)
renal           <- as.integer(eGFR < 30)
cancer          <- rb(0.07)
leukemia        <- rb(0.02)
lymphoma        <- rb(0.02)
sev_liver       <- rb(0.02)
metastatic_cancer <- rb(0.03)
hiv             <- rb(0.02)

# ============================================================
# 19  FRAX risk factors
# ============================================================
# prior_fracture, parent_fracture, steroids defined above
secondary_op   <- secondary_op   # already defined

# ============================================================
# 20  Sarc-F (0-2 each)
# ============================================================
strength <- ri(0, 2)
Strength <- strength
walking  <- ri(0, 2)
Walking  <- walking
chair    <- ri(0, 2)
Chair    <- chair
stairs   <- ri(0, 2)
Stairs   <- stairs
falls    <- ri(0, 2)
Falls    <- falls

# ============================================================
# 21  Allostatic load (columns matched by threshold names)
# ============================================================
# Uses columns from existing data (sbp, dbp, CRP, etc.) plus:
cholesterol_HDL_ratio <- round(TC / HDL_c, 2)

# ============================================================
# 22  Metabolic risk features (specific column names)
# ============================================================
chol_total    <- TC
chol_ldl      <- LDL_c
chol_hdl      <- HDL_c
triglycerides <- TG
age_year      <- age

# ============================================================
# 23  Psych diagnosis / medication flags
# ============================================================
dx_mdd         <- rb(0.12)
dx_anxiety     <- rb(0.14)
dx_adhd        <- rb(0.06)
dx_bipolar     <- rb(0.04)
dx_scz         <- rb(0.02)
dx_sud         <- rb(0.08)
med_ssri       <- rb(0.12)
med_snri       <- rb(0.06)
med_antipsychotic  <- rb(0.04)
med_mood_stabilizer <- rb(0.03)
med_anxiolytic     <- rb(0.07)

# ============================================================
# 24  Psych questionnaire items
#     All items use zero-padded 2-digit suffixes matching
#     the score function defaults.
# ============================================================

## PHQ-9: 9 items, 0-3
phq9_mat <- matrix(ri(0, 3), nrow = n, ncol = 9)
for (j in 1:9) assign(sprintf("phq9_%02d", j), phq9_mat[, j])

## GAD-7: 7 items, 0-3
gad7_mat <- matrix(ri(0, 3), nrow = n, ncol = 7)
for (j in 1:7) assign(sprintf("gad7_%02d", j), gad7_mat[, j])

## K-6: 6 items, 0-4
k6_mat <- matrix(ri(0, 4), nrow = n, ncol = 6)
for (j in 1:6) assign(sprintf("k6_%02d", j), k6_mat[, j])

## K-10: 10 items, 1-5
k10_mat <- matrix(ri(1, 5), nrow = n, ncol = 10)
for (j in 1:10) assign(sprintf("k10_%02d", j), k10_mat[, j])

## WHO-5: 5 items, 0-5
who5_mat <- matrix(ri(0, 5), nrow = n, ncol = 5)
for (j in 1:5) assign(sprintf("who5_%02d", j), who5_mat[, j])

## GHQ-12: 12 items, 0-1 (Likert 0-3 also valid; use 0-1 here)
ghq12_mat <- matrix(rb(0.35), nrow = n, ncol = 12)
for (j in 1:12) assign(sprintf("ghq12_%02d", j), ghq12_mat[, j])

## ISI: 7 items, 0-4
isi_mat <- matrix(ri(0, 4), nrow = n, ncol = 7)
for (j in 1:7) assign(sprintf("isi_%02d", j), isi_mat[, j])

## MDQ: 13 symptom items (0/1) + 3 screening items (0/1)
mdq_mat <- matrix(rb(0.30), nrow = n, ncol = 13)
for (j in 1:13) assign(sprintf("mdq_%02d", j), mdq_mat[, j])
mdq_clustering      <- rb(0.25)
mdq_impairment      <- rb(0.25)
mdq_total_check     <- as.integer(rowSums(mdq_mat) >= 7)

## ASRS v1.1: 18 items, 0-4 (Part A = items 1-6)
asrs_mat <- matrix(ri(0, 4), nrow = n, ncol = 18)
for (j in 1:18) assign(sprintf("asrs_%02d", j), asrs_mat[, j])

## BIS-11: 30 items, 1-4
bis_mat <- matrix(ri(1, 4), nrow = n, ncol = 30)
for (j in 1:30) assign(sprintf("bis_%02d", j), bis_mat[, j])

## SPQ: 74 items, 0-1
spq_mat <- matrix(rb(0.25), nrow = n, ncol = 74)
for (j in 1:74) assign(sprintf("spq_%02d", j), spq_mat[, j])

# ============================================================
# 25  Cognitive tasks (arbitrary names; used via col_map$tasks)
# ============================================================
cog_memory    <- ri(55, 100)
cog_attention <- ri(50, 100)
cog_executive <- ri(45, 100)
cog_language  <- ri(55, 100)

# ============================================================
# 26  QRISK3 / RiskScorescvd extras
# ============================================================
systolic.bp              <- sbp
total.chol               <- TC
total.hdl                <- HDL_c
typical_symptoms.num     <- ri(0, 3)
ecg.normal               <- rb(0.75)
ecg.st.depression        <- rb(0.10)
ecg.twi                  <- rb(0.08)
presentation_hstni       <- rn(0.5, 20, 2)
second_hstni             <- round(presentation_hstni * runif(n, 0.8, 1.3), 2)
number.of.episodes.24h   <- ri(1, 5)
abn.repolarisation       <- rb(0.10)
pain.radiation           <- rb(0.25)
pleuritic                <- rb(0.08)
palpation                <- rb(0.12)
heart.rate               <- ri(50, 100)
Ethnicity                <- ethnicity
Gender                   <- sex_c
Age                      <- age
pulse                    <- heart.rate
sweating                 <- rb(0.15)
previous.cabg            <- rb(0.08)
previous.pci             <- rb(0.10)
killip.class             <- ri(1, 4)
systolic_blood_pressure  <- sbp
aspirin                  <- rb(0.30)
chronic_kidney_disease   <- as.integer(eGFR < 60)

# ============================================================
# 27  Frailty items (di package; user-specified cols)
#     Include generic deficit columns d01-d20
# ============================================================
fi_mat <- matrix(rb(0.25), nrow = n, ncol = 20)
for (j in 1:20) assign(sprintf("d%02d", j), fi_mat[, j])

# ============================================================
# 28  Other misc columns present in old sim data
# ============================================================
patid     <- id
trace     <- rn(0.1, 2.0, 2)
Gender    <- sex_c

# ============================================================
# Assemble data frame
# ============================================================
# Collect all objects in creation order
all_vars <- ls()
# Remove helper functions and matrices
exclude  <- c("rn","ri","rb","rc","n","is_male","is_female",
              "phq9_mat","gad7_mat","k6_mat","k10_mat","who5_mat",
              "ghq12_mat","isi_mat","mdq_mat","asrs_mat","bis_mat",
              "spq_mat","fi_mat","all_vars","exclude","j","sex")
keep_vars <- setdiff(all_vars, exclude)

d <- as.data.frame(
  lapply(mget(keep_vars), function(x) {
    if (is.matrix(x)) x[, 1] else x    # safety
  }),
  stringsAsFactors = FALSE
)

# Add sex (integer 1/2) explicitly
d$sex <- sex

cat("Simulated:", nrow(d), "rows x", ncol(d), "cols\n")

# ============================================================
# Save
# ============================================================
out_path <- "inst/extdata/simulated_hm_data.rds"
saveRDS(d, out_path)
cat("Saved to", out_path, "\n")
