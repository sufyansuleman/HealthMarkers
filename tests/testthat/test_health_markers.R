# tests/test_health_markers.R

df_ins <- tibble(
  G0=5.0,I0=10.0,G30=7.5,I30=15.0,G120=6.0,I120=12.0,FFA=0.3,
  weight=70.0,bmi=24.0,BMI=24.0,waist=80.0,height=170.0,age=30.0,sex="male",
  TG=1.0,HDL_c=1.5,TC=5.0,total_chol=5.0,LDL_c=3.0,
  GGT=30,AST=25,ALT=20,platelets=250,albumin=45,bilirubin=1.0,creatinine=0.8,
  glucose=5.5,sbp=120,dbp=80,bp_sys=120,bp_dia=80,bp_treated=FALSE,smoker=FALSE,diabetes=FALSE,
  race="white",ethnicity="Caucasian",fev1=3.0,fvc=4.0,
  rate_palmitate=0.1,rate_glycerol=0.2,fat_mass=20.0
)

col_map <- list(
  G0="G0",I0="I0",G30="G30",I30="I30",G120="G120",I120="I120",FFA="FFA",
  TG="TG",HDL_c="HDL_c",TC="TC",total_chol="total_chol",LDL_c="LDL_c",
  weight="weight",bmi="bmi",waist="waist",age="age",sex="sex",
  sbp="sbp",dbp="dbp",bp_sys="bp_sys",bp_dia="bp_dia",bp_treated="bp_treated",
  smoker="smoker",diabetes="diabetes",race="race",
  rate_palmitate="rate_palmitate",rate_glycerol="rate_glycerol",fat_mass="fat_mass"
)

test_that("all_insulin_indices returns IS and IR sets correctly", {
  both_tbl <- all_insulin_indices(df_ins, col_map, normalize="none", mode="both", verbose=FALSE)
  expect_true(any(grepl("^IR_", names(both_tbl))))
  is_tbl <- all_insulin_indices(df_ins, col_map, mode="IS", verbose=FALSE)
  ir_tbl <- all_insulin_indices(df_ins, col_map, mode="IR", verbose=FALSE)
  expect_false(any(grepl("^IR_", names(is_tbl))))
  expect_true(all(grepl("^IR_", names(ir_tbl))))
  expect_equal(ncol(ir_tbl), ncol(is_tbl))
})

test_that("metabolic_markers insulin only subset", {
  m_ins <- metabolic_markers(df_ins, col_map, which="insulin", normalize="none", mode="both", verbose=FALSE)
  expect_true(any(grepl("^IR_", names(m_ins))))
  expect_false(any(grepl("^FLI$", names(m_ins)))) # liver not included
})

test_that("metabolic_markers insulin + lipid adds lipid cols", {
  m_il <- metabolic_markers(df_ins, col_map, which=c("insulin","lipid"),
                            normalize="none", mode="both", verbose=FALSE)
  expect_true(any(grepl("non_HDL_c", names(m_il))))
})

test_that("metabolic_markers liver + mets produce key outputs", {
  m_lm <- metabolic_markers(df_ins, col_map, which=c("liver","mets"),
                            normalize="none", mode="both", verbose=FALSE)
  expect_true(all(c("FLI","NFS","APRI") %in% names(m_lm)))
  expect_true(any(grepl("^MetS", names(m_lm))) || "MetSSS" %in% names(m_lm))
})

test_that("all_health_markers('all') returns many derived columns", {
  out <- all_health_markers(df_ins, col_map, which="all",
                            normalize="none", mode="both", verbose=FALSE)
  expect_gt(ncol(out), ncol(df_ins) + 25)
  expect_true(any(grepl("^IR_", names(out))))
})

test_that("all_health_markers can select subset categories", {
  out2 <- all_health_markers(df_ins, col_map,
                             which=c("lipid","liver","glycemic"),
                             include_insulin=FALSE,
                             verbose=FALSE)
  expect_true(all(c("FLI","NFS","APRI") %in% names(out2)))
  expect_true(any(grepl("ratio_TC_HDL", names(out2))))
  expect_false(any(grepl("^IR_", names(out2)))) # insulin excluded
})

test_that("all_health_markers errors on invalid category", {
  expect_error(all_health_markers(df_ins, col_map, which="nope"), "Unknown")
})
