# tests/test-health-markers.R

library(testthat)
library(tibble)

# Prepare dummy data and column map covering all calculators
# This dataset includes insulin, lipid, pulmonary, and excretory inputs
# and satisfies requirements for individual and wrapper functions.
df_ins <- tibble(
  # Insulin inputs
  G0             = 5.0,
  I0             = 10.0,
  G30            = 7.5,
  I30            = 15.0,
  G120           = 6.0,
  I120           = 12.0,
  # Body measures
  weight         = 70.0,
  bmi            = 24.0,
  BMI            = 24.0,
  waist          = 80.0,
  height         = 170.0,
  # Lipid measures
  TG             = 1.0,
  HDL_c          = 1.5,
  TC             = 5.0,
  triglycerides  = 150.0,
  total_chol     = 5.0,
  LDL_c          = 3.0,
  # Liver measures
  GGT            = 30.0,
  AST            = 25.0,
  ALT            = 20.0,
  platelets      = 250.0,
  albumin        = 45.0,
  bilirubin      = 1.0,
  creatinine     = 0.8,
  # Glucose measures
  glucose        = 5.5,
  # Pulmonary
  age            = 30.0,
  sex            = "male",
  ethnicity      = "Caucasian",
  fev1           = 3.0,
  fvc            = 4.0,
  # Blood pressure
  bp_sys         = 120.0,
  bp_dia         = 80.0,
  sbp            = 120.0,
  bp_treated     = FALSE,
  # Lifestyle
  smoker         = FALSE,
  diabetes       = FALSE,
  # Tracer/DXA inputs
  rate_palmitate = 0.1,
  rate_glycerol  = 0.2,
  fat_mass       = 20.0,
  # MetSSS race
  race           = "NHW"
)

col_map <- list(
  G0             = "G0",
  I0             = "I0",
  G30            = "G30",
  I30            = "I30",
  G120           = "G120",
  I120           = "I120",
  TG             = "TG",
  HDL_c          = "HDL_c",
  FFA            = "FFA",
  TC             = "TC",
  waist          = "waist",
  weight         = "weight",
  bmi            = "bmi",
  age            = "age",
  sex            = "sex",
  rate_palmitate = "rate_palmitate",
  rate_glycerol  = "rate_glycerol",
  fat_mass       = "fat_mass"
)

# 1. Test all_insulin_indices()

test_that("all_insulin_indices returns expected IS and IR columns", {
  out_fasting <- fasting_is(df_ins, col_map)
  out_ogtt    <- ogtt_is(df_ins, col_map)
  out_adipo   <- adipo_is(df_ins, col_map)
  out_tracer  <- tracer_dxa_is(df_ins, col_map)
  
  both_tbl <- all_insulin_indices(df_ins, col_map, normalize = "none", mode = "both", verbose = FALSE)
  # Contains all IS columns
  expect_true(all(names(out_fasting) %in% names(both_tbl)))
  expect_true(all(names(out_ogtt)    %in% names(both_tbl)))
  expect_true(all(names(out_adipo)   %in% names(both_tbl)))
  expect_true(all(names(out_tracer)  %in% names(both_tbl)))
  # Contains IR_ columns matching IS count
  ir_names <- grep("^IR_", names(both_tbl), value = TRUE)
  total_is <- ncol(out_fasting) + ncol(out_ogtt) + ncol(out_adipo) + ncol(out_tracer)
  expect_equal(length(ir_names), total_is)
  
  is_tbl <- all_insulin_indices(df_ins, col_map, normalize = "none", mode = "IS", verbose = FALSE)
  expect_true(all(c(names(out_fasting), names(out_ogtt), names(out_adipo), names(out_tracer)) %in% names(is_tbl)))
  expect_false(any(grepl("^IR_", names(is_tbl))))
  
  ir_tbl <- all_insulin_indices(df_ins, col_map, normalize = "none", mode = "IR", verbose = FALSE)
  expect_true(all(grepl("^IR_", names(ir_tbl))))
  expect_false(any(names(out_fasting) %in% names(ir_tbl)))
})

# 2. Test metabolic_markers()

test_that("metabolic_markers adds only requested marker groups", {
  m_ins <- metabolic_markers(df_ins, col_map, which = "insulin", normalize = "none", mode = "both", verbose = FALSE)
  # Only insulin indices present
  expect_true(all(names(all_insulin_indices(df_ins, col_map, normalize = "none", mode = "both", verbose = FALSE)) %in% names(m_ins)))
  expect_false(any(grepl("^lipid_", names(m_ins))))
  expect_false(any(grepl("^cardio_", names(m_ins))))
  
  m_ins_lip <- metabolic_markers(df_ins, col_map, which = c("insulin", "lipid"), normalize = "none", mode = "both", verbose = FALSE)
  expect_true(any(grepl("^lipid_", names(m_ins_lip))))
  expect_true(all(names(all_insulin_indices(df_ins, col_map, normalize = "none", mode = "both", verbose = FALSE)) %in% names(m_ins_lip)))
  
  groups <- c("insulin", "cardio", "lipid", "liver", "glycemic", "mets")
  m_all   <- metabolic_markers(df_ins, col_map, which = groups, normalize = "none", mode = "both", verbose = FALSE)
  expect_true(any(grepl("^lipid_", names(m_all))))
  expect_true(any(grepl("^cardio_", names(m_all))))
  expect_true(any(grepl("^liver_", names(m_all))))
  expect_true(any(grepl("^glycemic_", names(m_all))))
  expect_true(any(grepl("METSSEV", names(m_all))))
})

# 3. Test all_health_markers()

test_that("all_health_markers includes pulmonary, excretory, and metabolic markers", {
  h_all <- all_health_markers(df_ins, col_map, normalize = "none", mode = "both", verbose = FALSE)
  expect_true("fev1_pred" %in% names(h_all))
  expect_true(any(grepl("^urine_", names(h_all))))
  expect_true(any(grepl("^saliva_", names(h_all))))
  expect_true(any(grepl("^sweat_", names(h_all))))
  expect_true(any(grepl("^IR_", names(h_all))))
  expect_true(any(grepl("^lipid_", names(h_all))))
  expect_true(any(grepl("^cardio_", names(h_all))))
  expect_true(any(grepl("^liver_", names(h_all))))
  expect_true(any(grepl("^glycemic_", names(h_all))))
  expect_true(any(grepl("METSSEV", names(h_all))))
})

# 4. Test error handling for invalid 'which'

test_that("metabolic_markers errors on invalid 'which' argument", {
  expect_error(
    metabolic_markers(df_ins, col_map, which = "invalid_group", normalize = "none", mode = "both"),
    "should be one of"
  )
})
