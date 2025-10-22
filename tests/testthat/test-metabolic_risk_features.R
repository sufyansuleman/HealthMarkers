library(testthat)
library(tibble)
library(HealthMarkers)

test_that("returns tibble with four factor flags and correct names/levels", {
  df <- tibble(
    chol_total = 6.0, chol_ldl = 3.5, chol_hdl = 1.0, triglycerides = 1.2,
    age_year = 25, z_HOMA = 1.5, glucose = 5.8, HbA1c = 40,
    bp_sys_z = 1.7, bp_dia_z = 1.0
  )
  cm <- as.list(names(df)); names(cm) <- names(df)
  out <- metabolic_risk_features(df, col_map = cm)

  expect_s3_class(out, "tbl_df")
  expect_named(out, c("dyslipidemia", "insulin_resistance", "hyperglycemia", "hypertension"))
  lapply(out, function(x) {
    expect_true(is.factor(x))
    expect_identical(levels(x), c("0", "1"))
  })

  # All flags should be "1"
  vals <- vapply(out, function(x) as.integer(as.character(x)), integer(1))
  expect_equal(unname(vals), c(1L, 1L, 1L, 1L))
})

test_that("custom col_map works", {
  df <- tibble(
    CT = 6.0, LDL = 3.5, HDL = 1.0, TG = 1.2,
    AGE = 25, ZH = 1.5, GLU = 5.8, A1C = 40, SBPZ = 1.7, DBPZ = 1.0
  )
  cm <- list(
    chol_total = "CT", chol_ldl = "LDL", chol_hdl = "HDL", triglycerides = "TG",
    age_year = "AGE", z_HOMA = "ZH", glucose = "GLU", HbA1c = "A1C",
    bp_sys_z = "SBPZ", bp_dia_z = "DBPZ"
  )
  out <- metabolic_risk_features(df, col_map = cm)
  expect_s3_class(out, "tbl_df")
  expect_named(out, c("dyslipidemia", "insulin_resistance", "hyperglycemia", "hypertension"))
})

test_that("errors if mapped columns are missing in data", {
  df <- tibble(
    chol_total = 6.0, chol_ldl = 3.5, chol_hdl = 1.0, triglycerides = 1.2,
    age_year = 25, z_HOMA = 1.5, glucose = 5.8,
    bp_sys_z = 1.7, bp_dia_z = 1.0
    # HbA1c missing
  )
  cm <- list(
    chol_total = "chol_total", chol_ldl = "chol_ldl", chol_hdl = "chol_hdl", triglycerides = "triglycerides",
    age_year = "age_year", z_HOMA = "z_HOMA", glucose = "glucose", HbA1c = "HbA1c",
    bp_sys_z = "bp_sys_z", bp_dia_z = "bp_dia_z"
  )
  expect_error(
    metabolic_risk_features(df, col_map = cm),
    "mapped columns not found in data"
  )
})

test_that("errors if required col_map entries are missing", {
  df <- tibble(
    chol_total = 6.0, chol_ldl = 3.5, chol_hdl = 1.0, triglycerides = 1.2,
    age_year = 25, z_HOMA = 1.5, glucose = 5.8, HbA1c = 40, bp_sys_z = 1.7, bp_dia_z = 1.0
  )
  cm <- list(
    chol_total = "chol_total", chol_ldl = "chol_ldl", chol_hdl = "chol_hdl", triglycerides = "triglycerides",
    age_year = "age_year", z_HOMA = "z_HOMA", glucose = "glucose"
    # HbA1c, bp_sys_z, bp_dia_z missing
  )
  expect_error(
    metabolic_risk_features(df, col_map = cm),
    "missing col_map entries"
  )
})

test_that("na_action='error' aborts when required inputs contain NA", {
  df <- tibble(
    chol_total = 6.0, chol_ldl = 3.5, chol_hdl = 1.0, triglycerides = 1.2,
    age_year = 25, z_HOMA = 1.5, glucose = NA_real_, HbA1c = 40, bp_sys_z = 1.7, bp_dia_z = 1.0
  )
  cm <- as.list(names(df)); names(cm) <- names(df)
  expect_error(
    suppressWarnings(metabolic_risk_features(df, col_map = cm, na_action = "error")),
    "required inputs contain missing values"
  )
})

test_that("na_action='omit' drops rows with NA", {
  df <- tibble(
    chol_total = c(6.0, 6.0), chol_ldl = c(3.5, 3.5), chol_hdl = c(1.0, 1.0), triglycerides = c(1.2, 1.2),
    age_year = c(25, 25), z_HOMA = c(1.5, 1.5), glucose = c(5.8, NA_real_), HbA1c = c(40, 40),
    bp_sys_z = c(1.7, 1.7), bp_dia_z = c(1.0, 1.0)
  )
  cm <- as.list(names(df)); names(cm) <- names(df)
  out <- suppressWarnings(metabolic_risk_features(df, col_map = cm, na_action = "omit"))
  expect_equal(nrow(out), 1L)
})

test_that("na_action='keep' yields NA only when the rule cannot be decided", {
  # Make dyslipidemia undecidable due to NA (no other trigger is TRUE)
  df <- tibble(
    chol_total = 5.0, chol_ldl = NA_real_, chol_hdl = 1.2, triglycerides = 1.0,
    age_year = 25, z_HOMA = 0.0, glucose = 5.0, HbA1c = 40, bp_sys_z = 0.0, bp_dia_z = 0.0
  )
  cm <- as.list(names(df)); names(cm) <- names(df)
  out <- suppressWarnings(metabolic_risk_features(df, col_map = cm, na_action = "keep"))
  expect_true(is.na(out$dyslipidemia))
})

test_that("check_extreme='warn' detects extremes and warns (not altered)", {
  # Only HbA1c is extreme to avoid other diagnostics
  df <- tibble(
    chol_total = 5.0, chol_ldl = 3.0, chol_hdl = 1.2, triglycerides = 1.0,
    age_year = 30, z_HOMA = 0, glucose = 5.0, HbA1c = 300,
    bp_sys_z = 0, bp_dia_z = 0
  )
  cm <- as.list(names(df)); names(cm) <- names(df)
  expect_warning(
    metabolic_risk_features(df, col_map = cm, check_extreme = TRUE, extreme_action = "warn"),
    "detected .* extreme input values \\(not altered\\)"
  )
})

test_that("check_extreme='cap' caps extremes and warns", {
  df <- tibble(
    chol_total = 5.0, chol_ldl = 3.0, chol_hdl = 1.2, triglycerides = 1.0,
    age_year = 30, z_HOMA = 0, glucose = 5.0, HbA1c = 300,
    bp_sys_z = 0, bp_dia_z = 0
  )
  cm <- as.list(names(df)); names(cm) <- names(df)
  expect_warning(
    metabolic_risk_features(df, col_map = cm, check_extreme = TRUE, extreme_action = "cap"),
    "capped .* extreme input values"
  )
})

test_that("check_extreme='error' aborts on extremes", {
  df <- tibble(
    chol_total = 5.0, chol_ldl = 3.0, chol_hdl = 1.2, triglycerides = 1.0,
    age_year = 30, z_HOMA = 0, glucose = 5.0, HbA1c = 300,
    bp_sys_z = 0, bp_dia_z = 0
  )
  cm <- as.list(names(df)); names(cm) <- names(df)
  expect_error(
    metabolic_risk_features(df, col_map = cm, check_extreme = TRUE, extreme_action = "error"),
    "detected .* extreme input values"
  )
})

test_that("invalid extreme_rules aborts with informative error", {
  df <- tibble(
    chol_total = 5.0, chol_ldl = 3.0, chol_hdl = 1.2, triglycerides = 1.0,
    age_year = 30, z_HOMA = 0, glucose = 5.0, HbA1c = 40,
    bp_sys_z = 0, bp_dia_z = 0
  )
  cm <- as.list(names(df)); names(cm) <- names(df)
  bad_rules <- list(HbA1c = c(100, 50)) # min > max
  expect_error(
    metabolic_risk_features(df, col_map = cm, extreme_rules = bad_rules),
    "extreme_rules\\[\\['HbA1c'\\]\\]` must be numeric length-2 with min <= max\\."
  )
})

test_that("high missingness warning is emitted when proportion exceeds threshold", {
  df <- tibble(
    chol_total = c(5, 5), chol_ldl = c(3, 3), chol_hdl = c(1.2, 1.2), triglycerides = c(1, 1),
    age_year = c(30, 30), z_HOMA = c(0, 0), glucose = c(5, 5),
    HbA1c = c(NA_real_, 40), bp_sys_z = c(0, 0), bp_dia_z = c(0, 0)
  )
  cm <- as.list(names(df)); names(cm) <- names(df)
  expect_warning(
    metabolic_risk_features(df, col_map = cm, na_action = "warn", na_warn_prop = 0.2),
    "has high missingness"
  )
})

test_that("verbose prints progress and completion summary", {
  df <- tibble(
    chol_total = 5, chol_ldl = 3, chol_hdl = 1.2, triglycerides = 1.0,
    age_year = 30, z_HOMA = 0, glucose = 5.0, HbA1c = 40,
    bp_sys_z = 0, bp_dia_z = 0
  )
  cm <- as.list(names(df)); names(cm) <- names(df)
  expect_message(
    metabolic_risk_features(df, col_map = cm, verbose = TRUE),
    "-> metabolic_risk_features: validating inputs"
  )
})

test_that("extreme_action='NA' sets flagged inputs to NA and propagates to outputs", {
  df <- tibble(
    chol_total = 5.0, chol_ldl = 3.0, chol_hdl = 1.2, triglycerides = 1.0,
    age_year = 30, z_HOMA = 0, glucose = 5.0, HbA1c = 300,  # extreme HbA1c
    bp_sys_z = 0, bp_dia_z = 0
  )
  cm <- as.list(names(df)); names(cm) <- names(df)
  out <- metabolic_risk_features(df, col_map = cm, check_extreme = TRUE, extreme_action = "NA")
  expect_true(is.na(out$hyperglycemia))
})

test_that("numeric coercion warns when NAs introduced", {
  df <- tibble(
    chol_total = c("5.0","oops"), chol_ldl = c(3.0, 3.0), chol_hdl = c(1.2, 1.2), triglycerides = c(1.0, 1.0),
    age_year = c(30, 30), z_HOMA = c(0, 0), glucose = c(5.0, 5.0), HbA1c = c(40, 40),
    bp_sys_z = c(0, 0), bp_dia_z = c(0, 0)
  )
  cm <- as.list(names(df)); names(cm) <- names(df)
  expect_warning(
    metabolic_risk_features(df, col_map = cm),
    "coerced to numeric; NAs introduced"
  )
})

test_that("na_action='omit' drops rows with required NA and announces omission", {
  df <- tibble(
    chol_total = c(5.0, NA), chol_ldl = c(3.0, 3.0), chol_hdl = c(1.2, 1.2), triglycerides = c(1.0, 1.0),
    age_year = c(30, 30), z_HOMA = c(0, 0), glucose = c(5.0, 5.0), HbA1c = c(40, 40),
    bp_sys_z = c(0, 0), bp_dia_z = c(0, 0)
  )
  cm <- as.list(names(df)); names(cm) <- names(df)
  expect_message(
    out <- metabolic_risk_features(df, col_map = cm, na_action = "omit", verbose = TRUE),
    "omitting 1 rows with NA in required inputs"
  )
  expect_equal(nrow(out), 1L)
})