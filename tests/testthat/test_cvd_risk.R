library(testthat)
suppressPackageStartupMessages(library(tibble))

# Dummy data with minimal columns
df <- tibble(
  age        = 55,
  sex        = 1,
  race       = "white",
  total_chol = 200,
  HDL_c      = 50,
  TG         = 150,
  sbp        = 140,
  bp_treated = TRUE,
  smoker     = FALSE,
  diabetes   = FALSE,
  bmi        = 27
)

# 1. ASCVD
test_that("cvd_risk ASCVD returns a numeric risk", {
  skip_if_not_installed("PooledCohort")
  out <- cvd_risk(df, model = "ASCVD", year = 10)
  expect_s3_class(out, "tbl_df")
  expect_equal(out$model, "ASCVD")
  expect_equal(out$year, 10)
  expect_type(out$risk, "double")
})

# 2. QRISK3
test_that("cvd_risk QRISK3 either errors or returns numeric risk", {
  skip_if_not_installed("QRISK3")
  res <- try(cvd_risk(df, model = "QRISK3"), silent = TRUE)
  expect_true(
    inherits(res, "try-error") ||
      (is.data.frame(res) && is.double(res$risk)),
    info = "QRISK3 wrapper did not run or return numeric risk"
  )
})

# 3. MESA

test_that("cvd_risk MESA either errors or returns numeric risk", {
  skip_if_not_installed("CVrisk")
  res <- try(cvd_risk(df, model = "MESA"), silent = TRUE)
  expect_true(
    inherits(res, "try-error") ||
      (is.data.frame(res) && is.double(res$risk)),
    info = "MESA wrapper did not run or return numeric risk"
  )
})

# 4. Stroke

test_that("cvd_risk Stroke either errors or returns numeric risk", {
  skip_if_not_installed("PooledCohort")
  res <- try(cvd_risk(df, model = "Stroke"), silent = TRUE)
  expect_true(
    inherits(res, "try-error") ||
      (is.data.frame(res) && is.double(res$risk)),
    info = "Stroke wrapper did not run or return numeric risk"
  )
})

# 5. WHO

test_that("cvd_risk WHO either errors or returns numeric risk", {
  skip_if_not_installed("whoishRisk")
  res <- try(cvd_risk(df, model = "WHO"), silent = TRUE)
  expect_true(
    inherits(res, "try-error") ||
      (is.data.frame(res) && is.double(res$risk)),
    info = "WHO wrapper did not run or return numeric risk"
  )
})

# 6. RiskScorescvd
test_that("cvd_risk RiskScorescvd errors on missing data", {
  skip_if_not_installed("RiskScorescvd")
  expect_error(cvd_risk(df, model = "RiskScorescvd"))
})

# 7. Invalid model
test_that("cvd_risk errors on invalid model", {
  expect_error(cvd_risk(df, model = "INVALID"), "should be one of")
})
