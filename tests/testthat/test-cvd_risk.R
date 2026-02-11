# tests/testthat/test_cvd_risk.R

library(testthat)
library(HealthMarkers)

# Reusable dummy data (minimal columns)
dummy_df <- tibble::tibble(
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

test_that("dummy_df is a tibble", {
  expect_s3_class(dummy_df, "tbl_df")
})

# ---- AIP (no optional deps) ----
test_that("cvd_risk AIP returns log10(TG/HDL_c)", {
  out <- cvd_risk(dummy_df, model = "AIP", col_map = list(TG = "TG", HDL_c = "HDL_c"))
  expect_s3_class(out, "tbl_df")
  expect_identical(out$model, "AIP")
  expect_equal(out$value, log10(150 / 50))
})

# ---- LDL_PN (no optional deps) ----
test_that("cvd_risk LDL_PN returns ApoB value", {
  df2 <- tibble::tibble(ApoB = 120)
  out2 <- cvd_risk(df2, model = "LDL_PN", col_map = list(ApoB = "ApoB"))
  expect_s3_class(out2, "tbl_df")
  expect_identical(out2$model, "LDL_PN")
  expect_identical(out2$value, 120)
})

# ---- ASCVD (PooledCohort) ----
test_that("cvd_risk ASCVD returns tibble with numeric risk when PooledCohort present", {
  skip_if_not_installed("PooledCohort")
  out <- cvd_risk(dummy_df, model = "ASCVD", year = 10)
  expect_s3_class(out, "tbl_df")
  expect_identical(out$model, "ASCVD")
  expect_identical(out$year, 10L)            # integer now
  expect_type(out$risk, "double")
  expect_equal(length(out$risk), nrow(dummy_df))
})

# ---- Stroke (PooledCohort, PCE equations) ----
test_that("cvd_risk Stroke returns tibble with numeric risk when PooledCohort present", {
  skip_if_not_installed("PooledCohort")
  out <- cvd_risk(dummy_df, model = "Stroke")
  expect_s3_class(out, "tbl_df")
  expect_identical(out$model, "Stroke")
  expect_identical(out$year, 10L)            # integer now
  expect_type(out$risk, "double")
  expect_equal(length(out$risk), nrow(dummy_df))
})

# ---- QRISK3 (many inputs; handle both NA fallback and real result) ----
test_that("cvd_risk QRISK3 returns tibble when QRISK3 present (NA fallback or numeric)", {
  skip_if_not_installed("QRISK3")
  out <- cvd_risk(dummy_df, model = "QRISK3")
  expect_s3_class(out, "tbl_df")
  expect_identical(out$model, "QRISK3")
  expect_identical(out$year, 10L)
  if (all(is.na(out$risk))) {
    expect_equal(nrow(out), 1L)  # backend error path -> placeholder
  } else {
    expect_type(out$risk, "double")
    expect_equal(length(out$risk), nrow(dummy_df))
  }
})

# ---- RiskScorescvd passthrough ----
# Skip if package not installed; structure depends on upstream.
test_that("cvd_risk RiskScorescvd returns a data frame when package present", {
  skip_if_not_installed("RiskScorescvd")
  out <- cvd_risk(dummy_df, model = "RiskScorescvd")
  expect_true(is.data.frame(out))
  expect_true("model" %in% names(out))
})

# ---- Invalid model ----
test_that("cvd_risk errors on invalid model", {
  expect_error(cvd_risk(dummy_df, model = "INVALID"))
})

# ---- Dispatcher: ALL ----
test_that("cvd_risk ALL returns one row per model with expected columns", {
  out_all <- cvd_risk(dummy_df, model = "ALL", year = 10)
  expect_s3_class(out_all, "tbl_df")
  expect_true(all(c("model", "year", "risk", "value") %in% names(out_all)))
  expect_setequal(
    out_all$model,
    c("ASCVD", "QRISK3", "Stroke", "RiskScorescvd", "AIP", "LDL_PN")
  )
  expect_equal(nrow(out_all), 6L)
})
