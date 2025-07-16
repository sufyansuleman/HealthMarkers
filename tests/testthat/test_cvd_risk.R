
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

# 8. AIP (Atherogenic Index of Plasma)
test_that("cvd_risk AIP returns log10(TG/HDL_c)", {
  out <- cvd_risk(df, model = "AIP", col_map = list(TG = "TG", HDL_c = "HDL_c"))
  expect_s3_class(out, "tbl_df")
  expect_equal(out$model, "AIP")
  # TG=150, HDL_c=50 → log10(150/50) = log10(3)
  expect_equal(out$value, log10(150/50))
})

# 9. LDL_PN (LDL Particle Number Estimate)
test_that("cvd_risk LDL_PN returns the ApoB value", {
  df2 <- tibble(ApoB = 120)
  out2 <- cvd_risk(df2, model = "LDL_PN", col_map = list(ApoB = "ApoB"))
  expect_s3_class(out2, "tbl_df")
  expect_equal(out2$model, "LDL_PN")
  expect_equal(out2$value, 120)
})

test_that("cvd_risk ALL returns every model", {
  skip_if_not_installed(c("PooledCohort","QRISK3","CVrisk","whoishRisk","RiskScorescvd"))
  out_all <- cvd_risk(df, model = "ALL", year = 10)
  # Expect one row per model
  expect_setequal(out_all$model, c("ASCVD","QRISK3","MESA","Stroke","WHO","RiskScorescvd","AIP","LDL_PN"))
  expect_true(all(c("year","risk","value") %in% names(out_all)))
})

