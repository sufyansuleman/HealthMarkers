library(testthat)

# 1. PooledCohort wrappers
pooledcohort_fns <- c(
  "pooledcohort_predict_30yr_stroke_risk",
  "pooledcohort_predict_30yr_hf_risk",
  "pooledcohort_predict_30yr_chd_risk",
  "pooledcohort_predict_30yr_ascvd_risk",
  "pooledcohort_predict_30yr_cvd_risk",
  "pooledcohort_predict_10yr_stroke_risk",
  "pooledcohort_predict_10yr_hf_risk",
  "pooledcohort_predict_10yr_cvd_risk",
  "pooledcohort_predict_5yr_ascvd_risk",
  "pooledcohort_predict_10yr_chd_risk",
  "pooledcohort_predict_10yr_ascvd_risk"
)

test_that("PooledCohort wrappers exist and forward call", {
  skip_if_not_installed("PooledCohort")
  for (fn in pooledcohort_fns) {
    expect_true(exists(fn, mode = "function"), info = fn)
    wrapper <- get(fn)
    expect_error(wrapper(), info = fn)
  }
})

# 2. QRISK3 wrapper
test_that("QRISK3 wrapper exists and errors on missing data", {
  skip_if_not_installed("QRISK3")
  expect_true(exists("qrisk3_QRISK3_2017", mode = "function"))
  expect_error(qrisk3_QRISK3_2017(), info = "QRISK3_2017")
})

# 3. CVrisk wrappers
cvrisk_fns <- c(
  "cvrisk_chd_10y_mesa",
  "cvrisk_ascvd_10y_accaha",
  "cvrisk_compute_CVrisk",
  "cvrisk_ascvd_10y_frs_simple",
  "cvrisk_ascvd_10y_frs",
  "cvrisk_chd_10y_mesa_cac"
)

test_that("CVrisk wrappers exist and forward call", {
  skip_if_not_installed("CVrisk")
  for (fn in cvrisk_fns) {
    expect_true(exists(fn, mode = "function"), info = fn)
    wrapper <- get(fn)
    expect_error(wrapper(), info = fn)
  }
})

# 4. whoishRisk wrapper
test_that("whoishRisk wrapper exists and errors on missing data", {
  skip_if_not_installed("whoishRisk")
  expect_true(exists("whoishrisk_WHO_ISH_Risk", mode = "function"))
  expect_error(whoishrisk_WHO_ISH_Risk(), info = "WHO_ISH_Risk")
})

# 5. RiskScorescvd wrappers
riskscores_fns <- c(
  "riskscorescvd_round_to_nearest_digit",
  "riskscorescvd_HEART_scores",
  "riskscorescvd_GRACE",
  "riskscorescvd_GRACE_scores",
  "riskscorescvd_SCORE2",
  "riskscorescvd_ASCVD",
  "riskscorescvd_calc_scores",
  "riskscorescvd_SCORE2_Diabetes",
  "riskscorescvd_TIMI_scores",
  "riskscorescvd_EDACS",
  "riskscorescvd_SCORE2_CKD",
  "riskscorescvd_HEART",
  "riskscorescvd_ASCVD_scores",
  "riskscorescvd_TIMI",
  "riskscorescvd_EDACS_scores",
  "riskscorescvd_SCORE2_CKD_scores",
  "riskscorescvd_SCORE2_scores"
)

test_that("RiskScorescvd wrappers exist and forward call", {
  skip_if_not_installed("RiskScorescvd")
  for (fn in riskscores_fns) {
    expect_true(exists(fn, mode = "function"), info = fn)
    wrapper <- get(fn)
    expect_error(wrapper(), info = fn)
  }
})
