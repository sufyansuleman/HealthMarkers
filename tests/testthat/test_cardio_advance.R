library(tibble)
test_that("metabolic_risk_features produces four factors", {
  df <- tibble(
    chol_total=6.0, chol_ldl=3.5, chol_hdl=1.0, triglycerides=1.2,
    age_year=25, z_HOMA=1.5, glucose=5.8, HbA1c=40,
    bp_sys_z=1.7, bp_dia_z=1.0
  )
  out <- metabolic_risk_features(df)
  expect_named(out, c("dyslipidemia","insulin_resistance","hyperglycemia","hypertension"))
  expect_type(out$dyslipidemia, "integer") # factor underlying integer
})
