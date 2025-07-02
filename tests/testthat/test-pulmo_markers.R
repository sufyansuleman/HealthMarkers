library(tibble)
test_that("pulmo_markers handles FEV1 percent predicted", {
  # minimal smoke test: data must include FEV1 and demographics
  df <- tibble(age=50, sex="male", height=170, FEV1=3.0)
  out <- pulmo_markers(df)
  expect_true("FEV1_z" %in% names(out) || "FEV1_pctpred" %in% names(out))
})
