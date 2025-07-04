library(tibble)
test_that("adipo_is computes adipose indices including McAuley", {
  df <- tibble(G0=5.5, I0=60, TG=1.2, HDL_c=1.0, FFA=0.45, waist=80, bmi=24)
  out <- adipo_is(df,
                  col_map = list(
                    G0="G0", I0="I0", TG="TG", HDL_c="HDL_c",
                    FFA="FFA", waist="waist", bmi="bmi"
                  ),
                  normalize="none", verbose=FALSE)
  expect_true("McAuley_index" %in% names(out))
  expect_equal(ncol(out), 10)
})
