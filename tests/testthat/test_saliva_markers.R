library(tibble)
test_that("saliva_markers computes logs and CAR_AUC", {
  df <- tibble(
    saliva_cort1=10, saliva_cort2=15, saliva_cort3=12,
    saliva_amylase=100, saliva_glucose=80
  )
  out <- saliva_markers(df)
  expect_named(out, c("log_cortisol_wake","CAR_AUC","log_amylase","saliva_glucose"))
  expect_equal(out$log_cortisol_wake, log(10))
})
