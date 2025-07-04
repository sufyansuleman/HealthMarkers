library(tibble)
test_that("adiposity_sds_strat returns correct SDS by sex", {
  df <- tibble::tibble(sex=c("M","F"), BMI=c(25,20))
  ref <- list(
    M=list(BMI=c(mean=23,sd=3)),
    F=list(BMI=c(mean=21,sd=3))
  )
  out <- adiposity_sds_strat(df, ref, sex_col="sex")
  expect_equal(out$BMI_SDS, c((25-23)/3, (20-21)/3))
})
