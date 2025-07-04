
library(tibble)

test_that("adiposity_sds returns correct SDS", {
  df <- tibble(BMI = 22, waist = 75, body_fat_pct = 18)
  ref <- list(BMI = c(mean = 18, sd = 4), waist = c(mean = 70, sd = 10), body_fat_pct = c(mean = 20, sd = 5))
  out <- adiposity_sds(df, ref, verbose = FALSE)
  expect_named(out, c("BMI_SDS", "waist_SDS", "body_fat_pct_SDS"))
  expect_equal(unname(out$BMI_SDS), (22 - 18) / 4)
})

test_that("adiposity_sds errors on bad ref", {
  expect_error(adiposity_sds(tibble(), list(x = 1:3)), "must be a numeric vector")
})
