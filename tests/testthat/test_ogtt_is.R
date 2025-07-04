library(tibble)
library(testthat)

test_that("ogtt_is returns expected OGTT indices", {
  df <- tibble(
    G0 = 5.5,
    I0 = 60,
    G30 = 7.8,
    I30 = 90,
    G120 = 6.2,
    I120 = 50,
    weight = 70,
    bmi = 24,
    age = 30,
    sex = 1
  )
  
  out <- ogtt_is(
    df,
    col_map = list(
      G0 = "G0",
      I0 = "I0",
      G30 = "G30",
      I30 = "I30",
      G120 = "G120",
      I120 = "I120",
      weight = "weight",
      bmi = "bmi",
      age = "age",
      sex = "sex"
    ),
    normalize = "none",
    verbose = FALSE
  )
  
  expect_true(all(c("Matsuda_ISI", "Gutt_index") %in% names(out)))
  expect_gt(out$Isi_120, 0)
})

test_that("ogtt_is errors on missing cols", {
  expect_error(
    ogtt_is(tibble(), col_map = rep(list("x"), 10)),
    "you must supply col_map entries"
  )
})
