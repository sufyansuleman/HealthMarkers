# tests/testthat/test_adiposity_sds_strat.R

# 1) ref must be a named list with M and F
test_that("errors if ref is not a list with M & F", {
  df <- tibble(sex = "M", BMI = 25)
  bad_refs <- list(
    NULL,
    list(M = list(BMI = c(mean = 23, sd = 3))),
    list(F = list(BMI = c(mean = 21, sd = 3))),
    list(X = list(BMI = c(mean = 23, sd = 3)), F = list(BMI = c(mean = 21, sd = 3)))
  )
  for (r in bad_refs) {
    expect_error(
      adiposity_sds_strat(df, ref = r, sex_col = "sex"),
      "`ref` must be a named list with elements 'M' and 'F'"
    )
  }
})

# 2) ref$M and ref$F must have identical variable names
test_that("errors if ref$M and ref$F variable sets differ", {
  df <- tibble(sex = "M", BMI = 25)
  ref <- list(
    M = list(BMI = c(mean = 23, sd = 3), waist = c(mean = 80, sd = 10)),
    F = list(BMI = c(mean = 21, sd = 3))
  )
  expect_error(
    adiposity_sds_strat(df, ref = ref),
    "ref\\$M and ref\\$F must contain the same variable names"
  )
})

# 3) each ref[[sex]][[var]] must be numeric length 2 with names mean & sd
 test_that("errors if ref stats element is not c(mean=…, sd=…)", {
     df <- tibble(sex = "M", BMI = 25)
     bad_ref <- list(
         M = list(BMI = c(23, 3)),
         F = list(BMI = c(mean = 21, sd = 3))
       )
     expect_error(
         adiposity_sds_strat(df, ref = bad_ref),
         "ref\\[\\['M'\\]\\]\\[\\['BMI'\\]\\] must be c\\(mean.*sd.*\\)"
       )
   })

# 4) sd must be > 0
test_that("errors if any sd <= 0", {
  df <- tibble(sex = "F", BMI = 20)
  ref0 <- list(
    M = list(BMI = c(mean = 23, sd = 3)),
    F = list(BMI = c(mean = 21, sd = 0))
  )
  expect_error(
    adiposity_sds_strat(df, ref = ref0),
    "sd for F/BMI must be > 0"
  )
})

# 5) data must contain sex_col
test_that("errors if sex_col not in data", {
  ref <- list(
    M = list(BMI = c(mean = 23, sd = 3)),
    F = list(BMI = c(mean = 21, sd = 3))
  )
  df <- tibble(gender = "M", BMI = 25)
  expect_error(
    adiposity_sds_strat(df, ref = ref, sex_col = "sex"),
    "Column 'sex' not found in data"
  )
})

# 6) sex_col values must be only "M" or "F"
test_that("errors if sex_col has invalid values", {
  ref <- list(
    M = list(BMI = c(mean = 23, sd = 3)),
    F = list(BMI = c(mean = 21, sd = 3))
  )
  df <- tibble(sex = c("M", "X"), BMI = c(25, 22))
  expect_error(
    adiposity_sds_strat(df, ref = ref),
    "All values in 'sex' must be 'M' or 'F'"
  )
})

# 7) correct SDS for one variable, vectorized and multiple variables
test_that("computes correct SDS for single and multiple vars", {
  ref <- list(
    M = list(BMI   = c(mean = 23, sd = 3),
             waist = c(mean = 85, sd = 10)),
    F = list(BMI   = c(mean = 21, sd = 3),
             waist = c(mean = 75, sd =  9))
  )
  df <- tibble(
    sex   = c("M","F","M"),
    BMI   = c(25, 20, 26),
    waist = c(95, 70, 80)
  )
  out <- adiposity_sds_strat(df, ref = ref)
  # Manual check for first row (M): (25-23)/3, (95-85)/10
  expect_equal(out$BMI_SDS[1],   (25 - 23) / 3)
  expect_equal(out$waist_SDS[1], (95 - 85) / 10)
  # Second row (F): (20-21)/3, (70-75)/9
  expect_equal(out$BMI_SDS[2],   (20 - 21) / 3)
  expect_equal(out$waist_SDS[2], (70 - 75) / 9)
  # Check output shape
  expect_equal(ncol(out), 2)
  expect_equal(nrow(out), 3)
  expect_named(out, c("BMI_SDS","waist_SDS"))
})

# 8) works with custom sex_col name
test_that("respects custom sex_col argument", {
  ref <- list(
    M = list(BMI = c(mean = 23, sd = 3)),
    F = list(BMI = c(mean = 21, sd = 3))
  )
  df <- tibble(gender = c("M","F"), BMI = c(24,22))
  out <- adiposity_sds_strat(df, ref = ref, sex_col = "gender")
  expect_equal(out$BMI_SDS, c((24-23)/3, (22-21)/3))
})

# 9) verbose = TRUE prints a progress message
test_that("verbose = TRUE prints a progress message", {
  ref <- list(
    M = list(BMI = c(mean = 23, sd = 3), waist = c(mean = 85, sd = 10)),
    F = list(BMI = c(mean = 21, sd = 3), waist = c(mean = 75, sd =  9))
  )
  df <- tibble(sex = "M", BMI = 25, waist = 95)
  expect_message(
    adiposity_sds_strat(df, ref = ref, verbose = TRUE),
    "-> adiposity_sds_strat: computing sex"
  )
})
