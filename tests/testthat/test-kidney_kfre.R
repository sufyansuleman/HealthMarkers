library(testthat)
library(tibble)

test_that("kidney_failure_risk returns a tibble with 2 risk estimates between 0 and 1", {
  df <- tibble(
    age  = 60,
    sex  = 1,
    eGFR = 45,
    UACR = 300
  )

  out <- kidney_failure_risk(
    data = df,
    col_map = list(age = "age", sex = "sex", eGFR = "eGFR", UACR = "UACR")
  )

  expect_s3_class(out, "tbl_df")
  expect_named(out, c("KFRE_2yr", "KFRE_5yr"))
  expect_true(all(is.numeric(out$KFRE_2yr)))
  expect_true(all(is.numeric(out$KFRE_5yr)))
  expect_true(all(out$KFRE_2yr >= 0 & out$KFRE_2yr <= 1))
  expect_true(all(out$KFRE_5yr >= 0 & out$KFRE_5yr <= 1))
})

test_that("kidney_failure_risk errors if mapped columns are missing in data", {
  df_bad <- tibble(
    age  = 60,
    sex  = 1,
    eGFR = 45
    # UACR missing
  )

  expect_error(
    kidney_failure_risk(
      data = df_bad,
      col_map = list(age = "age", sex = "sex", eGFR = "eGFR", UACR = "UACR")
    ),
    "missing required columns: UACR"
  )
})

test_that("kidney_failure_risk errors if required col_map entries are missing", {
  df <- tibble(
    age  = 60,
    sex  = 1,
    eGFR = 45,
    UACR = 300
  )
  # drop UACR key from col_map
  cm <- list(age = "age", sex = "sex", eGFR = "eGFR")
  expect_error(
    kidney_failure_risk(data = df, col_map = cm),
    "you must supply col_map entries for: UACR"
  )
})

test_that("kidney_failure_risk respects a custom col_map", {
  df2 <- tibble(A = 70, SEX = 2, GFR = 90, PROT = 20)

  out2 <- kidney_failure_risk(
    data = df2,
    col_map = list(age = "A", sex = "SEX", eGFR = "GFR", UACR = "PROT")
  )

  expect_named(out2, c("KFRE_2yr", "KFRE_5yr"))
  expect_true(all(out2$KFRE_2yr >= 0 & out2$KFRE_2yr <= 1))
  expect_true(all(out2$KFRE_5yr >= 0 & out2$KFRE_5yr <= 1))
})

test_that("na_action='error' aborts when required inputs contain NA", {
  df <- tibble(age = 60, sex = 1, eGFR = 45, UACR = NA_real_)
  cm <- list(age = "age", sex = "sex", eGFR = "eGFR", UACR = "UACR")
  expect_error(
    suppressWarnings(kidney_failure_risk(df, col_map = cm, na_action = "error")),
    "required inputs contain missing values"
  )
})

test_that("na_action='omit' drops rows with NA", {
  df <- tibble(
    age = c(60, 70),
    sex = c(1, 2),
    eGFR = c(45, NA_real_),
    UACR = c(300, 100)
  )
  cm <- as.list(names(df)); names(cm) <- names(df)
  out <- suppressWarnings(kidney_failure_risk(df, col_map = cm, na_action = "omit"))
  expect_equal(nrow(out), 1L)
  expect_s3_class(out, "tbl_df")
})

test_that("invalid sex coding errors", {
  df <- tibble(age = 60, sex = 3, eGFR = 45, UACR = 300)
  cm <- list(age = "age", sex = "sex", eGFR = "eGFR", UACR = "UACR")
  expect_error(
    kidney_failure_risk(df, col_map = cm),
    "sex' must be coded as 1=male or 2=female"
  )
})

test_that("check_extreme='cap' caps values and warns", {
  df <- tibble(age = 10, sex = 1, eGFR = 0.5, UACR = 20000)
  cm <- list(age = "age", sex = "sex", eGFR = "eGFR", UACR = "UACR")
  expect_warning(
    out <- kidney_failure_risk(df, col_map = cm, check_extreme = TRUE, extreme_action = "cap"),
    "capped .* extreme input values"
  )
  # Default caps: age=18, eGFR=1, UACR=10000
  expect_true(is.numeric(out$KFRE_2yr))
  expect_true(is.numeric(out$KFRE_5yr))
})

test_that("check_extreme='error' aborts on out-of-range", {
  df <- tibble(age = 10, sex = 1, eGFR = 0.5, UACR = 20000)
  cm <- list(age = "age", sex = "sex", eGFR = "eGFR", UACR = "UACR")
  expect_error(
    kidney_failure_risk(df, col_map = cm, check_extreme = TRUE, extreme_action = "error"),
    "detected .* out-of-range values"
  )
})

test_that("verbose prints progress and summary", {
  df <- tibble(age = 60, sex = 1, eGFR = 45, UACR = 300)
  cm <- list(age = "age", sex = "sex", eGFR = "eGFR", UACR = "UACR")
  expect_message(
    kidney_failure_risk(df, col_map = cm, verbose = TRUE),
    "-> kidney_failure_risk: computing KFRE risks"
  )
})

test_that("non-positive eGFR triggers log warning", {
  df <- tibble(age = 60, sex = 1, eGFR = 0, UACR = 1)
  cm <- list(age = "age", sex = "sex", eGFR = "eGFR", UACR = "UACR")
  expect_warning(
    kidney_failure_risk(df, col_map = cm),
    "'eGFR'.*log\\(\\) undefined"
  )
})

test_that("non-positive UACR triggers log warning", {
  df <- tibble(age = 60, sex = 1, eGFR = 1, UACR = 0)
  cm <- list(age = "age", sex = "sex", eGFR = "eGFR", UACR = "UACR")
  expect_warning(
    kidney_failure_risk(df, col_map = cm),
    "'UACR'.*log\\(\\) undefined"
  )
})
