test_that("kidney_failure_risk returns a tibble with 2 risk estimates between 0 and 1", {
  df <- tibble(
    age  = 60,
    sex  = 1,
    eGFR = 45,
    UACR = 300
  )

  out <- kidney_failure_risk(
    data = df,
    col_map = list(
      age = "age",
      sex = "sex",
      eGFR = "eGFR",
      UACR = "UACR"
    )
  )

  expect_s3_class(out, "tbl_df")
  expect_named(out, c("KFRE_2yr", "KFRE_5yr"))
  expect_true(all(is.numeric(out$KFRE_2yr)))
  expect_true(all(is.numeric(out$KFRE_5yr)))
  expect_true(all(out$KFRE_2yr >= 0 & out$KFRE_2yr <= 1))
  expect_true(all(out$KFRE_5yr >= 0 & out$KFRE_5yr <= 1))
})

test_that("kidney_failure_risk errors if required columns are missing", {
  df_bad <- tibble(
    age  = 60,
    sex  = 1,
    eGFR = 45
    # UACR missing
  )

  expect_error(
    kidney_failure_risk(
      data = df_bad,
      col_map = list(
        age = "age",
        sex = "sex",
        eGFR = "eGFR",
        UACR = "UACR"
      )
    ),
    "missing required columns"
  )
})

test_that("kidney_failure_risk respects a custom col_map", {
  df2 <- tibble(
    A = 70,
    SEX = 2,
    GFR = 90,
    PROT = 20
  )

  out2 <- kidney_failure_risk(
    data = df2,
    col_map = list(
      age = "A",
      sex = "SEX",
      eGFR = "GFR",
      UACR = "PROT"
    )
  )

  expect_named(out2, c("KFRE_2yr", "KFRE_5yr"))
  expect_true(all(out2$KFRE_2yr >= 0 & out2$KFRE_2yr <= 1))
})
