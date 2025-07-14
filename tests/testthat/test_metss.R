# tests/testthat/test_metss.R

# Test: missing required columns

test_that("metss errors if missing required columns", {
  df <- tibble::tibble(
    bp_sys  = 120,
    bp_dia  = 80,
    TG      = 1.5,
    HDL_c   = 1.1,
    glucose = 5.3,
    sex     = 1,
    race    = "NHW"
  )
  expect_error(
    metss(df),
    "missing required columns: waist"
  )
})

# Test: correct computation for default NHW male

test_that("metss computes correct score for default NHW male", {
  df <- tibble::tibble(
    waist   = 94,
    bp_sys  = 120,
    bp_dia  = 80,
    TG      = 1.5,
    HDL_c   = 1.1,
    glucose = 5.3,
    sex     = 1,
    race    = "NHW"
  )
  out <- metss(df)
  
  # Manual MAP and z-score
  
  MAP      <- (2 * 80 + 120) / 3
  z_MAP    <- (MAP - 97) / 11
  expected <- -2.344 + 0.466 * z_MAP
  
  expect_named(out, "MetSSS")
  expect_type(out$MetSSS, "double")
  expect_equal(unname(out$MetSSS), expected, tolerance = 1e-6)
})

# Test: vectorization

test_that("metss is vectorized over multiple rows", {
  df <- tibble::tibble(
    waist   = c(94, 100),
    bp_sys  = c(120, 130),
    bp_dia  = c(80, 85),
    TG      = c(1.5, 2),
    HDL_c   = c(1.1, 1.2),
    glucose = c(5.3, 6),
    sex     = c(1, 1),
    race    = c("NHW", "NHW")
  )
  out <- metss(df)
  expect_equal(nrow(out), 2)
  
  # First value matches single-row result
  
  single_out <- metss(df[1, ])
  expect_equal(
    unname(out$MetSSS[1]),
    unname(single_out$MetSSS),
    tolerance = 1e-6
  )
})

# Test: missing params for female NHW key

# When params for that sex-race are absent, returns empty tibble

test_that("metss returns empty tibble if params missing for given sex-race key", {
  df_f <- tibble::tibble(
    waist   = 100,
    bp_sys  = 130,
    bp_dia  = 85,
    TG      = 2,
    HDL_c   = 1.2,
    glucose = 6,
    sex     = 2,
    race    = "NHW"
  )
  out_f <- metss(df_f)
  expect_s3_class(out_f, "tbl_df")
  expect_equal(nrow(out_f), 0)
  expect_named(out_f, "MetSSS")
})

# Test: verbose message

test_that("verbose = TRUE prints a startup message", {
  df <- tibble::tibble(
    waist   = 94,
    bp_sys  = 120,
    bp_dia  = 80,
    TG      = 1.5,
    HDL_c   = 1.1,
    glucose = 5.3,
    sex     = 1,
    race    = "NHW"
  )
  expect_message(
    metss(df, verbose = TRUE),
    "-> computing MetSSS"
  )
})
