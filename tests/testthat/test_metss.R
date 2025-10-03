# tests/testthat/test_metss.R

library(testthat)
library(tibble)

# ---- Missing required columns ----
test_that("metss errors if missing required columns", {
  df <- tibble(
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
    "metss\\(\\): missing required columns: waist"
  )
})

# ---- Correct computation (default params NHW male) ----
test_that("metss computes correct score for default NHW male", {
  df <- tibble(
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
  MAP <- (2 * 80 + 120) / 3
  z_MAP <- (MAP - 97) / 11
  expected <- -2.344 + 0.466 * z_MAP
  expect_s3_class(out, "tbl_df")
  expect_named(out, "MetSSS")
  expect_type(out$MetSSS, "double")
  expect_equal(unname(out$MetSSS), expected, tolerance = 1e-6)
})

# ---- Vectorization ----
test_that("metss is vectorized over multiple rows", {
  df <- tibble(
    waist   = c(94, 100),
    bp_sys  = c(120, 130),
    bp_dia  = c(80, 85),
    TG      = c(1.5, 2.0),
    HDL_c   = c(1.1, 1.2),
    glucose = c(5.3, 6.0),
    sex     = c(1, 1),
    race    = c("NHW", "NHW")
  )
  out <- metss(df)
  expect_equal(nrow(out), 2L)
  single_out <- metss(df[1, ])
  expect_equal(unname(out$MetSSS[1]), unname(single_out$MetSSS), tolerance = 1e-6)
})

# ---- Missing params key ----
test_that("metss errors when params key for sex-race is missing", {
  df_f <- tibble(
    waist   = 100,
    bp_sys  = 130,
    bp_dia  = 85,
    TG      = 2.0,
    HDL_c   = 1.2,
    glucose = 6.0,
    sex     = 2,
    race    = "NHW"
  )
  expect_error(
    metss(df_f),
    "params does not contain a key 'NHW_F'"
  )
})

# ---- Verbose messages ----
test_that("verbose = TRUE prints validation and compute messages", {
  df <- tibble(
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
    "-> metss: validating inputs"
  )
  expect_message(
    metss(df, verbose = TRUE),
    "-> metss: computing score"
  )
})

# ---- NA handling policies ----
test_that("na_action = error and omit behave as expected", {
  df <- tibble(
    waist   = c(94, NA_real_),
    bp_sys  = c(120, 120),
    bp_dia  = c(80, 80),
    TG      = c(1.5, 1.5),
    HDL_c   = c(1.1, 1.1),
    glucose = c(5.3, 5.3),
    sex     = c(1, 1),
    race    = c("NHW", "NHW")
  )
  expect_error(
    suppressWarnings(metss(df, na_action = "error")),
    "required inputs contain missing values"
  )
  out_omit <- suppressWarnings(metss(df, na_action = "omit"))
  expect_equal(nrow(out_omit), 1L)
})

# ---- Extreme detection (warn vs cap) ----
test_that("extreme input detection and capping warn as expected", {
  df_ext <- tibble(
    waist   = 400,
    bp_sys  = 500,
    bp_dia  = 250,
    TG      = 50,
    HDL_c   = 10,
    glucose = 80,
    sex     = 1,
    race    = "NHW"
  )
  # Suppress diagnostic range warnings; focus on extreme detection warnings
  expect_warning(
    out_warn <- metss(df_ext, check_extreme = TRUE, extreme_action = "warn", diagnostics = FALSE),
    "detected .* extreme input values \\(not altered\\)"
  )
  expect_warning(
    out_cap <- metss(df_ext, check_extreme = TRUE, extreme_action = "cap", diagnostics = FALSE),
    "capped .* extreme input values into allowed ranges"
  )
  expect_false(isTRUE(all.equal(out_warn$MetSSS, out_cap$MetSSS)))
})

# ---- Multiple keys warning ----
test_that("multiple sex/race keys triggers warning and uses first-row key", {
  df <- tibble(
    waist   = c(94, 100),
    bp_sys  = c(120, 130),
    bp_dia  = c(80, 85),
    TG      = c(1.5, 2.0),
    HDL_c   = c(1.1, 1.2),
    glucose = c(5.3, 6.0),
    sex     = c(1, 2),            # different sexes
    race    = c("NHW", "NHB")     # different races
  )
  # Provide param sets for NHW_M only to force use of first row key
  expect_warning(
    out <- metss(df),
    "multiple sex/race keys detected"
  )
  expect_equal(nrow(out), 2L)
})

# ---- Custom params and computation check ----
test_that("custom params applied correctly", {
  custom_params <- list(
    NHW_M = list(
      intercept = 0,
      waist     = c(mean = 100, sd = 10, coef = 1),
      TG        = c(mean = 2,   sd = 0.5, coef = 0.5),
      HDL       = c(mean = 1.2, sd = 0.2, coef = -0.7),
      glucose   = c(mean = 6,   sd = 0.5, coef = 0.8),
      MAP       = c(mean = 100, sd = 10,  coef = 0.3)
    )
  )
  df <- tibble(
    waist   = 110,
    bp_sys  = 130,
    bp_dia  = 85,
    TG      = 2.5,
    HDL_c   = 1.0,
    glucose = 6.5,
    sex     = 1,
    race    = "NHW"
  )
  MAP <- (2 * 85 + 130) / 3
  z_waist <- (110 - 100) / 10
  z_TG    <- (2.5 - 2) / 0.5
  z_HDL   <- (1.0 - 1.2) / 0.2
  z_glu   <- (6.5 - 6) / 0.5
  z_MAP   <- (MAP - 100) / 10
  expected <- 0 + 1*z_waist + 0.5*z_TG + (-0.7)*z_HDL + 0.8*z_glu + 0.3*z_MAP
  out <- metss(df, params = custom_params)
  expect_equal(unname(out$MetSSS), expected, tolerance = 1e-6)
})
