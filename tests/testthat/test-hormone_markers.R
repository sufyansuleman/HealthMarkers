library(testthat)
library(tibble)
library(HealthMarkers)

test_that("errors if any required col_map entries are missing", {
  df <- tibble(x = 1)
  # Only one key provided; the rest are missing -> hm_validate_inputs/missing cols error
  expect_error(
    hormone_markers(df, col_map = list(total_testosterone = "x")),
    "missing required columns"
  )
})

test_that("computes all nine ratios correctly", {
  df <- tibble(
    total_testosterone = 10, SHBG = 2,
    LH = 8, FSH = 4,
    estradiol = 100, progesterone = 50,
    free_T3 = 5, free_T4 = 10,
    aldosterone = 20, renin = 4,
    insulin = 20, glucagon = 10,
    GH = 2, IGF1 = 4,
    prolactin = 12,
    cortisol_0 = 200, cortisol_30 = 260
  )
  cm <- setNames(names(df), names(df))
  cm <- setNames(as.list(names(df)), names(df))
  out <- hormone_markers(df, col_map = cm, na_action = "keep")
  expect_named(out, c(
    "FAI", "LH_FSH", "E2_P", "T3_T4", "ARR",
    "Ins_Glu", "GH_IGF1", "PRL_T", "CAR_slope"
  ))
  expect_equal(out$FAI, (10 / 2) * 100)
  expect_equal(out$LH_FSH, 8 / 4)
  expect_equal(out$E2_P, 100 / 50)
  expect_equal(out$T3_T4, 5 / 10)
  expect_equal(out$ARR, 20 / 4)
  expect_equal(out$Ins_Glu, 20 / 10)
  expect_equal(out$GH_IGF1, 2 / 4)
  expect_equal(out$PRL_T, 12 / 10)
  expect_equal(out$CAR_slope, (260 - 200) / 30)
})

test_that("verbose = TRUE prints completion summary", {
  df <- as.list(rep(1, 17))
  names(df) <- c(
    "total_testosterone", "SHBG", "LH", "FSH", "estradiol", "progesterone",
    "free_T3", "free_T4", "aldosterone", "renin", "insulin", "glucagon",
    "GH", "IGF1", "prolactin", "cortisol_0", "cortisol_30"
  )
  df <- as_tibble(df)
  expect_message(
    hormone_markers(df, col_map = setNames(as.list(names(df)), names(df)), verbose = TRUE, na_action = "keep"),
    "Completed hormone_markers:"
  )
})

test_that("handles NA inputs gracefully", {
  df <- tibble(
    total_testosterone = NA, SHBG = 2,
    LH = 1, FSH = 1,
    estradiol = 1, progesterone = 1,
    free_T3 = 1, free_T4 = 1,
    aldosterone = 1, renin = 1,
    insulin = 1, glucagon = 1,
    GH = 1, IGF1 = 1,
    prolactin = 1,
    cortisol_0 = 1, cortisol_30 = 1
  )
  cm <- setNames(names(df), names(df))
  cm <- setNames(as.list(names(df)), names(df))
  out <- hormone_markers(df, col_map = cm, na_action = "keep")
  expect_true(is.na(out$FAI))  # NA numerator propagates
  expect_equal(out$LH_FSH, 1 / 1)
})
