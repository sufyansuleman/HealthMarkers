# File: tests/testthat/test_inflammatory_markers.R

library(testthat)
library(tibble)

test_that("errors if data is not a data.frame", {
  expect_error(
    inflammatory_markers("oops", col_map = list()),
    "data.frame or tibble"
  )
})

test_that("inflammatory_markers errors on missing col_map entries", {
  df <- tibble(
    neutrophils = 1, lymphocytes = 1, monocytes = 1,
    eosinophils = 1, platelets = 1, CRP = 1, albumin = 1
  )
  bad_map <- list(
    neutrophils = "neutrophils", lymphocytes = "lymphocytes",
    monocytes = "monocytes", eosinophils = "eosinophils",
    platelets = "platelets", CRP = "CRP"
    # albumin missing
  )
  expect_error(
    inflammatory_markers(df, col_map = bad_map),
    "missing col_map entries for: albumin"
  )
})

test_that("inflammatory_markers errors if required columns contain missing values (na_action='error')", {
  df <- tibble(
    neutrophils = 1, lymphocytes = 1, monocytes = 1,
    eosinophils = 1, platelets = 1, CRP = 1, albumin = NA_real_
  )
  cm <- as.list(names(df)); names(cm) <- names(df)
  # Suppress expected high-missingness warning
  expect_error(
    suppressWarnings(inflammatory_markers(df, col_map = cm)),
    "required columns.*missing values"
  )
})

test_that("inflammatory_markers prints verbose message", {
  df <- tibble(
    neutrophils = 1, lymphocytes = 1, monocytes = 1,
    eosinophils = 1, platelets = 1, CRP = 1, albumin = 1
  )
  cm <- as.list(names(df)); names(cm) <- names(df)
  expect_message(
    inflammatory_markers(df, col_map = cm, verbose = TRUE),
    "-> inflammatory_markers: computing indices"
  )
})

test_that("inflammatory_markers computes all indices correctly and includes ESR if mapped", {
  df <- tibble(
    neutrophils = 4, lymphocytes = 2, monocytes = 1, eosinophils = 1,
    platelets = 200, CRP = 5, albumin = 40, ESR = 15
  )
  cm <- as.list(names(df)); names(cm) <- names(df)
  out <- inflammatory_markers(df, col_map = cm)
  expect_named(out, c(
    "NLR", "PLR", "LMR", "NER", "SII", "SIRI", "PIV",
    "CLR", "CAR", "PCR", "mGPS", "ESR"
  ))
  expect_equal(out$NLR, 4 / 2)
  expect_equal(out$PLR, 200 / 2)
  expect_equal(out$SII, (200 * 4) / 2)
  expect_equal(out$CAR, 5 / 40)
  expect_equal(out$mGPS, 0L)
})

test_that("inflammatory_markers omits ESR when not mapped", {
  df <- tibble(
    neutrophils = 3, lymphocytes = 1, monocytes = 1, eosinophils = 0.5,
    platelets = 150, CRP = 20, albumin = 30
  )
  cm <- as.list(names(df)[1:7]); names(cm) <- names(df)[1:7]
  out <- inflammatory_markers(df, col_map = cm)
  expect_false("ESR" %in% names(out))
  # CRP>10 & albumin<35 -> mGPS=2
  expect_equal(out$mGPS, 2L)
})

test_that("na_action='omit' drops rows with NA in required inputs", {
  df <- tibble(
    neutrophils = c(1, 2),
    lymphocytes = c(1, NA_real_),
    monocytes = c(1, 1),
    eosinophils = c(1, 1),
    platelets = c(100, 100),
    CRP = c(5, 5),
    albumin = c(40, 40)
  )
  cm <- as.list(names(df)); names(cm) <- names(df)
  out <- suppressWarnings(inflammatory_markers(df, col_map = cm, na_action = "omit"))
  expect_s3_class(out, "tbl_df")
  expect_equal(nrow(out), 1L)
  expect_equal(out$NLR, 1 / 1)
})

test_that("na_action='keep' propagates NA in outputs", {
  df <- tibble(
    neutrophils = c(1, 2),
    lymphocytes = c(1, NA_real_),
    monocytes = c(1, 1),
    eosinophils = c(1, 1),
    platelets = c(100, 100),
    CRP = c(5, 5),
    albumin = c(40, 40)
  )
  cm <- as.list(names(df)); names(cm) <- names(df)
  out <- suppressWarnings(inflammatory_markers(df, col_map = cm, na_action = "keep"))
  expect_true(is.na(out$NLR[2]))
  expect_equal(out$NLR[1], 1 / 1)
})

test_that("check_extreme='cap' caps out-of-range values and warns", {
  df <- tibble(
    neutrophils = 40,      # > 30 -> cap to 30
    lymphocytes = 1,       # non-zero to avoid denominator warning
    monocytes = 10,        # > 5 -> cap to 5
    eosinophils = 10,      # > 5 -> cap to 5
    platelets = 5000,      # > 1000 -> cap to 1000
    CRP = 500,             # > 300 -> cap to 300
    albumin = 5            # < 10 -> cap to 10
  )
  cm <- as.list(names(df)); names(cm) <- names(df)
  expect_warning(
    out <- inflammatory_markers(df, col_map = cm, check_extreme = TRUE, extreme_action = "cap"),
    "capped .* extreme input values"
  )
  # After capping
  expect_equal(out$CAR, 300 / 10)
  expect_equal(out$SII, (1000 * 30) / 1)
})

test_that("check_extreme='error' aborts on out-of-range values", {
  df <- tibble(
    neutrophils = 40, lymphocytes = 1, monocytes = 1, eosinophils = 1,
    platelets = 200, CRP = 5, albumin = 40
  )
  cm <- as.list(names(df)); names(cm) <- names(df)
  expect_error(
    inflammatory_markers(df, col_map = cm, check_extreme = TRUE, extreme_action = "error"),
    "detected .* extreme input values"
  )
})

test_that("zero denominators emit a warning and produce Inf/NaN as in base R", {
  df <- tibble(
    neutrophils = 2, lymphocytes = 0, monocytes = 1, eosinophils = 0,
    platelets = 100, CRP = 5, albumin = 40
  )
  cm <- as.list(names(df)); names(cm) <- names(df)
  expect_warning(
    out <- inflammatory_markers(df, col_map = cm),
    "zero denominators detected"
  )
  expect_true(is.infinite(out$NLR))  # 2/0 -> Inf
  expect_true(is.infinite(out$PLR))  # 100/0 -> Inf
  expect_true(is.infinite(out$NER))  # 2/0 -> Inf
})
