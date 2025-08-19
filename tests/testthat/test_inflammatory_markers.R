# File: tests/testthat/test_inflammatory_markers.R

test_that("errors if data is not a data.frame", {
  expect_error(
    inflammatory_markers("oops", col_map = list()),
    "`data` must be a data.frame"
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

test_that("inflammatory_markers errors if required columns are absent", {
  df <- tibble(
    neutrophils = 1, lymphocytes = 1, monocytes = 1,
    eosinophils = 1, platelets = 1, CRP = 1, albumin = NA
  )
  cm <- as.list(names(df))
  names(cm) <- names(df)
  # e.g. if albumin is NA, validate_inputs will catch missing or non-numeric
  expect_error(
    inflammatory_markers(df, col_map = cm),
    "required columns"
  )
})

test_that("inflammatory_markers prints verbose message", {
  # minimal valid data
  df <- tibble(
    neutrophils = 1, lymphocytes = 1, monocytes = 1,
    eosinophils = 1, platelets = 1, CRP = 1, albumin = 1
  )
  cm <- as.list(names(df))
  names(cm) <- names(df)
  expect_message(
    inflammatory_markers(df, col_map = cm, verbose = TRUE),
    "-> computing inflammatory markers"
  )
})

test_that("inflammatory_markers computes all indices correctly and includes ESR if mapped", {
  df <- tibble(
    neutrophils = 4, lymphocytes = 2, monocytes = 1, eosinophils = 1,
    platelets = 200, CRP = 5, albumin = 40, ESR = 15
  )
  cm <- as.list(names(df))
  names(cm) <- names(df)
  out <- inflammatory_markers(df, col_map = cm)
  expect_named(out, c(
    "NLR", "PLR", "LMR", "NER", "SII", "SIRI", "PIV",
    "CLR", "CAR", "PCR", "mGPS", "ESR"
  ))
  expect_equal(out$NLR, 4 / 2)
  expect_equal(out$PLR, 200 / 2)
  expect_equal(out$SII, (200 * 4) / 2)
  expect_equal(out$CAR, 5 / 40)
  expect_equal(out$mGPS, 0L) # CRP=5→no mGPS points
})

test_that("inflammatory_markers omits ESR when not mapped", {
  df <- tibble(
    neutrophils = 3, lymphocytes = 1, monocytes = 1, eosinophils = 0.5,
    platelets = 150, CRP = 20, albumin = 30
  )
  cm <- as.list(names(df)[1:7])
  names(cm) <- names(df)[1:7]
  out <- inflammatory_markers(df, col_map = cm)
  expect_false("ESR" %in% names(out))
  # CRP>10 & albumin<35 → mGPS=2
  expect_equal(out$mGPS, 2L)
})
