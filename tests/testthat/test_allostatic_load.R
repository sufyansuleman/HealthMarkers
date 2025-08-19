# File: tests/testthat/test_allostatic_load.R



test_that("allostatic_load counts correctly", {
  df <- tibble(
    A = c(1, 5, 10, NA),
    B = c(0, 2, 3, 4),
    C = c(100, 50, 25, 10)
  )
  thr <- list(A = 4, B = 2, C = 30)
  out <- allostatic_load(df, thr)
  expect_s3_class(out, "tbl_df")
  expect_named(out, "AllostaticLoad")
  # row-by-row:
  # row1: A=1≤4 →0, B=0≤2→0, C=100>30→1  → sum=1
  # row2: A=5>4→1, B=2≤2→0, C=50>30→1   → sum=2
  # row3: A=10>4→1, B=3>2→1, C=25≤30→0  → sum=2
  # row4: A=NA→0, B=4>2→1, C=10≤30→0   → sum=1
  expect_equal(out$AllostaticLoad, c(1L, 2L, 2L, 1L))
})

test_that("allostatic_load errors on bad inputs", {
  df <- tibble(X = 1:3)
  expect_error(allostatic_load(df, thresholds = list()), "named list")
  expect_error(allostatic_load("not a df", list(A = 1)), "`data` must be a data.frame")
  expect_error(allostatic_load(df, thresholds = list(A = "foo")), "must be numeric")
  expect_error(allostatic_load(df, thresholds = list(A = 1)), "missing columns")
})

test_that("verbose message prints biomarkers used", {
  df <- tibble(A = 1:2, B = 3:4)
  thr <- list(A = 1, B = 3)
  expect_message(
    allostatic_load(df, thr, verbose = TRUE),
    "Computing Allostatic Load for biomarkers: A, B"
  )
})



test_that("biomarkers must be numeric", {
  df <- tibble::tibble(A = letters[1:3])
  expect_error(
    allostatic_load(df, list(A = 1)),
    "biomarker 'A' must be numeric"
  )
})

test_that(">= threshold counts as high risk if you switch to >= logic", {
  df <- tibble::tibble(X = c(5, 10))
  thr <- list(X = 10)
  out <- allostatic_load(df, thr)
  # if >=, row2 should be 1; if >, it'd be 0.
  expect_equal(out$AllostaticLoad, c(0L, 1L))
})

test_that("empty data or empty thresholds returns zeros or errors", {
  df0 <- tibble::tibble()
  # Option A: zero‐row data ⇒ zero‐row output
  expect_equal(nrow(allostatic_load(df0, list())), 0)
  # Option B: empty thresholds ⇒ error
  expect_error(allostatic_load(tibble::tibble(A = 1), list()), "must be a named list of numeric cutoffs")
})
