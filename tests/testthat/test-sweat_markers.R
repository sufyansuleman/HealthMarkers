# tests/testthat/test_sweat_markers.R
library(testthat)
library(tibble)

test_that("sweat_markers errors if missing required columns", {
  # Missing sweat_chloride
  df1 <- tibble(
    sweat_Na          = 50,
    sweat_K           = 5,
    sweat_lactate     = 10,
    weight_before     = 70,
    weight_after      = 69.5,
    duration          = 1,
    body_surface_area = 1.8
  )
  expect_error(
    sweat_markers(df1),
    "missing columns: sweat_chloride"
  )

  # Missing multiple columns
  df2 <- tibble(
    sweat_chloride    = 30,
    sweat_Na          = 50,
    # sweat_K missing
    weight_before     = 70,
    weight_after      = 69.5
    # missing sweat_lactate, duration, body_surface_area
  )
  expect_error(
    sweat_markers(df2),
    "missing columns: .*sweat_K.*sweat_lactate.*duration.*body_surface_area"
  )
})

test_that("sweat_markers computes ionic and rate metrics correctly", {
  df <- tibble(
    sweat_chloride    = 30,
    sweat_Na          = 50,
    sweat_K           = 5,
    sweat_lactate     = 10,
    weight_before     = 70,
    weight_after      = 69.5,
    duration          = 1,
    body_surface_area = 1.8
  )
  out <- sweat_markers(df)

  expect_named(
    out,
    c("sweat_chloride", "Na_K_ratio", "sweat_lactate", "sweat_rate")
  )
  expect_equal(out$Na_K_ratio, 50 / 5)
  expect_equal(out$sweat_rate, 0.5 / 1.8)
  expect_equal(out$sweat_chloride, 30)
  expect_equal(out$sweat_lactate, 10)
})

test_that("sweat_markers is vectorized over multiple rows", {
  df <- tibble(
    sweat_chloride    = c(30, 40),
    sweat_Na          = c(50, 60),
    sweat_K           = c(5, 6),
    sweat_lactate     = c(10, 20),
    weight_before     = c(70, 80),
    weight_after      = c(69.5, 79),
    duration          = c(1, 2),
    body_surface_area = c(1.8, 2.0)
  )
  out <- sweat_markers(df)
  expect_equal(nrow(out), 2)
  expect_equal(out$Na_K_ratio[2], 60 / 6)
  expect_equal(out$sweat_rate[2], (80 - 79) / 2 / 2)
})

test_that("verbose emits column mapping and results messages", {
  df <- tibble(
    sweat_chloride    = 30,
    sweat_Na          = 50,
    sweat_K           = 5,
    sweat_lactate     = 10,
    weight_before     = 70,
    weight_after      = 69.5,
    duration          = 1,
    body_surface_area = 1.8
  )
  withr::local_options(healthmarkers.verbose = "inform")
  expect_message(sweat_markers(df, verbose = TRUE), "sweat_markers")
  expect_message(sweat_markers(df, verbose = TRUE), "column mapping")
  expect_message(sweat_markers(df, verbose = TRUE), "results:")
})

test_that("verbose double-fire guard", {
  df <- tibble(
    sweat_chloride    = 30,
    sweat_Na          = 50,
    sweat_K           = 5,
    sweat_lactate     = 10,
    weight_before     = 70,
    weight_after      = 69.5,
    duration          = 1,
    body_surface_area = 1.8
  )
  withr::local_options(healthmarkers.verbose = "inform")
  msgs <- testthat::capture_messages(sweat_markers(df, verbose = TRUE))
  expect_equal(sum(grepl("column mapping", msgs)), 1L)
  expect_equal(sum(grepl("results:",   msgs)), 1L)
})

test_that("na_action policies: error and omit behave as expected", {
  df_na <- tibble(
    sweat_chloride    = 30,
    sweat_Na          = 50,
    sweat_K           = 5,
    sweat_lactate     = 10,
    weight_before     = NA_real_,  # NA in required input
    weight_after      = 69.5,
    duration          = 1,
    body_surface_area = 1.8
  )
  expect_error(
    suppressWarnings(sweat_markers(df_na, na_action = "error")),
    "required inputs contain missing values"
  )
  out_omit <- suppressWarnings(sweat_markers(df_na, na_action = "omit"))
  expect_equal(nrow(out_omit), 0L)
})

test_that("check_extreme removed: function passes through outlier rows", {
  df_ext <- tibble(
    sweat_chloride    = 300,
    sweat_Na          = 300,
    sweat_K           = 50,
    sweat_lactate     = 60,
    weight_before     = 500,
    weight_after      = 600,
    duration          = 0.01,
    body_surface_area = 0.1
  )
  out <- sweat_markers(df_ext)
  expect_equal(nrow(out), 1L)
})

test_that("zero denominators emit a consolidated warning and yield NA in ratios", {
  df_zero <- tibble(
    sweat_chloride    = 30,
    sweat_Na          = 50,
    sweat_K           = 0,    # zero -> Na/K undefined
    sweat_lactate     = 10,
    weight_before     = 70,
    weight_after      = 69.5,
    duration          = 0,    # zero -> rate undefined
    body_surface_area = 1.8
  )
  expect_warning(
    out_zero <- sweat_markers(df_zero),
    "zero denominators detected"
  )
  expect_true(is.na(out_zero$Na_K_ratio))
  expect_true(is.na(out_zero$sweat_rate))
})
