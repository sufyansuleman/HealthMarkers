# tests/testthat/test_sweat_markers.R

test_that("sweat_markers errors if missing required columns", {
  # Missing sweat_chloride
  df1 <- tibble::tibble(
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
  df2 <- tibble::tibble(
    sweat_chloride    = 30,
    sweat_Na          = 50,
    # sweat_K missing
    weight_before     = 70,
    weight_after      = 69.5
    # missing sweat_lactate, duration, body_surface_area
  )
  expect_error(
    sweat_markers(df2),
    "missing columns: sweat_K, sweat_lactate, duration, body_surface_area"
  )
})

test_that("sweat_markers computes ionic and rate metrics correctly", {
  df <- tibble::tibble(
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
  
  # Check column names
  expect_named(
    out,
    c("sweat_chloride","Na_K_ratio","sweat_lactate","sweat_rate")
  )
  # Na/K ratio = 50/5
  expect_equal(out$Na_K_ratio, 50/5)
  # sweat_rate = (70-69.5)/1 /1.8 = 0.5/1.8
  expect_equal(out$sweat_rate, 0.5/1.8)
  # other columns passed through
  expect_equal(out$sweat_chloride, 30)
  expect_equal(out$sweat_lactate, 10)
})

test_that("sweat_markers is vectorized over multiple rows", {
  df <- tibble::tibble(
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
  # Second row: Na/K = 60/6 =10, rate = (80-79)/2/2 =1/2/2 =0.25
  expect_equal(out$Na_K_ratio[2], 60/6)
  expect_equal(out$sweat_rate[2], (80-79)/2/2)
})

test_that("verbose = TRUE prints a progress message", {
  df <- tibble::tibble(
    sweat_chloride    = 30,
    sweat_Na          = 50,
    sweat_K           = 5,
    sweat_lactate     = 10,
    weight_before     = 70,
    weight_after      = 69.5,
    duration          = 1,
    body_surface_area = 1.8
  )
  expect_message(
    sweat_markers(df, verbose = TRUE),
    "-> computing sweat markers"
  )
})
