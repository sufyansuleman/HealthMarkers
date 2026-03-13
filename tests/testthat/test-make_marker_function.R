library(testthat)
library(HealthMarkers)

# ---- make_marker_function ---------------------------------------------------
# This function generates and assigns a new marker function into the caller's
# environment. Tests verify creation behavior and the generated function contract.

test_that("make_marker_function creates a function in the caller environment", {
  local_env <- new.env(parent = environment())
  # Call from local_env so the function is assigned there
  local({
    make_marker_function(
      name = "test_mmf_marker",
      required_keys = c("A", "B"),
      compute_body = list(
        fn = function(data, col_map) {
          tibble::tibble(result = data[[col_map$A]] + data[[col_map$B]])
        }
      )
    )
  }, envir = local_env)
  expect_true(exists("test_mmf_marker", envir = local_env))
  expect_true(is.function(local_env$test_mmf_marker))
})

test_that("make_marker_function errors if name is not a length-1 character", {
  expect_error(
    make_marker_function(name = c("a", "b"), required_keys = "x", compute_body = list()),
    class = "simpleError"
  )
  expect_error(
    make_marker_function(name = 123, required_keys = "x", compute_body = list()),
    class = "simpleError"
  )
})

test_that("make_marker_function with include_extremes=FALSE does not error", {
  local_env <- new.env(parent = environment())
  local({
    make_marker_function(
      name = "simple_mmf_marker",
      required_keys = "val",
      compute_body = list(
        fn = function(data, col_map) tibble::tibble(out = data[[col_map$val]])
      ),
      include_extremes = FALSE
    )
  }, envir = local_env)
  expect_true(is.function(local_env$simple_mmf_marker))
})
