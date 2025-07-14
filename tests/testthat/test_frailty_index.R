# tests/testthat/test_frailty_index.R

# Helper to unload di if already loaded
unload_di_if_loaded <- function() {
  if ("di" %in% loadedNamespaces()) {
    unloadNamespace("di")
  }
}

# 1) frailty_index errors if 'di' not installed
test_that("frailty_index errors without di installed", {
  unload_di_if_loaded()
  empty_lib <- tempfile("libs")
  dir.create(empty_lib)
  with_libpaths(new = empty_lib, action = "replace", {
    expect_error(
      frailty_index(tibble(a = 1, b = 0)),
      "Please install the 'di' package"
    )
  })
})

# 2) frailty_index returns the expected list structure
test_that("frailty_index returns list(di, columns)", {
  df <- tibble(var1 = c(1,0,1), var2 = c(0,1,1), var3 = c(1,1,0))
  res <- frailty_index(df, cols = c("var1","var2","var3"))
  expect_type(res, "list")
  expect_named(res, c("di","columns"))
  expect_length(res$di, nrow(df))
  expect_true(all(res$di >= 0 & res$di <= 1))
  expect_true(is.matrix(res$columns) || is.data.frame(res$columns))
})

# 3) auto‐select numeric deficits when cols = NULL
test_that("frailty_index auto-selects numeric cols when cols=NULL", {
  df <- tibble(age = c(30,40), d1 = c(1,0), d2 = c(0,1), d3 = c(1,1))
  res <- frailty_index(df, age = "age")
  expect_false("age" %in% colnames(res$columns))
  expect_setequal(colnames(res$columns), c("d1","d2","d3"))
})

# 4) plot_frailty_age errors if 'di' not installed
test_that("plot_frailty_age errors without di installed", {
  unload_di_if_loaded()
  empty_lib <- tempfile("libs")
  dir.create(empty_lib)
  with_libpaths(new = empty_lib, action = "replace", {
    expect_error(
      plot_frailty_age(tibble(a = 1, b = 0), age = NULL),
      "Please install the 'di' package"
    )
  })
})

# 5) plot_frailty_age never errors when di is installed
test_that("plot_frailty_age never errors when di is installed", {
  skip_if_not_installed("di")
  df <- tibble(
    age = rep(50, 3),
    d1  = c(1,0,1),
    d2  = c(0,1,0)
  )
  expect_error(
    plot_frailty_age(df, cols = c("d1","d2"), age = "age", bins = 5),
    NA
  )
})
