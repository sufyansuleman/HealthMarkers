# tests/test_utils.R


# ----------------------------------------------------------------------------
# Tests for infer_cols()
# ----------------------------------------------------------------------------

test_that("infer_cols() can map provided columns by name", {
  df <- tibble::tibble(
    G0 = 1,
    I0 = 2,
    G30 = 3,
    I30 = 4
  )
  # prepare a minimal map for keys
  map <- list(G0 = NULL, I0 = NULL, G30 = NULL, I30 = NULL)
  result <- infer_cols(df, map, verbose = FALSE)
  expect_named(result, names(map))
  expect_equal(result$G0, "G0")
  expect_equal(result$I0, "I0")
  expect_equal(result$G30, "G30")
  expect_equal(result$I30, "I30")
})

# ----------------------------------------------------------------------------
# Tests for validate_inputs()
# ----------------------------------------------------------------------------

test_that("validate_inputs() errors when col_map is missing required entries for lipid_markers", {
  df <- tibble::tibble(
    TG    = c(1, 2),
    HDL_c = c(1, 2)
  )
  # simulate a lipid_markers call: needs TG, HDL_c, LDL_c, TC
  col_map <- list(
    TG    = "TG",
    HDL_c = "HDL_c",
    LDL_c = NULL,
    TC    = NULL
  )
  test_that("validate_inputs() errors when col_map is missing required entries for lipid_markers", {
    df <- tibble::tibble(
      TG    = c(1, 2),
      HDL_c = c(1, 2)
    )
    # simulate a lipid_markers call: needs TG, HDL_c, LDL_c, TC
    col_map <- list(
      TG    = "TG",
      HDL_c = "HDL_c",
      LDL_c = NULL,
      TC    = NULL
    )
    expect_error(
      validate_inputs(df, col_map, fun_name = "lipid_markers"),
      "you must supply col_map entries for: LDL_c, TC"
    )
  })
  "missing required columns: LDL_c, TC"
})


test_that("validate_inputs() passes when all required columns are present for lipid_markers", {
  df <- tibble::tibble(
    TG    = c(1, 2),
    HDL_c = c(1, 2),
    LDL_c = c(1, 2),
    TC    = c(1, 2)
  )
  col_map <- list(
    TG    = "TG",
    HDL_c = "HDL_c",
    LDL_c = "LDL_c",
    TC    = "TC"
  )
  expect_silent(
    validate_inputs(df, col_map, fun_name = "lipid_markers")
  )
})
