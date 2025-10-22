# tests/test-utils_infer_cols.R

# ----------------------------------------------------------------------------
# Tests for infer_cols()
# ----------------------------------------------------------------------------

test_that("infer_cols maps by name when present", {
  df <- tibble::tibble(G0 = 1, I0 = 2, TG = 3, `HDL-c` = 4)
  spec <- list(G0 = NULL, I0 = NULL, TG = NULL, HDL_c = NULL)
  res <- infer_cols(df, spec, verbose = FALSE)
  expect_named(res, names(spec))
  expect_equal(res$G0, "G0")
  expect_equal(res$I0, "I0")
  expect_equal(res$TG, "TG")
  expect_true(res$HDL_c %in% c("HDL-c", "HDL_c"))
})

# ----------------------------------------------------------------------------
# Tests for validate_inputs()
# ----------------------------------------------------------------------------

test_that("validate_inputs errors for missing lipid_markers keys", {
  df <- tibble::tibble(TG = 1, HDL_c = 1)
  col_map <- list(TG = "TG", HDL_c = "HDL_c", LDL_c = NULL, TC = NULL)
  expect_error(
    validate_inputs(df, col_map, fun_name = "lipid_markers"),
    "you must supply col_map entries for: LDL_c, TC"
  )
})

test_that("validate_inputs passes when all lipid_markers keys present", {
  df <- tibble::tibble(TG = 1, HDL_c = 1, LDL_c = 1, TC = 1)
  col_map <- list(TG = "TG", HDL_c = "HDL_c", LDL_c = "LDL_c", TC = "TC")
  expect_silent(validate_inputs(df, col_map, fun_name = "lipid_markers"))
})
