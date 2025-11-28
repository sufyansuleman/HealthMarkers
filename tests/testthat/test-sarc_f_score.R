library(testthat)

cm <- list(strength="Strength", walking="Walking", chair="Chair", stairs="Stairs", falls="Falls")

test_that("mapping validation and missing columns error", {
  df <- data.frame(Strength=0, Walking=0, Chair=0, Stairs=0, Falls=0)
  expect_error(sarc_f_score(df, list()), class = "healthmarkers_sarcf_error_missing_map")
  expect_error(
    sarc_f_score(df, list(strength="", walking="Walking", chair="Chair", stairs="Stairs", falls="Falls")),
    class = "healthmarkers_sarcf_error_bad_map_values"
  )
  expect_error(
    sarc_f_score(data.frame(A=1,B=1,C=1,D=1,E=1), cm),
    class = "healthmarkers_sarcf_error_missing_columns"
  )
})

test_that("verbose emits progress messages", {
  df <- data.frame(Strength=0, Walking=0, Chair=0, Stairs=0, Falls=0)
  expect_message(sarc_f_score(df, cm, verbose = TRUE), "-> sarc_f_score: preparing inputs")
  expect_message(sarc_f_score(df, cm, verbose = TRUE), "-> sarc_f_score: computing")
})

test_that("numeric coercion warning when strings introduce NAs", {
  df <- data.frame(
    Strength=c("0","oops"),
    Walking =c("1","1"),
    Chair   =c("1","1"),
    Stairs  =c("0","0"),
    Falls   =c("2","2")
  )
  expect_warning(sarc_f_score(df, cm), class = "healthmarkers_sarcf_warn_na_coercion")
})

test_that("NA policies: keep, omit, error, warn", {
  df <- data.frame(
    Strength=c(0, NA, 2),
    Walking =c(1, 1, 2),
    Chair   =c(1, 1, NA),
    Stairs  =c(0, 1, 2),
    Falls   =c(0, 1, 2)
  )

  out_keep <- sarc_f_score(df, cm, na_action = "keep")
  expect_equal(nrow(out_keep), 3L)
  expect_true(is.na(out_keep$sarc_f_score[2]) || is.na(out_keep$sarc_f_score[3]))

  out_omit <- sarc_f_score(df, cm, na_action = "omit")
  expect_equal(nrow(out_omit), 1L)

  expect_error(
    sarc_f_score(df, cm, na_action = "error"),
    class = "healthmarkers_sarcf_error_missing_values"
  )

  expect_warning(
    sarc_f_score(df, cm, na_action = "warn"),
    class = "healthmarkers_sarcf_warn_missing_inputs"
  )
})

test_that("domain warnings for values outside 0–2", {
  df <- data.frame(
    Strength=c(-1,0),
    Walking =c(3,1),
    Chair   =c(1,1),
    Stairs  =c(0,0),
    Falls   =c(0,0)
  )
  expect_warning(
    sarc_f_score(df, cm),
    class = "healthmarkers_sarcf_warn_out_of_range"
  )
})

test_that("extreme scan behaviors: warn, cap, NA, error", {
  df <- data.frame(
    Strength=c(-1, 0, 3),
    Walking =c(0, 2, 1),
    Chair   =c(1, 1, 1),
    Stairs  =c(0, 0, 0),
    Falls   =c(0, 0, 0)
  )

  # helper: suppress only the out_of_range warning, keep extremes_detected visible
  suppress_out_of_range <- function(expr) {
    withCallingHandlers(
      expr,
      warning = function(w) {
        if (inherits(w, "healthmarkers_sarcf_warn_out_of_range")) {
          invokeRestart("muffleWarning")
        }
      }
    )
  }

  # 1) warn: expect only the extremes_detected class here
  expect_warning(
    suppress_out_of_range(
      sarc_f_score(df, cm, check_extreme = TRUE, extreme_action = "warn")
    ),
    class = "healthmarkers_sarcf_warn_extremes_detected"
  )

  # 2) cap
  out_cap <- suppressWarnings(
    sarc_f_score(df, cm, check_extreme = TRUE, extreme_action = "cap")
  )
  expect_true(all(out_cap$sarc_f_score >= 0 | is.na(out_cap$sarc_f_score)))

  # 3) NA
  out_na <- suppressWarnings(
    sarc_f_score(df, cm, check_extreme = TRUE, extreme_action = "NA")
  )
  expect_true(any(is.na(out_na$sarc_f_score)))

  # 4) error
  expect_error(
    suppressWarnings(
      sarc_f_score(df, cm, check_extreme = TRUE, extreme_action = "error")
    ),
    class = "healthmarkers_sarcf_error_extremes"
  )
})

test_that("thresholding: high risk is TRUE for score >= 4", {
  df <- data.frame(
    Strength=c(2,1,0,NA),
    Walking =c(1,1,1,1),
    Chair   =c(1,1,1,1),
    Stairs  =c(0,0,0,0),
    Falls   =c(0,0,0,0)
  )

  out <- sarc_f_score(df, cm)
  expect_equal(out$sarc_f_score[1], 4)
  expect_equal(out$sarc_f_score[2], 3)
  expect_equal(out$sarc_f_score[3], 2)

  expect_true(out$sarc_f_high_risk[1])
  expect_false(out$sarc_f_high_risk[2])
  expect_false(out$sarc_f_high_risk[3])
  expect_true(is.na(out$sarc_f_high_risk[4]))
})

test_that("padding preserved for keep/warn", {
  df <- data.frame(
    Strength=c(0,NA,2),
    Walking =c(1,1,2),
    Chair   =c(1,1,2),
    Stairs  =c(0,1,2),
    Falls   =c(0,1,2)
  )

  out_keep <- sarc_f_score(df, cm, na_action = "keep")
  out_warn <- suppressWarnings(sarc_f_score(df, cm, na_action = "warn"))

  expect_equal(nrow(out_keep), 3L)
  expect_equal(nrow(out_warn), 3L)
})