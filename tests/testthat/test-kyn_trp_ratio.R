library(testthat)

cm <- list(kynurenine = "Kyn_nM", tryptophan = "Trp_uM")

test_that("mapping validation and missing columns error", {
  df <- data.frame(Kyn_nM = 2000, Trp_uM = 60)

  expect_error(
    kyn_trp_ratio(df, list()),
    class = "healthmarkers_ktr_error_colmap_type"
  )

  expect_error(
    kyn_trp_ratio(df, list(kynurenine = "", tryptophan = "Trp_uM")),
    class = "healthmarkers_ktr_error_bad_map_values"
  )

  expect_error(
    kyn_trp_ratio(data.frame(A = 1, B = 1), cm),
    class = "healthmarkers_ktr_error_missing_columns"
  )
})

test_that("verbose emits progress messages", {
  df <- data.frame(Kyn_nM = 2000, Trp_uM = 60)
  expect_message(kyn_trp_ratio(df, cm, verbose = TRUE), "-> kyn_trp_ratio: preparing inputs")
  expect_message(kyn_trp_ratio(df, cm, verbose = TRUE), "-> kyn_trp_ratio: computing")
})

test_that("numeric coercion warning when strings introduce NAs", {
  df <- data.frame(Kyn_nM = c("2000", "oops"), Trp_uM = c("60", "55"))
  expect_warning(kyn_trp_ratio(df, cm), class = "healthmarkers_ktr_warn_na_coercion")
})

test_that("NA policies: keep, omit, error, warn", {
  df <- data.frame(Kyn_nM = c(2000, NA, 8000), Trp_uM = c(60, 50, NA))
  out_keep <- kyn_trp_ratio(df, cm, na_action = "keep")
  expect_equal(nrow(out_keep), 3L)
  expect_true(is.na(out_keep$kyn_trp_ratio[2]) || is.na(out_keep$kyn_trp_ratio[3]))

  out_omit <- kyn_trp_ratio(df, cm, na_action = "omit")
  expect_equal(nrow(out_omit), 1L)

  expect_error(kyn_trp_ratio(df, cm, na_action = "error"),
               class = "healthmarkers_ktr_error_missing_values")

  expect_warning(kyn_trp_ratio(df, cm, na_action = "warn"),
                 class = "healthmarkers_ktr_warn_missing_inputs")
})

test_that("division by zero or nonpositive Trp sets NA with warning", {
  df <- data.frame(Kyn_nM = c(2000, 3000), Trp_uM = c(0, -5))
  out <- expect_warning(kyn_trp_ratio(df, cm), class = "healthmarkers_ktr_warn_nonpositive_trp")
  expect_true(all(is.na(out$kyn_trp_ratio)))
})

test_that("ratio computation and high ratio warning", {
  df <- data.frame(Kyn_nM = c(2400, 8000), Trp_uM = c(60, 50))

  # Only checking ratio correctness here; hide the high-ratio warning
  out <- suppressWarnings(kyn_trp_ratio(df, cm))
  expect_equal(out$kyn_trp_ratio, c(2400/60, 8000/50), tolerance = 1e-12)

  # Here we explicitly assert on the high-ratio warning
  df_hi <- data.frame(Kyn_nM = 15000, Trp_uM = 50)
  expect_warning(
    kyn_trp_ratio(df_hi, cm),
    class = "healthmarkers_ktr_warn_ratio_high"
  )
})

test_that("extreme scan behaviors: warn, cap, NA, error", {
  df <- data.frame(
    Kyn_nM = c(50, 25000, 3000, 15000),
    Trp_uM = c(5, 200, 60, 50)
  )

  suppress_ratio_high <- function(expr) {
    withCallingHandlers(
      expr,
      warning = function(w) {
        if (inherits(w, "healthmarkers_ktr_warn_ratio_high")) {
          invokeRestart("muffleWarning")
        }
      }
    )
  }

  # warn: we want only the extremes_detected warning to reach expect_warning
  expect_warning(
    suppress_ratio_high(
      kyn_trp_ratio(df, cm, check_extreme = TRUE, extreme_action = "warn")
    ),
    class = "healthmarkers_ktr_warn_extremes_detected"
  )

  # cap: hide ratio_high and extremes_capped warnings entirely
  out_cap <- suppressWarnings(
    suppress_ratio_high(
      kyn_trp_ratio(df, cm, check_extreme = TRUE, extreme_action = "cap")
    )
  )
  expect_true(all(is.finite(out_cap$kyn_trp_ratio) | is.na(out_cap$kyn_trp_ratio)))

  # NA: hide all warnings, just check NA introduction
  out_na <- suppressWarnings(
    suppress_ratio_high(
      kyn_trp_ratio(df, cm, check_extreme = TRUE, extreme_action = "NA")
    )
  )
  expect_true(any(is.na(out_na$kyn_trp_ratio)))

  # error: hide warnings, assert on error class
  expect_error(
    suppressWarnings(
      suppress_ratio_high(
        kyn_trp_ratio(df, cm, check_extreme = TRUE, extreme_action = "error")
      )
    ),
    class = "healthmarkers_ktr_error_extremes"
  )
})

test_that("padding preserved for keep/warn", {
  df <- data.frame(Kyn_nM = c(2000, NA, 8000), Trp_uM = c(60, 50, NA))
  out_keep <- kyn_trp_ratio(df, cm, na_action = "keep")
  out_warn <- suppressWarnings(kyn_trp_ratio(df, cm, na_action = "warn"))
  expect_equal(nrow(out_keep), 3L)
  expect_equal(nrow(out_warn), 3L)
})