library(testthat)
library(tibble)

skip_if_no_rspiro <- function() {
  skip_if_not_installed("rspiro")
  ns <- tryCatch(asNamespace("rspiro"), error = function(e) NULL)
  skip_if(is.null(ns), "rspiro namespace not available")
  # Require the three helper families for at least one equation
  has_eq <- function(eq) {
    req <- c(paste0("pred_", eq), paste0("zscore_", eq), paste0("LLN_", eq))
    all(vapply(req, function(fn) exists(fn, envir = ns, inherits = FALSE), logical(1)))
  }
  available <- c("GLI", "GLIgl", "NHANES3")[vapply(c("GLI", "GLIgl", "NHANES3"), has_eq, logical(1))]
  skip_if(length(available) == 0, "rspiro lacks required prediction/zscore/LLN functions")
  invisible(available)
}

test_that("pulmo_markers computes expected pulmonary metrics across equations", {
  available_eqs <- skip_if_no_rspiro()

  df <- tibble(
    age = 45,
    sex = "male",
    height = 170, # cm; function will convert to metres
    ethnicity = "Caucasian",
    fev1 = 3.0,
    fvc = 4.0
  )

  for (eq in available_eqs) {
    out <- pulmo_markers(df, equation = eq, verbose = FALSE)

    expected_cols <- c(
      "fev1_pred", "fev1_z", "fev1_pctpred", "fev1_LLN",
      "fvc_pred", "fvc_z", "fvc_pctpred", "fvc_LLN",
      "fev1_fvc_ratio", "fev1_fvc_pred", "fev1_fvc_z", "fev1_fvc_pctpred", "fev1_fvc_LLN"
    )
    expect_true(all(expected_cols %in% names(out)))

    # Basic finiteness for predictions and percent predicted
    expect_true(is.finite(out$fev1_pred[1]))
    expect_true(is.finite(out$fvc_pred[1]))
    expect_true(is.finite(out$fev1_pctpred[1]))
    expect_true(is.finite(out$fvc_pctpred[1]))

    # z-scores may be NA for some ref sets; only range-check when finite
    if (is.finite(out$fev1_z[1])) expect_true(out$fev1_z[1] > -10 && out$fev1_z[1] < 10)
    if (is.finite(out$fvc_z[1]))  expect_true(out$fvc_z[1]  > -10 && out$fvc_z[1]  < 10)

    # FEV1/FVC ratio should be (0,1] for positive volumes
    expect_true(is.finite(out$fev1_fvc_ratio[1]))
    expect_true(out$fev1_fvc_ratio[1] > 0 && out$fev1_fvc_ratio[1] <= 1)
  }
})

test_that("height auto-conversion (cm vs m) yields identical predictions/z-scores", {
  available_eqs <- skip_if_no_rspiro()

  df_cm <- tibble(age = 50, sex = "female", height = 162, ethnicity = "Caucasian", fev1 = 2.6, fvc = 3.2)
  df_m  <- tibble(age = 50, sex = "female", height = 1.62, ethnicity = "Caucasian", fev1 = 2.6, fvc = 3.2)

  for (eq in available_eqs) {
    out_cm <- pulmo_markers(df_cm, equation = eq)
    out_m  <- pulmo_markers(df_m,  equation = eq)
    # Predictions should match closely
    expect_equal(out_cm$fev1_pred, out_m$fev1_pred, tolerance = 1e-6)
    expect_equal(out_cm$fvc_pred,  out_m$fvc_pred,  tolerance = 1e-6)
    # z-scores may differ in floating noise only
    if (is.finite(out_cm$fev1_z[1]) && is.finite(out_m$fev1_z[1])) {
      expect_equal(out_cm$fev1_z, out_m$fev1_z, tolerance = 1e-6)
    }
    if (is.finite(out_cm$fvc_z[1]) && is.finite(out_m$fvc_z[1])) {
      expect_equal(out_cm$fvc_z, out_m$fvc_z, tolerance = 1e-6)
    }
  }
})

test_that("na_action policies: error and omit behave as expected", {
  available_eqs <- skip_if_no_rspiro()
  eq <- available_eqs[[1]]

  df_na <- tibble(
    age = 45, sex = "male", height = 170, ethnicity = "Caucasian",
    fev1 = NA_real_, fvc = 4.0
  )
  expect_error(
    suppressWarnings(
      pulmo_markers(df_na, equation = eq, na_action = "error")
    ),
    "required inputs contain missing values"
  )

  out_omit <- suppressWarnings(
    pulmo_markers(df_na, equation = eq, na_action = "omit")
  )
  expect_equal(nrow(out_omit), 0L)
})

test_that("sex and ethnicity mappings are robust to codes and strings", {
  available_eqs <- skip_if_no_rspiro()
  # GLI uses 5-level ethnicity; GLIgl ignores ethnicity. NHANES3 requires 1–3; skip it here.
  eqs_to_test <- intersect(available_eqs, c("GLI", "GLIgl"))
  skip_if(length(eqs_to_test) == 0, "No GLI/GLIgl equations available to test mappings")

  df_str <- tibble(age = 40, sex = "male",   height = 180, ethnicity = "Caucasian", fev1 = 3.5, fvc = 4.5)
  df_num <- tibble(age = 40, sex = 1,        height = 180, ethnicity = "white",     fev1 = 3.5, fvc = 4.5)
  df_fem <- tibble(age = 40, sex = "female", height = 165, ethnicity = "Other",     fev1 = 2.8, fvc = 3.6)

  for (eq in eqs_to_test) {
    out_str <- pulmo_markers(df_str, equation = eq)
    out_num <- pulmo_markers(df_num, equation = eq)
    # male string vs numeric code 1 should match
    expect_equal(out_str$fev1_pred, out_num$fev1_pred, tolerance = 1e-6)
    expect_equal(out_str$fvc_pred,  out_num$fvc_pred,  tolerance = 1e-6)

    # female runs without error and yields finite predictions
    out_fem <- pulmo_markers(df_fem, equation = eq)
    expect_true(is.finite(out_fem$fev1_pred[1]))
    expect_true(is.finite(out_fem$fvc_pred[1]))
  }
})
