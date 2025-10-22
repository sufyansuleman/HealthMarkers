# tests/testthat/test-nutrient_markers.R

library(testthat)
library(tibble)
library(HealthMarkers)

cm_id <- function(df) { m <- as.list(names(df)); names(m) <- names(df); m }

test_that("nutrient_markers computes all indices correctly", {
  df <- tibble(
    ferritin         = 100,
    transferrin_sat  = 50,
    albumin          = 40,
    total_protein    = 70,
    EPA              = 1,
    DHA              = 2,
    Mg               = 1.0,
    creatinine       = 50,
    glycated_albumin = 4,
    uric_acid        = 400,
    BUN              = 10,
    phosphate        = 1.2,
    calcium          = 2.2,
    Na               = 140,
    K                = 4,
    Cl               = 100,
    HCO3             = 24,
    Tyr              = 60,
    Phe              = 30
  )
  out <- nutrient_markers(df, col_map = cm_id(df))
  expect_s3_class(out, "tbl_df")
  expect_named(out, c(
    "FerritinTS", "AGR", "Omega3Index", "Mg_Cr_Ratio",
    "GlycatedAlbuminPct", "UA_Cr_Ratio", "BUN_Cr_Ratio",
    "Ca_x_Phosphate", "AnionGap", "Tyr_Phe_Ratio"
  ))
  expect_equal(out$FerritinTS, 100 / 50, tolerance = 1e-12)
  expect_equal(out$AGR, 40 / (70 - 40), tolerance = 1e-8)
  expect_equal(out$Omega3Index, 1 + 2, tolerance = 1e-12)
  expect_equal(out$Mg_Cr_Ratio, 1.0 / 50, tolerance = 1e-8)
  expect_equal(out$GlycatedAlbuminPct, (4 / 40) * 100, tolerance = 1e-8)
  expect_equal(out$UA_Cr_Ratio, 400 / 50, tolerance = 1e-8)
  expect_equal(out$BUN_Cr_Ratio, 10 / 50, tolerance = 1e-8)
  expect_equal(out$Ca_x_Phosphate, 2.2 * 1.2, tolerance = 1e-8)
  expect_equal(out$AnionGap, (140 + 4) - (100 + 24), tolerance = 1e-12)
  expect_equal(out$Tyr_Phe_Ratio, 60 / 30, tolerance = 1e-8)
})

test_that("nutrient_markers is vectorized over rows", {
  df <- tibble(
    ferritin         = c(80, 120),
    transferrin_sat  = c(40, 60),
    albumin          = c(42, 38),
    total_protein    = c(74, 72)
  )
  out <- nutrient_markers(df, col_map = cm_id(df))
  expect_equal(nrow(out), 2L)
  expect_equal(out$FerritinTS, c(80 / 40, 120 / 60), tolerance = 1e-8)
  expect_equal(out$AGR, c(42 / (74 - 42), 38 / (72 - 38)), tolerance = 1e-8)
  expect_true(all(is.na(out$Omega3Index)))
})

test_that("missing inputs yield NA rather than error", {
  df <- tibble(
    ferritin        = 90,
    transferrin_sat = 45,
    albumin         = 42
  )
  out <- nutrient_markers(df, col_map = cm_id(df))
  expect_equal(out$FerritinTS, 90 / 45, tolerance = 1e-8)
  expect_true(is.na(out$AGR))
  expect_true(is.na(out$Omega3Index))
  expect_true(is.na(out$Mg_Cr_Ratio))
})

test_that("verbose printing honors HM-CS verbosity options", {
  df <- tibble(ferritin = 50, transferrin_sat = 25)
  old <- getOption("healthmarkers.verbose"); on.exit(options(healthmarkers.verbose = old), add = TRUE)
  options(healthmarkers.verbose = "inform")
  expect_message(nutrient_markers(df, col_map = cm_id(df), verbose = TRUE), "-> nutrient_markers: validating inputs")
  expect_message(nutrient_markers(df, col_map = cm_id(df), verbose = TRUE), "-> nutrient_markers: computing markers")
  expect_message(nutrient_markers(df, col_map = cm_id(df), verbose = TRUE), "Completed nutrient_markers:")
})

test_that("errors if `data` is not a data.frame", {
  expect_error(nutrient_markers("not a df", col_map = list()), "data\\.frame|tibble")
})

test_that("na_action='error' aborts when used inputs contain NA", {
  df <- tibble(
    ferritin        = c(100, 90),
    transferrin_sat = c(50, NA_real_)
  )
  expect_error(
    suppressWarnings(nutrient_markers(df, col_map = cm_id(df), na_action = "error")),
    "used input columns contain missing values"
  )
})

test_that("na_action='omit' drops rows with NA in used inputs", {
  df <- tibble(
    ferritin        = c(100, 90),
    transferrin_sat = c(50, NA_real_)
  )
  old <- getOption("healthmarkers.verbose"); on.exit(options(healthmarkers.verbose = old), add = TRUE)
  options(healthmarkers.verbose = "inform")
  expect_message(
    out <- nutrient_markers(df, col_map = cm_id(df), na_action = "omit", verbose = TRUE),
    "omitting 1 rows with NA in used inputs"
  )
  expect_equal(nrow(out), 1L)
  expect_equal(out$FerritinTS, 100 / 50, tolerance = 1e-8)
})

test_that("check_extreme warns when extremes detected (warn vs cap vs error)", {
  df <- tibble(ferritin = 5000, transferrin_sat = 50)
  expect_warning(
    out_warn <- nutrient_markers(df, col_map = cm_id(df), check_extreme = TRUE, extreme_action = "warn"),
    "detected .* extreme input values \\(not altered\\)"
  )
  expect_warning(
    out_cap <- nutrient_markers(df, col_map = cm_id(df), check_extreme = TRUE, extreme_action = "cap"),
    "capped .* extreme input values into allowed ranges"
  )
  expect_false(isTRUE(all.equal(out_warn$FerritinTS, out_cap$FerritinTS)))
  expect_error(
    nutrient_markers(df, col_map = cm_id(df), check_extreme = TRUE, extreme_action = "error"),
    "detected .* extreme input values\\."
  )
})

test_that("denominator zero emits a single summary warning and yields NA outputs", {
  df <- tibble(
    Mg         = 0.9,
    uric_acid  = 300,
    BUN        = 14,
    creatinine = 0
  )
  out <- NULL
  expect_warning(
    out <- nutrient_markers(df, col_map = cm_id(df)),
    "zero denominators detected"
  )
  expect_true(all(is.na(out$Mg_Cr_Ratio)))
  expect_true(all(is.na(out$UA_Cr_Ratio)))
  expect_true(all(is.na(out$BUN_Cr_Ratio)))
})

test_that("col_map with unrecognized keys warns and ignores them", {
  df <- tibble(ferritin = 100, transferrin_sat = 50)
  cm <- list(ferritin = "ferritin", transferrin_sat = "transferrin_sat", LDL = "LDL")
  expect_warning(
    nutrient_markers(df, col_map = cm),
    "ignoring unrecognized keys in col_map"
  )
})
