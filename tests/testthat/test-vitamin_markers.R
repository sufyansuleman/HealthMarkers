library(testthat)
library(tibble)

cm_id <- function(df) { m <- as.list(names(df)); names(m) <- names(df); m }

make_full_df <- function(n = 1) {
  tibble(
    VitD = rep(50, n), VitD_ref_mean = rep(40, n), VitD_ref_sd = rep(5, n),
    B12 = rep(300, n), Folate = rep(15, n),
    Ferritin = rep(100, n), TSat = rep(0.25, n),
    Cortisol = rep(200, n), DHEAS = rep(100, n),
    Testosterone = rep(12, n), Estradiol = rep(120, n),
    TSH = rep(2, n), free_T4 = rep(14, n),
    Retinol = rep(0.8, n), Retinol_ref_mean = rep(0.9, n), Retinol_ref_sd = rep(0.2, n),
    Tocopherol = rep(30, n), Total_lipids = rep(3, n),
    PIVKA_II = rep(5, n),
    VitC = rep(60, n),
    Homocysteine = rep(10, n),
    MMA = rep(0.3, n),
    Magnesium = rep(0.8, n), Zinc = rep(15, n), Copper = rep(15, n)
  )
}

test_that("errors if `data` is not a data.frame", {
  expect_error(
    vitamin_markers("not a df", col_map = list()),
    "must be a data\\.frame or tibble"
  )
})

test_that("errors if `col_map` is not a named list", {
  df <- make_full_df()
  expect_error(
    vitamin_markers(df, col_map = c(VitD = "VitD")),
    "`col_map` must be a named list"
  )
})

test_that("errors when mapped columns are missing in data", {
  df <- tibble(VitD = 50, VitD_ref_mean = 40, VitD_ref_sd = 5)
  req_keys <- c(
    "VitD","VitD_ref_mean","VitD_ref_sd","B12","Folate","Ferritin","TSat",
    "Cortisol","DHEAS","Testosterone","Estradiol","TSH","free_T4",
    "Retinol","Retinol_ref_mean","Retinol_ref_sd","Tocopherol","Total_lipids",
    "PIVKA_II","VitC","Homocysteine","MMA","Magnesium","Zinc","Copper"
  )
  cm <- as.list(req_keys); names(cm) <- req_keys
  expect_error(
    vitamin_markers(df, col_map = cm),
    "missing required columns in `data`"
  )
})

test_that("numeric coercion warns when NAs introduced and propagates to outputs", {
  df <- make_full_df(2)
  df$B12 <- c("300", "oops")
  df$TSat <- c("0.25", "0.25")
  cm <- cm_id(df)
  expect_warning(
    out <- vitamin_markers(df, col_map = cm),
    "coerced to numeric; NAs introduced"
  )
  expect_equal(out$Ferr_TSat_R, c(100 / 0.25, 100 / 0.25), tolerance = 1e-8)
  expect_equal(out$B12_Fol_Ratio, c(300 / 15, NA_real_))
})

test_that("na_action='omit' with all rows dropped returns empty tibble with expected columns", {
  df <- make_full_df(2)
  df$VitD <- c(NA_real_, NA_real_)
  cm <- cm_id(df)
  out <- suppressWarnings(vitamin_markers(df, col_map = cm, na_action = "omit"))
  expect_s3_class(out, "tbl_df")
  expect_equal(nrow(out), 0L)
  expect_true(all(c(
    "VitD_Z","B12_Fol_Ratio","Ferr_TSat_R","Cort_DHEA_R","T_E2_Ratio",
    "TSH_fT4_R","Retinol_Z","Toco_Lip_R","PIVKA_II","VitC",
    "Homocysteine","MMA","Mg_Zn_R","Cu_Zn_R"
  ) %in% names(out)))
})

test_that("verbose prints validating, computing, and completion summary with counters", {
  df <- make_full_df(1)
  cm <- cm_id(df)
  old <- getOption("healthmarkers.verbose"); on.exit(options(healthmarkers.verbose = old), add = TRUE)
  options(healthmarkers.verbose = "inform")
  expect_message(vitamin_markers(df, col_map = cm, verbose = TRUE), "-> vitamin_markers: validating inputs")
  expect_message(vitamin_markers(df, col_map = cm, verbose = TRUE), "-> vitamin_markers: computing markers")
  expect_message(vitamin_markers(df, col_map = cm, verbose = TRUE), "Completed vitamin_markers:")
  expect_message(vitamin_markers(df, col_map = cm, verbose = TRUE), "capped=")
  expect_message(vitamin_markers(df, col_map = cm, verbose = TRUE), "denom_zero=")
})

test_that("extreme detection: ignore=no warning (unchanged), warn warns (unchanged), cap warns (changed)", {
  base <- make_full_df(1)
  df <- base
  df$VitD <- 1000
  df$Ferritin <- 5000
  cm <- cm_id(df)

  out0 <- vitamin_markers(base, col_map = cm_id(base))
  expect_warning(
    out_ignore <- vitamin_markers(df, col_map = cm, check_extreme = TRUE, extreme_action = "ignore"),
    NA
  )
  expect_equal(out_ignore$VitD_Z, (1000 - 40) / 5, tolerance = 1e-8)

  expect_warning(
    out_warn <- vitamin_markers(df, col_map = cm, check_extreme = TRUE, extreme_action = "warn"),
    "extreme input values"
  )
  expect_equal(out_warn, out_ignore, tolerance = 1e-12)

  expect_warning(
    out_cap <- vitamin_markers(df, col_map = cm, check_extreme = TRUE, extreme_action = "cap"),
    "capped .* extreme input values into allowed ranges"
  )
  expect_false(isTRUE(all.equal(out_cap, out_warn)))
  expect_false(isTRUE(all.equal(out0, out_warn)))
})
