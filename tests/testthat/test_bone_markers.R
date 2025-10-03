# File: tests/testthat/test_bone_markers.R

cm_full <- list(
  age = "age", weight = "weight", height = "height",
  ALM = "ALM", FM = "FM",
  BMD = "BMD", BMD_ref_mean = "BMD_ref_mean", BMD_ref_sd = "BMD_ref_sd",
  TBS = "TBS", HSA = "HSA", PINP = "PINP",
  CTX = "CTX", BSAP = "BSAP", Osteocalcin = "Osteocalcin"
)

df_full <- tibble(
  age = c(65, 70),
  weight = c(60, 65),
  height = c(1.6, 1.7),
  ALM = c(18, 20),
  FM = c(20, 25),
  BMD = c(0.9, 1.0),
  BMD_ref_mean = c(1.0, 1.0),
  BMD_ref_sd = c(0.1, 0.1),
  TBS = c(1.1, 1.2),
  HSA = c(0.3, 0.3),
  PINP = c(40, 50),
  CTX = c(0.3, 0.4),
  BSAP = c(12, 15),
  Osteocalcin = c(10, 12)
)

test_that("bone_markers errors if required col_map entries missing", {
  bad_map <- cm_full[-1]
  expect_error(
    bone_markers(df_full, col_map = bad_map),
    "missing col_map entries"
  )
})

test_that("bone_markers computes core indices correctly", {
  out <- bone_markers(df_full, col_map = cm_full)
  expect_equal(out$OSTA, (df_full$weight - df_full$age) * 0.2)
  expect_equal(out$ALMI, df_full$ALM / df_full$height^2)
  expect_equal(out$FMI, df_full$FM / df_full$height^2)
  expect_equal(
    out$BMD_Tscore,
    (df_full$BMD - df_full$BMD_ref_mean) / df_full$BMD_ref_sd
  )
})

test_that("optional biomarkers are passed through when present", {
  out <- bone_markers(df_full, col_map = cm_full)
  expect_equal(out$TBS, df_full$TBS)
  expect_equal(out$HSA, df_full$HSA)
  expect_equal(out$PINP, df_full$PINP)
  expect_equal(out$CTX, df_full$CTX)
  expect_equal(out$BSAP, df_full$BSAP)
  expect_equal(out$Osteocalcin, df_full$Osteocalcin)
})

test_that("missing optional biomarkers yield NA columns", {
  # drop all optional keys
  cm_core <- cm_full[setdiff(
    names(cm_full),
    c("TBS", "HSA", "PINP", "CTX", "BSAP", "Osteocalcin")
  )]
  df_core <- df_full[, names(cm_core)]
  out <- bone_markers(df_core, col_map = cm_core)
  expect_true(all(is.na(out$TBS)))
  expect_true(all(is.na(out$HSA)))
  expect_true(all(is.na(out$PINP)))
  expect_true(all(is.na(out$CTX)))
  expect_true(all(is.na(out$BSAP)))
  expect_true(all(is.na(out$Osteocalcin)))
})

test_that("verbose = TRUE prints a message", {
  expect_message(
    bone_markers(df_full, col_map = cm_full, verbose = TRUE),
    "computing bone markers"
  )
})

test_that("na_action = 'warn_zero' warns on non-finite and high NA", {
  df_bad <- df_full
  df_bad$BMD[1] <- NA_real_
  expect_warning(
    bone_markers(df_bad, col_map = cm_full, na_action = "warn_zero", na_warn_prop = 1),
    "Non-finite values found"
  )
})

test_that("na_action = 'error' stops on missing/non-finite inputs", {
  df_bad <- df_full
  df_bad$height[2] <- NA_real_
  expect_error(
    bone_markers(df_bad, col_map = cm_full, na_action = "error"),
    "Missing or non-finite values"
  )
})

test_that("argument constraints are enforced (height > 0, ref_sd > 0)", {
  df_h0 <- df_full; df_h0$height[1] <- 0
  expect_error(bone_markers(df_h0, col_map = cm_full), "height' must be positive for non-missing rows")
  df_sd0 <- df_full; df_sd0$BMD_ref_sd[1] <- 0
  expect_error(bone_markers(df_sd0, col_map = cm_full), "BMD_ref_sd' must be positive for non-missing rows")
})

test_that("SDS extremes can warn or error when enabled", {
  df_sds <- df_full
  df_sds$ALMI_sds <- c(0, 7)  # one extreme value > 6
  cm_sds <- cm_full
  cm_sds$ALMI_sds <- "ALMI_sds"
  expect_warning(
    bone_markers(df_sds, col_map = cm_sds, check_extreme_sds = TRUE, sds_limit = 6, extreme_sds_action = "warn"),
    "Extreme SDS-like values detected"
  )
  expect_error(
    bone_markers(df_sds, col_map = cm_sds, check_extreme_sds = TRUE, sds_limit = 6, extreme_sds_action = "error"),
    "Extreme SDS-like values detected"
  )
})
