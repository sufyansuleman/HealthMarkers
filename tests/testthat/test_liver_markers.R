library(testthat)
library(tibble)

test_that("liver_markers returns all seven markers and correct values", {
  df <- tibble(
    BMI           = 24,
    waist         = 80,
    triglycerides = 150, # mg/dL
    GGT           = 30,
    age           = 30,
    AST           = 25,
    ALT           = 20,
    platelets     = 250,
    albumin       = 45,
    diabetes      = FALSE,
    bilirubin     = 1.0,
    creatinine    = 0.8
  )
  cm <- as.list(names(df)); names(cm) <- names(df)
  out <- liver_markers(df, col_map = cm)

  expect_s3_class(out, "tbl_df")
  expect_named(out, c("FLI", "NFS", "APRI", "FIB4", "BARD", "ALBI", "MELD_XI"))

  # FLI
  L <- 0.953 * log(150) + 0.139 * 24 + 0.718 * log(30) + 0.053 * 80 - 15.745
  expect_equal(out$FLI, exp(L) / (1 + exp(L)) * 100, tolerance = 1e-8)

  # NFS
  expect_equal(
    out$NFS,
    -1.675 + 0.037 * 30 + 0.094 * 24 + 1.13 * 0 + 0.99 * (25 / 20) - 0.013 * 250 - 0.66 * 45,
    tolerance = 1e-8
  )

  # APRI
  expect_equal(out$APRI, (25 / 40) / 250 * 100, tolerance = 1e-8)

  # FIB-4
  expect_equal(out$FIB4, (30 * 25) / (250 * sqrt(20)), tolerance = 1e-8)

  # BARD
  expected_bard <- as.integer((24 >= 28) + (25 / 20 >= 0.8) + (0 == 1))
  expect_equal(out$BARD, expected_bard)

  # ALBI
  expect_equal(out$ALBI, log10(1.0 * 17.1) * 0.66 + 45 * (-0.0852), tolerance = 1e-8)

  # MELD_XI
  expect_equal(out$MELD_XI, 5.11 * log(1.0) + 11.76 * log(0.8) + 9.44, tolerance = 1e-8)
})

test_that("errors if mapped columns are missing in data", {
  df <- tibble(
    BMI = 24, waist = 80, triglycerides = 150, GGT = 30, age = 30,
    AST = 25, ALT = 20, platelets = 250, albumin = 45, diabetes = FALSE
    # bilirubin and creatinine missing
  )
  expect_error(
    liver_markers(df, col_map = list(
      BMI = "BMI", waist = "waist", triglycerides = "triglycerides", GGT = "GGT",
      age = "age", AST = "AST", ALT = "ALT", platelets = "platelets",
      albumin = "albumin", diabetes = "diabetes",
      bilirubin = "bilirubin", creatinine = "creatinine"
    )),
    regexp = "mapped columns not found in data|missing required columns"
  )
})

test_that("errors if required col_map entries are missing", {
  df <- tibble(
    BMI = 24, waist = 80, triglycerides = 150, GGT = 30, age = 30,
    AST = 25, ALT = 20, platelets = 250, albumin = 45, diabetes = FALSE,
    bilirubin = 1.0, creatinine = 0.8
  )
  cm <- list(
    BMI = "BMI", waist = "waist", triglycerides = "triglycerides", GGT = "GGT",
    age = "age", AST = "AST", ALT = "ALT", platelets = "platelets",
    albumin = "albumin", diabetes = "diabetes"
    # bilirubin/creatinine keys missing
  )
  expect_error(
    liver_markers(df, col_map = cm),
    regexp = "missing col_map entries|you must supply col_map entries"
  )
})

test_that("na_action='error' aborts when required inputs contain NA", {
  df <- tibble(
    BMI = 24, waist = 80, triglycerides = 150, GGT = 30, age = 30,
    AST = 25, ALT = 20, platelets = 250, albumin = 45, diabetes = FALSE,
    bilirubin = NA_real_, creatinine = 0.9
  )
  cm <- as.list(names(df)); names(cm) <- names(df)
  expect_error(
    suppressWarnings(liver_markers(df, col_map = cm, na_action = "error")),
    "required inputs contain missing values"
  )
})

test_that("na_action='omit' drops rows with NA", {
  df <- tibble(
    BMI = c(24, 24), waist = c(80, 80), triglycerides = c(150, 150), GGT = c(30, 30),
    age = c(30, 30), AST = c(25, 25), ALT = c(20, 20), platelets = c(250, NA_real_),
    albumin = c(45, 45), diabetes = c(FALSE, FALSE), bilirubin = c(1.0, 1.0), creatinine = c(0.9, 0.9)
  )
  cm <- as.list(names(df)); names(cm) <- names(df)
  out <- suppressWarnings(liver_markers(df, col_map = cm, na_action = "omit"))
  expect_equal(nrow(out), 1L)
})

test_that("na_action='keep' propagates NA to outputs", {
  df <- tibble(
    BMI = 24, waist = 80, triglycerides = 150, GGT = 30, age = 30,
    AST = 25, ALT = 20, platelets = 250, albumin = 45, diabetes = FALSE,
    bilirubin = NA_real_, creatinine = 0.9
  )
  cm <- as.list(names(df)); names(cm) <- names(df)
  out <- suppressWarnings(liver_markers(df, col_map = cm, na_action = "keep"))
  expect_true(is.na(out$ALBI))
  expect_true(is.na(out$MELD_XI))
})

test_that("check_extreme='cap' caps out-of-range values and warns", {
  df <- tibble(
    BMI = 80,              # cap to 70
    waist = 300,           # cap to 200
    triglycerides = 5000,  # cap to 1500
    GGT = 5000,            # cap to 2000
    age = 150,             # cap to 120
    AST = 6000,            # cap to 5000
    ALT = 6000,            # cap to 5000
    platelets = 5,         # cap to 10
    albumin = 5,           # cap to 15
    diabetes = FALSE,
    bilirubin = 0.05,      # cap to 0.1
    creatinine = 50        # cap to 20
  )
  cm <- as.list(names(df)); names(cm) <- names(df)
  expect_warning(
    out <- liver_markers(df, col_map = cm, check_extreme = TRUE, extreme_action = "cap"),
    "capped .* extreme input values"
  )
  expect_s3_class(out, "tbl_df")
  expect_equal(ncol(out), 7L)
})

test_that("check_extreme='error' aborts on out-of-range", {
  df <- tibble(
    BMI = 80, waist = 300, triglycerides = 5000, GGT = 5000, age = 150,
    AST = 6000, ALT = 6000, platelets = 5, albumin = 5, diabetes = FALSE,
    bilirubin = 0.05, creatinine = 50
  )
  cm <- as.list(names(df)); names(cm) <- names(df)
  expect_error(
    liver_markers(df, col_map = cm, check_extreme = TRUE, extreme_action = "error"),
    "detected .* extreme input values"
  )
})

test_that("extreme_action='warn' emits detection warning without capping", {
  # Avoid unrelated log() warnings by keeping positives where not needed
  df <- tibble(
    BMI = 5, waist = 30, triglycerides = 2, GGT = 1,  # GGT>0 to avoid log warning
    age = 10, AST = 1, ALT = 1,                       # keep >0 to avoid extra transforms
    platelets = 5, albumin = 5, diabetes = FALSE,
    bilirubin = 0.05, creatinine = 0.1
  )
  cm <- as.list(names(df)); names(cm) <- names(df)

  expect_warning(
    liver_markers(df, col_map = cm, check_extreme = TRUE, extreme_action = "warn"),
    "detected .* extreme input values \\(not altered\\)"
  )
})

test_that("extreme_action='ignore' does not warn on extremes", {
  df <- tibble(
    BMI = 80, waist = 300, triglycerides = 5000, GGT = 5000, age = 150,
    AST = 6000, ALT = 6000, platelets = 5, albumin = 5, diabetes = FALSE,
    bilirubin = 0.05, creatinine = 50
  )
  cm <- as.list(names(df)); names(cm) <- names(df)

  expect_warning(
    liver_markers(df, col_map = cm, check_extreme = TRUE, extreme_action = "ignore"),
    NA
  )
})

test_that("invalid extreme_rules aborts with informative error", {
  df <- tibble(
    BMI = 24, waist = 80, triglycerides = 150, GGT = 30, age = 30,
    AST = 25, ALT = 20, platelets = 250, albumin = 45, diabetes = FALSE,
    bilirubin = 1.0, creatinine = 0.9
  )
  cm <- as.list(names(df)); names(cm) <- names(df)

  bad_rules <- list(ALT = c(5, 1)) # min > max
  expect_error(
    liver_markers(df, col_map = cm, extreme_rules = bad_rules),
    "extreme_rules\\[\\['ALT'\\]\\]` must be numeric length-2 with min <= max\\."
  )
})

test_that("denominator and transform warnings fire (isolated tests)", {
  # Zero platelets -> denominator warning for APRI/FIB4
  df1 <- tibble(
    BMI = 24, waist = 80, triglycerides = 150, GGT = 30, age = 30,
    AST = 25, ALT = 20, platelets = 0, albumin = 45, diabetes = FALSE,
    bilirubin = 1.0, creatinine = 0.9
  )
  cm1 <- as.list(names(df1)); names(cm1) <- names(df1)
  expect_warning(liver_markers(df1, col_map = cm1), "zero denominators detected")

  # Non-positive for log: triglycerides
  df2 <- tibble(
    BMI = 24, waist = 80, triglycerides = 0, GGT = 30, age = 30,
    AST = 25, ALT = 20, platelets = 250, albumin = 45, diabetes = FALSE,
    bilirubin = 1.0, creatinine = 0.9
  )
  cm2 <- as.list(names(df2)); names(cm2) <- names(df2)
  expect_warning(liver_markers(df2, col_map = cm2), "triglycerides.*log\\(\\) undefined")

  # ALT negative -> target only the sqrt() warning; muffle other warnings
  df3 <- tibble(
    BMI = 24, waist = 80, triglycerides = 150, GGT = 30, age = 30,
    AST = 25, ALT = -1, platelets = 250, albumin = 45, diabetes = FALSE,
    bilirubin = 1.0, creatinine = 0.9
  )
  cm3 <- as.list(names(df3)); names(cm3) <- names(df3)
  expect_warning(
    withCallingHandlers(
      liver_markers(df3, col_map = cm3),
      # Muffle base numeric warning from sqrt()
      warning = function(w) {
        if (identical(conditionMessage(w), "NaNs produced")) invokeRestart("muffleWarning")
      },
      # Allow only our targeted rlang warning through
      rlang_warning = function(w) {
        msg <- conditionMessage(w)
        if (grepl("sqrt\\(\\) undefined", msg)) return(invisible(NULL))
        invokeRestart("muffleWarning")
      }
    ),
    "sqrt\\(\\) undefined"
  )
})

test_that("diabetes='1' as character does not warn and BARD is computed as 3", {
  df <- tibble(
    BMI = 30, waist = 90, triglycerides = 150, GGT = 30, age = 50,
    AST = 40, ALT = 30, platelets = 250, albumin = 45, diabetes = "1",
    bilirubin = 1.0, creatinine = 1.0
  )
  cm <- as.list(names(df)); names(cm) <- names(df)

  expect_warning(
    out <- liver_markers(df, col_map = cm),
    NA
  )
  expect_equal(out$BARD, 3L)
})

test_that("non-binary diabetes values warn about coercion and propagate NA into BARD", {
  df <- tibble(
    BMI = 30, waist = 90, triglycerides = 150, GGT = 30, age = 50,
    AST = 40, ALT = 30, platelets = 250, albumin = 45, diabetes = "yes",
    bilirubin = 1.0, creatinine = 1.0
  )
  cm <- as.list(names(df)); names(cm) <- names(df)

  expect_warning(
    out <- withCallingHandlers(
      liver_markers(df, col_map = cm),
      warning = function(w) {
        if (grepl("^NAs introduced by coercion$", conditionMessage(w))) invokeRestart("muffleWarning")
      }
    ),
    "diabetes.*coercing"
  )
  expect_true(is.na(out$BARD))
})

test_that("validating inputs message appears when verbose is TRUE", {
  df <- tibble(
    BMI = 24, waist = 80, triglycerides = 150, GGT = 30, age = 30,
    AST = 25, ALT = 20, platelets = 250, albumin = 45, diabetes = FALSE,
    bilirubin = 1.0, creatinine = 0.9
  )
  cm <- as.list(names(df)); names(cm) <- names(df)

  expect_message(
    liver_markers(df, col_map = cm, verbose = TRUE),
    "-> liver_markers: validating inputs"
  )
})

test_that("verbose summary reports Inf counts for zero denominators", {
  df <- tibble(
    BMI = 24, waist = 80, triglycerides = 150, GGT = 30, age = 30,
    AST = 25, ALT = 20, platelets = 0, albumin = 45, diabetes = FALSE,
    bilirubin = 1.0, creatinine = 0.9
  )
  cm <- as.list(names(df)); names(cm) <- names(df)

  expect_message(
    suppressWarnings(liver_markers(df, col_map = cm, verbose = TRUE)),
    "APRI=1, FIB4=1"
  )
})
