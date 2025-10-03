library(testthat)
library(tibble)

cm <- list(
  creatinine        = "Cr",
  age               = "Age",
  sex               = "Sex",
  race              = "Race",
  BUN               = "BUN",
  cystatin_C        = "Cys",
  urea_serum        = "U_s",
  creatinine_urine  = "Cr_u",
  urea_urine        = "U_u",
  NGAL              = "NGAL",
  KIM1              = "KIM1",
  NAG               = "NAG",
  beta2_micro       = "B2M",
  IL18              = "IL18",
  L_FABP            = "LFBP"
)

df <- tibble(
  Cr   = 1.0,
  Age  = 40,
  Sex  = 1,
  Race = "white",
  BUN  = 14,
  Cys  = 1.0,
  U_s  = 40,
  Cr_u = 2,
  U_u  = 80,
  NGAL = 100,
  KIM1 = 20,
  NAG  = 5,
  B2M  = 0.3,
  IL18 = 50,
  LFBP = 0.1
)

test_that("renal_markers errors if required col_map entry is missing", {
  expect_error(
    renal_markers(df, list(creatinine = "Cr", age = "Age")),
    "missing col_map entries"
  )
})

test_that("renal_markers computes BUN_Cr_ratio and FE_Urea correctly", {
  out <- renal_markers(df, cm)
  expect_equal(out$BUN_Cr_ratio, 14 / 1.0)
  expect_equal(out$FE_Urea, (80 / 40) / (2 / 1.0) * 100)
})

test_that("renal_markers returns urine markers when provided", {
  out <- renal_markers(df, cm)
  expect_equal(out$NGAL, 100)
  expect_equal(out$KIM1, 20)
  expect_equal(out$NAG, 5)
  expect_equal(out$Beta2Micro, 0.3)
  expect_equal(out$IL18, 50)
  expect_equal(out$L_FABP, 0.1)
})

test_that("renal_markers computes eGFR_cr, eGFR_cys, and eGFR_combined as numerics", {
  out <- renal_markers(df, cm)
  expect_true(is.numeric(out$eGFR_cr))
  expect_true(is.numeric(out$eGFR_cys))
  expect_true(is.numeric(out$eGFR_combined))
})

test_that("verbose = TRUE prints a progress message", {
  expect_message(
    renal_markers(df, cm, verbose = TRUE),
    "-> renal_markers: computing markers"
  )
})

test_that("optional inputs produce NA when missing (when not mapped)", {
  # Use a reduced col_map that omits optional keys
  cm_req <- list(
    creatinine = "Cr",
    age = "Age",
    sex = "Sex",
    race = "Race",
    BUN = "BUN"
  )
  df2 <- df[, c("Cr", "Age", "Sex", "Race", "BUN")]
  out2 <- renal_markers(df2, cm_req)
  expect_true(all(is.na(out2$eGFR_cys)))
  expect_true(all(is.na(out2$FE_Urea)))
  expect_true(all(is.na(out2$NGAL)))
  expect_true(all(is.na(out2$KIM1)))
  expect_true(all(is.na(out2$NAG)))
  expect_true(all(is.na(out2$Beta2Micro)))
  expect_true(all(is.na(out2$IL18)))
  expect_true(all(is.na(out2$L_FABP)))
})

test_that("na_action policies: error and omit behave as expected", {
  df_na <- tibble(
    Cr = NA_real_, Age = 50, Sex = 1, Race = "white", BUN = 12,
    Cys = 1.1, U_s = 40, Cr_u = 2, U_u = 80
  )
  # error -> abort (suppress high-missingness warning)
  expect_error(
    suppressWarnings(renal_markers(df_na, cm, na_action = "error")),
    "required inputs contain missing values"
  )
  # omit -> drops row to 0
  out_omit <- suppressWarnings(renal_markers(df_na, cm, na_action = "omit"))
  expect_equal(nrow(out_omit), 0L)
})

test_that("extreme input detection and capping warn as expected", {
  df_ext <- tibble(
    Cr = 100, Age = 40, Sex = 1, Race = "white", BUN = 300, # out of default ranges
    Cys = 10, U_s = 500, Cr_u = 800, U_u = 5000
  )
  # warn: detect but do not alter
  expect_warning(
    out_warn <- renal_markers(df_ext, cm, check_extreme = TRUE, extreme_action = "warn"),
    "detected .* extreme input values \\(not altered\\)"
  )
  # cap: detect and cap
  expect_warning(
    out_cap <- renal_markers(df_ext, cm, check_extreme = TRUE, extreme_action = "cap"),
    "capped .* extreme input values into allowed ranges"
  )
  # Outputs likely differ after capping
  expect_false(isTRUE(all.equal(out_warn$eGFR_cr, out_cap$eGFR_cr)))
})

test_that("zero denominator in ratios emits a consolidated warning and yields NA", {
  df_zero <- tibble(
    Cr = 0, Age = 40, Sex = 1, Race = "white", BUN = 10,
    Cys = 1.0, U_s = 40, Cr_u = 2, U_u = 80
  )
  expect_warning(
    out_zero <- renal_markers(df_zero, cm),
    "zero denominators detected"
  )
  expect_true(is.na(out_zero$BUN_Cr_ratio))
})
