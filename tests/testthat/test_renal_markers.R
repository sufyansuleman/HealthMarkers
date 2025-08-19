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

test_that("renal_markers computes eGFR_cr and eGFR_cys", {
  out <- renal_markers(df, cm)
  # For Cr=1, age=40, male white:
  # eGFR_cr = 141*(1/0.9)^-1.209*(1/0.9)^(-0.411)?*0.993^40
  expect_true(is.numeric(out$eGFR_cr))
  expect_true(is.numeric(out$eGFR_cys))
  expect_true(is.numeric(out$eGFR_combined))
})

test_that("verbose = TRUE prints a progress message", {
  expect_message(
    renal_markers(df, cm, verbose = TRUE),
    "-> computing renal markers"
  )
})

test_that("optional inputs produce NA when missing", {
  df2 <- df[, c("Cr", "Age", "Sex", "Race", "BUN")]
  out2 <- renal_markers(df2, cm)
  expect_true(all(is.na(out2$eGFR_cys)))
  expect_true(all(is.na(out2$FE_Urea)))
  expect_true(all(is.na(out2$NGAL)))
})
