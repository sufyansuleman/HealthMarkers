test_that("liver_markers returns all seven markers and correct values", {
  df <- tibble(
    BMI           = 24,
    waist         = 80,
    triglycerides = 1.2, # mg/dL
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
  out <- liver_markers(df,
    col_map = list(
      BMI           = "BMI",
      waist         = "waist",
      triglycerides = "triglycerides",
      GGT           = "GGT",
      age           = "age",
      AST           = "AST",
      ALT           = "ALT",
      platelets     = "platelets",
      albumin       = "albumin",
      diabetes      = "diabetes",
      bilirubin     = "bilirubin",
      creatinine    = "creatinine"
    )
  )

  # names
  expect_named(out, c("FLI", "NFS", "APRI", "FIB4", "BARD", "ALBI", "MELD_XI"))

  # FLI:   exp(L)/(1+exp(L))*100 where L = 0.953*log(1.2) + 0.139*24 + 0.718*log(30) + 0.053*80 - 15.745
  L <- 0.953 * log(1.2) + 0.139 * 24 + 0.718 * log(30) + 0.053 * 80 - 15.745
  expect_equal(out$FLI, exp(L) / (1 + exp(L)) * 100, tolerance = 1e-8)

  # NFS: -1.675 + 0.037*age + 0.094*BMI + 1.13*diabetes + 0.99*(AST/ALT) - 0.013*platelets - 0.66*albumin
  expect_equal(out$NFS,
    -1.675 + 0.037 * 30 + 0.094 * 24 + 1.13 * 0 + 0.99 * (25 / 20) - 0.013 * 250 - 0.66 * 45,
    tolerance = 1e-8
  )

  # APRI: (AST/40)/platelets*100
  expect_equal(out$APRI, (25 / 40) / 250 * 100, tolerance = 1e-8)

  # FIB-4: (age*AST)/(platelets*sqrt(ALT))
  expect_equal(out$FIB4, (30 * 25) / (250 * sqrt(20)), tolerance = 1e-8)

  # BARD: sum(BMI>=28, AST/ALT>=0.8, diabetes) as integer
  expected_bard <- as.integer((24 >= 28) + (25 / 20 >= 0.8) + (0 == 1))
  expect_equal(out$BARD, expected_bard)

  # ALBI: log10(bilirubin*17.1)*0.66 + albumin*(-0.0852)
  expect_equal(out$ALBI, log10(1.0 * 17.1) * 0.66 + 45 * (-0.0852), tolerance = 1e-8)

  # MELD_XI: 5.11*log(bilirubin) + 11.76*log(creatinine) + 9.44
  expect_equal(out$MELD_XI, 5.11 * log(1.0) + 11.76 * log(0.8) + 9.44, tolerance = 1e-8)
})
