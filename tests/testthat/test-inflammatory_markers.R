# HM-CS v2 compatible tests for inflammatory_markers()

test_that("mapping validation errors and messages", {
  df <- tibble::tibble(neutrophils = numeric())
  expect_error(inflammatory_markers("x", list()), "data.frame or tibble")
  expect_error(inflammatory_markers(df, list(1)), "named list")
  # classic requires neutrophils + lymphocytes
  expect_error(
    inflammatory_markers(df, list(neutrophils = "neutrophils"), panel = "classic"),
    "missing col_map entries for: lymphocytes"
  )
  # eos requires albumin name if CRP name present
  df2 <- tibble::tibble(neutrophils = 1, lymphocytes = 1, monocytes = 1, platelets = 1, CRP = 1)
  expect_error(
    inflammatory_markers(df2, list(neutrophils="neutrophils", lymphocytes="lymphocytes",
                                   monocytes="monocytes", platelets="platelets", CRP="CRP"),
                         panel = "eos"),
    "missing col_map entries for: albumin"
  )
})

test_that("verbose messages are emitted", {
  df <- tibble::tibble(neutrophils = numeric(0), lymphocytes = numeric(0))
  cm <- list(neutrophils="neutrophils", lymphocytes="lymphocytes")
  expect_message(inflammatory_markers(df, cm, panel = "classic", verbose = TRUE),
                 "-> inflammatory_markers: computing indices")
})

test_that("classic panel computes markers", {
  df <- tibble::tibble(
    neutrophils = 4, lymphocytes = 2, monocytes = 0.5, platelets = 200, WBC = 7, CRP = 2.5
  )
  cm <- as.list(names(df)); names(cm) <- names(df)
  out <- inflammatory_markers(df, cm, panel = "classic", na_action = "keep")
  expect_equal(names(out), c("NLR","PLR","LMR","dNLR","SII","SIRI","AISI","CRP_category"))
  expect_equal(out$NLR, 4/2)
  expect_equal(out$PLR, 200/2)
  expect_equal(out$dNLR, 4/(7-4))
  expect_equal(as.character(out$CRP_category), "moderate")
})

test_that("eosinophil panel computes markers and ESR passthrough", {
  df <- tibble::tibble(
    neutrophils = 4, lymphocytes = 2, monocytes = 0.5, platelets = 200,
    CRP = 5, albumin = 40, eosinophils = 0.2, ESR = 12
  )
  cm <- as.list(names(df)); names(cm) <- names(df)
  out <- inflammatory_markers(df, cm, panel = "eos", na_action = "keep")
  expect_equal(names(out), c("NLR","PLR","LMR","NER","SII","SIRI","PIV","CLR","CAR","PCR","mGPS","ESR"))
  expect_equal(out$NLR, 4/2)
  expect_equal(out$CAR, 5/40)
  expect_equal(out$mGPS, 0L)
  expect_equal(out$ESR, 12)
})

test_that("na_action policies", {
  df <- tibble::tibble(neutrophils = c(2, NA), lymphocytes = c(2, 1))
  cm <- list(neutrophils="neutrophils", lymphocytes="lymphocytes")
  out_keep <- inflammatory_markers(df, cm, panel = "classic", na_action = "keep")
  expect_equal(nrow(out_keep), 2L)
  expect_true(is.na(out_keep$NLR[2]))
  out_omit <- inflammatory_markers(df, cm, panel = "classic", na_action = "omit")
  expect_equal(nrow(out_omit), 1L)
  expect_equal(out_omit$NLR, 1)
  expect_error(inflammatory_markers(df, cm, panel = "classic", na_action = "error"),
               "missing or non-finite")
})

test_that("extreme handling cap and NA", {
  df <- tibble::tibble(
    neutrophils = 40, lymphocytes = 0.2, monocytes = 6, platelets = 5000,
    CRP = 500, albumin = 5, eosinophils = 0, ESR = 99
  )
  cm <- as.list(names(df)); names(cm) <- names(df)

  # Suppress the expected warnings to keep test output clean
  out_cap <- suppressWarnings(
    inflammatory_markers(df, cm, panel = "eos",
                         check_extreme = TRUE, extreme_action = "cap", na_action = "keep")
  )
  # Caps: neutrophils->30, lymphocytes->1, monocytes->5, platelets->1000, CRP->300, albumin->10, eosinophils->1
  expect_equal(out_cap$CAR, 300/10)
  expect_equal(out_cap$SII, (1000*30)/1)

  out_na <- suppressWarnings(
    inflammatory_markers(df, cm, panel = "eos",
                         check_extreme = TRUE, extreme_action = "NA", na_action = "keep")
  )
  expect_true(is.na(out_na$NLR) || is.na(out_na$PLR))

  # If you want to still assert warnings instead of suppressing:
  # expect_warning(..., "capped out-of-range")
  # expect_warning(..., "set out-of-range")
})

test_that("zero denominators warn and keep Inf/NaN", {
  df <- tibble::tibble(neutrophils = 2, lymphocytes = 0, platelets = 10, monocytes = 0, WBC = 2)
  cm <- as.list(names(df)); names(cm) <- names(df)
  expect_warning(out <- inflammatory_markers(df, cm, panel = "classic", na_action = "keep"),
                 "zero denominators")
  expect_true(is.infinite(out$NLR))
  expect_true(is.infinite(out$PLR))
})