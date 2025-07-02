library(tibble)
test_that("glycemic_markers computes SPISE, METS_IR, flags, and HOMA_CP", {
  df <- tibble(HDL_c=1, TG=1.3, BMI=24, glucose=5.5, HbA1c=44, C_peptide=300, G0=5.5, I0=60)
  out <- glycemic_markers(df)
  expect_true(all(c("SPISE","METS_IR","prediabetes","diabetes","HOMA_CP") %in% names(out)))
  expect_equal(out$prediabetes, 1L)
})
