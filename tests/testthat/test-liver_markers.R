library(tibble)
test_that("liver_markers returns all seven markers", {
  df <- tibble(
    BMI=24, waist=80, triglycerides=1.2, GGT=30,
    age=30, AST=25, ALT=20, platelets=250, albumin=45, diabetes=FALSE,
    bilirubin=1.0, creatinine=0.8
  )
  out <- liver_markers(df)
  expect_equal(names(out), c("FLI","NFS","APRI","FIB4","BARD","ALBI","MELD_XI"))
})
