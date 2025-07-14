

test_that("pulmo_markers computes expected pulmonary metrics across equations", {
  df <- tibble(
    age = 45,
    sex = "male",
    height = 170, # in cm; function will convert
    ethnicity = "Caucasian",
    fev1 = 3.0,
    fvc = 4.0
  )
  
  
  for (eq in c("GLI", "GLIgl", "NHANES3")) {
    out <- pulmo_markers(df, equation = eq, verbose = FALSE)
    
    expected_cols <- c(
      "fev1_pred", "fev1_z", "fev1_pctpred", "fev1_LLN",
      "fvc_pred", "fvc_z", "fvc_pctpred", "fvc_LLN",
      "fev1_fvc_ratio", "fev1_fvc_pred", "fev1_fvc_z", "fev1_fvc_pctpred", "fev1_fvc_LLN"
    )
    expect_true(all(expected_cols %in% names(out)))
    
    expect_type(out$fev1_pred, "double")
    expect_type(out$fev1_z, "double")
    expect_true(out$fev1_z > -5 && out$fev1_z < 5)
    
    expect_type(out$fvc_pred, "double")
    expect_type(out$fvc_z, "double")
    expect_true(out$fvc_z > -5 && out$fvc_z < 5)
    
    expect_type(out$fev1_fvc_ratio, "double")
    expect_true(out$fev1_fvc_ratio > 0 && out$fev1_fvc_ratio <= 1)
  }
})
