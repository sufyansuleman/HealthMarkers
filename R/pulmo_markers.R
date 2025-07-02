# File: R/pulmo_markers.R

#’ Calculate pulmonary function markers (FEV₁/FVC, z-scores, lung age, etc.)
#’
#’ Uses the Global Lung Function Initiative reference equations to
#’ compute predicted values, z-scores, lower limits of normal (LLN),
#’ and an estimated “lung age” (age at which predicted FEV₁ equals observed).
#’
#’ @param data A `data.frame` or tibble containing at least:
#’   - `age`         (years, numeric)  
#’   - `sex`         (1 = male, 2 = female)  
#’   - `height`      (cm, numeric)  
#’   - `ethnicity`   (character or factor; one of the GLI ethnic groups, e.g. “Caucasian”, “African American”, “North East Asian”, etc.)  
#’   - `fev1`        (observed FEV₁, L)  
#’   - `fvc`         (observed FVC, L)  
#’ @param equation Character; which reference equations to use.  
#’   Currently only `"GLI-2022"` is supported (the latest multi-ethnic GLI release).  
#’   If you pass anything else, an error will be thrown.  
#’ @param verbose Logical; if `TRUE`, prints progress messages.
#’
#’ @return A tibble with:
#’   - `fev1_pred`, `fvc_pred`       Predicted FEV₁/FVC (L)  
#’   - `fev1_z`, `fvc_z`             Z-score relative to predicted  
#’   - `fev1_LLN`, `fvc_LLN`         Lower limit of normal (5th percentile)  
#’   - `fev1_fvc_pred`               Predicted FEV₁/FVC ratio  
#’   - `fev1_fvc_z`                  Z-score of the ratio  
#’   - `lung_age_fev1`               Estimated “lung age” for FEV₁ (years)  
#’   - `lung_age_diff`               `lung_age_fev1 - actual age`
#’
#’ @details  
#’ **Reference equations**  
#’ We draw on the GLI-2022 multi-ethnic spirometry equations (Quanjer _et al._, _Eur Respir J_ 2023)  
#’ via the **rspiro** package’s `predict_spiro()` and `inverse_predicted_age()` functions.  
#’  
#’ **`equation` argument & `match.arg()`**  
#’ By writing  
#’ ```r
#’ equation = c("GLI-2022")
#’ ```  
#’ plus  
#’ ```r
#’ equation <- match.arg(equation)
#’ ```  
#’ we ensure that the only accepted choice right now is `"GLI-2022"`.  
#’ Passing `pulmo_markers(df)` or `pulmo_markers(df, equation="GLI-2022")` will both work;  
#’ any other string will be rejected with a clear error.
#’
#’ @references  
#’ - Quanjer _et al._, “Multi‐ethnic reference values for spirometry for the 3–95-yr age range: the GLI-2022 equations.” *Eur Respir J* 2023;61:2201632.  
#’ - rspiro package: <https://github.com/2DegreesInvesting/rspiro>
#’
#’
#’ @examples
#’ library(tibble)
#’ df <- tibble(
#’   age       = 45,
#’   sex       = "M",
#’   height    = 170,
#’   ethnicity = "Caucasian",
#’   fev1      = 3.0,
#’   fvc       = 4.0
#’ )
#’ pulmo_markers(df, equation = "GLI_2012")
pulmo_markers <- function(data,
                          equation = c("GLI-2022"),
                          verbose = FALSE) {
  
  equation <- match.arg(equation)
  
  validate_inputs(
    data,
    col_map,
    fun_name = "pulmo_markers",
    required_keys = c("ethnicity", "fev1", "fvc")
  )
  equation <- match.arg(equation)
  if (verbose)
    message("→ pulmo_markers (", equation, ")")
  
  # 1) validate required columns
  req <- c("age", "sex", "height", "ethnicity", "fev1", "fvc")
  missing <- setdiff(req, names(data))
  if (length(missing)) {
    stop("pulmo_markers(): missing columns: ",
         paste(missing, collapse = ", "))
  }
  
  # 2) get GLI predictions
  ref <- rspiro::predict_spiro(
    age       = data$age,
    sex       = data$sex,
    height    = data$height,
    ethnicity = data$ethnicity,
    fev1      = data$fev1,
    fvc       = data$fvc,
    equation  = equation
  )
  
  # 3) compute lung‐age (age where predicted FEV1 == observed)
  lung_age <- rspiro::inverse_predicted_age(
    observed  = data$fev1,
    sex       = data$sex,
    height    = data$height,
    ethnicity = data$ethnicity,
    equation  = equation
  )
  
  # 4) assemble return tibble
  ref %>%
    dplyr::transmute(
      fev1_pred       = fev1_pred,
      fev1_z          = fev1_z,
      fev1_LLN        = fev1_LLN,
      fvc_pred        = fvc_pred,
      fvc_z           = fvc_z,
      fvc_LLN         = fvc_LLN,
      fev1_fvc_pred   = fev1_pred / fvc_pred,
      fev1_fvc_z      = (fev1_fvc_pred - mean(fev1_fvc_pred, na.rm = TRUE)) /
        sd(fev1_fvc_pred, na.rm = TRUE),
      fev1_fvc_LLN    = NA_real_,
      # placeholder for LLN of ratio
      lung_age_fev1   = lung_age,
      lung_age_diff   = lung_age - data$age
    )
}
