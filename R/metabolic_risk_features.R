# R/metabolic_risk_features.R

#' Calculate metabolic risk feature flags
#'
#' Given routine labs and z‐scores, computes four key binary risk markers:
#'  - dyslipidemia  
#'  - insulin_resistance  
#'  - hyperglycemia  
#'  - hypertension  
#'
#' @param data A data.frame or tibble containing at least:
#'   - `chol_total`, `chol_ldl`, `chol_hdl`, `triglycerides` (numeric)  
#'   - `age_year`     (numeric, years)  
#'   - `z_HOMA`       (numeric, standardized HOMA‐IR)  
#'   - `glucose`      (numeric, mmol/L)  
#'   - `HbA1c`        (numeric, mmol/mol)  
#'   - `bp_sys_z`, `bp_dia_z` (numeric, systolic/diastolic BP z‐scores)  
#'
#' @return A tibble with one column per marker-each a factor with levels `0`/`1`.
#' @importFrom dplyr transmute if_else
#' @export
#' @examples
#' df <- tibble::tibble(
#'   chol_total   = 6.0,
#'   chol_ldl     = 3.5,
#'   chol_hdl     = 1.0,
#'   triglycerides= 1.2,
#'   age_year     = 25,
#'   z_HOMA       = 1.5,
#'   glucose      = 5.8,
#'   HbA1c        = 40,
#'   bp_sys_z     = 1.7,
#'   bp_dia_z     = 1.0
#' )
#' metabolic_risk_features(df)
metabolic_risk_features <- function(data) {
  required_cols <- c(
    "chol_total", "chol_ldl", "chol_hdl", "triglycerides",
    "age_year", "z_HOMA", "glucose", "HbA1c",
    "bp_sys_z", "bp_dia_z"
  )
  missing_cols <- setdiff(required_cols, names(data))
  if (length(missing_cols)) {
    stop(
      "metabolic_risk_features(): missing required columns: ",
      paste(missing_cols, collapse = ", ")
    )
  }
  
  dplyr::transmute(
    data,
    
    dyslipidemia = factor(
      dplyr::if_else(
        chol_total    > 5.2 |
          chol_ldl      > 3.4 |
          chol_hdl      < 1.0 |
          (triglycerides > 1.1 & age_year %in% 0:9)  |
          (triglycerides > 1.5 & age_year %in% 10:19),
        1L, 0L
      ),
      levels = c(0L, 1L)
    ),
    
    insulin_resistance = factor(
      dplyr::if_else(z_HOMA > 1.28, 1L, 0L),
      levels = c(0L, 1L)
    ),
    
    hyperglycemia = factor(
      dplyr::if_else(
        (glucose > 5.6 & glucose < 6.9) |
          (HbA1c   > 39  & HbA1c   < 47),
        1L, 0L
      ),
      levels = c(0L, 1L)
    ),
    
    hypertension = factor(
      dplyr::if_else(
        bp_sys_z > 1.64 | bp_dia_z > 1.64,
        1L, 0L
      ),
      levels = c(0L, 1L)
    )
  )
}
