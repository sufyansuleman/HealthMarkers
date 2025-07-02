# R/calc_cardio_advanced.R

#’ Calculate advanced cardiovascular risk scores & indices
#’
#’ Computes:
#’  • 10-year ASCVD risk via Pooled Cohort Equations  
#’  • Pulse Pressure (PP) and Pulse-Pressure Index (PPI = PP/SBP)  
#’  • Atherogenic Index of Plasma (AIP = log10[TG/HDL])  
#’  • Castelli Risk I (TC/HDL) & II (LDL/HDL)  
#’
#’ @param data A data.frame/tibble with at least:
#’   - age (years), sex (0=female/1=male), race (“white”/“black”/“other”)  
#’   - total_chol, HDL_c, LDL_c, TG (mg/dL or mmol/L; see units)  
#’   - sbp (mmHg), bp_treated (logical), smoker (logical), diabetes (logical)  
#’ @param units One of c("mgdl","mmoll"); if "mmoll", will convert to mg/dL internally  
#’ @param verbose Logical; if TRUE, messages progress  
#’ @return A tibble with columns:
#’   - PCE_risk            (10-year ASCVD %, PCE2013)  
#’   - PP                  (pulse pressure, mmHg)  
#’   - PPI                 (PP / SBP)  
#’   - AIP                 (log10[TG/HDL])  
#’   - Castelli_I          (TC/HDL)  
#’   - Castelli_II         (LDL/HDL)  
#’ @importFrom PooledCohort pce
#’ @importFrom dplyr transmute if_else
#’ @export
cardio_advance <- function(data,
                                 units   = c("mgdl","mmoll"),
                                 verbose = FALSE) {
  units <- match.arg(units)
  if (verbose) message("→ calc_cardio_advanced")
  
  # validate
  req <- c("age","sex","race","total_chol","HDL_c","LDL_c","TG",
           "sbp","bp_treated","smoker","diabetes")
  miss <- setdiff(req, names(data))
  if (length(miss)) stop("calc_cardio_advanced: missing columns: ",
                         paste(miss, collapse = ", "))
  
  df <- data
  
  # convert mmol/L → mg/dL if needed
  if (units=="mmoll") {
    df <- df %>%
      mutate(
        total_chol = total_chol * 38.67,
        HDL_c      = HDL_c      * 38.67,
        LDL_c      = LDL_c      * 38.67,
        TG         = TG         * 88.57
      )
  }
  
  # call PCE (ACC/AHA 2013) for 10-yr ASCVD risk
  pce_risk <- PooledCohort::pce(
    age        = df$age,
    female     = !as.logical(df$sex),
    black      = (df$race=="black"),
    totChol    = df$total_chol,
    hdlChol    = df$HDL_c,
    sysBP      = df$sbp,
    treatBP    = as.logical(df$bp_treated),
    smoker     = as.logical(df$smoker),
    diabetes   = as.logical(df$diabetes)
  )
  
  # pulse pressure
  PP  <- df$sbp - df$dbp
  PPI <- PP / df$sbp
  
  # lipid ratios
  AIP         <- log10(df$TG / df$HDL_c)
  Castelli_I  <- df$total_chol / df$HDL_c
  Castelli_II <- df$LDL_c / df$HDL_c
  
  tibble::tibble(
    PCE_risk       = pce_risk,
    PP             = PP,
    PPI            = PPI,
    AIP            = AIP,
    Castelli_I     = Castelli_I,
    Castelli_II    = Castelli_II
  )
}
