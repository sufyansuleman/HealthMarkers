# File: R/metss.R

#’ Calculate Metabolic Syndrome Severity Score (MetSSS)
#’
#’ Computes the continuous MetS severity z-score per Wiley & Carrington (2016).
#’
#’ @param data A data.frame or tibble with columns:
#’   - `waist`   (cm)
#’   - `bp_sys`  (systolic BP, mmHg)
#’   - `bp_dia`  (diastolic BP, mmHg)
#’   - `TG`      (triglycerides, mmol/L)
#’   - `HDL_c`   (HDL cholesterol, mmol/L)
#’   - `glucose` (fasting glucose, mmol/L)
#’   - `sex`     (1 = male, 2 = female)
#’   - `race`    (character/factor; one of `"NHW"`, `"NHB"`, `"HW"`, `"HA"`)
#’ @param params Named list of parameter sets by sex–race, each a list with:
#’   - `intercept`  
#’   - `waist`, `TG`, `HDL`, `glucose`, `MAP`: named numeric vectors with 
#’     `mean`, `sd`, and `coef`.  
#’   Defaults to the NHW-male set from Wiley & Carrington (2016).
#’ @param verbose Logical; if `TRUE`, prints a startup message.
#’ @return A tibble with one column, `MetSSS`, the continuous severity z-score.
#’ @importFrom tibble tibble
#’ @export
#’ @examples
#’ library(tibble)
#’ df <- tibble(
#’   waist   = 94,
#’   bp_sys  = 120,
#’   bp_dia  =  80,
#’   TG      = 1.5,
#’   HDL_c   = 1.1,
#’   glucose = 5.3,
#’   sex     = 1,
#’   race    = "NHW"
#’ )
#’ metss(df)
metss <- function(data,
                  params = list(
                    NHW_M = list(
                      intercept = -2.344,
                      waist     = c(mean = 94.0, sd = 12.4, coef = 0.846),
                      TG        = c(mean = 1.5, sd = 0.6,  coef = 0.701),
                      HDL       = c(mean = 1.1, sd = 0.3,  coef = -0.663),
                      glucose   = c(mean = 5.3, sd = 0.6,  coef = 0.658),
                      MAP       = c(mean = 97,  sd = 11,   coef = 0.466)
                    )
                  ),
                  verbose = FALSE) {
  # 1) Validate required columns
  req <- c("waist","bp_sys","bp_dia","TG","HDL_c","glucose","sex","race")
  miss <- setdiff(req, names(data))
  if (length(miss)) {
    stop("metss(): missing required columns: ", paste(miss, collapse = ", "))
  }
  if (verbose) message("→ computing MetSSS (metabolic syndrome severity score)")
  
  # 2) Build key (e.g. "NHW_M")
  key <- paste0(
    ifelse(data$race %in% c("NHW","White"), "NHW", toupper(as.character(data$race))),
    "_",
    ifelse(data$sex == 1, "M", "F")
  )
  param_row <- params[[ key[1] ]]
  
  # 3) Compute mean arterial pressure
  MAP <- (2 * data$bp_dia + data$bp_sys) / 3
  
  # 4) Standardize each component
  z_waist <- (data$waist   - param_row$waist["mean"])   / param_row$waist["sd"]
  z_TG    <- (data$TG      - param_row$TG   ["mean"])   / param_row$TG   ["sd"]
  z_HDL   <- (data$HDL_c   - param_row$HDL  ["mean"])   / param_row$HDL  ["sd"]
  z_glu   <- (data$glucose - param_row$glucose["mean"]) / param_row$glucose["sd"]
  z_MAP   <- (MAP         - param_row$MAP    ["mean"]) / param_row$MAP    ["sd"]
  
  # 5) Combine into severity score
  MetSSS <- param_row$intercept +
    param_row$waist  ["coef"] * z_waist +
    param_row$TG     ["coef"] * z_TG    +
    param_row$HDL    ["coef"] * z_HDL   +
    param_row$glucose["coef"] * z_glu   +
    param_row$MAP    ["coef"] * z_MAP
  
  # 6) Return as tibble
  tibble::tibble(MetSSS = MetSSS)
}
