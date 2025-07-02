# R/adipo_is.R

#’ Calculate adipose‐based insulin sensitivity indices
#’
#’ Given fasting glucose & insulin plus lipid and adiposity measures,
#’ computes:
#’ * Revised_QUICKI
#’ * VAI_Men_inv
#’ * VAI_Women_inv
#’ * TG_HDL_C_inv
#’ * TyG_inv
#’ * LAP_Men_inv
#’ * LAP_Women_inv
#’ * McAuley_index
#’ * Adipo_inv
#’ * Belfiore_inv_FFA
#’
#’ @param data A data.frame or tibble containing at least the columns mapped by `col_map`.
#’ @param col_map Named list mapping:
#’   * `G0` → fasting glucose (mmol/L)
#’   * `I0` → fasting insulin (pmol/L)
#’   * `TG` → triglycerides (mmol/L)
#’   * `HDL_c` → HDL cholesterol (mmol/L)
#’   * `FFA` → free fatty acids (mmol/L)
#’   * `waist` → waist circumference (cm)
#’   * `bmi` → body‐mass index (kg/m²)
#’ @param normalize One of `c("none","z","inverse","range","robust")`—method to scale each index.
#’ @param verbose Logical; if `TRUE`, prints a progress message.
#’
#’ @return A tibble with the 10 adipose‐based index columns.
#’ @importFrom tibble tibble
#’ @importFrom dplyr mutate across everything
#’ @export
#’ @examples
#’ df <- tibble::tibble(
#’   G0    = 5.5, I0    = 60,
#’   TG    = 1.2, HDL_c = 1.0,
#’   FFA   = 0.45, waist = 80, bmi = 24
#’ )
#’ adipo_is(
#’   df,
#’   col_map = list(
#’     G0    = "G0",
#’     I0    = "I0",
#’     TG    = "TG",
#’     HDL_c = "HDL_c",
#’     FFA   = "FFA",
#’     waist = "waist",
#’     bmi   = "bmi"
#’   ),
#’   normalize = "none"
#’ )
adipo_is <- function(data,
                     col_map,
                     normalize = "none",
                     verbose   = FALSE) {
  # 0) validate
  validate_inputs(
    data,
    col_map,
    fun_name = "adipo_is",
    required_keys = c("G0", "I0", "TG", "HDL_c", "FFA", "waist", "bmi")
  )
  
  # 1) extract & convert
  G0    <- data[[col_map$G0]]    * 18      # → mg/dL
  I0    <- data[[col_map$I0]]    / 6       # → µU/mL
  TG    <- data[[col_map$TG]]    * 88.57   # → mg/dL
  HDL   <- data[[col_map$HDL_c]] * 38.67   # → mg/dL
  FFA   <- data[[col_map$FFA]]              # mmol/L
  waist <- data[[col_map$waist]]            # cm
  bmi   <- data[[col_map$bmi]]              # kg/m²
  
  if (verbose)
    message("→ adipo_is: computing adipose‐based indices")
  
  # 2) compute
  out <- tibble::tibble(
    Revised_QUICKI   = 1 / (log10(I0) + log10(G0) + log10(FFA)),
    VAI_Men_inv      = -((waist / 39.68 + 1.88 * bmi) * (TG / 1.03) * (1.31 /
                                                                         HDL)),
    VAI_Women_inv    = -((waist / 36.58 + 1.89 * bmi) * (TG / 0.81) * (1.52 /
                                                                         HDL)),
    TG_HDL_C_inv     = -(TG / HDL),
    TyG_inv          = -log(TG * G0 / 2),
    LAP_Men_inv      = -((waist - 65) * TG),
    LAP_Women_inv    = -((waist - 58) * TG),
    McAuley_index    = exp(2.63 - 0.28 * log(I0) - 0.31 * log(TG)),
    Adipo_inv        = -(FFA * I0),
    Belfiore_inv_FFA = -(2 / ((I0 * FFA) + 1))
  )
  
  # 3) normalize
  out <- dplyr::mutate(out,
                       dplyr::across(
                         dplyr::everything(),
                         ~ HealthMarkers::normalize_vec(.x, method = normalize)
                       ))
  
  out
}
