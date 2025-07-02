# R/fasting_is.R

#’ Calculate fasting‐based insulin sensitivity indices
#’
#’ Given a data.frame with fasting glucose & insulin, compute:
#’ * Fasting_inv
#’ * Raynaud
#’ * HOMA_IR_inv
#’ * FIRI
#’ * QUICKI
#’ * Belfiore_basal
#’ * Ig_ratio_basal
#’ * Isi_basal
#’ * Bennett
#’ * HOMA_IR_rev_inv
#’
#’ @param data A data.frame or tibble containing at least two columns:
#’   * fasting glucose at time 0
#’   * fasting insulin at time 0
#’   Column names are given in `col_map`.
#’ @param col_map Named list mapping
#’   `G0` → your fasting glucose column,
#’   `I0` → your fasting insulin column
#’ @param normalize One of `c("none","z","inverse","range","robust")` — how to scale each index.
#’ @param verbose Logical; if `TRUE`, prints a progress message.
#’
#’ @return A tibble with the 10 fasting‐index columns.
#’ @importFrom tibble tibble
#’ @importFrom dplyr mutate across everything
#’ @export
#’ @examples
#’ library(dplyr)
#’
#’ df <- tibble::tibble(
#’   G0 = 5.5,    # fasting glucose (mmol/L)
#’   I0 = 60      # fasting insulin (pmol/L)
#’ )
#’
#’ fasting_is(
#’   df,
#’   col_map   = list(G0 = "G0", I0 = "I0"),
#’   normalize = "none"
#’ )
fasting_is <- function(data,
                       col_map,
                       normalize = "none",
                       verbose   = FALSE) {
  validate_inputs(data,
                  col_map,
                  fun_name      = "fasting_is",
                  required_keys = c("G0", "I0"))
  G0 <- data[[ col_map$G0 ]]
  I0 <- data[[ col_map$I0 ]]
  if (verbose) message("→ fasting_is: computing fasting indices")
  G0_mg <- G0 * 18
  I0_u  <- I0 / 6
  out <- tibble::tibble(
    Fasting_inv     = -I0_u,
    Raynaud         =  40 / I0_u,
    HOMA_IR_inv     = -((G0_mg * I0_u) / 22.5),
    FIRI            =  (G0_mg * I0_u) / 25,
    QUICKI          =  1 / (log(G0_mg) + log(I0_u)),
    Belfiore_basal  =  2 / ((I0_u * G0_mg) + 1),
    Ig_ratio_basal  = -(I0_u / G0_mg),
    Isi_basal       =  10000 / (G0_mg * I0_u),
    Bennett         =  1 / (log(I0_u) * log(G0_mg)),
    HOMA_IR_rev_inv = -((I0_u * G0_mg) / 405)
  )
  dplyr::mutate(out,
                dplyr::across(dplyr::everything(),
                              ~ HealthMarkers::normalize_vec(.x, method = normalize)
                )
  )
}
