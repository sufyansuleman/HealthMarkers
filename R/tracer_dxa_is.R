# R/tracer_dxa_is.R

#’ Calculate tracer/DXA‐based insulin sensitivity indices
#’
#’ Given fasting insulin, tracer rates, fat mass, weight, HDL, and BMI,
#’ computes:
#’ * LIRI_inv
#’ * Lipo_inv
#’ * ATIRI_inv
#’
#’ @param data A data.frame or tibble containing at least the columns mapped in `col_map`.
#’ @param col_map Named list mapping:
#’   * `I0` → fasting insulin (pmol/L)
#’   * `rate_palmitate` → palmitate tracer rate
#’   * `rate_glycerol` → glycerol tracer rate
#’   * `fat_mass` → fat mass (kg)
#’   * `weight` → body weight (kg)
#’   * `HDL_c` → HDL cholesterol (mmol/L)
#’   * `bmi` → body‐mass index (kg/m²)
#’ @param normalize One of `c("none","z","inverse","range","robust")`—method to scale each index.
#’ @param verbose Logical; if TRUE, prints a progress message.
#’
#’ @return A tibble with the three tracer/DXA‐based index columns.
#’ @importFrom tibble tibble
#’ @importFrom dplyr mutate across everything
#’ @export
#’ @examples
#’ df <- tibble::tibble(
#’   I0              = 60,
#’   rate_palmitate  = 1.5,
#’   rate_glycerol   = 2.0,
#’   fat_mass        = 20,
#’   weight          = 70,
#’   HDL_c           = 1.0,
#’   bmi             = 24
#’ )
#’ tracer_dxa_is(
#’   df,
#’   col_map = list(
#’     I0             = "I0",
#’     rate_palmitate = "rate_palmitate",
#’     rate_glycerol  = "rate_glycerol",
#’     fat_mass       = "fat_mass",
#’     weight         = "weight",
#’     HDL_c          = "HDL_c",
#’     bmi            = "bmi"
#’   ),
#’   normalize = "none"
#’ )
tracer_dxa_is <- function(data,
                          col_map,
                          normalize = "none",
                          verbose   = FALSE) {
  # 0) validate that col_map is fully specified and data has those cols
  validate_inputs(
    data,
    col_map,
    fun_name = "tracer_dxa_is",
    required_keys = c(
      "I0",
      "rate_palmitate",
      "rate_glycerol",
      "fat_mass",
      "weight",
      "HDL_c",
      "bmi"
    )
  )
  
  # 1) Extract & convert
  I0_u    <- data[[col_map$I0]]             / 6      # pmol/L → µU/mL
  Ra_pal  <- data[[col_map$rate_palmitate]]         # tracer rate
  Ra_gly  <- data[[col_map$rate_glycerol]]          # tracer rate
  FM      <- data[[col_map$fat_mass]]               # kg
  wt      <- data[[col_map$weight]]                 # kg
  HDL_mg  <- data[[col_map$HDL_c]]  * 38.67         # mmol/L → mg/dL
  bmi_val <- data[[col_map$bmi]]                    # kg/m²
  
  if (verbose)
    message("→ tracer_dxa_is: computing tracer/DXA indices")
  
  # 2) Raw index calculations
  LIRI_inv  <- -1 * (
    -0.091 +
      0.4   * log10(((I0_u + I0_u) / 2) * 6) +
      0.346 * log10((FM / wt) * 100) -
      0.408 * log10(HDL_mg) +
      0.435 * log10(bmi_val)
  )
  Lipo_inv  <- -(Ra_gly * I0_u)
  ATIRI_inv <- -(Ra_pal * I0_u)
  
  out <- tibble::tibble(LIRI_inv  = LIRI_inv,
                        Lipo_inv  = Lipo_inv,
                        ATIRI_inv = ATIRI_inv)
  
  # 3) Normalize if requested
  out <- dplyr::mutate(out,
                       dplyr::across(
                         dplyr::everything(),
                         ~ HealthMarkers::normalize_vec(.x, method = normalize)
                       ))
  
  out
}
