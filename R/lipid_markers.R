# R/lipid_markers.R

#' Calculate lipid-panel markers and Visceral Adiposity Index (VAI)
#'
#' Given total cholesterol, HDL, TG (and optionally LDL, ApoB/ApoA1, waist, BMI),
#' computes:
#' - `non_HDL_c`
#' - `remnant_c`
#' - `ratio_TC_HDL`
#' - `ratio_TG_HDL`
#' - `ratio_LDL_HDL`
#' - `ApoB_ApoA1` (if both apolipoproteins present; otherwise `NA`)
#' - `VAI_Men`, `VAI_Women` (if `waist` and `BMI` provided in `col_map`)
#'
#' @param data A data.frame or tibble containing your lipid (and optional anthropometry) data.
#' @param col_map Named list mapping:
#'   - `TC`    -> total cholesterol
#'   - `HDL_c` -> HDL-C
#'   - `TG`    -> triglycerides
#'   - `LDL_c` -> (optional) LDL-C; if missing, estimated via Friedewald
#'   - `ApoB`, `ApoA1` -> (optional) apolipoproteins
#'   - `waist` -> (optional) waist circumference (cm)
#'   - `BMI`   -> (optional) body mass index (kg/m^2)
#' @param verbose Logical; if `TRUE`, prints a message about computing markers.
#'
#' @return A tibble with:
#'   - `non_HDL_c`, `remnant_c`,
#'   - `ratio_TC_HDL`, `ratio_TG_HDL`, `ratio_LDL_HDL`,
#'   - `ApoB_ApoA1`, `VAI_Men`, `VAI_Women`
#'
#' @importFrom dplyr transmute if_else
#' @importFrom tibble tibble
#' @export
#'
#' @examples
#' df <- tibble::tibble(
#'   TC    = 5.0,
#'   HDL_c = 1.0,
#'   TG    = 1.3,
#'   LDL_c = 3.0,
#'   ApoB  = 1.1,
#'   ApoA1 = 1.5,
#'   waist = 85,
#'   BMI   = 26
#' )
#' lipid_markers(df, col_map = list(
#'   TC    = "TC",
#'   HDL_c = "HDL_c",
#'   TG    = "TG",
#'   LDL_c = "LDL_c",
#'   ApoB  = "ApoB",
#'   ApoA1 = "ApoA1",
#'   waist = "waist",
#'   BMI   = "BMI"
#' ))
lipid_markers <- function(data,
                          col_map = list(
                            TC    = "TC",
                            HDL_c = "HDL_c",
                            TG    = "TG",
                            LDL_c = "LDL_c",
                            ApoB  = "ApoB",
                            ApoA1 = "ApoA1",
                            waist = NULL,
                            BMI   = NULL
                          ),
                          verbose = FALSE) {
  # core lipid inputs
  validate_inputs(data,
                  col_map,
                  fun_name      = "lipid_markers",
                  required_keys = c("TC", "HDL_c", "TG"))
  if (verbose) message("-> computing lipid markers")
  
  TC  <- data[[col_map$TC]]
  HDL <- data[[col_map$HDL_c]]
  TG  <- data[[col_map$TG]]
  
  # LDL: data or Friedewald estimate
  LDL <- if (!is.null(col_map$LDL_c) && col_map$LDL_c %in% names(data)) {
    data[[col_map$LDL_c]]
  } else {
    warning("lipid_markers(): estimating LDL_c via Friedewald (LDL = TC - HDL - TG/5)")
    TC - HDL - TG/5
  }
  
  # ApoB/ApoA1 ratio
  ApoB_ApoA1 <- if (!is.null(col_map$ApoB) &&
                    !is.null(col_map$ApoA1) &&
                    all(c(col_map$ApoB, col_map$ApoA1) %in% names(data))) {
    data[[col_map$ApoB]] / data[[col_map$ApoA1]]
  } else {
    NA_real_
  }
  
  # Prepare output list
  out <- tibble::tibble(
    non_HDL_c     = TC - HDL,
    remnant_c     = TC - (HDL + LDL),
    ratio_TC_HDL  = TC / HDL,
    ratio_TG_HDL  = TG / HDL,
    ratio_LDL_HDL = LDL / HDL,
    ApoB_ApoA1    = ApoB_ApoA1
  )
  
  # If waist & BMI provided, compute VAI
  if (!is.null(col_map$waist) &&
      !is.null(col_map$BMI) &&
      all(c(col_map$waist, col_map$BMI) %in% names(data))) {
    W  <- data[[col_map$waist]]
    BM <- data[[col_map$BMI]]
    VAI_Men   <- (W / (39.68 + 1.88 * BM)) * (TG / 1.03) * (1.31 / HDL)
    VAI_Women <- (W / (36.58 + 1.89 * BM)) * (TG / 0.81) * (1.52 / HDL)
    out <- dplyr::bind_cols(out,
                            tibble::tibble(VAI_Men = VAI_Men,
                                           VAI_Women = VAI_Women))
  }
  
  out
}
