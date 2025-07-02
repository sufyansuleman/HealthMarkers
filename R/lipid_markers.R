# R/lipid_markers.R

#’ Calculate lipid–panel markers
#’
#’ Given total cholesterol, HDL, TG (and optionally LDL, ApoB/ApoA1),
#’ computes:
#’  • non_HDL_c
#’  • remnant_c
#’  • ratio_TC_HDL
#’  • ratio_TG_HDL
#’  • ratio_LDL_HDL
#’  • ApoB_ApoA1 (if both apolipoproteins are in `data`)
#’
#’ @param data A data.frame or tibble containing your lipid data.
#’ @param col_map Named list mapping the keys below → your column names:
#’   - `TC`    → total cholesterol
#’   - `HDL_c` → HDL‐C
#’   - `TG`    → triglycerides
#’   - `LDL_c` → (optional) LDL‐C; if missing, estimated via Friedewald
#’   - `ApoB`, `ApoA1` → (optional) apolipoproteins
#’ @param verbose Logical; if `TRUE`, prints a message about computing lipid markers.
#’
#’ @return A tibble with:
#’   - `non_HDL_c`, `remnant_c`,
#’   - `ratio_TC_HDL`, `ratio_TG_HDL`, `ratio_LDL_HDL`,
#’   - `ApoB_ApoA1` (if both `ApoB` and `ApoA1` are present; otherwise `NA`)
#’ @importFrom dplyr transmute if_else
#’ @importFrom tibble tibble
#’ @export
#’ @examples
#’ df <- tibble::tibble(
#’   TC    = 5.0,
#’   HDL_c = 1.0,
#’   TG    = 1.3,
#’   LDL_c = 3.0,
#’   ApoB  = 1.1,
#’   ApoA1 = 1.5
#’ )
#’ lipid_markers(df, col_map = list(
#’   TC="TC", HDL_c="HDL_c", TG="TG",
#’   LDL_c="LDL_c", ApoB="ApoB", ApoA1="ApoA1"
#’ ))
lipid_markers <- function(data,
                          col_map = list(
                            TC = "TC",
                            HDL_c = "HDL_c",
                            TG = "TG",
                            LDL_c = NULL,
                            ApoB = NULL,
                            ApoA1 = NULL
                          ),
                          verbose = FALSE) {
  validate_inputs(data,
                  col_map,
                  fun_name = "lipid_markers",
                  required_keys = c("TC", "HDL_c", "TG"))
  if (verbose)
    message("→ computing lipid markers")
  
  # pull out lipids
  TC  <- data[[col_map$TC]]
  HDL <- data[[col_map$HDL_c]]
  TG  <- data[[col_map$TG]]
  
  # LDL: from data or Friedewald (assumes mg/dL units)
  if (!is.null(col_map$LDL_c) && col_map$LDL_c %in% names(data)) {
    LDL <- data[[col_map$LDL_c]]
  } else {
    warning("lipid_markers(): estimating LDL_c via Friedewald (LDL = TC - HDL - TG/5)")
    LDL <- TC - HDL - TG / 5
  }
  
  # apolipoprotein ratio if available
  has_apo <- !is.null(col_map$ApoB) &&
    !is.null(col_map$ApoA1) &&
    all(c(col_map$ApoB, col_map$ApoA1) %in% names(data))
  
  tibble::tibble(
    non_HDL_c     = TC - HDL,
    remnant_c     = TC - (HDL + LDL),
    ratio_TC_HDL  = TC / HDL,
    ratio_TG_HDL  = TG / HDL,
    ratio_LDL_HDL = LDL / HDL,
    ApoB_ApoA1    = if (has_apo) {
      data[[col_map$ApoB]] / data[[col_map$ApoA1]]
    } else {
      NA_real_
    }
  )
}
