# R/lipid_markers.R

#' Calculate lipid-panel markers, Visceral Adiposity Index (VAI),
#' Lipid Accumulation Product (LAP), and TyG–BMI index
#'
#' Given total cholesterol, HDL, TG (and optionally LDL, ApoB/ApoA1,
#' waist, BMI, glucose), computes:
#' - `non_HDL_c`, `remnant_c`
#' - `ratio_TC_HDL`, `ratio_TG_HDL`, `ratio_LDL_HDL`
#' - `ApoB_ApoA1`
#' - `VAI_Men`, `VAI_Women`
#' - `LAP_Men`, `LAP_Women`
#' - `TyG_BMI`
#'
#' @param data A `data.frame` or `tibble` containing your lipid
#'   (and optional anthropometry/glucose) data.
#' @param col_map Named list mapping:
#'   - `TC`    → total cholesterol
#'   - `HDL_c` → HDL-C
#'   - `TG`    → triglycerides
#'   - `LDL_c` → (optional) LDL-C; if missing, estimated via Friedewald
#'   - `ApoB`, `ApoA1` → (optional) apolipoproteins
#'   - `waist` → (optional) waist circumference (cm)
#'   - `BMI`   → (optional) body mass index (kg/m²)
#' @param verbose Logical; if `TRUE`, prints a message about computing markers.
#'
#' @return A tibble with:
#'   - `non_HDL_c`, `remnant_c`
#'   - `ratio_TC_HDL`, `ratio_TG_HDL`, `ratio_LDL_HDL`
#'   - `ApoB_ApoA1`
#'   - `VAI_Men`, `VAI_Women`
#'   - `LAP_Men`, `LAP_Women`
#'   - `TyG_BMI`
#'
#' @references
#' - Friedewald WT, Levy RI, Fredrickson DS. Estimation of the
#'   concentration of LDL cholesterol in plasma, without use of
#'   preparative ultracentrifuge. *Clin Chem*. 1972;18(6):499–502.
#' - Amato MC, Giordano C, Galia M, et al. Visceral adiposity index:
#'   a reliable indicator of visceral fat function associated with
#'   cardiometabolic risk. *Diabetes Care*. 2010;33(4):920–922.
#' - Kahn HS. Lipid accumulation product performs better than BMI as
#'   a marker of cardiovascular risk. *J Clin Endocrinol Metab*.
#'   2005;90(11):6292–6298.
#' - Lee YH, Kim JE, et al. Triglyceride-glucose-body mass index
#'   (TyG-BMI) predicts nonalcoholic fatty liver disease. *Int J Obes*.
#'   2020;44(9):2101–2110.
#'
#' @importFrom dplyr bind_cols
#' @importFrom tibble tibble
#' @export
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
  validate_inputs(
    data,
    col_map,
    fun_name      = "lipid_markers",
    required_keys = c("TC", "HDL_c", "TG")
  )
  if (verbose) message("-> computing lipid markers")

  TC <- data[[col_map$TC]]
  HDL <- data[[col_map$HDL_c]]
  TG <- data[[col_map$TG]]

  # LDL: measured or Friedewald
  LDL <- if (!is.null(col_map$LDL_c) &&
    col_map$LDL_c %in% names(data)) {
    data[[col_map$LDL_c]]
  } else {
    warning("lipid_markers(): estimating LDL_c via Friedewald (TC - HDL - TG/5)")
    TC - HDL - TG / 5
  }

  ApoB_ApoA1 <- if (!is.null(col_map$ApoB) &&
    !is.null(col_map$ApoA1) &&
    all(c(col_map$ApoB, col_map$ApoA1) %in% names(data))) {
    data[[col_map$ApoB]] / data[[col_map$ApoA1]]
  } else {
    NA_real_
  }

  out <- tibble::tibble(
    non_HDL_c     = TC - HDL,
    remnant_c     = TC - (HDL + LDL),
    ratio_TC_HDL  = TC / HDL,
    ratio_TG_HDL  = TG / HDL,
    ratio_LDL_HDL = LDL / HDL,
    ApoB_ApoA1    = ApoB_ApoA1
  )

  # Visceral Adiposity Index
  if (!is.null(col_map$waist) &&
    !is.null(col_map$BMI) &&
    all(c(col_map$waist, col_map$BMI) %in% names(data))) {
    W <- data[[col_map$waist]]
    BM <- data[[col_map$BMI]]
    VAI_Men <- (W / (39.68 + 1.88 * BM)) * (TG / 1.03) * (1.31 / HDL)
    VAI_Women <- (W / (36.58 + 1.89 * BM)) * (TG / 0.81) * (1.52 / HDL)
    out <- bind_cols(out, tibble::tibble(VAI_Men, VAI_Women))
  }

  # Lipid Accumulation Product
  if (!is.null(col_map$waist) && col_map$waist %in% names(data)) {
    W <- data[[col_map$waist]]
    LAP_Men <- (W - 65) * TG
    LAP_Women <- (W - 58) * TG
    out <- bind_cols(out, tibble::tibble(LAP_Men, LAP_Women))
  }

  # TyG-BMI index
  if ("glucose" %in% names(data) &&
    !is.null(col_map$BMI) &&
    col_map$BMI %in% names(data)) {
    TG_mgdl <- TG * 88.57
    Glu_mgdl <- data$glucose * 18
    TyG <- log(TG_mgdl * Glu_mgdl / 2)
    TyG_BMI <- TyG * data[[col_map$BMI]]
    out <- bind_cols(out, tibble::tibble(TyG_BMI))
  }

  out
}
