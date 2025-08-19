# File: R/bone_markers.R

#' Compute Bone Health & Body-Composition Markers
#'
#' Given DXA, anthropometry, and optional bone‐turnover markers, computes:
#' - **OSTA**       Osteoporosis Self-assessment Tool for Asians: (weight – age) × 0.2
#' - **ALMI**       Appendicular Lean Mass Index = ALM / height²
#' - **FMI**        Fat Mass Index = FM / height²
#' - **BMD_Tscore** BMD T-score = (BMD – ref_mean) / ref_sd
#' and (if in `col_map` + data) passes through: **TBS**, **HSA**, **PINP**, **CTX**, **BSAP**, **Osteocalcin**.
#'
#' @param data A `data.frame` or `tibble` with your subject‐level DXA/anthro data.
#' @param col_map Named list mapping **required** keys:
#'   - `age`           → age (years)
#'   - `weight`        → body weight (kg)
#'   - `height`        → height (m)
#'   - `ALM`           → appendicular lean mass (kg)
#'   - `FM`            → total fat mass (kg)
#'   - `BMD`           → bone mineral density (g/cm²)
#'   - `BMD_ref_mean`  → reference mean BMD for T-score
#'   - `BMD_ref_sd`    → reference SD BMD for T-score
#'   **Optional** keys (if present will be carried through):
#'   - `TBS`, `HSA`, `PINP`, `CTX`, `BSAP`, `Osteocalcin`
#' @param verbose Logical; if `TRUE`, prints a message.
#'
#' @return A tibble with columns
#'   `OSTA`, `ALMI`, `FMI`, `BMD_Tscore`, `TBS`, `HSA`, `PINP`, `CTX`, `BSAP`, `Osteocalcin`.
#'
#' @references
#' - Woo J *et al.* (2002). The osteoporosis self-assessment tool for Asians (OSTA). *Maturitas* 41(2):227–232.
#' - Cruz-Jentoft AJ *et al.* (2019). Sarcopenia clinical definitions. *Age Ageing* 48(1):16–31.
#' - Kelly TL *et al.* (2009). Fat mass index vs BMI. *Int J Obes* 33:783–789.
#' - World Health Organization (1994). Assessment of fracture risk and its application to screening for postmenopausal osteoporosis. *WHO Tech Rep Ser* No. 843.
#'
#' @importFrom tibble tibble
#' @export
bone_markers <- function(data, col_map, verbose = FALSE) {
  required <- c("age", "weight", "height", "ALM", "FM", "BMD", "BMD_ref_mean", "BMD_ref_sd")
  missing_map <- setdiff(required, names(col_map))
  if (length(missing_map)) {
    stop(
      "bone_markers(): missing col_map entries for: ",
      paste(missing_map, collapse = ", ")
    )
  }
  # Ensure required columns exist
  validate_inputs(data, col_map,
    fun_name      = "bone_markers",
    required_keys = required
  )
  if (verbose) message("-> computing bone markers")
  n <- nrow(data)
  # extract required
  age <- data[[col_map$age]]
  weight <- data[[col_map$weight]]
  height <- data[[col_map$height]]
  ALM <- data[[col_map$ALM]]
  FM <- data[[col_map$FM]]
  BMD <- data[[col_map$BMD]]
  ref_mean <- data[[col_map$BMD_ref_mean]]
  ref_sd <- data[[col_map$BMD_ref_sd]]
  # compute core indices
  OSTA <- (weight - age) * 0.2
  ALMI <- ALM / (height^2)
  FMI <- FM / (height^2)
  BMD_Tscore <- (BMD - ref_mean) / ref_sd
  # helper for optional pass-through
  get_opt <- function(key) {
    if (key %in% names(col_map) && col_map[[key]] %in% names(data)) {
      data[[col_map[[key]]]]
    } else {
      rep(NA_real_, n)
    }
  }
  TBS <- get_opt("TBS")
  HSA <- get_opt("HSA")
  PINP <- get_opt("PINP")
  CTX <- get_opt("CTX")
  BSAP <- get_opt("BSAP")
  Osteocalcin <- get_opt("Osteocalcin")
  tibble::tibble(
    OSTA,
    ALMI,
    FMI,
    BMD_Tscore,
    TBS,
    HSA,
    PINP,
    CTX,
    BSAP,
    Osteocalcin
  )
}
