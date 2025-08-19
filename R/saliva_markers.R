# R/saliva_markers.R

#' Calculate saliva‐based stress & glycemic markers
#'
#' Computes:
#'  - **log_cortisol_wake** (log‐transformed waking cortisol)
#'  - **CAR_AUC**           (Cortisol Awakening Response, trapezoidal AUC over 0-60 min)
#'  - **log_amylase**       (log‐transformed salivary α-amylase)
#'  - **saliva_glucose**    (raw salivary glucose)
#'
#' @param data A data.frame or tibble containing at least:
#'   - `saliva_cort1` (nmol/L at wake)
#'   - `saliva_cort2` (nmol/L ~30 min)
#'   - `saliva_cort3` (nmol/L ~60 min)
#'   - `saliva_amylase` (U/mL)
#'   - `saliva_glucose` (mg/dL)
#' @param verbose Logical; if `TRUE`, prints progress messages.
#'
#' @return A tibble with columns:
#'   - `log_cortisol_wake`
#'   - `CAR_AUC`
#'   - `log_amylase`
#'   - `saliva_glucose`
#' @export
#' @examples
#' df <- tibble::tibble(
#'   saliva_cort1    = 12.5,
#'   saliva_cort2    = 18.0,
#'   saliva_cort3    = 16.2,
#'   saliva_amylase  = 85,
#'   saliva_glucose  = 4.2
#' )
#' saliva_markers(df)
saliva_markers <- function(data, verbose = FALSE) {
  # 1) validate required columns
  req <- c(
    "saliva_cort1",
    "saliva_cort2",
    "saliva_cort3",
    "saliva_amylase",
    "saliva_glucose"
  )
  missing_cols <- setdiff(req, names(data))
  if (length(missing_cols)) {
    stop(
      "saliva_markers(): missing columns: ",
      paste(missing_cols, collapse = ", ")
    )
  }
  if (verbose) {
    message("-> computing saliva markers")
  }

  # 2) log‐transform waking cortisol and amylase
  log_cortisol_wake <- log(data$saliva_cort1)
  log_amylase <- log(data$saliva_amylase)

  # 3) trapezoidal AUC for cortisol over 0, 30, and 60 minutes
  times <- c(0, 30, 60)
  cort_mat <- cbind(data$saliva_cort1, data$saliva_cort2, data$saliva_cort3)
  CAR_AUC <- apply(cort_mat, 1, function(x) {
    sum((x[-length(x)] + x[-1]) / 2 * diff(times))
  })

  # 4) assemble output
  tibble::tibble(log_cortisol_wake,
    CAR_AUC,
    log_amylase,
    saliva_glucose = data$saliva_glucose
  )
}
