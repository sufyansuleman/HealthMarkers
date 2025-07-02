# File: R/glycemic_markers.R

#’ Calculate glycemic‐ and C-peptide‐based insulin sensitivity/resistance markers
#’
#’ Given fasting labs and anthropometry, computes:
#’  - **SPISE**      (Single‐Point Insulin Sensitivity Estimator)
#’  - **METS_IR**    (Metabolic Score for Insulin Resistance)
#’  - **prediabetes** flag (HbA1c ≥ 42 mmol/mol)
#’  - **diabetes**    flag (HbA1c ≥ 48 mmol/mol)
#’  - **HOMA_CP**     (C-peptide‐based HOMA-IR)
#’
#’ @param data A `data.frame` or `tibble` containing at least:
#’   - `HDL_c`     (HDL cholesterol, mmol/L)
#’   - `TG`        (triglycerides, mmol/L)
#’   - `BMI`       (body mass index, kg/m²)
#’   - optionally `glucose`    (fasting glucose, mmol/L)
#’   - optionally `HbA1c`      (mmol/mol)
#’   - optionally `C_peptide`  (pmol/L), `G0` (glucose, mmol/L), `I0` (insulin, pmol/L)
#’ @param verbose Logical; if `TRUE`, prints progress messages.
#’ @return A tibble with columns:
#’   - `SPISE`
#’   - `METS_IR`
#’   - `prediabetes` (0/1)
#’   - `diabetes`    (0/1)
#’   - `HOMA_CP`     (numeric or `NA`)
#’ @importFrom tibble tibble
#’ @export
#’ @examples
#’ library(tibble)
#’ df <- tibble(
#’   HDL_c     = 1.0,
#’   TG        = 1.3,
#’   BMI       = 24,
#’   glucose   = 5.6,
#’   HbA1c     = 44,
#’   C_peptide = 300,
#’   G0        = 5.5,
#’   I0        = 60
#’ )
#’ glycemic_markers(df)
# R/glycemic_markers.R

glycemic_markers <- function(data, verbose = FALSE) {
  # no col_map, but tests expect SPISE, METS_IR, integer flags, and HOMA_CP
  req <- c("HDL_c", "TG", "BMI")
  missing <- setdiff(req, names(data))
  if (length(missing)) {
    stop("glycemic_markers(): missing required columns: ",
         paste(missing, collapse = ", "))
  }
  if (verbose)
    message("→ glycemic_markers: computing")
  SPISE    <- 600 * data$HDL_c ^ 0.185 / (data$TG ^ 0.2 * data$BMI ^ 1.338)
  METS_IR  <- if ("glucose" %in% names(data))
    log(2 * data$glucose + data$TG) * data$BMI / log(data$HDL_c)
  else
    NA_real_
  prediabetes <- if ("HbA1c" %in% names(data))
    as.integer(data$HbA1c >= 42)
  else
    NA_integer_
  diabetes    <- if ("HbA1c" %in% names(data))
    as.integer(data$HbA1c >= 48)
  else
    NA_integer_
  HOMA_CP     <- if (all(c("C_peptide", "G0") %in% names(data)))
    (data$G0 * (data$C_peptide / 6)) / 22.5
  else
    NA_real_
  tibble::tibble(SPISE, METS_IR, prediabetes, diabetes, HOMA_CP)
}
