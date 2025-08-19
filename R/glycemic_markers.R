# File: R/glycemic_markers.R

#' Calculate glycemic‐, C‐peptide‐, and additional metabolic markers
#'
#' Given fasting labs and anthropometry, computes:
#'  - **SPISE**            (Single‐Point Insulin Sensitivity Estimator; Paulmichl *et al.* 2015)
#'  - **METS_IR**          (Metabolic Score for Insulin Resistance; Bello-Chavolla *et al.* 2018 )
#'  - **prediabetes** flag (HbA1c ≥ 42 mmol/mol)
#'  - **diabetes**    flag (HbA1c ≥ 48 mmol/mol)
#'  - **HOMA_CP**         (C‐peptide‐based HOMA-IR)
#'  - **LAR**             (Leptin/Adiponectin Ratio; Frühbeck *et al.* 2013)
#'  - **ASI**             (Adiponectin Sensitivity Index; Yang *et al.* 2006)
#'  - **TyG_index**       (Triglyceride-Glucose Index; Simental-Mendía *et al.* 2008)
#'
#' @param data A `data.frame` or `tibble` containing at least:
#'   - `HDL_c`       (HDL cholesterol, mmol/L)
#'   - `TG`          (triglycerides, mmol/L)
#'   - `BMI`         (body mass index, kg/m^2)
#'   - optionally `glucose`     (fasting glucose, mmol/L)
#'   - optionally `HbA1c`       (mmol/mol)
#'   - optionally `C_peptide`   (pmol/L), `G0` (glucose, mmol/L), `I0` (insulin, pmol/L)
#'   - optionally `leptin`      (ng/mL), `adiponectin` (ng/mL)
#' @param verbose Logical; if `TRUE`, prints progress messages.
#'
#' @return A tibble with columns:
#'   - `SPISE`
#'   - `METS_IR`
#'   - `prediabetes`
#'   - `diabetes`
#'   - `HOMA_CP`
#'   - `LAR`
#'   - `ASI`
#'   - `TyG_index`
#'
#' @references
#' - Paulmichl F, Schneditz D, et al. A Single-Point Insulin Sensitivity Estimator (SPISE): a novel index for assessing insulin sensitivity in clinical practice. *J Clin Endocrinol Metab*. 2015;100(5):E536–E541.
#' - Bello-Chavolla OY, Almeda-Valdés P, et al. METS-IR, a novel score to evaluate insulin sensitivity, is predictive of visceral adiposity and incident type 2 diabetes. *Eur J Endocrinol*. 2018;178(3):235–245.
#' - Frühbeck G, Gómez-Ambrosi J, et al. Role of the leptin/adiponectin ratio in insulin resistance. *Int J Obes*. 2013;37(4):510–514.
#' - Yang X, Zhang L, et al. Adiponectin Sensitivity Index (ASI): adiponectin/insulin ratio as a marker of adipocyte insulin sensitivity in type 2 diabetes. *Diabetes Res Clin Pract*. 2006;71(1):28–36.
#' - Simental-Mendía LE, Rodríguez-Morán M, Guerrero-Romero F. The triglyceride-glucose (TyG) index is a simple and accurate surrogate marker of insulin sensitivity. *Eur J Clin Invest*. 2008;38(7):331–336.
#' @importFrom tibble tibble
#' @export
#'
#' @examples
#' library(tibble)
#' df <- tibble(
#'   HDL_c       = 1.0,
#'   TG          = 1.3,
#'   BMI         = 24,
#'   glucose     = 5.6,
#'   HbA1c       = 44,
#'   C_peptide   = 300,
#'   G0          = 5.5,
#'   I0          = 60,
#'   leptin      = 10,
#'   adiponectin = 8
#' )
#' glycemic_markers(df, verbose = TRUE)
glycemic_markers <- function(data, verbose = FALSE) {
  # 1) required core columns
  req <- c("HDL_c", "TG", "BMI")
  missing <- setdiff(req, names(data))
  if (length(missing)) {
    stop(
      "glycemic_markers(): missing required columns: ",
      paste(missing, collapse = ", ")
    )
  }
  if (verbose) message("-> glycemic_markers: computing")

  # 2) core estimators
  SPISE <- 600 * data$HDL_c^0.185 / (data$TG^0.2 * data$BMI^1.338)
  METS_IR <- if ("glucose" %in% names(data)) {
    log(2 * data$glucose + data$TG) * data$BMI / log(data$HDL_c)
  } else {
    NA_real_
  }
  prediabetes <- if ("HbA1c" %in% names(data)) {
    as.integer(data$HbA1c >= 42)
  } else {
    NA_integer_
  }
  diabetes <- if ("HbA1c" %in% names(data)) {
    as.integer(data$HbA1c >= 48)
  } else {
    NA_integer_
  }
  HOMA_CP <- if (all(c("C_peptide", "G0") %in% names(data))) {
    (data$G0 * (data$C_peptide / 6)) / 22.5
  } else {
    NA_real_
  }

  # 3) Leptin/Adiponectin Ratio (LAR)
  LAR <- if (all(c("leptin", "adiponectin") %in% names(data))) {
    data$leptin / data$adiponectin
  } else {
    NA_real_
  }

  # 4) Adiponectin Sensitivity Index (ASI)
  ASI <- if (all(c("adiponectin", "I0") %in% names(data))) {
    data$adiponectin / data$I0
  } else {
    NA_real_
  }

  # 5) TyG index: ln( TG_mg/dL * Glu_mg/dL / 2 )
  TyG_index <- if ("glucose" %in% names(data)) {
    TG_mgdl <- data$TG * 88.57
    Glu_mgdl <- data$glucose * 18
    log(TG_mgdl * Glu_mgdl / 2)
  } else {
    NA_real_
  }

  # 6) assemble tibble
  tibble::tibble(
    SPISE,
    METS_IR,
    prediabetes,
    diabetes,
    HOMA_CP,
    LAR,
    ASI,
    TyG_index
  )
}
