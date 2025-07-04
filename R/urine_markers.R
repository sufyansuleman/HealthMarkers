# R/urine_markers.R

#' Calculate urine‐based renal & protein markers
#'
#' Computes:
#'  - **UACR** (Albumin-to-Creatinine Ratio, mg/g)  
#'  - **microalbuminuria** flag ("normal" vs "micro")  
#'  - **eGFR_CKD_EPI** (mL/min/1.73 m^2, race-free CKD-EPI 2021)  
#'  - **FENa** (Fractional Excretion of Sodium, %)  
#'  - **UPCR** (Urine Protein-to-Creatinine Ratio, mg/g)  
#'
#' @param data A data.frame or tibble containing at least:
#'   - `urine_albumin`     (mg/L)  
#'   - `urine_creatinine`  (mg/dL)  
#'   - `serum_creatinine`  (mg/dL)  
#'   - `plasma_Na`, `urine_Na` (mmol/L)  
#'   - `age`               (years)  
#'   - `sex`               (1 = male, 2 = female)  
#'   - optionally `urine_protein` (mg/L) for UPCR  
#' @param verbose Logical; if `TRUE`, prints progress messages.  
#'
#' @return A tibble with:
#'   `UACR`, `microalbuminuria`, `eGFR_CKD_EPI`, `FENa`, `UPCR`  
#' @export
#' @examples
#' df <- tibble::tibble(
#'   urine_albumin    = 30,  
#'   urine_creatinine = 1.2,  
#'   serum_creatinine = 0.9,  
#'   plasma_Na        = 140,  
#'   urine_Na         = 100,  
#'   age              = 55,  
#'   sex              = 2,  
#'   urine_protein    = 150
#' )
#' urine_markers(df)
urine_markers <- function(data, verbose = FALSE) {
  # 1) required columns
  req <- c(
    "urine_albumin", "urine_creatinine",
    "serum_creatinine", "plasma_Na", "urine_Na",
    "age", "sex"
  )
  missing_cols <- setdiff(req, names(data))
  if (length(missing_cols)) {
    stop("urine_markers(): missing columns: ",
         paste(missing_cols, collapse = ", "))
  }
  if (verbose) message("-> computing urine markers")
  
  # 2) UACR (mg albumin per g creatinine)
  UACR <- (data$urine_albumin / data$urine_creatinine) * 1000
  
  # 3) microalbuminuria: 30-300 mg/g
  microalbuminuria <- factor(
    ifelse(UACR >= 30 & UACR <= 300, "micro", "normal"),
    levels = c("normal","micro")
  )
  
  # 4) eGFR via race-free CKD-EPI 2021
  kappa <- ifelse(data$sex == 1, 0.9, 0.7)
  alpha <- ifelse(data$sex == 1, -0.302, -0.241)
  sex_mult <- ifelse(data$sex == 1, 1.0, 1.012)
  Scr_k <- data$serum_creatinine / kappa
  eGFR_CKD_EPI <- 142 *
    pmin(Scr_k, 1)^alpha *
    pmax(Scr_k, 1)^(-1.200) *
    (0.9938^data$age) *
    sex_mult
  
  # 5) Fractional Excretion of Sodium (FENa, %)
  FENa <- (data$urine_Na * data$serum_creatinine) /
    (data$plasma_Na * data$urine_creatinine) * 100
  
  # 6) UPCR if available (mg protein per g creatinine)
  UPCR <- if ("urine_protein" %in% names(data)) {
    (data$urine_protein / (data$urine_creatinine * 0.01))
  } else {
    rep(NA_real_, nrow(data))
  }
  
  # 7) return tibble
  tibble::tibble(
    UACR,
    microalbuminuria,
    eGFR_CKD_EPI,
    FENa,
    UPCR
  )
}
