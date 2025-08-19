# R/liver_markers.R

#' Calculate a panel of blood-based liver markers
#'
#' Given routine labs and anthropometry, computes:
#' * **FLI**      — Fatty Liver Index (Bedogni et al. 2006)
#' * **NFS**      — NAFLD Fibrosis Score (Angulo et al. 2007)
#' * **APRI**     — AST-to-Platelet Ratio Index
#' * **FIB4**     — Fibrosis-4 Index
#' * **BARD**     — BMI-AST/ALT-Diabetes score
#' * **ALBI**     — Albumin-Bilirubin score
#' * **MELD_XI**  — MELD excluding INR
#'
#' @param data A data.frame or tibble containing your liver & anthropometry data.
#' @param col_map Named list mapping these keys → your column names:
#'   - `BMI`        → body mass index (kg/m²)
#'   - `waist`      → waist circumference (cm)
#'   - `triglycerides` → fasting TG (mg/dL)
#'   - `GGT`        → gamma-GT (U/L)
#'   - `age`        → age (years)
#'   - `AST`        → aspartate aminotransferase (U/L)
#'   - `ALT`        → alanine aminotransferase (U/L)
#'   - `platelets`  → platelet count (10⁹/L)
#'   - `albumin`    → serum albumin (g/L)
#'   - `diabetes`   → diabetes status (0/1 or logical)
#'   - `bilirubin`  → total bilirubin (mg/dL)
#'   - `creatinine` → serum creatinine (mg/dL)
#' @param verbose Logical; if `TRUE`, prints a message when running.
#'
#' @return A tibble with one column per marker:
#'   `FLI`, `NFS`, `APRI`, `FIB4`, `BARD`, `ALBI`, `MELD_XI`.
#' @importFrom dplyr transmute
#' @export
#'
#' @examples
#' df <- tibble::tibble(
#'   BMI           = 24,
#'   waist         = 80,
#'   triglycerides = 150, # mg/dL
#'   GGT           = 30,
#'   age           = 30,
#'   AST           = 25,
#'   ALT           = 20,
#'   platelets     = 250,
#'   albumin       = 45,
#'   diabetes      = FALSE,
#'   bilirubin     = 1.0,
#'   creatinine    = 0.8
#' )
#' liver_markers(df)
liver_markers <- function(data,
                          col_map = list(
                            BMI           = "BMI",
                            waist         = "waist",
                            triglycerides = "triglycerides",
                            GGT           = "GGT",
                            age           = "age",
                            AST           = "AST",
                            ALT           = "ALT",
                            platelets     = "platelets",
                            albumin       = "albumin",
                            diabetes      = "diabetes",
                            bilirubin     = "bilirubin",
                            creatinine    = "creatinine"
                          ),
                          verbose = FALSE) {
  # 0) check col_map entries
  missing_map <- names(col_map)[vapply(col_map, is.null, logical(1))]
  if (length(missing_map)) {
    stop(
      "liver_markers(): must supply col_map entries for: ",
      paste(missing_map, collapse = ", ")
    )
  }
  # 1) validate data columns
  validate_inputs(data, col_map, fun_name = "liver_markers")
  if (verbose) message("-> computing liver markers")

  # 2) pull out each vector
  BMI <- data[[col_map$BMI]]
  waist <- data[[col_map$waist]]
  TG <- data[[col_map$triglycerides]]
  GGT <- data[[col_map$GGT]]
  age <- data[[col_map$age]]
  AST <- data[[col_map$AST]]
  ALT <- data[[col_map$ALT]]
  platelets <- data[[col_map$platelets]]
  albumin <- data[[col_map$albumin]]
  diabetes <- as.integer(data[[col_map$diabetes]])
  bilirubin <- data[[col_map$bilirubin]]
  creatinine <- data[[col_map$creatinine]]

  # 3) compute each marker
  ## Fatty Liver Index (FLI)
  L <- 0.953 * log(TG) +
    0.139 * BMI +
    0.718 * log(GGT) +
    0.053 * waist -
    15.745
  FLI <- exp(L) / (1 + exp(L)) * 100

  ## NAFLD Fibrosis Score (NFS, Angulo et al. 2007)
  NFS <- -1.675 +
    0.037 * age +
    0.094 * BMI +
    1.13 * diabetes +
    0.99 * (AST / ALT) -
    0.013 * platelets -
    0.66 * albumin

  ## APRI (AST-to-Platelet Ratio Index)
  APRI <- (AST / 40) / platelets * 100

  ## FIB-4 Index
  FIB4 <- (age * AST) / (platelets * sqrt(ALT))

  ## BARD Score
  BARD <- as.integer((BMI >= 28) +
    (AST / ALT >= 0.8) +
    (diabetes == 1))

  ## ALBI Score
  bili_umol <- bilirubin * 17.1
  ALBI <- log10(bili_umol) * 0.66 + albumin * -0.0852

  ## MELD-XI Score
  MELD_XI <- 5.11 * log(bilirubin) +
    11.76 * log(creatinine) +
    9.44

  # 4) return results
  tibble::tibble(
    FLI     = FLI,
    NFS     = NFS,
    APRI    = APRI,
    FIB4    = FIB4,
    BARD    = BARD,
    ALBI    = ALBI,
    MELD_XI = MELD_XI
  )
}
