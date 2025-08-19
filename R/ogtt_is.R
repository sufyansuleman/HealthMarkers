# R/ogtt_is.R

#' Calculate OGTT‐based insulin sensitivity indices
#'
#' Given glucose & insulin at 0, 30, 120 min (plus weight, BMI, age, sex),
#' computes:
#' * Isi_120
#' * Cederholm_index
#' * Gutt_index
#' * Avignon_Si0
#' * Avignon_Si120
#' * Avignon_Sim
#' * Modified_stumvoll
#' * Stumvoll_Demographics
#' * Matsuda_AUC
#' * Matsuda_ISI
#' * BigttSi
#' * Ifc_inv
#' * HIRI_inv
#' * Belfiore_isi_gly
#'
#' @param data A data.frame or tibble containing at least the columns mapped by `col_map`.
#' @param col_map Named list mapping:
#'   * `G0`, `G30`, `G120` -> glucose at 0, 30, 120 min (mmol/L)
#'   * `I0`, `I30`, `I120` -> insulin at 0, 30, 120 min (pmol/L)
#'   * `weight` -> body weight (kg)
#'   * `bmi` -> body‐mass index (kg/m^2)
#'   * `age` -> age (years)
#'   * `sex` -> sex (1 = male, 2 = female)
#' @param normalize One of `c("none","z","inverse","range","robust")` - method to scale each index.
#' @param verbose Logical; if `TRUE`, prints a progress message.
#'
#' @return A tibble with the OGTT‐based index columns.
#' @importFrom tibble tibble
#' @importFrom dplyr mutate across everything
#' @export
#' @examples
#' df <- tibble::tibble(
#'   G0 = 5.5, I0 = 60,
#'   G30 = 7.8, I30 = 90,
#'   G120 = 6.2, I120 = 50,
#'   weight = 70, bmi = 24, age = 30, sex = 1
#' )
#' ogtt_is(
#'   df,
#'   col_map = list(
#'     G0 = "G0", I0 = "I0",
#'     G30 = "G30", I30 = "I30",
#'     G120 = "G120", I120 = "I120",
#'     weight = "weight", bmi = "bmi",
#'     age = "age", sex = "sex"
#'   ),
#'   normalize = "none"
#' )
ogtt_is <- function(data,
                    col_map,
                    normalize = "none",
                    verbose = FALSE) {
  validate_inputs(
    data, col_map,
    fun_name = "ogtt_is",
    required_keys = c("G0", "I0", "G30", "I30", "G120", "I120", "weight", "bmi", "age", "sex")
  )

  # 1) Extract & convert raw inputs
  G0 <- data[[col_map$G0]] * 18 # mmol/L -> mg/dL
  G30 <- data[[col_map$G30]] * 18
  G120 <- data[[col_map$G120]] * 18
  I0 <- data[[col_map$I0]] / 6 # pmol/L  -> µU/mL
  I30 <- data[[col_map$I30]] / 6
  I120 <- data[[col_map$I120]] / 6

  wt <- data[[col_map$weight]]
  bmi <- data[[col_map$bmi]]
  age <- data[[col_map$age]]
  sex <- data[[col_map$sex]]

  if (verbose) {
    cat("-> ogtt_is: computing OGTT indices\n")
  }


  # 2) Areas under curve & means
  I_AUC <- 0.5 * ((I0 + I30) * 30 + (I30 + I120) * 90)
  G_AUC <- 0.5 * ((G0 + G30) * 30 + (G30 + G120) * 90)
  I_mean <- rowMeans(cbind(I0, I30, I120), na.rm = TRUE)
  G_mean <- rowMeans(cbind(G0, G30, G120), na.rm = TRUE)

  # 3) Compute indices
  out <- tibble::tibble(
    Isi_120 = 10000 / (G120 * I120),
    Cederholm_index = (75000 + (G0 - G120) * 1.15 * 180 * 0.19 * wt) /
      (120 * ((G0 + G120) / 2) * log(I0 + I120)),
    Gutt_index = (75000 + (G0 - G120) * 0.19 * wt) /
      (120 * ((G0 + G120) / 2) * log((I0 + I120) / 2)),
    Avignon_Si0 = 1e8 / ((G0 * I0) * wt * 150),
    Avignon_Si120 = 1e8 / ((G120 * I120) * wt * 150),
    Avignon_Sim = (Avignon_Si0 + Avignon_Si120) / 2,
    Modified_stumvoll = 0.156 -
      0.0000459 * data[[col_map$I120]] -
      0.000321 * data[[col_map$I0]] -
      0.00541 * data[[col_map$G120]],
    Stumvoll_Demographics = 0.222 -
      0.00333 * bmi -
      0.0000779 * data[[col_map$I120]] -
      0.000422 * age,
    Matsuda_AUC = 10000 / sqrt(G0 * I0 * G_AUC * I_AUC),
    Matsuda_ISI = 10000 / sqrt(G0 * I0 * G_mean * I_mean),
    BigttSi = exp(
      4.90 -
        0.00402 * data[[col_map$I0]] -
        0.000565 * data[[col_map$I30]] -
        0.00127 * data[[col_map$I120]] -
        0.152 * data[[col_map$G0]] -
        0.00871 * data[[col_map$G30]] -
        0.0373 * data[[col_map$G120]] -
        ifelse(sex == 1, 0.145, 0) -
        0.0376 * bmi
    ),
    Ifc_inv = -log(data[[col_map$I120]] / data[[col_map$I0]]),
    HIRI_inv = -(((G0 + G30) / 2) / 100 * ((I0 + I30) / 2)),
    Belfiore_isi_gly = 2 / ((I_AUC * G_AUC) + 1)
  )

  # 4) Normalize if requested
  out <- dplyr::mutate(
    out,
    dplyr::across(
      dplyr::everything(),
      ~ HealthMarkers::normalize_vec(.x, method = normalize)
    )
  )

  out
}
