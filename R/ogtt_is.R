# R/ogtt_is.R

#' Calculate OGTT-based insulin sensitivity indices
#'
#' Given glucose & insulin at 0, 30, 120 min (plus weight, BMI, age, sex),
#' computes:
#' - Isi_120
#' - Cederholm_index
#' - Gutt_index
#' - Avignon_Si0
#' - Avignon_Si120
#' - Avignon_Sim
#' - Modified_stumvoll
#' - Stumvoll_Demographics
#' - Matsuda_AUC
#' - Matsuda_ISI
#' - BigttSi
#' - Ifc_inv
#' - HIRI_inv
#' - Belfiore_isi_gly
#'
#' Units assumed:
#' - OGTT glucose in mmol/L (internally converted to mg/dL via *18 for select indices)
#' - OGTT insulin in pmol/L (internally converted to muU/mL via /6 for select indices)
#' - weight in kg; BMI in kg/m^2; age in years; sex coded 1 = male, 2 = female
#'
#' Notes
#' - Conversions mirror existing implementation to preserve outputs. Some
#'   formulas intentionally use unconverted inputs (as in prior code).
#' - Logs are safe: log(x) becomes NA when x <= 0 or non-finite.
#'
#' @param data A data.frame or tibble containing at least the columns mapped by `col_map`.
#' @param col_map Named list mapping:
#'   - G0, G30, G120 -> glucose at 0, 30, 120 min (mmol/L)
#'   - I0, I30, I120 -> insulin at 0, 30, 120 min (pmol/L)
#'   - weight -> body weight (kg)
#'   - bmi -> body-mass index (kg/m^2)
#'   - age -> age (years)
#'   - sex -> sex (1 = male, 2 = female)
#' @param normalize One of c("none","z","inverse","range","robust") used by normalize_vec().
#' @param verbose Logical; if TRUE, prints progress messages via hm_inform().
#' @param na_action One of c("keep","omit","error") for missing/non-finite required inputs. Default "keep".
#' @param na_warn_prop Proportion (0-1) for high-missingness diagnostics (debug). Default 0.2.
#' @param check_extreme Logical; if TRUE, scan outputs for |value| > extreme_limit. Default FALSE.
#' @param extreme_limit Positive numeric magnitude threshold for extremes. Default 1e3.
#' @param extreme_action One of c("warn","cap","error","ignore","NA") when extremes detected. Default "warn".
#'
#' @return A tibble with the OGTT-based index columns listed above.
#' @importFrom tibble tibble
#' @importFrom dplyr mutate across everything
#' @export
#'
#' @seealso [fasting_is()], [normalize_vec()]
#'
#' @references
#'  Original derivations of OGTT- and fasting-based indices
#'  Matsuda M, DeFronzo RA. Insulin sensitivity indices obtained from oral glucose tolerance testing. 
#'   Diabetes Care. 1999;22(9):1462-1470. \doi{10.2337/diacare.22.9.1462} (Matsuda index, ISI-OGTT)
#'  Gutt M, Davis CL, Spitzer SB, et al. Validation of the insulin sensitivity index (ISI0,120) derived from oral glucose tolerance testing. 
#'   Diabetes Res Clin Pract. 2000;47(3):177-184. \doi{10.1016/S0168-8227(99)00116-3} (Gutt index)
#'  Stumvoll M, Mitrakou A, Pimenta W, et al. Use of the oral glucose tolerance test to assess insulin release and sensitivity. 
#'   Diabetes Care. 2000;23(3):295-301. \doi{10.2337/diacare.23.3.295} (Stumvoll indices)
#'  Hansen T, Drivsholm T, Urhammer SA, et al. The BIGTT test: a novel test for simultaneous measurement of pancreatic beta-cell function, insulin sensitivity, and glucose tolerance. 
#'   Diabetes Care. 2007;30(2):257-262. \doi{10.2337/dc06-1240} (BIGTT-Si index)
#'  Avignon A, Charles MA, Rabasa-Lhoret R, et al. Assessment of insulin sensitivity from oral glucose tolerance test in normal subjects and in insulin-resistant patients. 
#'   Int J Obes Relat Metab Disord. 1999;23(5):512-517. \doi{10.1038/sj.ijo.0800860} (Avignon index)
#'  Belfiore F, Iannello S, Volpicelli G. Insulin sensitivity indices calculated from fasting plasma insulin and glucose concentrations. 
#'   Mol Genet Metab. 1998;63(2):134-141. \doi{10.1006/mgme.1997.2719} (Fasting indices from glycemia-insulinemia)
#'  Matthews DR, Hosker JP, Rudenski AS, Naylor BA, Treacher DF, Turner RC. Homeostasis model assessment: insulin resistance and beta-cell function from fasting plasma glucose and insulin concentrations in man. 
#'   Diabetologia. 1985;28(7):412-419. \doi{10.1007/BF00280883} (HOMA-IR, HOMA-beta)
#'
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
#'   normalize = "none",
#'   verbose = TRUE
#' )
ogtt_is <- function(data,
                    col_map,
                    normalize = "none",
                    verbose = FALSE,
                    na_action = c("keep","omit","error"),
                    na_warn_prop = 0.2,
                    check_extreme = FALSE,
                    extreme_limit = 1e3,
                    extreme_action = c("warn","cap","error","ignore","NA")) {
  na_action <- match.arg(na_action)
  extreme_action <- match.arg(extreme_action)
  .ogtt_validate_args(normalize, na_warn_prop, check_extreme, extreme_limit, verbose)

  # HM-CS v2: required keys validation
  hm_validate_inputs(
    data, col_map,
    required_keys = c("G0", "I0", "G30", "I30", "G120", "I120", "weight", "bmi", "age", "sex"),
    fn = "ogtt_is"
  )

  if (isTRUE(verbose)) hm_inform(level = "inform", msg = "-> ogtt_is: validating and preparing inputs")

  # Coerce required columns to numeric if needed (warn on NAs introduced)
  keys_to_check <- c("G0","I0","G30","I30","G120","I120","weight","bmi","age","sex")
  for (k in keys_to_check) {
    cn <- col_map[[k]]
    if (!is.numeric(data[[cn]])) {
      old <- data[[cn]]
      suppressWarnings(new <- as.numeric(old))
      introduced_na <- sum(is.na(new) & !is.na(old))
      if (introduced_na > 0L) rlang::warn(sprintf("ogtt_is(): column '%s' coerced to numeric; NAs introduced: %d", cn, introduced_na))
      data[[cn]] <- new
    }
    data[[cn]][!is.finite(data[[cn]])] <- NA_real_
  }

  # High-missingness diagnostics (debug verbosity)
  .ogtt_high_missing_diag(
    data,
    vars = unname(unlist(col_map[keys_to_check], use.names = FALSE)),
    na_warn_prop = na_warn_prop
  )

  # NA policy on required inputs
  used_cols <- unname(unlist(col_map[keys_to_check], use.names = FALSE))
  if (na_action == "error") {
    has_na <- Reduce(`|`, lapply(used_cols, function(cn) is.na(data[[cn]])))
    if (any(has_na)) {
      rlang::abort("ogtt_is(): required inputs contain missing values (na_action='error').",
                   class = "healthmarkers_ogtt_is_error_missing_values")
    }
  } else if (na_action == "omit" && length(used_cols)) {
    keep <- !Reduce(`|`, lapply(used_cols, function(cn) is.na(data[[cn]])))
    if (isTRUE(verbose)) hm_inform(level = "inform", msg = sprintf("-> ogtt_is: omitting %d rows with NA in required inputs", sum(!keep)))
    data <- data[keep, , drop = FALSE]
  }

  if (isTRUE(verbose)) hm_inform(level = "debug", msg = "-> converting units (glucose mmol/L -> mg/dL; insulin pmol/L -> muU/mL)")

  # 1) Extract & convert raw inputs
  G0   <- data[[col_map$G0]]   * 18 # mg/dL
  G30  <- data[[col_map$G30]]  * 18
  G120 <- data[[col_map$G120]] * 18
  I0   <- data[[col_map$I0]]   / 6  # muU/mL
  I30  <- data[[col_map$I30]]  / 6
  I120 <- data[[col_map$I120]] / 6

  wt   <- data[[col_map$weight]]
  bmi  <- data[[col_map$bmi]]
  age  <- data[[col_map$age]]
  sex  <- data[[col_map$sex]]

  if (isTRUE(verbose)) hm_inform(level = "inform", msg = "-> computing indices")

  # Helpers: safe log and safe division
  lg <- function(x) .ogtt_log(x)
  dv <- function(a, b) .ogtt_safe_div(a, b)

  # 2) Areas under curve & means (trapezoid with intervals 0-30-120)
  I_AUC <- 0.5 * ((I0 + I30) * 30 + (I30 + I120) * 90)
  G_AUC <- 0.5 * ((G0 + G30) * 30 + (G30 + G120) * 90)
  I_mean <- rowMeans(cbind(I0, I30, I120), na.rm = TRUE)
  G_mean <- rowMeans(cbind(G0, G30, G120), na.rm = TRUE)

  # 3) Compute indices
  out <- tibble::tibble(
    Isi_120 = dv(10000, (G120 * I120)),
    Cederholm_index = dv(75000 + (G0 - G120) * 1.15 * 180 * 0.19 * wt,
                         120 * ((G0 + G120) / 2) * lg(I0 + I120)),
    Gutt_index = dv(75000 + (G0 - G120) * 0.19 * wt,
                    120 * ((G0 + G120) / 2) * lg((I0 + I120) / 2)),
    Avignon_Si0   = dv(1e8, (G0 * I0)   * wt * 150),
    Avignon_Si120 = dv(1e8, (G120 * I120) * wt * 150),
    Avignon_Sim   = (Avignon_Si0 + Avignon_Si120) / 2,
    Modified_stumvoll = 0.156 -
      0.0000459 * data[[col_map$I120]] -
      0.000321  * data[[col_map$I0]]   -
      0.00541   * data[[col_map$G120]],
    Stumvoll_Demographics = 0.222 -
      0.00333   * bmi -
      0.0000779 * data[[col_map$I120]] -
      0.000422  * age,
    Matsuda_AUC = dv(10000, sqrt(G0 * I0 * G_AUC * I_AUC)),
    Matsuda_ISI = dv(10000, sqrt(G0 * I0 * G_mean * I_mean)),
    BigttSi = exp(
      4.90 -
        0.00402 * data[[col_map$I0]]   -
        0.000565* data[[col_map$I30]]  -
        0.00127 * data[[col_map$I120]] -
        0.152   * data[[col_map$G0]]   -
        0.00871 * data[[col_map$G30]]  -
        0.0373  * data[[col_map$G120]] -
        ifelse(sex == 1, 0.145, 0) -
        0.0376  * bmi
    ),
    Ifc_inv = -lg(dv(data[[col_map$I120]], data[[col_map$I0]])),
    HIRI_inv = -(((G0 + G30) / 2) / 100 * ((I0 + I30) / 2)),
    Belfiore_isi_gly = dv(2, (I_AUC * G_AUC) + 1)
  )

  # 4) Optional extremes check/cap on output magnitudes
  extreme_count <- 0L
  if (isTRUE(check_extreme)) {
    vals <- as.matrix(out)
    is_ext <- is.finite(vals) & abs(vals) > extreme_limit
    extreme_count <- sum(is_ext, na.rm = TRUE)
    if (extreme_count > 0L) {
      if (extreme_action == "error") {
        rlang::abort(sprintf("ogtt_is: %d extreme values beyond +/-%g detected.", extreme_count, extreme_limit),
                     class = "healthmarkers_ogtt_is_error_extremes")
      } else if (extreme_action == "cap") {
        out[] <- .ogtt_cap_matrix(vals, extreme_limit)
        rlang::warn(sprintf("ogtt_is: capped %d extreme values beyond +/-%g.", extreme_count, extreme_limit))
      } else if (extreme_action == "warn") {
        rlang::warn(sprintf("ogtt_is: detected %d extreme values beyond +/-%g (not capped).", extreme_count, extreme_limit))
      } else if (extreme_action == "NA") {
        vals[is_ext] <- NA_real_
        out[] <- vals
        rlang::warn(sprintf("ogtt_is: set %d extreme values to NA (>|+/-%g|).", extreme_count, extreme_limit))
      }
      # "ignore": no-op
    }
  }

  # 5) Normalize if requested
  out <- dplyr::mutate(
    out,
    dplyr::across(
      dplyr::everything(),
      ~ HealthMarkers::normalize_vec(.x, method = normalize)
    )
  )

  if (isTRUE(verbose)) {
    hm_inform(level = "inform", msg = sprintf(
      "Completed ogtt_is: %d rows; extremes=%d",
      nrow(data), extreme_count
    ))
  }

  out
}

# ---- internal helpers (not exported) -----------------------------------------

.ogtt_validate_args <- function(normalize, na_warn_prop, check_extreme, extreme_limit, verbose) {
  ok_norm <- normalize %in% c("none","z","inverse","range","robust")
  if (!ok_norm) rlang::abort("`normalize` must be one of: 'none','z','inverse','range','robust'.",
                             class = "healthmarkers_ogtt_is_error_normalize")
  if (!(is.numeric(na_warn_prop) && length(na_warn_prop) == 1L && is.finite(na_warn_prop) &&
        na_warn_prop >= 0 && na_warn_prop <= 1)) {
    rlang::abort("`na_warn_prop` must be a single numeric in [0, 1].",
                 class = "healthmarkers_ogtt_is_error_na_warn_prop")
  }
  if (!(is.logical(check_extreme) && length(check_extreme) == 1L && !is.na(check_extreme))) {
    rlang::abort("`check_extreme` must be a single logical value.",
                 class = "healthmarkers_ogtt_is_error_check_extreme")
  }
  if (!(is.numeric(extreme_limit) && length(extreme_limit) == 1L && is.finite(extreme_limit) && extreme_limit > 0)) {
    rlang::abort("`extreme_limit` must be a single positive finite numeric.",
                 class = "healthmarkers_ogtt_is_error_extreme_limit")
  }
  if (!(is.logical(verbose) && length(verbose) == 1L && !is.na(verbose))) {
    rlang::abort("`verbose` must be a single logical value.",
                 class = "healthmarkers_ogtt_is_error_verbose")
  }
  invisible(TRUE)
}

.ogtt_high_missing_diag <- function(df, vars, na_warn_prop = 0.2) {
  if (!length(vars)) return(invisible(TRUE))
  for (v in vars) {
    x <- df[[v]]
    n <- length(x); if (!n) next
    pna <- sum(is.na(x)) / n
    if (pna >= na_warn_prop && pna > 0) {
      hm_inform(level = "debug", msg = sprintf("ogtt_is(): column '%s' has high missingness (%.1f%%).", v, 100 * pna))
    }
  }
  invisible(TRUE)
}

.ogtt_safe_div <- function(num, den) {
  out <- num / den
  out[!is.finite(out)] <- NA_real_
  out
}

.ogtt_log <- function(x) {
  y <- x
  y[!(is.finite(y) & y > 0)] <- NA_real_
  out <- suppressWarnings(log(y))
  out[!is.finite(out)] <- NA_real_
  out
}

.ogtt_cap_matrix <- function(m, lim) {
  out <- m
  over <- is.finite(out) & out > lim
  under <- is.finite(out) & out < -lim
  out[over]  <- lim
  out[under] <- -lim
  out
}
