# R/fasting_is.R

#' Calculate fasting-based insulin sensitivity indices
#'
#' @description
#' Compute 10 fasting indices from glucose (mmol/L) and insulin (pmol/L):
#' Fasting_inv, Raynaud, HOMA_IR_inv, FIRI, QUICKI, Belfiore_basal, Ig_ratio_basal,
#' Isi_basal, Bennett, HOMA_IR_rev_inv. Units converted internally:
#' G0_mg = G0*18 (mg/dL), I0_u = I0/6 (muU/mL).
#'
#' @param data Data frame with required inputs.
#' @param col_map Named list mapping required keys:
#'   - G0: fasting glucose (mmol/L)
#'   - I0: fasting insulin (pmol/L)
#' @param normalize One of: "none","z","inverse","range","robust".
#' @param na_action One of:
#'   - "keep"  (retain all rows; indices become NA where inputs missing/non-finite)
#'   - "omit"  (drop rows with any missing/non-finite required inputs)
#'   - "error" (abort if any required input is missing/non-finite)
#' @param check_extreme Logical; if TRUE, scan computed indices for large magnitudes.
#' @param extreme_limit Positive numeric threshold used when check_extreme = TRUE.
#' @param extreme_action One of: "cap","NA","error".
#' @param verbose Logical; if TRUE, emit progress/completion messages.
#'
#' @return Tibble with 10 columns (indices listed above).
#' @references
#' Matthews DR, Hosker JP, Rudenski AS, Naylor BA, Treacher DF, Turner RC (1985). Homeostasis model assessment (HOMA): insulin resistance and beta-cell function. Diabetologia, 28(7):412-419. \doi{10.1007/BF00280883}
#' Katz A, Nambi S, Mather K, Baron AD, Follmann DA, Sullivan G, Quon MJ (2000). Quantitative insulin sensitivity check index (QUICKI). J Clin Endocrinol Metab, 85(7):2402-2410. \doi{10.1210/jcem.85.7.6661}
#' Raynaud E, Perez-Martin A, Brun JF, Benhaddad AA, Mercier J (1999). Fasting plasma insulin and insulin resistance indices. Diabetes Metab, 25(6):524-532.
#' Avignon A, Boegner C, Sultan A (1999). Simple assessment of insulin sensitivity from fasting insulin and glucose. Int J Obes Relat Metab Disord, 23(5):512-517.
#' Belfiore F, Iannello S, Volpicelli G (1998). Insulin sensitivity indices from basal insulin and glucose. Mol Genet Metab, 63(2):134-141.
#' Sluiter D, Erkelens DW, Reitsma WD, Doorenbos H (1976). Glucose tolerance and insulin release: a mathematical approach. Diabetes, 25:245-249.
#' Hanson RL et al. (2000). Evaluation of simple indices of insulin sensitivity and insulin secretion for use in epidemiologic studies. Am J Epidemiol, 151(2):190-198.
#' Anderson RL et al. (1995). Exploration of simple measures of insulin resistance. Am J Epidemiol, 142(7):724-732.
#'
#' @examples
#' # Minimal example (units: G0 in mmol/L, I0 in pmol/L)
#' df <- data.frame(G0 = c(5.2, 6.1, 4.8), I0 = c(60, 120, 80))
#' res <- fasting_is(df, col_map = list(G0 = "G0", I0 = "I0"))
#' head(res)
#'
#' # With NA handling and extreme checking
#' df2 <- data.frame(G0 = c(5.0, NA), I0 = c(90, 150))
#' fasting_is(df2, col_map = list(G0 = "G0", I0 = "I0"), na_action = "keep",
#'            check_extreme = TRUE, extreme_limit = 1e3, extreme_action = "cap")
#' @export
fasting_is <- function(
  data,
  col_map,
  normalize = c("none","z","inverse","range","robust"),
  na_action = c("keep","omit","error"),
  check_extreme = FALSE,
  extreme_limit = 1e3,
  extreme_action = c("cap","NA","error"),
  verbose = FALSE
) {
  # Validate normalize early
  allowed_norm <- c("none","z","inverse","range","robust")
  if (length(normalize) == 1L && !normalize %in% allowed_norm) {
    rlang::abort(
      sprintf("`normalize` must be one of: %s", paste(allowed_norm, collapse = ", ")),
      class = "healthmarkers_fi_error_normalize"
    )
  }
  normalize <- match.arg(normalize, allowed_norm)
  na_action <- match.arg(na_action)
  extreme_action <- match.arg(extreme_action)

  # Explicit input validation (HM-CS v3)
  if (!is.data.frame(data)) {
    rlang::abort("fasting_is(): `data` must be a data.frame or tibble.",
                 class = "healthmarkers_fi_error_data_type")
  }
  if (!is.list(col_map) || is.null(names(col_map))) {
    rlang::abort("fasting_is(): `col_map` must be a named list with entries for G0 and I0.",
                 class = "healthmarkers_fi_error_colmap_type")
  }
  req_keys <- c("G0","I0")
  missing_keys <- setdiff(req_keys, names(col_map))
  if (length(missing_keys)) {
    rlang::abort(
      paste0("fasting_is(): missing col_map entries for: ", paste(missing_keys, collapse = ", ")),
      class = "healthmarkers_fi_error_missing_map"
    )
  }
  req_cols <- unname(unlist(col_map[req_keys], use.names = FALSE))
  missing_cols <- setdiff(req_cols, names(data))
  if (length(missing_cols)) {
    rlang::abort(
      paste0("fasting_is(): missing required columns in data: ", paste(missing_cols, collapse = ", ")),
      class = "healthmarkers_fi_error_missing_columns"
    )
  }

  if (!(is.logical(check_extreme) && length(check_extreme) == 1L && !is.na(check_extreme))) {
    rlang::abort("`check_extreme` must be a single logical.", class = "healthmarkers_fi_error_checkextreme")
  }
  if (!(is.numeric(extreme_limit) && length(extreme_limit) == 1L && is.finite(extreme_limit) && extreme_limit > 0)) {
    rlang::abort("`extreme_limit` must be a single positive numeric.", class = "healthmarkers_fi_error_extremlimit")
  }

  if (isTRUE(verbose)) {
    hm_inform("-> fasting_is: validating and preparing inputs", level = "inform")
  } else {
    hm_inform("fasting_is(): preparing inputs", level = "debug")
  }

  # Coerce to numeric; warn if NAs introduced; non-finite -> NA
  for (nm in req_keys) {
    cn <- col_map[[nm]]
    if (!is.numeric(data[[cn]])) {
      old <- data[[cn]]
      suppressWarnings(data[[cn]] <- as.numeric(old))
      introduced <- sum(is.na(data[[cn]]) & !is.na(old))
      if (introduced > 0) {
        rlang::warn(sprintf("Column '%s' coerced to numeric; NAs introduced.", cn),
                    class = "healthmarkers_fi_warn_na_coercion")
      }
    }
    data[[cn]][!is.finite(data[[cn]])] <- NA_real_
  }

  G0 <- data[[col_map$G0]]
  I0 <- data[[col_map$I0]]

  # NA policy
  rows_with_na <- is.na(G0) | is.na(I0)
  if (na_action == "error" && any(rows_with_na)) {
    rlang::abort("fasting_is(): missing/non-finite inputs with na_action='error'.",
                 class = "healthmarkers_fi_error_missing")
  } else if (na_action == "omit" && any(rows_with_na)) {
    keep <- !rows_with_na
    G0 <- G0[keep]; I0 <- I0[keep]
  }

  if (isTRUE(verbose)) hm_inform("-> converting units (mmol/L->mg/dL; pmol/L->muU/mL)", level = "inform")

  G0_mg <- G0 * 18
  I0_u  <- I0 / 6

  if (isTRUE(verbose)) hm_inform("-> computing indices", level = "inform")

  lg <- function(x) {
    y <- x
    y[!(is.finite(y) & y > 0)] <- NA_real_
    out <- suppressWarnings(log(y))
    out[!is.finite(out)] <- NA_real_
    out
  }
  sdiv <- function(a, b) {
    z <- a / b
    z[!is.finite(z)] <- NA_real_
    z
  }

  out <- tibble::tibble(
    Fasting_inv     = -I0_u,
    Raynaud         = sdiv(40, I0_u),
    HOMA_IR_inv     = -sdiv(G0_mg * I0_u, 22.5),
    FIRI            =  sdiv(G0_mg * I0_u, 25),
    QUICKI          = sdiv(1, lg(G0_mg) + lg(I0_u)),
    Belfiore_basal  = sdiv(2, (I0_u * G0_mg) + 1),
    Ig_ratio_basal  = -sdiv(I0_u, G0_mg),
    Isi_basal       = sdiv(10000, G0_mg * I0_u),
    Bennett         = sdiv(1, lg(I0_u) * lg(G0_mg)),
    HOMA_IR_rev_inv = -sdiv(I0_u * G0_mg, 405)
  )

  # Optional extreme handling on outputs
  if (isTRUE(check_extreme)) {
    m <- as.matrix(out)
    ext_mask <- is.finite(m) & abs(m) > extreme_limit
    n_ext <- sum(ext_mask)
    if (n_ext > 0) {
      if (extreme_action == "error") {
        rlang::abort(sprintf("fasting_is(): %d extreme values beyond +/-%g.", n_ext, extreme_limit),
                     class = "healthmarkers_fi_error_extreme")
      } else if (extreme_action == "NA") {
        m[ext_mask] <- NA_real_
        out[] <- m
      } else if (extreme_action == "cap") {
        m[ext_mask & m > 0] <-  extreme_limit
        m[ext_mask & m < 0] <- -extreme_limit
        out[] <- m
      }
    }
  }

  # Inline normalization
  if (normalize != "none") {
    normalize_vec <- function(x, method) {
      v <- as.numeric(x)
      nm <- is.na(v)
      v2 <- v[!nm]
      if (method == "z") {
        if (length(v2) < 2) return(rep(NA_real_, length(v)))
        mu <- mean(v2); sdv <- stats::sd(v2)
        if (!is.finite(sdv) || sdv == 0) return(rep(NA_real_, length(v)))
        (v - mu) / sdv
      } else if (method == "range") {
        if (length(v2) < 2) return(rep(NA_real_, length(v)))
        mn <- min(v2); mx <- max(v2)
        if (!is.finite(mx - mn) || mx - mn == 0) return(rep(NA_real_, length(v)))
        (v - mn) / (mx - mn)
      } else if (method == "inverse") {
        res <- 1 / v
        res[!is.finite(res)] <- NA_real_
        res
      } else if (method == "robust") {
        if (length(v2) < 2) return(rep(NA_real_, length(v)))
        med <- stats::median(v2, na.rm = TRUE)
        madv <- stats::mad(v2, center = med, constant = 1.4826, na.rm = TRUE)
        if (!is.finite(madv) || madv == 0) return(rep(NA_real_, length(v)))
        (v - med) / madv
      } else {
        v
      }
    }
    out[] <- lapply(out, normalize_vec, method = normalize)
  }

  # Completion message (test expects "Completed fasting_is:")
  if (isTRUE(verbose)) {
    message(sprintf("Completed fasting_is: %d rows.", nrow(out)))
  } else {
    hm_inform(sprintf("fasting_is(): completed (%d rows)", nrow(out)), level = "debug")
  }

  out
}
