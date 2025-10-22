# R/adipo_is.R

#' Compute adipose insulin sensitivity indices
#'
#' @description
#' Computes adipose insulin sensitivity/resistance indices from fasting measures:
#' - Revised_QUICKI = 1 / [log10(insulin µU/mL) + log10(glucose mg/dL) + log10(FFA mmol/L)]
#' - VAI (sex-specific, inverted so higher = better IS)
#' - TG_HDL_C_inv = -(TG / HDL) (mg/dL ratios)
#' - TyG_inv = -ln(TG × glucose / 2)
#' - LAP (sex-specific, inverted)
#' - McAuley_index = exp(2.63 − 0.28 ln(insulin) − 0.31 ln(TG))
#' - Adipo_inv = −(FFA × insulin)
#' - Belfiore_inv_FFA = − 2 / (insulin × FFA + 1)
#'
#' Expected input units (converted internally):
#' - Glucose G0 mmol/L -> mg/dL (* 18)
#' - Insulin I0 pmol/L -> µU/mL (/ 6)
#' - TG mmol/L -> mg/dL (* 88.57)
#' - HDL mmol/L -> mg/dL (* 38.67)
#'
#' @param data Data frame/tibble
#' @param col_map Named list mapping required keys to column names:
#'   - G0, I0, TG, HDL_c, FFA, waist, bmi
#' @param normalize One of: "none","z","inverse","range","robust"
#' @param na_action One of: "keep","omit","error"
#' @param check_extreme Logical; if TRUE, applies range checks before computing
#' @param extreme_action One of: "cap","NA","error"
#' @param extreme_rules Optional named list of c(min,max) per key (in original units)
#' @return Tibble with columns:
#' Revised_QUICKI, VAI_Men_inv, VAI_Women_inv, TG_HDL_C_inv, TyG_inv,
#' LAP_Men_inv, LAP_Women_inv, McAuley_index, Adipo_inv, Belfiore_inv_FFA
#' @export
adipo_is <- function(data,
                     col_map,
                     normalize = "none",
                     na_action = c("keep","omit","error"),
                     check_extreme = FALSE,
                     extreme_action = c("cap","NA","error"),
                     extreme_rules = NULL) {
  na_action <- match.arg(na_action)
  extreme_action <- match.arg(extreme_action)

  # Validate normalize up front
  allowed_norm <- c("none","z","inverse","range","robust")
  if (!normalize %in% allowed_norm) {
    stop(sprintf("`normalize` must be one of: %s", paste(allowed_norm, collapse = ", ")), call. = FALSE)
  }

  # Centralized validation
  req <- c("G0","I0","TG","HDL_c","FFA","waist","bmi")

  # Emit test-aligned error before generic helper (message without backticks)
  nm <- names(col_map)
  if (is.null(nm)) nm <- character(0)
  missing_keys <- setdiff(req, nm)
  if (length(missing_keys)) {
    rlang::abort(
      paste0("adipo_is(): missing col_map entries for: ",
             paste(missing_keys, collapse = ", ")),
      class = "healthmarkers_adipo_is_error_missing_map"
    )
  }

  hm_validate_inputs(data, col_map, req, fn = "adipo_is")

  hm_inform("adipo_is(): preparing inputs", level = "debug")

  # Coerce to numeric when needed
  mapped_cols <- unname(unlist(col_map[req], use.names = FALSE))
  for (key in req) {
    cn <- col_map[[key]]
    if (!is.numeric(data[[cn]])) {
      old <- data[[cn]]
      suppressWarnings(data[[cn]] <- as.numeric(old))
      intro <- sum(is.na(data[[cn]]) & !is.na(old))
      if (intro > 0) warning(sprintf("adipo_is(): column '%s' coerced to numeric; NAs introduced: %d", cn, intro), call. = FALSE)
    }
  }

  # NA handling
  if (na_action == "error") {
    any_na <- Reduce(`|`, lapply(mapped_cols, function(cn) is.na(data[[cn]])))
    if (any(any_na)) stop("adipo_is(): required inputs contain missing values (na_action='error').", call. = FALSE)
  } else if (na_action == "omit") {
    keep <- !Reduce(`|`, lapply(mapped_cols, function(cn) is.na(data[[cn]])))
    data <- data[keep, , drop = FALSE]
  }
  if (nrow(data) == 0L) {
    return(tibble::tibble(
      Revised_QUICKI = numeric(),
      VAI_Men_inv = numeric(),
      VAI_Women_inv = numeric(),
      TG_HDL_C_inv = numeric(),
      TyG_inv = numeric(),
      LAP_Men_inv = numeric(),
      LAP_Women_inv = numeric(),
      McAuley_index = numeric(),
      Adipo_inv = numeric(),
      Belfiore_inv_FFA = numeric()
    ))
  }

  # Optional extremes check
  if (isTRUE(check_extreme)) {
    rules <- if (is.null(extreme_rules)) {
      list(
        G0 = c(2, 30),       # mmol/L
        I0 = c(5, 3000),     # pmol/L
        TG = c(0.1, 20),     # mmol/L
        HDL_c = c(0.2, 5),   # mmol/L
        FFA = c(0.05, 3),    # mmol/L
        waist = c(30, 250),  # cm
        bmi = c(10, 80)      # kg/m^2
      )
    } else extreme_rules

    cap_to <- function(x, lo, hi) pmax(lo, pmin(hi, x))
    total_ex <- 0L
    for (key in intersect(names(rules), req)) {
      cn <- col_map[[key]]
      rng <- rules[[key]]
      x <- data[[cn]]
      bad <- is.finite(x) & (x < rng[1] | x > rng[2])
      nbad <- sum(bad, na.rm = TRUE)
      if (nbad > 0) {
        total_ex <- total_ex + nbad
        if (extreme_action == "error") {
          stop(sprintf("adipo_is(): values out of range for '%s' (%d cases).", key, nbad), call. = FALSE)
        } else if (extreme_action == "NA") {
          x[bad] <- NA_real_
          data[[cn]] <- x
        } else if (extreme_action == "cap") {
          data[[cn]] <- cap_to(x, rng[1], rng[2])
        }
      }
    }
    if (total_ex > 0 && extreme_action %in% c("cap","NA")) {
      warning(sprintf("adipo_is(): adjusted %d extreme input values (%s).", total_ex, extreme_action), call. = FALSE)
    }
  }

  # Extract and convert units
  G0  <- as.numeric(data[[col_map$G0]]) * 18
  I0  <- as.numeric(data[[col_map$I0]]) / 6
  TG  <- as.numeric(data[[col_map$TG]]) * 88.57
  HDL <- as.numeric(data[[col_map$HDL_c]]) * 38.67
  FFA <- as.numeric(data[[col_map$FFA]])
  waist <- as.numeric(data[[col_map$waist]])
  bmi   <- as.numeric(data[[col_map$bmi]])

  # Safe helpers
  safe_div <- function(num, den) { out <- num / den; out[!is.finite(out)] <- NA_real_; out }
  safe_log <- function(x) { out <- rep(NA_real_, length(x)); ok <- is.finite(x) & x > 0; out[ok] <- log(x[ok]); out }
  safe_log10 <- function(x) { out <- rep(NA_real_, length(x)); ok <- is.finite(x) & x > 0; out[ok] <- log10(x[ok]); out }

  # Indices
  Revised_QUICKI <- safe_div(1, safe_log10(I0) + safe_log10(G0) + safe_log10(FFA))

  VAI_Men_inv <- -(
    safe_div(waist, (39.68 + 1.88 * bmi)) *
      safe_div(TG, 1.03) *
      safe_div(1.31, HDL)
  )
  VAI_Women_inv <- -(
    safe_div(waist, (36.58 + 1.89 * bmi)) *
      safe_div(TG, 0.81) *
      safe_div(1.52, HDL)
  )

  TG_HDL_C_inv <- -safe_div(TG, HDL)
  TyG_inv <- -safe_log(TG * G0 / 2)
  LAP_Men_inv   <- -((waist - 65) * TG)
  LAP_Women_inv <- -((waist - 58) * TG)
  McAuley_index <- exp(2.63 - 0.28 * safe_log(I0) - 0.31 * safe_log(TG))
  Adipo_inv <- -(FFA * I0)
  Belfiore_inv_FFA <- -safe_div(2, (I0 * FFA + 1))

  out <- tibble::tibble(
    Revised_QUICKI = as.numeric(Revised_QUICKI),
    VAI_Men_inv = as.numeric(VAI_Men_inv),
    VAI_Women_inv = as.numeric(VAI_Women_inv),
    TG_HDL_C_inv = as.numeric(TG_HDL_C_inv),
    TyG_inv = as.numeric(TyG_inv),
    LAP_Men_inv = as.numeric(LAP_Men_inv),
    LAP_Women_inv = as.numeric(LAP_Women_inv),
    McAuley_index = as.numeric(McAuley_index),
    Adipo_inv = as.numeric(Adipo_inv),
    Belfiore_inv_FFA = as.numeric(Belfiore_inv_FFA)
  )

  # Optional normalization
  if (normalize != "none" && exists("normalize_vec", where = asNamespace("HealthMarkers"))) {
    norm_fun <- getExportedValue("HealthMarkers", "normalize_vec")
    out[] <- lapply(out, function(x) do.call(norm_fun, list(x = x, method = normalize)))
  }

  hm_inform("adipo_is(): computed adipose indices", level = "debug")
  out
}
