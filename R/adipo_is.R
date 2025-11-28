# R/adipo_is.R

#' Adipose insulin sensitivity indices (QUICKI, VAI, LAP, TyG, TG/HDL, Belfiore)
#'
#' @description
#' Computes adipose-related insulin sensitivity/resistance indices from fasting inputs.
#' Expected input units (converted internally):
#' - Glucose G0 mmol/L -> mg/dL (* 18)
#' - Insulin I0 pmol/L -> µU/mL (/ 6)
#' - TG mmol/L -> mg/dL (* 88.57)
#' - HDL mmol/L -> mg/dL (* 38.67)
#'
#' Reported indices (higher magnitude of negative “_inv” values implies worse adipose IR):
#' - Revised_QUICKI = 1 / [log10(I0[µU/mL]) + log10(G0[mg/dL]) + log10(FFA[mmol/L])]
#' - VAI (sex-specific; inverted as VAI_*_inv so larger negative = worse)
#' - TG_HDL_C_inv = -(TG/HDL) in mg/dL
#' - TyG_inv = -ln(TG[mg/dL] × G0[mg/dL] / 2)
#' - LAP (sex-specific; inverted)
#' - McAuley_index = exp(2.63 − 0.28 ln(I0[µU/mL]) − 0.31 ln(TG[mmol/L]))
#' - Adipo_inv = −(FFA × I0[µU/mL])
#' - Belfiore_inv_FFA = − 2 / (I0[µU/mL] × FFA + 1)
#'
#' @param data Data frame or tibble with required columns mapped by `col_map`
#' @param col_map Named list mapping keys to columns: G0, I0, TG, HDL_c, FFA, waist, bmi
#' @param normalize One of c("none","z","inverse","range","robust"); default "none"
#' @param na_action One of c("keep","omit","error"); default "keep"
#' @param check_extreme Logical; if TRUE, applies range checks before computing
#' @param extreme_action One of c("cap","NA","error"); default "cap" when `check_extreme = TRUE`
#' @param extreme_rules Optional named list of c(min,max) per key (in original units)
#' @param verbose Logical; when TRUE, emits progress via `hm_inform()`
#' @param ... Reserved
#'
#' @return A tibble with columns:
#' `Revised_QUICKI`, `VAI_Men_inv`, `VAI_Women_inv`, `TG_HDL_C_inv`, `TyG_inv`,
#' `LAP_Men_inv`, `LAP_Women_inv`, `McAuley_index`, `Adipo_inv`, `Belfiore_inv_FFA`
#'
#' @references
#' Katz A, et al. QUICKI. J Clin Endocrinol Metab (2000) \doi{10.1210/jcem.85.5.2402}
#'
#' Amato MC, et al. VAI. Diabetes Care (2010) \doi{10.2337/dc09-1825}
#'
#' Kahn HS. LAP. BMC Cardiovasc Disord (2005) \doi{10.1186/1471-2261-5-26}
#'
#' Guerrero-Romero F, et al. TyG. Diabetes Care (2018) \doi{10.2337/dc18-0010}
#'
#' Dobiasova M. Atherogenic indices (TG/HDL concept). Clin Chem Lab Med (2001) \doi{10.1515/CCLM.2001.095}
#'
#' Belfiore F, et al. Insulin sensitivity formulations. Mol Genet Metab (1998) \doi{10.1006/mgme.1998.2630}
#'
#' Raynaud E, et al. Revised QUICKI concept with NEFA/FFA. Diabetes Care (1999) \doi{10.2337/diacare.22.6.1003}
#'
#' @examples
#' df <- tibble::tibble(
#'   G0 = c(5.2, 6.1),      # mmol/L
#'   I0 = c(60, 110),       # pmol/L
#'   TG = c(1.2, 1.8),      # mmol/L
#'   HDL_c = c(1.3, 1.0),   # mmol/L
#'   FFA = c(0.4, 0.6),     # mmol/L
#'   waist = c(85, 102),    # cm
#'   bmi = c(24, 31)        # kg/m^2
#' )
#' cm <- as.list(names(df)); names(cm) <- names(df)
#' out <- adipo_is(df, cm, verbose = FALSE, na_action = "keep")
#' head(out)
#' @export
adipo_is <- function(data,
                     col_map,
                     normalize = "none",
                     na_action = c("keep","omit","error"),
                     check_extreme = FALSE,
                     extreme_action = c("cap","NA","error"),
                     extreme_rules = NULL,
                     verbose = FALSE,
                     ...) {
  na_action <- match.arg(na_action)
  extreme_action <- match.arg(extreme_action)

  # Validate normalize up front
  allowed_norm <- c("none","z","inverse","range","robust")
  if (!normalize %in% allowed_norm) {
    rlang::abort(
      sprintf("`normalize` must be one of: %s", paste(allowed_norm, collapse = ", ")),
      class = "healthmarkers_adipo_is_error_normalize_arg"
    )
  }

  # Centralized validation
  req <- c("G0","I0","TG","HDL_c","FFA","waist","bmi")

  # Emit test-aligned error before any generic helper
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

  # Ensure mapped columns exist in data
  mapped_cols <- unname(unlist(col_map[req], use.names = FALSE))
  not_found <- setdiff(mapped_cols, names(data))
  if (length(not_found)) {
    rlang::abort(
      paste0("adipo_is(): columns not found in data: ", paste(not_found, collapse = ", ")),
      class = "healthmarkers_adipo_is_error_missing_columns"
    )
  }

  # Progress message (hm_inform should gate via options)
  hm_inform(level = "debug", msg = "adipo_is(): preparing inputs")

  # Coerce to numeric when needed (warn once per column on NA introduction)
  for (key in req) {
    cn <- col_map[[key]]
    if (!is.numeric(data[[cn]])) {
      old <- data[[cn]]
      suppressWarnings(data[[cn]] <- as.numeric(old))
      intro <- sum(is.na(data[[cn]]) & !is.na(old))
      if (intro > 0) {
        rlang::warn(
          sprintf("adipo_is(): column '%s' coerced to numeric; NAs introduced: %d", cn, intro),
          class = "healthmarkers_adipo_is_warn_na_coercion"
        )
      }
    }
  }

  # NA handling
  if (na_action == "error") {
    any_na <- Reduce(`|`, lapply(mapped_cols, function(cn) is.na(data[[cn]])))
    if (any(any_na)) {
      rlang::abort(
        "adipo_is(): required inputs contain missing values (na_action='error').",
        class = "healthmarkers_adipo_is_error_missing_values"
      )
    }
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
          rlang::abort(
            sprintf("adipo_is(): values out of range for '%s' (%d cases).", key, nbad),
            class = "healthmarkers_adipo_is_error_extreme_values"
          )
        } else if (extreme_action == "NA") {
          x[bad] <- NA_real_
          data[[cn]] <- x
        } else if (extreme_action == "cap") {
          data[[cn]] <- cap_to(x, rng[1], rng[2])
        }
      }
    }
    if (total_ex > 0 && extreme_action %in% c("cap","NA")) {
      rlang::warn(
        sprintf("adipo_is(): adjusted %d extreme input values (%s).", total_ex, extreme_action),
        class = "healthmarkers_adipo_is_warn_extreme_adjust"
      )
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

  hm_inform(level = "debug", msg = "adipo_is(): computed adipose indices")
  out
}
