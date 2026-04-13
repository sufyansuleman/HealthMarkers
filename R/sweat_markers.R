
#' Calculate sweat-based ionic & metabolic markers
#'
#' Computes:
#'  - sweat_chloride (mmol/L)
#'  - Na_K_ratio     (sweat Na+/K+)
#'  - sweat_lactate  (mmol/L)
#'  - sweat_rate     (L/m^2/h) from body mass loss per hour per m^2
#'
#' Inputs are validated, missingness handled via `na_action`, safe divisions
#' are used to avoid Inf/NaN, and an optional extremes scan/cap is available.
#'
#' Expected units:
#' - sweat_chloride, sweat_Na, sweat_K: mmol/L
#' - sweat_lactate: mmol/L
#' - weight_before, weight_after: kg
#' - duration: hours
#' - body_surface_area: m^2
#'
#' @param data A data.frame or tibble containing sweat assay and anthropometrics.
#' @param col_map Named list mapping required inputs (defaults assume same names):
#'   - sweat_chloride, sweat_Na, sweat_K, sweat_lactate,
#'     weight_before, weight_after, duration, body_surface_area
#' @param verbose Logical; if `TRUE`, prints progress messages and a completion summary.
#' @param na_action One of `c("keep","omit","error")` for handling missing values in required inputs. Default "keep".
#' @param na_warn_prop Proportion \eqn{[0,1]} to trigger high-missingness diagnostics for required inputs (debug level). Default 0.2.
#'
#' @return A tibble with columns:
#'   `sweat_chloride`, `Na_K_ratio`, `sweat_lactate`, `sweat_rate`
#'
#' @importFrom tibble tibble
#' @importFrom rlang abort warn inform
#' @export
#' @examples
#' df <- tibble::tibble(
#'   sweat_chloride    = 45,
#'   sweat_Na          = 55,
#'   sweat_K           = 5,
#'   sweat_lactate     = 4.8,
#'   weight_before     = 70.0,
#'   weight_after      = 69.5,
#'   duration          = 1.0,
#'   body_surface_area = 1.9
#' )
#' sweat_markers(df)
#'
#' @references
#' Gibson LE, Cooke RE. A test for concentration of electrolytes in sweat in cystic fibrosis of the pancreas utilizing pilocarpine by iontophoresis. Pediatrics. 1959;23(3):545-549. (Sweat chloride test origin)
#' Dill DB, Costill DL. Calculation of percentage changes in volumes of blood, plasma, and red cells in dehydration. J Appl Physiol. 1974;37(2):247-248. \doi{10.1152/jappl.1974.37.2.247}
#' Farrell PM, White TB, Ren CL, et al. Diagnosis of cystic fibrosis: consensus guidelines from the Cystic Fibrosis Foundation. J Pediatr. 2017;181S:S4-S15.e1. \doi{10.1016/j.jpeds.2016.09.064}
#' Sawka MN, Cheuvront SN, Kenefick RW. Hypohydration and human performance: impact of environment and physiological mechanisms. Sports Med. 2015;45(Suppl 1):S51-S60. \doi{10.1007/s40279-015-0395-7}
sweat_markers <- function(data,
                          col_map = list(
                            sweat_chloride    = "sweat_chloride",
                            sweat_Na          = "sweat_Na",
                            sweat_K           = "sweat_K",
                            sweat_lactate     = "sweat_lactate",
                            weight_before     = "weight_before",
                            weight_after      = "weight_after",
                            duration          = "duration",
                            body_surface_area = "body_surface_area"
                          ),
                          verbose = TRUE,
                          na_action = c("keep","omit","error"),
                          na_warn_prop = 0.2) {
  fn_name <- "sweat_markers"
  id_col  <- .hm_detect_id_col(data)
  na_action <- match.arg(na_action)

  if (!is.data.frame(data)) {
    rlang::abort("sweat_markers(): `data` must be a data.frame or tibble.",
                 class = "healthmarkers_sweat_error_data_type")
  }

  # HM-CS v2: standardized validation
  required_keys <- c(
    "sweat_chloride", "sweat_Na", "sweat_K", "sweat_lactate",
    "weight_before", "weight_after", "duration", "body_surface_area"
  )
  hm_validate_inputs(data, col_map, required_keys = required_keys, fn = "sweat_markers")

  # Ensure mapped columns exist
  req_cols <- unname(unlist(col_map[required_keys], use.names = FALSE))
  missing_cols <- setdiff(req_cols, names(data))
  if (length(missing_cols)) {
    rlang::abort(
      paste0("sweat_markers(): missing columns: ", paste(missing_cols, collapse = ", ")),
      class = "healthmarkers_sweat_error_missing_columns"
    )
  }

  # Coerce to numeric where needed; warn on NAs introduced; non-finite -> NA
  for (cn in req_cols) {
    if (!is.numeric(data[[cn]])) {
      old <- data[[cn]]
      suppressWarnings(data[[cn]] <- as.numeric(old))
      introduced <- sum(is.na(data[[cn]]) & !is.na(old))
      if (introduced > 0) {
        rlang::warn(sprintf("sweat_markers(): column '%s' coerced to numeric; NAs introduced: %d", cn, introduced))
      }
    }
    data[[cn]][!is.finite(data[[cn]])] <- NA_real_
  }

  # High-missingness diagnostics (debug)
  for (cn in req_cols) {
    x <- data[[cn]]
    n <- length(x); if (!n) next
    pna <- sum(is.na(x)) / n
    if (pna >= na_warn_prop && pna > 0) {
      hm_inform(level = "debug", msg = sprintf("sweat_markers(): column '%s' has high missingness (%.1f%%).", cn, 100 * pna))
    }
  }

  if (isTRUE(verbose)) {
    map_parts <- vapply(required_keys, function(k) sprintf("%s -> '%s'", k, col_map[[k]]), character(1))
    hm_inform(sprintf("%s(): column mapping: %s", fn_name, paste(map_parts, collapse = ", ")), level = "inform")
    hm_inform(sprintf("%s(): computing markers:\n  sweat_chloride [sweat_chloride]\n  Na_K_ratio [sweat_Na, sweat_K]\n  sweat_lactate [sweat_lactate]\n  sweat_rate [weight_before, weight_after, duration, body_surface_area]", fn_name), level = "inform")
  }

  # NA policy
  if (na_action == "error") {
    has_na <- Reduce(`|`, lapply(req_cols, function(cn) is.na(data[[cn]])))
    if (any(has_na)) {
      rlang::abort("sweat_markers(): required inputs contain missing values (na_action='error').",
                   class = "healthmarkers_sweat_error_missing_values")
    }
  } else if (na_action == "omit") {
    keep <- !Reduce(`|`, lapply(req_cols, function(cn) is.na(data[[cn]])))
    if (sum(!keep) > 0L)
      hm_inform(level = if (isTRUE(verbose)) "inform" else "debug",
                msg   = sprintf("sweat_markers(): omitting %d rows with NA in required inputs", sum(!keep)))
    data <- data[keep, , drop = FALSE]
  }

  # Early return if empty
  if (nrow(data) == 0L) {
    return(tibble::tibble(
      sweat_chloride = numeric(),
      Na_K_ratio     = numeric(),
      sweat_lactate  = numeric(),
      sweat_rate     = numeric()
    ))
  }

  hm_inform("sweat_markers(): computing markers", level = "debug")

  # Safe division helper with consolidated zero-denominator tracking
  dz_env <- new.env(parent = emptyenv()); dz_env$counts <- list()
  safe_div <- function(num, den, label) {
    out <- num / den
    zero_den <- is.finite(den) & den == 0
    dz_env$counts[[label]] <- sum(zero_den, na.rm = TRUE)
    out[!is.finite(out)] <- NA_real_
    out
  }

  # Compute metrics
  # Map columns from col_map
  sc  <- data[[col_map$sweat_chloride]]
  sNa <- data[[col_map$sweat_Na]]
  sK  <- data[[col_map$sweat_K]]
  sla <- data[[col_map$sweat_lactate]]
  wb  <- data[[col_map$weight_before]]
  wa  <- data[[col_map$weight_after]]
  dur <- data[[col_map$duration]]
  bsa <- data[[col_map$body_surface_area]]

  Na_K_ratio <- safe_div(sNa, sK, "Na_K_ratio")

  mass_loss_kg <- wb - wa
  rate_kg_per_h <- safe_div(mass_loss_kg, dur, "sweat_rate_duration")
  sweat_rate <- safe_div(rate_kg_per_h, bsa, "sweat_rate_bsa")
  # 1 kg ~ 1 L water; units become L/m^2/h

  out <- tibble::tibble(
    sweat_chloride = as.numeric(sc),
    Na_K_ratio     = as.numeric(Na_K_ratio),
    sweat_lactate  = as.numeric(sla),
    sweat_rate     = as.numeric(sweat_rate)
  )

  # Consolidated zero-denominator warning
  dz <- dz_env$counts
  dz_total <- if (length(dz)) sum(unlist(dz), na.rm = TRUE) else 0L
  if (dz_total > 0L) {
    nz <- unlist(dz); nz <- nz[nz > 0]
    lbl <- paste(sprintf("%s=%d", names(nz), nz), collapse = ", ")
    rlang::warn(sprintf("sweat_markers(): zero denominators detected in %d cases (%s).", dz_total, lbl))
  }

  if (!is.null(id_col)) {
    id_vec <- data[[id_col]]
    out[[id_col]] <- id_vec
    out <- out[, c(id_col, setdiff(names(out), id_col)), drop = FALSE]
    out <- tibble::as_tibble(out)
  }
  if (isTRUE(verbose)) {
    hm_inform(hm_result_summary(out, fn_name), level = "inform")
  }

  out
}
