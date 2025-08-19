# R/tracer_dxa_is.R

#' Compute tracer/DXA‐based insulin sensitivity indices
#'
#' Uses stable isotope tracer infusion rates and DXA‐measured fat mass
#' to compute peripheral and adipose insulin sensitivity and related metrics.
#'
#' @param data A data.frame or tibble containing raw measurements.
#' @param col_map Named list with entries:
#'   - G0, G30, G120: glucose (mmol/L)
#'   - I0, I30, I120: insulin (pmol/L)
#'   - TG, HDL_c: lipids (mmol/L)
#'   - FFA: free fatty acids (mmol/L)
#'   - rate_glycerol, rate_palmitate: tracer rates (µmol/min)
#'   - fat_mass, weight, bmi: body composition
#' @param normalize Ignored
#' @param verbose Logical; if TRUE, messages progress
#' @return A tibble of tracer‐ and adipose‐SI indices.
#' @export
tracer_dxa_is <- function(data, col_map,
                          normalize = NULL,
                          verbose = TRUE) {
  # adipose-only mode if only core adipose keys present
  adipose_keys <- c("I0", "rate_palmitate", "rate_glycerol", "fat_mass", "weight", "HDL_c", "bmi")
  have_adipose <- all(adipose_keys %in% names(col_map)) &&
    !all(c("G0", "I30") %in% names(col_map))
  if (have_adipose) {
    if (verbose) message("-> tracer/DXA: adipose-only indices")
    I0_u <- data[[col_map$I0]] / 6
    Lipo_inv <- -1 * (data[[col_map$rate_glycerol]] * I0_u)
    ATIRI_inv <- -1 * (data[[col_map$rate_palmitate]] * I0_u)
    LIRI_inv <- rep(NA_real_, length(I0_u))
    return(tibble::tibble(
      LIRI_inv  = LIRI_inv,
      Lipo_inv  = Lipo_inv,
      ATIRI_inv = ATIRI_inv
    ))
  }
  # require full set
  required_keys <- names(col_map)
  missing_cols <- setdiff(required_keys, names(data))
  if (length(missing_cols)) {
    stop(
      "tracer_dxa_is(): missing required columns: ",
      paste(missing_cols, collapse = ", ")
    )
  }
  if (verbose) message("-> tracer/DXA: computing indices")
  # unit conversions
  I0_u <- data[[col_map$I0]] / 6
  I30_u <- data[[col_map$I30]] / 6
  I120_u <- data[[col_map$I120]] / 6
  G0_mg <- data[[col_map$G0]] * 18
  G30_mg <- data[[col_map$G30]] * 18
  G120_mg <- data[[col_map$G120]] * 18
  TG_mg <- data[[col_map$TG]] * 88.57
  HDL_mg <- data[[col_map$HDL_c]] * 38.67
  FFA_val <- data[[col_map$FFA]]
  # AUCs
  I_AUC <- 0.5 * ((I0_u + I30_u) * 30 + (I30_u + I120_u) * 90)
  FFA_AUC <- 0.5 * (FFA_val + FFA_val) * 120
  # tracer SI per fat mass
  tracer_palmitate_SI <- data[[col_map$rate_palmitate]] / data[[col_map$fat_mass]]
  tracer_glycerol_SI <- data[[col_map$rate_glycerol]] / data[[col_map$fat_mass]]
  # adipose indices
  LIRI_inv <- -1 * (
    -0.091 + log10((I0_u + I30_u) / 2 * 6) * 0.4 +
      log10((data[[col_map$fat_mass]] / data[[col_map$weight]]) * 100) * 0.346 -
      log10(HDL_mg) * 0.408 + log10(data[[col_map$bmi]]) * 0.435
  )
  Lipo_inv <- -1 * (data[[col_map$rate_glycerol]] * I0_u)
  ATIRI_inv <- -1 * (data[[col_map$rate_palmitate]] * I0_u)
  tibble::tibble(
    I_AUC               = I_AUC,
    FFA_AUC             = FFA_AUC,
    tracer_palmitate_SI = tracer_palmitate_SI,
    tracer_glycerol_SI  = tracer_glycerol_SI,
    LIRI_inv            = LIRI_inv,
    Lipo_inv            = Lipo_inv,
    ATIRI_inv           = ATIRI_inv
  )
}
