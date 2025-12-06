# R/tracer_dxa_is.R

#' Compute tracer/DXA-based insulin sensitivity indices
#'
#' Uses stable isotope tracer infusion rates and DXA-measured fat mass
#' to compute peripheral and adipose insulin sensitivity and related metrics.
#'
#' Modes:
#' - Adipose-only indices when only adipose-related keys are mapped (no OGTT glucose/insulin time series)
#' - Full indices otherwise
#'
#' Expected units:
#' - Glucose: mmol/L (internally converted to mg/dL when needed)
#' - Insulin: pmol/L (internally converted to muU/mL via /6)
#' - TG: mmol/L (to mg/dL via *88.57); HDL-c: mmol/L (to mg/dL via *38.67)
#' - Tracer rates: mumol/min
#' - Fat mass, weight: kg; BMI: kg/m^2
#'
#' @param data A data.frame or tibble containing raw measurements.
#' @param col_map Named list with entries (depending on mode):
#'   Adipose-only required:
#'     - I0: fasting insulin (pmol/L)
#'     - rate_glycerol, rate_palmitate: tracer rates (mumol/min)
#'     - fat_mass, weight, bmi: body composition
#'     - HDL_c: HDL cholesterol (mmol/L)
#'   Full mode additionally requires:
#'     - G0, G30, G120: glucose (mmol/L)
#'     - I30, I120: insulin (pmol/L)
#'     - TG: triglycerides (mmol/L)
#'     - FFA: free fatty acids (mmol/L)
#' @param normalize Ignored (kept for backward compatibility).
#' @param na_action One of c("keep","omit","error") for NA handling on required inputs. Default "keep".
#' @param na_warn_prop Proportion \eqn{[0,1]} to trigger high-missingness warnings on required inputs. Default 0.2.
#' @param check_extreme Logical; if TRUE, scan inputs for extreme values. Default FALSE.
#' @param extreme_action One of c("warn","cap","error","ignore") when extremes detected. Default "warn".
#' @param extreme_rules Optional named list of c(min,max) bounds for inputs (keys as in col_map).
#'   If NULL, broad defaults are used.
#' @param verbose Logical; if TRUE, prints progress messages and a completion summary.
#'
#' @return
#' - Adipose-only tibble columns: LIRI_inv, Lipo_inv, ATIRI_inv
#' - Full-mode tibble columns: I_AUC, FFA_AUC, tracer_palmitate_SI, tracer_glycerol_SI, LIRI_inv, Lipo_inv, ATIRI_inv
#'
#' @importFrom tibble tibble
#' @importFrom rlang abort warn inform
#' @export
#' @references
#'  Original derivations
#'  Groop LC, Bonadonna RC, Simonson DC, et al. Different effects of insulin and oral hypoglycemic agents on glucose and lipid metabolism in type II diabetes. 
#'   J Clin Invest. 1989;84(2):578-585. \doi{10.1172/JCI114192} (Tracer-based insulin sensitivity concepts)
#'  Steele R. Influences of glucose loading and of injected insulin on hepatic glucose output. 
#'   Ann N Y Acad Sci. 1959;82(2):420-430. \doi{10.1111/j.1749-6632.1959.tb44923.x} (Tracer methodology framework)
#'  Boston RC, Stefanovski D, Moate PJ, Sumner AE, Watanabe RM, Bergman RN. MINMOD Millennium: a computer program to calculate glucose effectiveness and insulin sensitivity from the frequently sampled IV glucose tolerance test. 
#'   Diabetes Technol Ther. 2003;5(6):1003-1015. \doi{10.1089/152091503322641115} (Modeling insulin sensitivity with FSIGT tracers)
#'  Roden M, Price TB, Perseghin G, et al. Mechanism of free fatty acid-induced insulin resistance in humans. 
#'   J Clin Invest. 1996;97(12):2859-2865. \doi{10.1172/JCI118742} (FFA tracer dynamics and insulin resistance)
#'
#'  Validation studies
#'  Gastaldelli A, Ferrannini E, Miyazaki Y, Matsuda M, DeFronzo RA. Beta-cell dysfunction and glucose intolerance: results from the San Antonio Metabolism Study. 
#'   Diabetologia. 2004;47(1):31-39. \doi{10.1007/s00125-003-1254-9} (Validation of tracer-derived insulin sensitivity vs. clamp)
#'  Karpe F, Dickmann JR, Frayn KN. Fatty acids, obesity, and insulin resistance: time for a reevaluation. 
#'   Diabetes. 2011;60(10):2441-2449. \doi{10.2337/db11-0425} (Adipose tissue tracer validation and interpretation)
#'  Petersen KF, Dufour S, Savage DB, et al. The role of skeletal muscle insulin resistance in the pathogenesis of the metabolic syndrome. 
#'   Proc Natl Acad Sci U S A. 2007;104(31):12587-12594. \doi{10.1073/pnas.0705408104} (Skeletal muscle insulin sensitivity validation)
#'  Santomauro AT, Boden G, Silva ME, et al. Overnight lowering of free fatty acids with Acipimox improves insulin resistance and glucose tolerance in obese diabetic and nondiabetic subjects. 
#'   Diabetes. 1999;48(9):1836-1841. \doi{10.2337/diabetes.48.9.1836} (FFA suppression and adipose insulin sensitivity)
tracer_dxa_is <- function(data, col_map,
                          normalize = NULL,
                          na_action = c("keep","omit","error"),
                          na_warn_prop = 0.2,
                          check_extreme = FALSE,
                          extreme_action = c("warn","cap","error","ignore"),
                          extreme_rules = NULL,
                          verbose = FALSE) {
  na_action <- match.arg(na_action)
  extreme_action <- match.arg(extreme_action)

  if (!is.null(normalize)) {
    # maintain signature compatibility; explicitly ignore
    # rlang::warn("tracer_dxa_is(): `normalize` argument is ignored.")
  }

  if (!is.data.frame(data)) {
    rlang::abort("tracer_dxa_is(): `data` must be a data.frame or tibble.",
                 class = "healthmarkers_tracer_error_data_type")
  }
  if (!is.list(col_map) || is.null(names(col_map)) || any(names(col_map) == "")) {
    rlang::abort("tracer_dxa_is(): `col_map` must be a named list mapping keys to column names.",
                 class = "healthmarkers_tracer_error_colmap_type")
  }

  if (isTRUE(verbose)) hm_inform(level = "inform", msg = "-> tracer_dxa_is: validating inputs")
  t0 <- Sys.time()

  adipose_keys <- c("I0", "rate_palmitate", "rate_glycerol", "fat_mass", "weight", "HDL_c", "bmi")
  # Adipose-only when all adipose keys present and not all OGTT keys present
  adipose_only <- all(adipose_keys %in% names(col_map)) &&
    !all(c("G0", "I30") %in% names(col_map))

  full_keys <- c(adipose_keys, "G0","G30","G120","I30","I120","TG","FFA")

  required_keys <- if (adipose_only) adipose_keys else full_keys
+
+  # HM-CS v2 standardized validation
+  hm_validate_inputs(data, col_map, required_keys = required_keys, fn = "tracer_dxa_is")

  # Validate col_map has required keys
  missing_map <- setdiff(required_keys, names(col_map))
  if (length(missing_map)) {
    rlang::abort(
      paste0("tracer_dxa_is(): missing `col_map` entries for: ", paste(missing_map, collapse = ", ")),
      class = "healthmarkers_tracer_error_missing_map"
    )
  }

  # Validate data contains the mapped columns
  mapped_cols <- unname(unlist(col_map[required_keys], use.names = FALSE))
  missing_cols <- setdiff(mapped_cols, names(data))
  if (length(missing_cols)) {
    rlang::abort(
      paste0("tracer_dxa_is(): missing required columns in `data`: ", paste(missing_cols, collapse = ", ")),
      class = "healthmarkers_tracer_error_missing_columns"
    )
  }

  # Coerce mapped numeric columns to numeric; warn on NAs introduced
  to_num_keys <- intersect(required_keys, c(
    "G0","G30","G120","I0","I30","I120","TG","HDL_c","FFA",
    "rate_glycerol","rate_palmitate","fat_mass","weight","bmi"
  ))
  for (key in to_num_keys) {
    cn <- col_map[[key]]
    if (!is.numeric(data[[cn]])) {
      old <- data[[cn]]
      suppressWarnings(data[[cn]] <- as.numeric(old))
      introduced <- sum(is.na(data[[cn]]) & !is.na(old))
      if (introduced > 0) {
        rlang::warn(sprintf("tracer_dxa_is(): column '%s' coerced to numeric; NAs introduced: %d", cn, introduced))
      }
    }
    # Set non-finite to NA for safety
    data[[cn]][!is.finite(data[[cn]])] <- NA_real_
  }

  # High-missingness warnings on required inputs
-  .tx_warn_high_missing(data, mapped_cols, na_warn_prop)
+  .tx_warn_high_missing(data, mapped_cols, na_warn_prop)

  # NA policy on required inputs
  if (na_action == "error") {
    has_na <- Reduce(`|`, lapply(mapped_cols, function(cn) is.na(data[[cn]])))
    if (any(has_na)) {
      rlang::abort("tracer_dxa_is(): required inputs contain missing values (na_action='error').",
                   class = "healthmarkers_tracer_error_missing_values")
    }
  } else if (na_action == "omit") {
    keep <- !Reduce(`|`, lapply(mapped_cols, function(cn) is.na(data[[cn]])))
    if (isTRUE(verbose)) hm_inform(level = "inform", msg = sprintf(
      "-> tracer_dxa_is: omitting %d rows with NA in required inputs", sum(!keep)
    ))
    data <- data[keep, , drop = FALSE]
  }

  # Early return if empty
  if (nrow(data) == 0L) {
    if (adipose_only) {
      return(tibble::tibble(
        LIRI_inv  = numeric(),
        Lipo_inv  = numeric(),
        ATIRI_inv = numeric()
      ))
    } else {
      return(tibble::tibble(
        I_AUC               = numeric(),
        FFA_AUC             = numeric(),
        tracer_palmitate_SI = numeric(),
        tracer_glycerol_SI  = numeric(),
        LIRI_inv            = numeric(),
        Lipo_inv            = numeric(),
        ATIRI_inv           = numeric()
      ))
    }
  }

  # Optional input extremes scan/cap based on keys present
  capped_n <- 0L
  if (isTRUE(check_extreme)) {
    rules <- if (is.null(extreme_rules)) .tx_default_extreme_rules(adipose_only) else extreme_rules
    ex_counts <- integer(0)
    for (key in intersect(names(rules), required_keys)) {
      cn <- col_map[[key]]
      rng <- rules[[key]]
      x <- data[[cn]]
      bad <- is.finite(x) & (x < rng[1] | x > rng[2])
      ex_counts[key] <- sum(bad, na.rm = TRUE)
      if (extreme_action == "cap") {
        x[bad & is.finite(x) & x < rng[1]] <- rng[1]
        x[bad & is.finite(x) & x > rng[2]] <- rng[2]
        data[[cn]] <- x
      }
    }
    total_ex <- sum(ex_counts, na.rm = TRUE)
    if (total_ex > 0) {
      if (extreme_action == "error") {
        rlang::abort(sprintf("tracer_dxa_is(): detected %d extreme input values.", total_ex),
                     class = "healthmarkers_tracer_error_extremes")
      } else if (extreme_action == "cap") {
        capped_n <- total_ex
        rlang::warn(sprintf("tracer_dxa_is(): capped %d extreme input values into allowed ranges.", total_ex))
      } else if (extreme_action == "warn") {
        rlang::warn(sprintf("tracer_dxa_is(): detected %d extreme input values (not altered).", total_ex))
      }
    }
  }

  dz_env <- new.env(parent = emptyenv()); dz_env$counts <- list()
  safe_div <- function(num, den, label) {
    out <- num / den
    zero_den <- is.finite(den) & den == 0
    dz_env$counts[[label]] <- sum(zero_den, na.rm = TRUE)
    out[!is.finite(out)] <- NA_real_
    out
  }
  safe_log10 <- function(x) {
    y <- x
    y[!(is.finite(y) & y > 0)] <- NA_real_
    out <- suppressWarnings(log10(y))
    out[!is.finite(out)] <- NA_real_
    out
  }

  if (adipose_only) {
    if (isTRUE(verbose)) hm_inform(level = "inform", msg = "-> tracer_dxa_is: adipose-only indices")

    I0_u <- data[[col_map$I0]] / 6
    Lipo_inv <- -1 * (data[[col_map$rate_glycerol]] * I0_u)
    ATIRI_inv <- -1 * (data[[col_map$rate_palmitate]] * I0_u)
    LIRI_inv <- -1 * (
      -0.091 +
        safe_log10((I0_u + I0_u) / 2 * 6) * 0.4 + # use I0 twice if I30 missing; preserve behavior
        safe_log10((safe_div(data[[col_map$fat_mass]], data[[col_map$weight]], "fm_wt") * 100)) * 0.346 -
        safe_log10(data[[col_map$HDL_c]] * 38.67) * 0.408 +
        safe_log10(data[[col_map$bmi]]) * 0.435
    )

    out <- tibble::tibble(
      LIRI_inv  = as.numeric(LIRI_inv),
      Lipo_inv  = as.numeric(Lipo_inv),
      ATIRI_inv = as.numeric(ATIRI_inv)
    )
  } else {
    if (isTRUE(verbose)) hm_inform(level = "inform", msg = "-> tracer_dxa_is: computing indices")

    # Unit conversions
    I0_u   <- data[[col_map$I0]]   / 6
    I30_u  <- data[[col_map$I30]]  / 6
    I120_u <- data[[col_map$I120]] / 6
    # Glucose present but not used directly in current formulas; keep conversions in case of future indices
    G0_mg   <- data[[col_map$G0]]   * 18
    G30_mg  <- data[[col_map$G30]]  * 18
    G120_mg <- data[[col_map$G120]] * 18
    TG_mg   <- data[[col_map$TG]]   * 88.57
    HDL_mg  <- data[[col_map$HDL_c]] * 38.67
    FFA_val <- data[[col_map$FFA]]

    # AUCs (minutes): insulin trapezoid (0-30, 30-120); FFA flat (single measure over 0-120)
    I_AUC <- 0.5 * ((I0_u + I30_u) * 30 + (I30_u + I120_u) * 90)
    FFA_AUC <- 0.5 * (FFA_val + FFA_val) * 120

    # Tracer sensitivity per kg fat mass
    tracer_palmitate_SI <- safe_div(data[[col_map$rate_palmitate]], data[[col_map$fat_mass]], "palmitate_fm")
    tracer_glycerol_SI  <- safe_div(data[[col_map$rate_glycerol]],  data[[col_map$fat_mass]], "glycerol_fm")

    # Adipose indices
    # Note: safe logs avoid non-positive values; units preserved as in prior code
    LIRI_inv <- -1 * (
      -0.091 +
        safe_log10((I0_u + I30_u) / 2 * 6) * 0.4 +
        safe_log10((safe_div(data[[col_map$fat_mass]], data[[col_map$weight]], "fm_wt") * 100)) * 0.346 -
        safe_log10(HDL_mg) * 0.408 +
        safe_log10(data[[col_map$bmi]]) * 0.435
    )
    Lipo_inv <- -1 * (data[[col_map$rate_glycerol]] * I0_u)
    ATIRI_inv <- -1 * (data[[col_map$rate_palmitate]] * I0_u)

    out <- tibble::tibble(
      I_AUC               = as.numeric(I_AUC),
      FFA_AUC             = as.numeric(FFA_AUC),
      tracer_palmitate_SI = as.numeric(tracer_palmitate_SI),
      tracer_glycerol_SI  = as.numeric(tracer_glycerol_SI),
      LIRI_inv            = as.numeric(LIRI_inv),
      Lipo_inv            = as.numeric(Lipo_inv),
      ATIRI_inv           = as.numeric(ATIRI_inv)
    )
  }

  # Consolidated zero-denominator warning
  dz <- dz_env$counts
  dz_total <- if (length(dz)) sum(unlist(dz), na.rm = TRUE) else 0L
  if (dz_total > 0L) {
    nz <- unlist(dz); nz <- nz[nz > 0]
    lbl <- paste(sprintf("%s=%d", names(nz), nz), collapse = ", ")
    rlang::warn(sprintf("tracer_dxa_is(): zero denominators detected in %d cases (%s).", dz_total, lbl))
  }

  if (isTRUE(verbose)) {
    na_counts <- vapply(out, function(x) sum(is.na(x) | !is.finite(x)), integer(1))
    elapsed <- as.numeric(difftime(Sys.time(), t0, units = "secs"))
    hm_inform(level = "inform", msg = sprintf(
      "Completed tracer_dxa_is: %d rows; NA/Inf -> %s; capped=%d; denom_zero=%d; elapsed=%.2fs",
      nrow(out),
      paste(sprintf("%s=%d", names(na_counts), na_counts), collapse = ", "),
      capped_n, dz_total, elapsed
    ))
  }

  out
}

# ---- internal helpers ---------------------------------------------------------

.tx_warn_high_missing <- function(data, cols, na_warn_prop = 0.2) {
  for (cn in cols) {
    x <- data[[cn]]
    n <- length(x)
    if (n == 0L) next
    pna <- sum(is.na(x)) / n
    if (pna >= na_warn_prop && pna > 0) {
      hm_inform(level = "debug", msg = sprintf(
        "tracer_dxa_is(): column '%s' has high missingness (%.1f%%).",
        cn, 100 * pna
      ))
    }
  }
  invisible(TRUE)
}

.tx_default_extreme_rules <- function(adipose_only) {
  # Very broad plausibility bounds; keys match col_map keys
  base <- list(
    I0             = c(0, 5000),   # pmol/L
    I30            = c(0, 5000),
    I120           = c(0, 5000),
    G0             = c(0, 40),     # mmol/L
    G30            = c(0, 40),
    G120           = c(0, 40),
    TG             = c(0, 50),     # mmol/L
    HDL_c          = c(0, 10),     # mmol/L
    FFA            = c(0, 5),      # mmol/L
    rate_glycerol  = c(0, 10000),  # mumol/min
    rate_palmitate = c(0, 10000),  # mumol/min
    fat_mass       = c(0.1, 200),  # kg
    weight         = c(1, 400),    # kg
    bmi            = c(5, 100)     # kg/m^2
  )
  if (isTRUE(adipose_only)) {
    base[c("I0","rate_glycerol","rate_palmitate","fat_mass","weight","HDL_c","bmi")]
  } else {
    base
  }
}
