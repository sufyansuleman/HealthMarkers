# R/metabolic_risk_features.R

#' Calculate metabolic risk feature flags (pediatric-friendly thresholds)
#'
#' Compute four binary risk flags from routine clinical measures:
#' - dyslipidemia
#' - insulin_resistance
#' - hyperglycemia (prediabetes-range glycemia)
#' - hypertension (BP >=95th percentile via z > 1.64)
#'
#' By default, behavior matches prior implementation: required columns are
#' validated, NA values are kept (propagate to outputs), no extreme-value
#' checks or capping are applied, and a tibble with 0/1 factor flags is returned.
#'
#' Units and criteria (no automatic unit conversion):
#' - Lipids (mmol/L): total cholesterol > 5.2 OR LDL-C > 3.4 OR HDL-C < 1.0 OR
#'   triglycerides > 1.1 (age 0-9) OR > 1.5 (age 10-19) => dyslipidemia = 1.
#' - Insulin resistance: z_HOMA > 1.28 (~=90th percentile) => insulin_resistance = 1.
#'   z_HOMA is a within-sample or external z-score of HOMA-IR.
#' - Hyperglycemia: fasting glucose in (5.6, 6.9) mmol/L OR HbA1c in (39, 47) mmol/mol
#'   => hyperglycemia = 1.
#' - Hypertension: either BP z-score > 1.64 (~=95th percentile) for systolic or diastolic
#'   => hypertension = 1.
#'
#' @param data A data.frame or tibble containing at least these numeric columns:
#'   - chol_total, chol_ldl, chol_hdl, triglycerides (mmol/L)
#'   - age_year (years)
#'   - z_HOMA (standardized HOMA-IR)
#'   - glucose (mmol/L)
#'   - HbA1c (mmol/mol; IFCC units)
#'   - bp_sys_z, bp_dia_z (BP z-scores)
#' @param col_map Optional named list to map required keys to column names in `data`.
#'   Keys: c("chol_total","chol_ldl","chol_hdl","triglycerides","age_year",
#'   "z_HOMA","glucose","HbA1c","bp_sys_z","bp_dia_z"). Default NULL (use same names).
#' @param na_action One of c("keep","omit","error","ignore","warn") controlling missing-data policy.
#'   - "keep": keep NA; outputs become NA where inputs are NA.
#'   - "omit": drop rows with NA in any required input.
#'   - "error": abort if any required input contains NA.
#'   - "ignore"/"warn": aliases of "keep"; "warn" also emits missingness diagnostics.
#' @param na_warn_prop Numeric in \eqn{[0,1]}; per-variable threshold for high-missingness warnings. Default 0.2.
#' @param check_extreme Logical; if TRUE, scan inputs for out-of-range values (see `extreme_rules`). Default FALSE.
#' @param extreme_action One of c("warn","cap","error","ignore","NA") used when extremes are detected
#'   (only when `check_extreme = TRUE`).
#'   - "warn": only warn (default), "cap": truncate to range and warn,
#'   - "error": abort, "ignore": do nothing, "NA": set flagged inputs to NA.
#' @param extreme_rules Optional named list of c(min,max) ranges for required keys. If NULL, broad defaults are used.
#' @param verbose Logical; if TRUE, prints stepwise messages and a final summary. Default FALSE.
#'
#' @return A tibble with four factor columns (levels c("0","1")):
#' - dyslipidemia
#' - insulin_resistance
#' - hyperglycemia
#' - hypertension
#'
#' @seealso [liver_markers()], [lipid_markers()], [kidney_failure_risk()], [inflammatory_markers()]
#' @importFrom rlang abort warn inform
#' @importFrom dplyr transmute if_else
#' @importFrom tibble as_tibble
#' @export
metabolic_risk_features <- function(
  data,
  col_map = NULL,
  na_action = c("keep","omit","error","ignore","warn"),
  na_warn_prop = 0.2,
  check_extreme = FALSE,
  extreme_action = c("warn","cap","error","ignore","NA"),
  extreme_rules = NULL,
  verbose = FALSE
) {
  na_action <- match.arg(na_action)
  na_action_raw <- na_action
  if (na_action %in% c("ignore","warn")) na_action <- "keep"
  extreme_action <- match.arg(extreme_action)

  t0 <- Sys.time()
  if (isTRUE(verbose)) rlang::inform("-> metabolic_risk_features: validating inputs")

  .mrf_validate_args(data, col_map, na_warn_prop, extreme_rules)

  # Required keys and column mapping
  required_cols <- c(
    "chol_total", "chol_ldl", "chol_hdl", "triglycerides",
    "age_year", "z_HOMA", "glucose", "HbA1c", "bp_sys_z", "bp_dia_z"
  )

  if (is.null(col_map)) {
    col_map <- stats::setNames(required_cols, required_cols)
  } else {
    miss_keys <- setdiff(required_cols, names(col_map))
    if (length(miss_keys)) {
      rlang::abort(
        paste0("metabolic_risk_features(): missing col_map entries for: ", paste(miss_keys, collapse = ", ")),
        class = "healthmarkers_mrf_error_missing_map"
      )
    }
  }

  # Ensure mapped columns exist
  mapped_cols <- unlist(col_map[required_cols], use.names = FALSE)
  miss_cols <- setdiff(mapped_cols, names(data))
  if (length(miss_cols)) {
    rlang::abort(
      paste0("metabolic_risk_features(): mapped columns not found in data: ", paste(miss_cols, collapse = ", ")),
      class = "healthmarkers_mrf_error_missing_columns"
    )
  }

  # HM-CS v2: numeric coercion for required inputs; warn if NAs introduced; set non-finite to NA
  for (cn in mapped_cols) {
    if (!is.numeric(data[[cn]])) {
      old <- data[[cn]]
      suppressWarnings(new <- as.numeric(old))
      introduced_na <- sum(is.na(new) & !is.na(old))
      if (introduced_na > 0L) rlang::warn(sprintf("metabolic_risk_features(): column '%s' coerced to numeric; NAs introduced: %d", cn, introduced_na))
      data[[cn]] <- new
    }
    data[[cn]][!is.finite(data[[cn]])] <- NA_real_
  }

  # High missingness diagnostics only when na_action_raw == "warn"
  if (identical(na_action_raw, "warn")) {
    .mrf_warn_high_missing(data, mapped_cols, na_warn_prop = na_warn_prop)
  }

  # NA policy
  if (na_action == "error") {
    any_na <- Reduce(`|`, lapply(required_cols, function(k) is.na(data[[col_map[[k]]]])))
    if (any(any_na)) {
      rlang::abort("metabolic_risk_features(): required inputs contain missing values (na_action='error').",
                   class = "healthmarkers_mrf_error_missing_values")
    }
  } else if (na_action == "omit") {
    keep <- !Reduce(`|`, lapply(required_cols, function(k) is.na(data[[col_map[[k]]]])))
    if (isTRUE(verbose)) rlang::inform(sprintf("-> metabolic_risk_features: omitting %d rows with NA in required inputs", sum(!keep)))
    data <- data[keep, , drop = FALSE]
  }

  # Optional extreme scan/capping
  capped_n <- 0L
  if (isTRUE(check_extreme)) {
    rules <- if (is.null(extreme_rules)) .mrf_default_extreme_rules() else extreme_rules
    ex <- .mrf_extreme_scan(data, col_map, rules, required_cols)
    if (ex$count > 0L) {
      if (extreme_action == "error") {
        rlang::abort(sprintf("metabolic_risk_features(): detected %d extreme input values.", ex$count),
                     class = "healthmarkers_mrf_error_extremes")
      } else if (extreme_action == "cap") {
        data <- .mrf_cap_inputs(data, ex$flags, col_map, rules)
        capped_n <- ex$count
        rlang::warn(sprintf("metabolic_risk_features(): capped %d extreme input values into allowed ranges.", ex$count))
      } else if (extreme_action == "warn") {
        rlang::warn(sprintf("metabolic_risk_features(): detected %d extreme input values (not altered).", ex$count))
      } else if (extreme_action == "NA") {
        for (cn in names(ex$flags)) {
          bad <- ex$flags[[cn]]
          if (cn %in% names(data)) {
            xi <- data[[cn]]
            xi[bad] <- NA_real_
            data[[cn]] <- xi
          }
        }
      }
    }
  }

  if (isTRUE(verbose)) rlang::inform("-> metabolic_risk_features: computing flags")

  # Shorthand for mapped columns
  CT  <- data[[col_map$chol_total]]
  LDL <- data[[col_map$chol_ldl]]
  HDL <- data[[col_map$chol_hdl]]
  TG  <- data[[col_map$triglycerides]]
  AGE <- data[[col_map$age_year]]
  ZH  <- data[[col_map$z_HOMA]]
  GLU <- data[[col_map$glucose]]
  A1c <- data[[col_map$HbA1c]]
  SBPz <- data[[col_map$bp_sys_z]]
  DBPz <- data[[col_map$bp_dia_z]]

  # Compute flags
  out <- dplyr::transmute(
    data,
    dyslipidemia = factor(
      dplyr::if_else(
        CT > 5.2 |
          LDL > 3.4 |
          HDL < 1.0 |
          (TG > 1.1 & AGE %in% 0:9) |
          (TG > 1.5 & AGE %in% 10:19),
        1L, 0L, missing = NA_integer_
      ),
      levels = c(0L, 1L)
    ),
    insulin_resistance = factor(
      dplyr::if_else(ZH > 1.28, 1L, 0L, missing = NA_integer_),
      levels = c(0L, 1L)
    ),
    hyperglycemia = factor(
      dplyr::if_else(
        (GLU > 5.6 & GLU < 6.9) |
          (A1c > 39 & A1c < 47),
        1L, 0L, missing = NA_integer_
      ),
      levels = c(0L, 1L)
    ),
    hypertension = factor(
      dplyr::if_else(SBPz > 1.64 | DBPz > 1.64, 1L, 0L, missing = NA_integer_),
      levels = c(0L, 1L)
    )
  )

  if (isTRUE(verbose)) {
    na_counts <- vapply(out, function(x) sum(is.na(x)), integer(1))
    elapsed <- as.numeric(difftime(Sys.time(), t0, units = "secs"))
    rlang::inform(sprintf(
      "Completed metabolic_risk_features: %d rows; NA -> %s; capped=%d; elapsed=%.2fs",
      nrow(out), paste(sprintf("%s=%d", names(na_counts), na_counts), collapse = ", "), capped_n, elapsed
    ))
  }

  return(tibble::as_tibble(out))
}

# ---- internal helpers (not exported) -----------------------------------------

.mrf_validate_args <- function(data, col_map, na_warn_prop, extreme_rules) {
  if (!is.data.frame(data)) {
    rlang::abort("metabolic_risk_features(): `data` must be a data.frame or tibble.",
                 class = "healthmarkers_mrf_error_data_type")
  }
  if (!is.null(col_map)) {
    if (!is.list(col_map) || is.null(names(col_map)) || any(names(col_map) == "")) {
      rlang::abort("metabolic_risk_features(): `col_map` must be a named list when supplied.",
                   class = "healthmarkers_mrf_error_colmap_type")
    }
  }
  if (!(is.numeric(na_warn_prop) && length(na_warn_prop) == 1L &&
        is.finite(na_warn_prop) && na_warn_prop >= 0 && na_warn_prop <= 1)) {
    rlang::abort("metabolic_risk_features(): `na_warn_prop` must be a single numeric in [0, 1].",
                 class = "healthmarkers_mrf_error_na_warn_prop")
  }
  if (!is.null(extreme_rules)) {
    if (!is.list(extreme_rules)) {
      rlang::abort("metabolic_risk_features(): `extreme_rules` must be NULL or a named list of c(min,max).",
                   class = "healthmarkers_mrf_error_extreme_rules_type")
    }
    for (nm in names(extreme_rules)) {
      rng <- extreme_rules[[nm]]
      if (!(is.numeric(rng) && length(rng) == 2L && all(is.finite(rng)) && rng[1] <= rng[2])) {
        rlang::abort(sprintf("metabolic_risk_features(): `extreme_rules[['%s']]` must be numeric length-2 with min <= max.", nm),
                     class = "healthmarkers_mrf_error_extreme_rules_value")
      }
    }
  }
  invisible(TRUE)
}

.mrf_warn_high_missing <- function(df, cols, na_warn_prop = 0.2) {
  for (cn in cols) {
    x <- df[[cn]]; n <- length(x); if (n == 0L) next
    pna <- sum(is.na(x)) / n
    if (pna >= na_warn_prop && pna > 0) {
      rlang::warn(sprintf("metabolic_risk_features(): column '%s' has high missingness (%.1f%%).", cn, 100 * pna))
    }
    if (grepl("chol|triglycerides", cn, ignore.case = TRUE)) {
      if (sum(is.finite(x) & x > 30) > 0L) {
        rlang::warn(sprintf("metabolic_risk_features(): '%s' has very large values (>30 mmol/L); check units (mmol/L expected).", cn))
      }
      if (sum(is.finite(x) & x < 0) > 0L) {
        rlang::warn(sprintf("metabolic_risk_features(): '%s' contains negative values; check input.", cn))
      }
    }
    if (identical(cn, "HbA1c")) {
      if (sum(is.finite(x) & x <= 14) > 0L) {
        rlang::warn("metabolic_risk_features(): 'HbA1c' appears to be in percent for some rows; expected mmol/mol.")
      }
    }
    if (identical(cn, "glucose")) {
      if (sum(is.finite(x) & x > 40) > 0L) {
        rlang::warn("metabolic_risk_features(): 'glucose' values > 40 mmol/L detected; check units.")
      }
      if (sum(is.finite(x) & x < 0) > 0L) {
        rlang::warn("metabolic_risk_features(): 'glucose' has negative values; check input.")
      }
    }
    if (grepl("bp_.*_z$", cn)) {
      if (sum(is.finite(x) & (x < -5 | x > 5)) > 0L) {
        rlang::warn(sprintf("metabolic_risk_features(): '%s' z-scores outside [-5,5]; check scaling.", cn))
      }
    }
  }
  invisible(TRUE)
}

.mrf_default_extreme_rules <- function() {
  list(
    chol_total   = c(0.5, 20),
    chol_ldl     = c(0.1, 15),
    chol_hdl     = c(0.1, 5),
    triglycerides= c(0.1, 30),
    age_year     = c(0, 120),
    z_HOMA       = c(-5, 5),
    glucose      = c(2, 40),
    HbA1c        = c(15, 200),
    bp_sys_z     = c(-5, 5),
    bp_dia_z     = c(-5, 5)
  )
}

.mrf_extreme_scan <- function(df, col_map, rules, required) {
  count <- 0L
  flags <- list()
  for (nm in intersect(names(rules), required)) {
    cn <- col_map[[nm]]
    if (!cn %in% names(df)) next
    x <- df[[cn]]
    rng <- rules[[nm]]
    bad <- is.finite(x) & (x < rng[1] | x > rng[2])
    flags[[cn]] <- bad
    count <- count + sum(bad, na.rm = TRUE)
  }
  list(count = count, flags = flags)
}

.mrf_cap_inputs <- function(df, flags, col_map, rules) {
  for (cn in names(flags)) {
    rn <- names(col_map)[match(cn, unlist(col_map, use.names = FALSE))]; rn <- rn[!is.na(rn)][1]
    if (is.na(rn) || is.null(rules[[rn]])) next
    rng <- rules[[rn]]; x <- df[[cn]]; bad <- flags[[cn]]
    x[bad & is.finite(x) & x < rng[1]] <- rng[1]
    x[bad & is.finite(x) & x > rng[2]] <- rng[2]
    df[[cn]] <- x
  }
  df
}
