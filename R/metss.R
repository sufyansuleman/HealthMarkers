# File: R/metss.R

#' Calculate Metabolic Syndrome Severity Score (MetSSS)
#'
#' @title Metabolic Syndrome Severity Score (MetSSS)
#'
#' @description
#' Computes a continuous metabolic syndrome severity z-score using sex- and
#' race-specific standardized components and coefficients (factor-loading style).
#'
#'
#' Behavior note:
#'  - Parameters are selected using ONLY the first row's (race, sex) key
#'    (for backward compatibility). A warning is issued if multiple keys present.
#'
#' Required columns (no unit conversion performed):
#'  - waist (cm), bp_sys (mmHg), bp_dia (mmHg)
#'  - TG, HDL_c, glucose (mmol/L)
#'  - sex (1=male, 2=female)
#'  - race (one of "NHW","NHB","HW","HA" or accepted synonyms)
#'
#' @param data data.frame / tibble.
#' @param params Named list keyed by "RACE_SEX" (e.g. "NHW_M"). Each element:
#'   list(intercept, waist, TG, HDL, glucose, MAP) where each component (except intercept)
#'   is a named numeric vector c(mean=, sd=, coef=).
#' @param verbose Logical; print progress.
#' @param na_action One of c("keep","omit","error") for required-input NAs. Default "keep".
#' @param na_warn_prop Proportion (0–1) above which high-missingness warning fires. Default 0.2.
#' @param check_extreme Logical; scan for extreme values if TRUE.
#' @param extreme_action One of c("warn","cap","error","ignore") when extremes detected. Default "warn".
#' @param extreme_rules Optional named list of c(min,max) for inputs (waist, bp_sys, bp_dia, TG, HDL_c, glucose).
#' @param diagnostics Logical; if TRUE (default) emit value/range diagnostic warnings
#'   (negative, out-of-range checks). Set FALSE to suppress these (e.g., in tests when
#'   also using check_extreme).
#'
#' @return tibble with one numeric column: MetSSS
#' @export 
#' @references
#' Gurka MJ, Lilly CL, Oliver MN, DeBoer MD (2014). Metabolic syndrome severity score and cardiovascular disease risk. J Clin Endocrinol Metab, 99(3):1073–1081. \doi{10.1210/jc.2013-3735}
#' DeBoer MD, Gurka MJ, Woo JG, Morrison JA (2015). Severity of metabolic syndrome and its association with risk for type 2 diabetes and cardiovascular disease. Diabetologia, 58(12):2745–2752. \doi{10.1007/s00125-015-3759-2}
#' Gurka MJ, Filipp SL, Musani SK, Sims M, DeBoer MD (2017). Independent associations between metabolic syndrome severity and future coronary heart disease by sex and race. Diabetes Care, 40(11):1693–1701. \doi{10.2337/dc17-0537}
#' DeBoer MD, Filipp SL, Gurka MJ (2018). Longitudinal metabolic syndrome severity score and risk for cardiovascular disease and type 2 diabetes: the Atherosclerosis Risk in Communities study. J Am Heart Assoc, 7(14):e008232. \doi{10.1161/JAHA.117.008232}
#' @importFrom tibble tibble
#' @importFrom rlang abort warn inform
#' @examples
#' df <- data.frame(
#'   waist = 95, bp_sys = 120, bp_dia = 80, TG = 1.5, HDL = 1.2
#' )
#' metss(df)
#' 
metss <- function(data,
                  params = list(
                    NHW_M = list(
                      intercept = -2.344,
                      waist     = c(mean = 94.0, sd = 12.4, coef = 0.846),
                      TG        = c(mean = 1.5,  sd = 0.6,  coef = 0.701),
                      HDL       = c(mean = 1.1,  sd = 0.3,  coef = -0.663),
                      glucose   = c(mean = 5.3,  sd = 0.6,  coef = 0.658),
                      MAP       = c(mean = 97,   sd = 11,   coef = 0.466)
                    )
                  ),
                  verbose = FALSE,
                  na_action = c("keep","omit","error"),
                  na_warn_prop = 0.2,
                  check_extreme = FALSE,
                  extreme_action = c("warn","cap","error","ignore"),
                  extreme_rules = NULL,
                  diagnostics = TRUE) {

  t0 <- Sys.time()
  na_action <- match.arg(na_action)
  extreme_action <- match.arg(extreme_action)

  if (isTRUE(verbose)) rlang::inform("-> metss: validating inputs")

  .metss_validate_data_frame(data)
  .metss_validate_params(params)

  req <- c("waist","bp_sys","bp_dia","TG","HDL_c","glucose","sex","race")
  miss <- setdiff(req, names(data))
  if (length(miss)) {
    rlang::abort(paste0("metss(): missing required columns: ", paste(miss, collapse = ", ")),
                 class = "healthmarkers_metss_error_missing_columns")
  }

  # Coerce numerics
  num_cols <- c("waist","bp_sys","bp_dia","TG","HDL_c","glucose")
  for (cn in num_cols) {
    if (!is.numeric(data[[cn]])) {
      old <- data[[cn]]
      suppressWarnings(data[[cn]] <- as.numeric(old))
      introduced <- sum(is.na(data[[cn]]) & !is.na(old))
      if (introduced > 0)
        rlang::warn(sprintf("metss(): column '%s' coerced to numeric; NAs introduced: %d", cn, introduced))
    }
  }
  if (!is.numeric(data$sex)) {
    old <- data$sex
    suppressWarnings(data$sex <- as.numeric(as.character(old)))
    introduced <- sum(is.na(data$sex) & !is.na(old))
    if (introduced > 0)
      rlang::warn(sprintf("metss(): 'sex' coerced to numeric; NAs introduced: %d", introduced))
  }

  # Missingness warnings
  .metss_warn_high_missing(data, req, na_warn_prop = na_warn_prop)
  # Basic domain diagnostics (optional)
  if (isTRUE(diagnostics)) {
    .metss_warn_value_diagnostics(data)
  }

  # NA policy
  if (na_action == "error") {
    any_na <- Reduce(`|`, lapply(req, function(cn) is.na(data[[cn]])))
    if (any(any_na))
      rlang::abort("metss(): required inputs contain missing values (na_action='error').",
                   class = "healthmarkers_metss_error_missing_values")
  } else if (na_action == "omit") {
    keep <- !Reduce(`|`, lapply(req, function(cn) is.na(data[[cn]])))
    if (isTRUE(verbose))
      rlang::inform(sprintf("-> metss: omitting %d rows with NA in required inputs", sum(!keep)))
    data <- data[keep, , drop = FALSE]
  }

  if (nrow(data) == 0L) return(tibble::tibble(MetSSS = numeric()))

  # Extremes
  capped_n <- 0L
  if (isTRUE(check_extreme)) {
    rules <- if (is.null(extreme_rules)) {
      list(
        waist = c(40, 200),
        bp_sys = c(60, 300),
        bp_dia = c(30, 200),
        TG = c(0, 20),
        HDL_c = c(0, 5),
        glucose = c(0, 40)
      )
    } else extreme_rules
    ex_counts <- integer(0)
    for (nm in intersect(names(rules), req)) {
      rng <- rules[[nm]]
      x <- data[[nm]]
      bad <- is.finite(x) & (x < rng[1] | x > rng[2])
      ex_counts[nm] <- sum(bad, na.rm = TRUE)
      if (extreme_action == "cap") {
        x[bad & x < rng[1] & is.finite(x)] <- rng[1]
        x[bad & x > rng[2] & is.finite(x)] <- rng[2]
        data[[nm]] <- x
      }
    }
    total_ex <- sum(ex_counts, na.rm = TRUE)
    if (total_ex > 0) {
      if (extreme_action == "error") {
        rlang::abort(sprintf("metss(): detected %d extreme input values.", total_ex),
                     class = "healthmarkers_metss_error_extremes")
      } else if (extreme_action == "cap") {
        capped_n <- total_ex
        rlang::warn(sprintf("metss(): capped %d extreme input values into allowed ranges.", total_ex))
      } else if (extreme_action == "warn") {
        rlang::warn(sprintf("metss(): detected %d extreme input values (not altered).", total_ex))
      }
    }
  }

  # Key derivation
  key_vec <- .metss_key_from_data(data)
  if (length(unique(key_vec)) > 1L) {
    rlang::warn(sprintf(
      "metss(): multiple sex/race keys detected (%s); using '%s' for all rows (first row).",
      paste(unique(key_vec), collapse = ", "), key_vec[1]
    ))
  }
  key <- key_vec[1]
  if (!key %in% names(params)) {
    rlang::abort(
      sprintf("metss(): params does not contain a key '%s'. Available: %s",
              key, paste(names(params), collapse = ", ")),
      class = "healthmarkers_metss_error_missing_param_key"
    )
  }
  p <- params[[key]]
  .metss_validate_param_entry(p, key)

  if (isTRUE(verbose)) rlang::inform("-> metss: computing score")

  # MAP
  MAP <- (2 * data$bp_dia + data$bp_sys) / 3

  z_waist <- (data$waist   - p$waist["mean"])   / p$waist["sd"]
  z_TG    <- (data$TG      - p$TG["mean"])      / p$TG["sd"]
  z_HDL   <- (data$HDL_c   - p$HDL["mean"])     / p$HDL["sd"]
  z_glu   <- (data$glucose - p$glucose["mean"]) / p$glucose["sd"]
  z_MAP   <- (MAP          - p$MAP["mean"])     / p$MAP["sd"]

  MetSSS <- p$intercept +
    p$waist["coef"]   * z_waist +
    p$TG["coef"]      * z_TG +
    p$HDL["coef"]     * z_HDL +
    p$glucose["coef"] * z_glu +
    p$MAP["coef"]     * z_MAP

  out <- tibble::tibble(MetSSS = as.numeric(MetSSS))

  if (isTRUE(verbose)) {
    bad <- sum(is.na(out$MetSSS) | !is.finite(out$MetSSS))
    elapsed <- as.numeric(difftime(Sys.time(), t0, units = "secs"))
    rlang::inform(sprintf(
      "Completed metss: %d rows; NA/Inf=%d; key=%s; capped=%d; elapsed=%.2fs",
      nrow(out), bad, key, capped_n, elapsed
    ))
  }

  out
}

# ---- helpers ----
.metss_validate_data_frame <- function(data) {
  if (!is.data.frame(data)) {
    rlang::abort("metss(): `data` must be a data.frame or tibble.",
                 class = "healthmarkers_metss_error_data_type")
  }
  invisible(TRUE)
}
.metss_validate_params <- function(params) {
  if (!is.list(params) || is.null(names(params)) || any(names(params) == "")) {
    rlang::abort("metss(): `params` must be a named list keyed by 'RACE_SEX'.",
                 class = "healthmarkers_metss_error_params_type")
  }
  invisible(TRUE)
}
.metss_validate_param_entry <- function(p, key) {
  needed <- c("intercept","waist","TG","HDL","glucose","MAP")
  miss <- setdiff(needed, names(p))
  if (length(miss))
    rlang::abort(sprintf("metss(): params['%s'] missing entries: %s",
                         key, paste(miss, collapse = ", ")),
                 class = "healthmarkers_metss_error_params_entry_missing")
  if (!(is.numeric(p$intercept) && length(p$intercept) == 1L && is.finite(p$intercept)))
    rlang::abort(sprintf("metss(): params['%s']$intercept must be finite numeric scalar.", key),
                 class = "healthmarkers_metss_error_params_intercept")
  for (comp in c("waist","TG","HDL","glucose","MAP")) {
    v <- p[[comp]]
    if (!(is.numeric(v) && all(c("mean","sd","coef") %in% names(v))))
      rlang::abort(sprintf("metss(): params['%s']$%s must be named numeric with mean, sd, coef.", key, comp),
                   class = "healthmarkers_metss_error_params_component")
    if (v["sd"] <= 0 || any(!is.finite(v[c("mean","sd","coef")])))
      rlang::abort(sprintf("metss(): params['%s']$%s entries must be finite; sd>0.", key, comp),
                   class = "healthmarkers_metss_error_params_component_value")
  }
  invisible(TRUE)
}
.metss_key_from_data <- function(data) {
  race_raw <- toupper(as.character(data$race))
  race_norm <- ifelse(race_raw %in% c("NHW","WHITE"), "NHW",
                 ifelse(race_raw %in% c("NHB","BLACK"), "NHB",
                   ifelse(race_raw %in% c("HW","HISPANIC","H/L"), "HW",
                     ifelse(race_raw %in% c("HA","ASIAN"), "HA", race_raw))))
  sex_norm <- ifelse(data$sex == 1, "M",
                ifelse(data$sex == 2, "F", NA_character_))
  paste0(race_norm, "_", sex_norm)
}
.metss_warn_high_missing <- function(df, cols, na_warn_prop = 0.2) {
  for (cn in cols) {
    x <- df[[cn]]; n <- length(x); if (n == 0L) next
    pna <- sum(is.na(x)) / n
    if (pna >= na_warn_prop && pna > 0) {
      rlang::warn(sprintf("metss(): column '%s' has high missingness (%.1f%%).", cn, 100 * pna))
    }
  }
  invisible(TRUE)
}
.metss_warn_value_diagnostics <- function(df) {
  .chk_nonneg(df$TG,      "TG (mmol/L)")
  .chk_nonneg(df$HDL_c,   "HDL_c (mmol/L)")
  .chk_nonneg(df$glucose, "glucose (mmol/L)")
  .chk_positive(df$waist, "waist (cm)")
  .chk_range(df$bp_sys,   "bp_sys (mmHg)", 60, 300)
  .chk_range(df$bp_dia,   "bp_dia (mmHg)", 30, 200)
  bad_sex <- sum(is.finite(df$sex) & !(df$sex %in% c(1,2)))
  if (bad_sex > 0) rlang::warn(sprintf("metss(): 'sex' has %d values not in {1,2}.", bad_sex))
  race_raw <- toupper(as.character(df$race))
  bad_race <- sum(!(race_raw %in% c("NHW","NHB","HW","HA","WHITE","BLACK","HISPANIC","H/L","ASIAN")))
  if (bad_race > 0) rlang::warn(sprintf("metss(): 'race' has %d unrecognized values.", bad_race))
  invisible(TRUE)
}
.chk_nonneg <- function(x,label){
  n <- sum(is.finite(x) & x < 0)
  if (n>0) rlang::warn(sprintf("metss(): '%s' has %d negative values; check units.", label, n))
  invisible(TRUE)
}
.chk_positive <- function(x,label){
  n <- sum(is.finite(x) & x <= 0)
  if (n>0) rlang::warn(sprintf("metss(): '%s' has %d non-positive values; check units.", label, n))
  invisible(TRUE)
}
.chk_range <- function(x,label,lo,hi){
  nlo <- sum(is.finite(x) & x < lo); nhi <- sum(is.finite(x) & x > hi)
  if ((nlo+nhi)>0) rlang::warn(sprintf("metss(): '%s' outside [%g,%g] in %d rows; check units.", label, lo, hi, nlo+nhi))
  invisible(TRUE)
}
