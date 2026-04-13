# HM-CS v2 utilities
# Note: hm_inform() and hm_get_verbosity() are defined in zzz-options.R (canonical).

# Canonical sex normalizer ----------------------------------------------------
# Maps any sex encoding to a consistent output:
#   "MF"  -> "M" or "F"
#   "12"  -> 1L (male) or 2L (female)   [ogtt_is, metss, kidney_failure_risk]
#   "10"  -> 1L (male) or 0L (female)   [renal_markers, cvd_risk]
#   "01"  -> 0L (male) or 1L (female)   [obesity_indices RFM]
#
# Accepted inputs (case-insensitive):
#   Male   : "m", "male", "1"
#   Female : "f", "female", "2", "0"
#   (Convention: when "0" or "2" are mixed, "1" always = male per package default)
#
# @keywords internal
.hm_normalize_sex <- function(x, to = c("MF", "12", "10", "01"), fn = NULL) {
  to <- match.arg(to)
  s  <- trimws(tolower(as.character(x)))
  mf <- ifelse(s %in% c("m", "male", "1"),          "M",
        ifelse(s %in% c("f", "female", "2", "0"),   "F", NA_character_))
  bad <- sum(is.na(mf) & !is.na(x) & nchar(trimws(as.character(x))) > 0)
  if (bad > 0L && !is.null(fn))
    hm_inform(level = "debug",
              msg   = sprintf("%s(): 'sex' has %d unrecognized value(s); treated as NA.", fn, bad))
  switch(to,
    "MF" = mf,
    "12" = ifelse(mf == "M", 1L, ifelse(mf == "F", 2L, NA_integer_)),
    "10" = ifelse(mf == "M", 1L, ifelse(mf == "F", 0L, NA_integer_)),
    "01" = ifelse(mf == "M", 0L, ifelse(mf == "F", 1L, NA_integer_))
  )
}

#' Normalise na_action aliases to canonical values
#'
#' Converts the backward-compat aliases "ignore" and "warn" to "keep" so that
#' the rest of each function only needs to handle c("keep","omit","error").
#' Returns a list with:
#'   - na_action_eff: the canonical value ("keep", "omit", or "error")
#'   - na_action_raw: the original matched value (for warn-diagnostic checks)
#'
#' @param na_action character(1) already matched via match.arg()
#' @keywords internal
.hm_normalize_na_action <- function(na_action) {
  na_action_eff <- if (na_action %in% c("ignore", "warn")) "keep" else na_action
  list(na_action_raw = na_action, na_action_eff = na_action_eff)
}

#' HM-CS v2 input validation hook
#' @param data data.frame
#' @param col_map named list or NULL
#' @param required_keys character vector of required keys
#' @param fn function name (string)
#' @keywords internal
hm_validate_inputs <- function(data, col_map, required_keys, fn) {
  if (!is.data.frame(data)) {
    rlang::abort(sprintf("%s(): `data` must be a data.frame or tibble.", fn),
                 class = sprintf("healthmarkers_%s_error_data_type", fn))
  }

  # If no keys required (e.g., summarizers), allow NULL or empty col_map
  if (length(required_keys) == 0L) {
    if (!is.null(col_map) && !is.list(col_map)) {
      rlang::abort(sprintf("%s(): `col_map` must be a named list or NULL.", fn),
                   class = sprintf("healthmarkers_%s_error_colmap_type", fn))
    }
    return(invisible(TRUE))
  }

  # Validate col_map structure
  if (is.null(col_map) || !is.list(col_map) || is.null(names(col_map)) || any(names(col_map) == "")) {
    rlang::abort(sprintf("%s(): `col_map` must be a named list of required keys -> column names.", fn),
                 class = sprintf("healthmarkers_%s_error_colmap_type", fn))
  }

  # Ensure all required keys present and mapped to non-empty names
  have_names <- rlang::`%||%`(names(col_map), character(0))
  missing_keys <- setdiff(required_keys, have_names)
  if (length(missing_keys)) {
    rlang::abort(
      sprintf("%s(): `col_map` missing entries for: %s", fn, paste(missing_keys, collapse = ", ")),
      class = sprintf("healthmarkers_%s_error_missing_map", fn)
    )
  }
  mapped <- vapply(required_keys, function(k) {
    val <- col_map[[k]]
    if (is.null(val) || is.na(val)) "" else as.character(val)
  }, character(1))
  if (any(!nzchar(mapped))) {
    bad <- required_keys[!nzchar(mapped)]
    rlang::abort(
      sprintf("%s(): `col_map` has empty mapping for: %s", fn, paste(bad, collapse = ", ")),
      class = sprintf("healthmarkers_%s_error_missing_map", fn)
    )
  }

  invisible(TRUE)
}

# Utility: coerce selected columns to numeric, warn when NAs introduced, non-finite -> NA
hm_coerce_numeric <- function(data, cols, fn = "healthmarkers") {
  for (cn in intersect(cols, names(data))) {
    if (!is.numeric(data[[cn]])) {
      old <- data[[cn]]
      suppressWarnings(data[[cn]] <- as.numeric(old))
      introduced <- sum(is.na(data[[cn]]) & !is.na(old))
      if (introduced > 0) {
        rlang::warn(sprintf("%s(): column '%s' coerced to numeric; NAs introduced: %d", fn, cn, introduced))
      }
    }
    data[[cn]][!is.finite(data[[cn]])] <- NA_real_
  }
  data
}

#' Quiet name repair binder (if you need it elsewhere)
#' @importFrom vctrs vec_as_names
#' @keywords internal
hm_bind_cols_quiet <- function(...) {
  dplyr::bind_cols(...,
    .name_repair = ~ vctrs::vec_as_names(., repair = "unique", quiet = TRUE)
  )
}

#' Format a column-map resolution line for verbose output
#'
#' Produces a single string like:
#' "fn(): column map: G0 -> 'col_a', I0 -> 'col_b', ..."
#'
#' @param col_map Named list of key -> column-name mappings (only the keys
#'   actually used by the function need to be supplied).
#' @param fn Optional function name prefixed to the message.
#' @keywords internal
hm_fmt_col_map <- function(col_map, fn = NULL) {
  nms   <- names(col_map)
  parts <- vapply(nms, function(k) paste0(k, " -> '", col_map[[k]], "'"), character(1))
  msg   <- paste(parts, collapse = ", ")
  if (!is.null(fn)) msg <- paste0(fn, "(): column map: ", msg)
  msg
}

# Auto-fill a col_map from hm_col_report() when the caller didn't supply one.
# Only fills keys present in the synonym dictionary; missing keys remain absent.
# Translates internal short keys (G0, I0, TG, ...) to their hm_col_report()
# dictionary key names, then back again so the returned list uses short keys.
# @param col_map  User-supplied col_map (may be NULL or missing).
# @param data     The data frame being analysed.
# @param keys     Character vector of short internal keys to fill.
# @param fn       Function name for informational message.
# @keywords internal
.hm_autofill_col_map <- function(col_map, data, keys, fn = "") {
  if (!is.null(col_map)) return(col_map)

  # Mapping: short internal key -> dictionary key used by hm_col_report()
  short_to_dict <- c(
    G0    = "fasting_glucose",  I0    = "fasting_insulin",
    G30   = "glucose_30m",      I30   = "insulin_30m",
    G120  = "glucose_120m",     I120  = "insulin_120m",
    TC    = "total_cholesterol",
    HDL_c = "HDL_c",            LDL_c = "LDL_c",
    TG    = "TG",
    total_chol = "total_cholesterol", sbp  = "sbp",
    smoker = "smoking",         bp_treated = "hypertension",
    diabetes = "diabetes",
    ApoA1 = "apoA1",            ApoB  = "apoB",
    ALT   = "ALT",              AST   = "AST",
    GGT   = "GGT",
    bilirubin  = "bilirubin",
    BMI   = "BMI",              bmi   = "BMI",
    waist = "waist",            weight = "weight",
    age   = "age",              sex   = "sex",
    eGFR  = "eGFR",             UACR  = "UACR",
    FFA   = "FFA",
    fat_mass     = "fat_mass",
    rate_palmitate = "rate_palmitate",
    rate_glycerol  = "rate_glycerol",
    creatinine   = "creatinine",
    albumin      = "albumin",   alb   = "albumin",
    calcium      = "calcium",   ca    = "calcium",
    platelets    = "platelets",
    height       = "height",
    ALM          = "ALM",       alm   = "ALM",
    bmd_t        = "BMD",       BMD   = "BMD",
    FEV1         = "FEV1",      FVC   = "FVC",
    fev1         = "FEV1",      fvc   = "FVC",
    FEV1pct      = "FEV1pct",
    kynurenine   = "kynurenine",tryptophan = "tryptophan",
    vitd         = "vitaminD",  vitaminD = "vitaminD",
    vitamin_d    = "vitaminD",
    # Inflammatory
    CRP          = "CRP",       IL6   = "IL6",        TNFa  = "TNFa",
    WBC          = "WBC",       neutrophils = "neutrophils",
    lymphocytes  = "lymphocytes", monocytes = "monocytes",
    eosinophils  = "eosinophils", platelets = "platelets",
    ESR          = "ESR",
    # Renal
    BUN          = "BUN",       race  = "ethnicity",
    cystatin_C   = "cystatin_C",urea_serum = "urea_serum",
    creatinine_urine = "creatinine_urine", urea_urine = "urea_urine",
    # Vitamins / micronutrients
    VitD         = "vitaminD",  B12   = "vitaminB12",
    Folate       = "folate",    Ferritin = "ferritin",
    TSat         = "transferrin_sat",
    Cortisol     = "Cortisol",  DHEAS = "DHEAS",
    Testosterone = "testosterone",  Estradiol = "estradiol",
    TSH          = "TSH",       free_T4 = "FT4",
    Retinol      = "Retinol",   Tocopherol = "Tocopherol",
    Total_lipids = "Total_lipids",
    VitC         = "VitC",      Homocysteine = "Homocysteine",
    MMA          = "MMA",       Magnesium = "magnesium",
    Zinc         = "zinc",      Copper = "copper",
    # Hormones
    total_testosterone = "testosterone",
    SHBG         = "SHBG",      LH    = "LH",         FSH   = "FSH",
    progesterone = "progesterone", free_T3 = "free_T3",
    aldosterone  = "aldosterone", renin = "renin",
    IGF1         = "IGF1",      prolactin = "prolactin",
    cortisol_0   = "cortisol_0",  cortisol_30 = "cortisol_30",
    insulin      = "fasting_insulin",
    # Sarcopenia
    strength     = "strength",  walking = "walking",
    chair        = "chair",     stairs  = "stairs",
    falls        = "falls",
    # BODE / pulmonary
    fev1_pct     = "FEV1pct",   sixmwd = "sixmwd",    mmrc  = "mmrc",
    fev1_pp      = "FEV1pct",
    # Saliva
    cort1        = "saliva_cort1", cort2 = "saliva_cort2",
    cort3        = "saliva_cort3", amylase = "saliva_amylase",
    glucose      = "saliva_glucose",
    # Endocrine / neuro
    nfl          = "nfl",
    GH           = "GH",        glucagon = "glucagon",
    PIVKA_II     = "PIVKA_II",
    # metabolic_risk_features column aliases
    chol_total   = "total_cholesterol",
    chol_ldl     = "LDL_c",
    chol_hdl     = "HDL_c",
    triglycerides = "TG",
    age_year     = "age",
    z_HOMA       = "fasting_insulin",
    bp_sys_z     = "sbp",
    bp_dia_z     = "dbp"
  )

  inferred <- tryCatch(
    hm_col_report(data, col_map = NULL, verbose = FALSE,
                  fuzzy = FALSE, show_unmatched = FALSE),
    error = function(e) list()
  )

  out <- list()
  for (k in keys) {
    dict_key <- if (!is.na(short_to_dict[k])) short_to_dict[[k]] else k
    if (!is.null(inferred[[dict_key]])) {
      out[[k]] <- inferred[[dict_key]]
    } else if (!is.null(inferred[[k]])) {
      # fallback: key already matches dictionary directly
      out[[k]] <- inferred[[k]]
    }
  }

  if (length(out) > 0L) {
    hm_inform(level = "debug",
              msg = sprintf("%s(): col_map not supplied -- auto-inferred %d/%d keys: %s",
                            fn, length(out), length(keys),
                            paste(names(out), unlist(out), sep = "->", collapse = ", ")))
  }
  out
}

#' Detect a participant/sample ID column in a data frame
#'
#' Checks column names against common patterns (case-insensitive exact match):
#' id, iid, participant_id, subject_id, sample_id, pid, sid, record_id.
#' Returns the first matching column name, or NULL if none found.
#' @keywords internal
.hm_detect_id_col <- function(data) {
  nms       <- names(data)
  nms_lower <- tolower(nms)
  candidates <- c("id", "iid", "participant_id", "participantid",
                  "subject_id",  "subjectid",  "sample_id",  "sampleid",
                  "pid",         "sid",         "record_id")
  for (cand in candidates) {
    m <- which(nms_lower == cand)
    if (length(m)) return(nms[m[1L]])
  }
  NULL
}

#' One-level pre-computation of missing keys from existing raw columns
#'
#' Given a dependency map (\code{deps}), for each key in \code{missing_keys}
#' checks whether its prerequisite columns are present in \code{data}. If yes,
#' the key is computed and added as a new column; otherwise an informational
#' message is emitted (when \code{verbose = TRUE}) indicating what to provide.
#'
#' @param data         data.frame to augment.
#' @param deps         Named list of dependency definitions, each with:
#'   \describe{
#'     \item{needs}{character vector of prerequisite column names}
#'     \item{describe}{short human-readable formula string}
#'     \item{compute}{function(data) returning a numeric vector}
#'   }
#' @param missing_keys character vector of key names to attempt to compute.
#' @param fn           calling function name (for messages).
#' @param verbose      logical.
#' @return A list with \code{$data} (augmented data.frame) and
#'   \code{$log} (character vector of per-key messages).
#' @keywords internal
.hm_precompute_from_deps <- function(data, deps, missing_keys,
                                     fn = "", verbose = FALSE) {
  if (length(missing_keys) == 0L) return(list(data = data, log = character(0)))
  log_msgs <- character(0)

  for (key in missing_keys) {
    dep <- deps[[key]]
    if (is.null(dep)) next

    prereqs_absent <- setdiff(dep$needs, names(data))

    if (length(prereqs_absent) == 0L) {
      computed <- tryCatch(dep$compute(data), error = function(e) NULL)
      if (!is.null(computed) && length(computed) == nrow(data)) {
        data[[key]] <- computed
        n_ok <- sum(!is.na(computed))
        msg  <- sprintf("  -> %s computed from %s [%s]: %d/%d valid",
                        key, paste(dep$needs, collapse = ", "),
                        dep$describe, n_ok, nrow(data))
        log_msgs <- c(log_msgs, msg)
        if (isTRUE(verbose)) {
          hm_inform(
            paste0(sprintf("%s(): pre-computation:\n", fn), msg),
            level = "inform"
          )
        }
      }
    } else {
      if (isTRUE(verbose)) {
        hm_inform(
          sprintf("%s(): pre-computation: %s cannot be derived — provide: %s",
                  fn, key, paste(prereqs_absent, collapse = ", ")),
          level = "inform"
        )
      }
    }
  }

  list(data = data, log = log_msgs)
}

#' Summarise a result tibble: count non-NA rows per output column
#'
#' Produces a single string like:
#' "fn(): results: col_a 28/30, col_b 30/30, col_c 25/30"
#'
#' @param result A tibble / data.frame returned by a HealthMarkers function.
#' @param fn Optional function name prefixed to the message.
#' @keywords internal
hm_result_summary <- function(result, fn = NULL) {
  n     <- nrow(result)
  parts <- vapply(names(result), function(cn) {
    ok <- sum(!is.na(result[[cn]]))
    paste0(cn, " ", ok, "/", n)
  }, character(1))
  msg <- paste(parts, collapse = ", ")
  if (!is.null(fn)) msg <- paste0(fn, "(): results: ", msg)
  msg
}