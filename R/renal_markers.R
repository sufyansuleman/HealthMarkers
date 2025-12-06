# File: R/renal_markers.R

#' Calculate a Suite of Renal Function, Injury, and Excretion Markers
#'
#' Given routine blood and urine assays, `renal_markers()` computes:
#'   - eGFR_cr: CKD-EPI creatinine equation (2009 variant; race factor retained to preserve prior behavior)
#'   - eGFR_cys: CKD-EPI cystatin C equation (if `cystatin_C` provided)
#'   - eGFR_combined: CKD-EPI combined creatinine+cystatin C (if both provided)
#'   - BUN_Cr_ratio: Blood urea nitrogen / serum creatinine
#'   - FE_Urea: Fractional excretion of urea (%)
#'   - NGAL, KIM1, NAG, Beta2Micro, IL18, L_FABP: pass-through urinary injury markers (if mapped)
#'
#' Robust validation is applied, including NA handling (`na_action`), high-missingness
#' diagnostics, safe divisions with a consolidated zero-denominator warning, and an
#' optional input extremes scan/cap. New arguments are appended for backward compatibility.
#'
#' Expected units (no automatic conversion performed):
#' - creatinine (serum): mg/dL
#' - cystatin C (serum): mg/L
#' - BUN (serum): mg/dL
#' - urea_serum, urea_urine: mg/dL
#' - creatinine_urine: mg/dL
#'
#' @param data A data.frame or tibble with renal lab data.
#' @param col_map Named list mapping:
#'   - creatinine -> serum creatinine (mg/dL)
#'   - age -> age (years)
#'   - sex -> sex indicator (1 = male, 0 = female). Also accepts "male"/"female".
#'   - race -> race ("white", "black", or "other"). Also accepts common aliases.
#'   - BUN -> blood urea nitrogen (mg/dL)
#'   - optional cystatin_C -> serum cystatin C (mg/L)
#'   - optional urea_serum -> serum urea (mg/dL)
#'   - optional creatinine_urine -> urine creatinine (mg/dL)
#'   - optional urea_urine -> urine urea (mg/dL)
#'   - optional NGAL, KIM1, NAG, beta2_micro, IL18, L_FABP -> urine injury markers
#' @param na_action One of c("keep","omit","error") for handling missing values in
#'   required inputs. Default "keep".
#' @param na_warn_prop Proportion \eqn{[0,1]} to trigger high-missingness warnings for
#'   required inputs. Default 0.2.
#' @param check_extreme Logical; if TRUE, scans selected inputs for out-of-range
#'   values using simple heuristics. Default FALSE.
#' @param extreme_action One of c("warn","cap","error","ignore") when extremes
#'   are detected. Default "warn".
#' @param extreme_rules Optional named list of c(min,max) bounds keyed by input
#'   names (e.g., "creatinine","BUN","cystatin_C","urea_serum","creatinine_urine","urea_urine").
#'   If NULL, built-in defaults are used.
#' @param verbose Logical; if TRUE, prints progress messages and a completion summary.
#'
#' @return A tibble with columns:
#'   eGFR_cr, eGFR_cys, eGFR_combined, BUN_Cr_ratio, FE_Urea,
#'   NGAL, KIM1, NAG, Beta2Micro, IL18, L_FABP
#'
#' @examples
#' df <- tibble::tibble(Cr = 1.0, Age = 40, Sex = 1, Race = "white", BUN = 14)
#' cm <- list(creatinine = "Cr", age = "Age", sex = "Sex", race = "Race", BUN = "BUN")
#' renal_markers(df, cm)
#'
#' @references
#'  Original derivations
#'  Levey AS, Stevens LA, Schmid CH, et al. A new equation to estimate glomerular filtration rate. 
#'   Ann Intern Med. 2009;150(9):604-612. \doi{10.7326/0003-4819-150-9-200905050-00006} (CKD-EPI creatinine equation)
#'  Inker LA, Schmid CH, Tighiouart H, et al. Estimating glomerular filtration rate from serum cystatin C. 
#'   N Engl J Med. 2012;367(1):20-29. \doi{10.1056/NEJMoa1114248} (CKD-EPI cystatin C equation)
#'  Inker LA, Eneanya ND, Coresh J, et al. New creatinine- and cystatin C-based equations for estimating GFR without race. 
#'   N Engl J Med. 2021;385(19):1737-1749. \doi{10.1056/NEJMoa2102953} (Race-free CKD-EPI equations)
#'
#'  Validation and biomarker studies
#'  Waikar SS, Sabbisetti VS, Bonventre JV. Fractional excretion of urea as a diagnostic index in acute kidney injury. 
#'   Clin J Am Soc Nephrol. 2009;4(4):802-809. \doi{10.2215/CJN.04870908} (FEUrea in AKI)
#'  Parikh CR, Coca SG, Thiessen-Philbrook H, et al. Urine NGAL as an early predictive biomarker of acute kidney injury. 
#'   J Am Soc Nephrol. 2011;22(12):1737-1747. \doi{10.1681/ASN.2010121302} (NGAL early biomarker)
#'  Vaidya VS, Ramirez V, Ichimura T, et al. Kidney injury molecule-1 outperforms traditional biomarkers of kidney injury in preclinical models. 
#'   J Clin Invest. 2010;120(11):3771-3783. \doi{10.1172/JCI42242} (KIM-1 tubular injury marker)
#'  Portilla D, Dent C, Sugaya T, et al. Urinary liver-type fatty acid-binding protein as a biomarker of acute kidney injury. 
#'   Toxicol Sci. 2008;101(2):365-373. \doi{10.1093/toxsci/kfm279} (L-FABP biomarker)
#'
#' @importFrom tibble tibble
#' @importFrom rlang abort warn inform
#' @export
renal_markers <- function(data,
                          col_map,
                          na_action = c("keep","omit","error"),
                          na_warn_prop = 0.2,
                          check_extreme = FALSE,
                          extreme_action = c("warn","cap","error","ignore"),
                          extreme_rules = NULL,
                          verbose = FALSE) {
  na_action <- match.arg(na_action)
  extreme_action <- match.arg(extreme_action)
  t0 <- Sys.time()
  if (isTRUE(verbose)) hm_inform(level = "inform", msg = "-> renal_markers: validating inputs")

  # 1) Validate mapping and data presence
  required <- c("creatinine", "age", "sex", "race", "BUN")
  missing_map <- setdiff(required, names(col_map))
  if (length(missing_map)) {
    rlang::abort(
      paste0("renal_markers(): missing col_map entries for: ", paste(missing_map, collapse = ", ")),
      class = "healthmarkers_renal_error_missing_map"
    )
  }
  # HM-CS v2 standardized validation
  hm_validate_inputs(data, col_map, required_keys = required, fn = "renal_markers")
  # Ensure mapped required columns exist in data
  req_cols <- unname(unlist(col_map[required], use.names = FALSE))
  not_in_df <- setdiff(req_cols, names(data))
  if (length(not_in_df)) {
    rlang::abort(
      sprintf("renal_markers(): mapped column(s) not found in data: %s", paste(not_in_df, collapse = ", ")),
      class = "healthmarkers_renal_error_missing_columns"
    )
  }
  .rm_validate_df_numeric(data, col_map, cols = c("creatinine","age","BUN",
                                                  "cystatin_C","urea_serum","creatinine_urine","urea_urine"),
                          warn = TRUE)

  # 2) High-missingness warnings on required inputs
  .rm_warn_high_missing(data, col_map[required], na_warn_prop = na_warn_prop)

  # 3) NA policy on required inputs
  used_cols <- unname(unlist(col_map[required], use.names = FALSE))
  if (na_action == "error") {
    has_na <- Reduce(`|`, lapply(used_cols, function(cn) is.na(data[[cn]])))
    if (any(has_na)) {
      rlang::abort("renal_markers(): required inputs contain missing values (na_action='error').",
                   class = "healthmarkers_renal_error_missing_values")
    }
  } else if (na_action == "omit") {
    keep <- !Reduce(`|`, lapply(used_cols, function(cn) is.na(data[[cn]])))
    if (isTRUE(verbose)) hm_inform(level = "inform", msg = sprintf("-> renal_markers: omitting %d rows with NA in required inputs", sum(!keep)))
    data <- data[keep, , drop = FALSE]
  }

  # Early return if empty
  if (nrow(data) == 0L) {
    return(tibble::tibble(
      eGFR_cr = numeric(), eGFR_cys = numeric(), eGFR_combined = numeric(),
      BUN_Cr_ratio = numeric(), FE_Urea = numeric(),
      NGAL = numeric(), KIM1 = numeric(), NAG = numeric(),
      Beta2Micro = numeric(), IL18 = numeric(), L_FABP = numeric()
    ))
  }

  # 4) Optional input extremes scan/cap
  capped_n <- 0L
  if (isTRUE(check_extreme)) {
    rules <- if (is.null(extreme_rules)) .rm_default_extreme_rules() else extreme_rules
    flags <- .rm_extreme_scan(data, col_map, rules)
    ex_count <- sum(vapply(flags, function(v) sum(v, na.rm = TRUE), integer(1)))
    if (ex_count > 0L) {
      if (extreme_action == "error") {
        rlang::abort(sprintf("renal_markers(): detected %d extreme input values.", ex_count),
                     class = "healthmarkers_renal_error_extremes")
      } else if (extreme_action == "cap") {
        data <- .rm_cap_inputs(data, col_map, flags, rules)
        capped_n <- ex_count
        rlang::warn(sprintf("renal_markers(): capped %d extreme input values into allowed ranges.", ex_count))
      } else if (extreme_action == "warn") {
        rlang::warn(sprintf("renal_markers(): detected %d extreme input values (not altered).", ex_count))
      }
      # "ignore": no-op
    }
  }

  if (isTRUE(verbose)) hm_inform(level = "inform", msg = "-> renal_markers: computing markers")

  # 5) Pull and normalize inputs (sex/race mapping)
  Cr   <- data[[col_map$creatinine]] # mg/dL
  age  <- data[[col_map$age]]
  sexi <- .rm_map_sex(data[[col_map$sex]]) # 1 male, 0 female
  race <- .rm_map_race(data[[col_map$race]]) # "black","white","other"
  BUN  <- data[[col_map$BUN]]

  # Optional inputs
  Cys <- if ("cystatin_C" %in% names(col_map)) data[[col_map$cystatin_C]] else NULL
  Urea_s <- if ("urea_serum" %in% names(col_map)) data[[col_map$urea_serum]] else NULL
  Cr_u   <- if ("creatinine_urine" %in% names(col_map)) data[[col_map$creatinine_urine]] else NULL
  Urea_u <- if ("urea_urine" %in% names(col_map)) data[[col_map$urea_urine]] else NULL

  # 6) Helpers: safe division with zero-denominator tracking
  dz_env <- new.env(parent = emptyenv()); dz_env$counts <- list()
  safe_div <- function(num, den, label) {
    out <- num / den
    zero_den <- is.finite(den) & den == 0
    dz_env$counts[[label]] <- sum(zero_den, na.rm = TRUE)
    out[!is.finite(out)] <- NA_real_
    out
  }

  # 7) eGFR via CKD-EPI creatinine (2009)
  kappa <- ifelse(sexi == 1, 0.9, 0.7)
  alpha <- ifelse(sexi == 1, -0.411, -0.329)
  min_ratio <- pmin(Cr / kappa, 1)
  max_ratio <- pmax(Cr / kappa, 1)
  factor_race <- ifelse(race == "black", 1.159, 1)
  factor_sex  <- 1 # male=1, female factor folded in alpha
  eGFR_cr <- 141 * (min_ratio^alpha) * (max_ratio^-1.209) *
    (0.993^age) * factor_race * factor_sex

  # 8) eGFR via cystatin C & combined equations
  if (!is.null(Cys)) {
    min_cys <- pmin(Cys / 0.8, 1)
    max_cys <- pmax(Cys / 0.8, 1)
    cys_sex <- ifelse(sexi == 1, 1, 0.932)
    eGFR_cys <- 133 * (min_cys^-0.499) * (max_cys^-1.328) *
      (0.996^age) * cys_sex
    # combined creatinine + cystatin
    eGFR_combined <- 135 *
      (min_ratio^alpha) * (max_ratio^-0.601) *
      (min_cys^-0.375) * (max_cys^-0.711) *
      (0.995^age) * factor_race * factor_sex
  } else {
    eGFR_cys <- NA_real_
    eGFR_combined <- NA_real_
  }

  # 9) BUN/Cr ratio
  BUN_Cr_ratio <- safe_div(BUN, Cr, "BUN_Cr_ratio")

  # 10) Fractional excretion of urea (%)
  if (!is.null(Urea_s) && !is.null(Cr_u) && !is.null(Urea_u)) {
    FE_Urea <- 100 * safe_div(safe_div(Urea_u, Urea_s, "FE_Urea_UreaS"),
                              safe_div(Cr_u, Cr, "FE_Urea_CrS"),
                              "FE_Urea_final")
  } else {
    FE_Urea <- NA_real_
  }

  # 11) Urine injury markers (pass-through if mapped)
  get_mark <- function(key) {
    if (key %in% names(col_map) && col_map[[key]] %in% names(data)) {
      as.numeric(data[[ col_map[[key]] ]])
    } else {
      rep(NA_real_, nrow(data))
    }
  }
  NGAL        <- get_mark("NGAL")
  KIM1        <- get_mark("KIM1")
  NAG         <- get_mark("NAG")
  Beta2Micro  <- get_mark("beta2_micro")
  IL18        <- get_mark("IL18")
  L_FABP      <- get_mark("L_FABP")

  out <- tibble::tibble(
    eGFR_cr        = as.numeric(eGFR_cr),
    eGFR_cys       = as.numeric(eGFR_cys),
    eGFR_combined  = as.numeric(eGFR_combined),
    BUN_Cr_ratio   = as.numeric(BUN_Cr_ratio),
    FE_Urea        = as.numeric(FE_Urea),
    NGAL           = as.numeric(NGAL),
    KIM1           = as.numeric(KIM1),
    NAG            = as.numeric(NAG),
    Beta2Micro     = as.numeric(Beta2Micro),
    IL18           = as.numeric(IL18),
    L_FABP         = as.numeric(L_FABP)
  )

  # 12) Emit consolidated zero-denominator warning if any
  dz <- dz_env$counts
  dz_total <- if (length(dz)) sum(unlist(dz), na.rm = TRUE) else 0L
  if (dz_total > 0L) {
    nz <- unlist(dz); nz <- nz[nz > 0]
    lbl <- paste(sprintf("%s=%d", names(nz), nz), collapse = ", ")
    rlang::warn(sprintf("renal_markers(): zero denominators detected in %d cases (%s).", dz_total, lbl))
  }

  if (isTRUE(verbose)) {
    na_counts <- vapply(out, function(x) sum(is.na(x) | !is.finite(x)), integer(1))
    elapsed <- as.numeric(difftime(Sys.time(), t0, units = "secs"))
    hm_inform(level = "inform", msg = sprintf(
       "Completed renal_markers: %d rows; NA/Inf -> %s; capped=%d; denom_zero=%d; elapsed=%.2fs",
       nrow(out),
       paste(sprintf("%s=%d", names(na_counts), na_counts), collapse = ", "),
       capped_n, dz_total, elapsed
    ))
  }

  out
}

# --------------------- internal helpers ---------------------------------------

.rm_validate_df_numeric <- function(data, col_map, cols, warn = TRUE) {
  for (key in cols) {
    if (!(key %in% names(col_map))) next
    cn <- col_map[[key]]
    if (!(cn %in% names(data))) next
    if (!is.numeric(data[[cn]])) {
      old <- data[[cn]]
      suppressWarnings(data[[cn]] <- as.numeric(old))
      if (isTRUE(warn)) {
        introduced <- sum(is.na(data[[cn]]) & !is.na(old))
        if (introduced > 0) rlang::warn(sprintf("renal_markers(): column '%s' coerced to numeric; NAs introduced: %d", cn, introduced))
      }
    }
  }
  invisible(TRUE)
}

.rm_warn_high_missing <- function(data, mapped_names, na_warn_prop = 0.2) {
  cols <- unname(unlist(mapped_names, use.names = FALSE))
  for (cn in cols) {
    x <- data[[cn]]
    n <- length(x)
    if (n == 0L) next
    pna <- sum(is.na(x)) / n
    if (pna >= na_warn_prop && pna > 0) {
      hm_inform(level = "debug", msg = sprintf("renal_markers(): column '%s' has high missingness (%.1f%%).", cn, 100 * pna))
    }
  }
  invisible(TRUE)
}

.rm_map_sex <- function(sex) {
  s <- tolower(as.character(sex))
  out <- rep(NA_integer_, length(s))
  # numeric-like mapping
  is_num <- suppressWarnings(!is.na(as.numeric(s)))
  out[is_num & s %in% c("1")] <- 1L
  out[is_num & s %in% c("0", "2")] <- 0L
  # character mapping
  out[s %in% c("male","m")] <- 1L
  out[s %in% c("female","f","fm","woman","girl")] <- 0L
  bad <- sum(is.na(out))
  if (bad > 0) rlang::warn(sprintf("renal_markers(): 'sex' has %d unmapped values; set to NA.", bad))
  out
}

.rm_map_race <- function(race) {
  r <- tolower(as.character(race))
  out <- ifelse(
    r %in% c("black","african-american","african american","aa","blk"), "black",
    ifelse(r %in% c("white","caucasian","european","non-hispanic white"), "white", "other")
  )
  out[is.na(out) | out == ""] <- "other"
  out
}

.rm_default_extreme_rules <- function() {
  list(
    creatinine       = c(0.1, 15),    # mg/dL
    BUN              = c(1, 200),     # mg/dL
    cystatin_C       = c(0.2, 8),     # mg/L
    urea_serum       = c(1, 300),     # mg/dL
    creatinine_urine = c(1, 500),     # mg/dL
    urea_urine       = c(1, 2000)     # mg/dL
  )
}

.rm_extreme_scan <- function(data, col_map, rules) {
  flags <- list()
  for (key in names(rules)) {
    if (!(key %in% names(col_map))) next
    cn <- col_map[[key]]
    if (!(cn %in% names(data))) next
    rng <- rules[[key]]
    x <- data[[cn]]
    flags[[key]] <- is.finite(x) & (x < rng[1] | x > rng[2])
  }
  flags
}

.rm_cap_inputs <- function(data, col_map, flags, rules) {
  for (key in names(flags)) {
    if (!key %in% names(col_map)) next
    cn <- col_map[[key]]
    if (!(cn %in% names(data))) next
    rng <- rules[[key]]
    x <- data[[cn]]
    bad <- flags[[key]]
    x[bad & is.finite(x) & x < rng[1]] <- rng[1]
    x[bad & is.finite(x) & x > rng[2]] <- rng[2]
    data[[cn]] <- x
  }
  data
}
