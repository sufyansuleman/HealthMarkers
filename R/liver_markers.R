
#' Compute liver-related indices (FLI, NFS, APRI, FIB-4, BARD, ALBI, MELD-XI) with validation and diagnostics
#'
#' Given routine labs and anthropometry, computes:
#' - FLI      - Fatty Liver Index (Bedogni et al. 2006)
#' - NFS      - NAFLD Fibrosis Score (Angulo et al. 2007)
#' - APRI     - AST-to-Platelet Ratio Index
#' - FIB4     - Fibrosis-4 Index
#' - BARD     - BMI-AST/ALT-Diabetes score
#' - ALBI     - Albumin-Bilirubin score
#' - MELD_XI  - MELD excluding INR
#'
#' Enhancements:
#' - Robust input validation (columns present, types) with informative errors.
#' - Configurable NA policy and optional extreme-value scanning/capping.
#' - Data-quality warnings (high missingness, non-positive logs, zero denominators).
#' - Verbose stepwise progress and completion summary.
#'
#' Units (no automatic conversion):
#' - BMI: kg/m^2; Waist: cm; TG: mg/dL; GGT/AST/ALT: U/L; Platelets: 10^9/L; Albumin: g/L; Bilirubin: mg/dL; Creatinine: mg/dL.
#' - ALBI uses bilirubin in mumol/L internally (converted as bilirubin (mg/dL) * 17.1).
#'
#' @param data A data.frame or tibble containing your liver and anthropometry data.
#' @param col_map Named list mapping these keys -> column names in `data`:
#'   - `BMI` (kg/m^2), `waist` (cm), `TG` (mg/dL), `GGT` (U/L),
#'   - `age` (years), `AST` (U/L), `ALT` (U/L), `platelets` (10^9/L),
#'   - `albumin` (g/L), `diabetes` (0/1 or logical),
#'   - `bilirubin` (mg/dL), `creatinine` (mg/dL).
#' @param verbose Logical; if TRUE, prints stepwise messages and a final summary. Default FALSE.
#' @param na_action One of c("keep","omit","error") controlling missing-data policy. Default "keep" (preserves prior behavior).
#'   - "keep": leave NAs; they propagate to outputs.
#'   - "omit": drop rows with NA in any required input.
#'   - "error": abort if any required input contains NA.
#' @param na_warn_prop Numeric in \eqn{[0,1]}; per-variable threshold for high-missingness warnings. Default 0.2.
#' @param check_extreme Logical; if TRUE, scan inputs for out-of-range values (see `extreme_rules`). Default FALSE.
#' @param extreme_action One of c("warn","cap","error","ignore") when extremes are detected (only used if `check_extreme = TRUE`).
#'   - "warn": only warn (default), "cap": truncate to allowed range, "error": abort, "ignore": do nothing.
#' @param extreme_rules Optional named list of c(min,max) ranges for keys in `col_map`. If NULL, broad defaults are used.
#'
#' @return A tibble with one column per marker: `FLI`, `NFS`, `APRI`, `FIB4`, `BARD`, `ALBI`, `MELD_XI`.
#'
#' @details
#' Formulas 
#' - FLI      = logistic(0.953*ln(TG) + 0.139*BMI + 0.718*ln(GGT) + 0.053*waist - 15.745) * 100
#' - NFS      = -1.675 + 0.037*age + 0.094*BMI + 1.13*diabetes + 0.99*(AST/ALT) - 0.013*platelets - 0.66*albumin
#' - APRI     = (AST / 40) / platelets * 100; assumes AST upper limit of normal = 40 U/L
#' - FIB-4    = (age * AST) / (platelets * sqrt(ALT))
#' - BARD     = 1 if BMI>=28, +1 if AST/ALT>=0.8, +1 if diabetes present; sum in 0,1,2,3
#' - ALBI     = 0.66*log10(bilirubin (mumol/L)) - 0.0852*albumin (g/L)
#' - MELD-XI  = 5.11*ln(bilirubin (mg/dL)) + 11.76*ln(creatinine (mg/dL)) + 9.44
#'
#' @seealso [inflammatory_markers()], [kidney_failure_risk()], [iAge()]
#'
#' @examples
#' library(tibble)
#' df <- tibble(
#'   BMI           = 24,
#'   waist         = 80,
#'   TG = 150, # mg/dL
#'   GGT           = 30,
#'   age           = 45,
#'   AST           = 25,
#'   ALT           = 20,
#'   platelets     = 250, # 10^9/L
#'   albumin       = 42,  # g/L
#'   diabetes      = FALSE,
#'   bilirubin     = 1.0, # mg/dL
#'   creatinine    = 0.9  # mg/dL
#' )
#' liver_markers(df, verbose = TRUE)
#'
#' \donttest{
#' # With extreme-value capping and diagnostics
#' liver_markers(
#'   df,
#'   check_extreme = TRUE,
#'   extreme_action = "cap",
#'   verbose = TRUE
#' )
#' }
#'
#' @references
#' \insertRef{bedogni2006fli}{HealthMarkers}
#' \insertRef{angulo2007nfs}{HealthMarkers}
#' \insertRef{wai2003apri}{HealthMarkers}
#' \insertRef{sterling2006fib4}{HealthMarkers}
#' \insertRef{harrison2008bard}{HealthMarkers}
#' \insertRef{johnson2015albi}{HealthMarkers}
#' \insertRef{heuman2007meldxi}{HealthMarkers}
#'
#' @importFrom tibble tibble
#' @importFrom rlang abort warn inform
#' @export
liver_markers <- function(data,
                          col_map = NULL,
                          verbose = FALSE,
                          na_action = c("keep","omit","error","ignore","warn"),
                          na_warn_prop = 0.2,
                          check_extreme = FALSE,
                          extreme_action = c("warn","cap","error","ignore","NA"),
                          extreme_rules = NULL) {
  .na <- .hm_normalize_na_action(match.arg(na_action))
  na_action_raw <- .na$na_action_raw
  na_action <- .na$na_action_eff
  extreme_action <- match.arg(extreme_action)

  hm_inform("liver_markers(): preparing inputs", level = if (isTRUE(verbose)) "inform" else "debug")

  required <- c("BMI","waist","TG","GGT","age","AST","ALT",
                "platelets","albumin","diabetes","bilirubin","creatinine")

  explicit_col_map <- !is.null(col_map)  # was a non-NULL col_map explicitly supplied?
  col_map <- .hm_autofill_col_map(col_map, data, required, fn = "liver_markers")

  if (explicit_col_map) {
    # Strict mode: error if a mapped column is absent, error if required key missing
    miss_cols <- setdiff(
      unlist(col_map[intersect(required, names(col_map))], use.names = FALSE),
      names(data))
    if (length(miss_cols)) {
      rlang::abort(
        paste0("liver_markers(): mapped columns not found in data: ", paste(miss_cols, collapse = ", ")),
        class = "healthmarkers_liver_error_missing_columns")
    }
    miss_keys <- setdiff(required, names(col_map))
    if (length(miss_keys)) {
      rlang::abort(
        paste0("liver_markers(): missing col_map entries for: ", paste(miss_keys, collapse = ", ")),
        class = "healthmarkers_liver_error_missing_map")
    }
  } else {
    # Lenient mode: identity fill any key whose column exists in data
    for (k in required) {
      if (is.null(col_map[[k]]) && k %in% names(data)) col_map[[k]] <- k
    }
    # Drop keys whose mapped column is still absent (grace: produce NA for those indices)
    for (k in names(col_map)) {
      if (!is.null(col_map[[k]]) && !col_map[[k]] %in% names(data)) col_map[[k]] <- NULL
    }
    miss_keys <- setdiff(required, names(col_map))
    if (length(miss_keys) > 0L) {
      hm_inform(level = "debug",
                msg = sprintf("liver_markers(): columns not found, affected indices will be NA: %s",
                              paste(miss_keys, collapse = ", ")))
    }
  }

  # Additional robust validation (structural only)
  .lm_validate_args(data, if (length(col_map) > 0L) col_map else list(placeholder = "x"),
                    na_warn_prop, extreme_rules)

  # Helper: pull column as numeric vector, NA if key unavailable
  .lm_col <- function(k) {
    cn <- col_map[[k]]
    if (is.null(cn)) rep(NA_real_, nrow(data)) else data[[cn]]
  }

  hm_inform(level = if (isTRUE(verbose)) "inform" else "debug",
            msg = hm_fmt_col_map(col_map[intersect(required, names(col_map))], "liver_markers"))

  avail_cols <- unlist(col_map[intersect(required, names(col_map))], use.names = FALSE)

  # HM-CS v3: coerce to numeric for all available required inputs
  for (cn in avail_cols) {
    if (identical(cn, col_map[["diabetes"]])) next
    if (!is.numeric(data[[cn]])) {
      old <- data[[cn]]
      suppressWarnings(new <- as.numeric(old))
      introduced_na <- sum(is.na(new) & !is.na(old))
      if (introduced_na > 0L) rlang::warn(sprintf("Column '%s' coerced to numeric; NAs introduced: %d", cn, introduced_na))
      data[[cn]] <- new
    }
    data[[cn]][!is.finite(data[[cn]])] <- NA_real_
  }
  # Missingness warnings only when na_action_raw == "warn"
  if (identical(na_action_raw, "warn")) {
    .lm_warn_high_missing(data, avail_cols, na_warn_prop = na_warn_prop)
  }

  # NA policy (only against available columns)
  if (identical(na_action, "error")) {
    avail_req <- intersect(required, names(col_map))
    any_na <- Reduce(`|`, lapply(avail_req, function(k) is.na(data[[col_map[[k]]]])))
    if (any(any_na)) {
      rlang::abort("liver_markers(): required inputs contain missing values (na_action='error').",
                   class = "healthmarkers_liver_error_missing_values")
    }
  } else if (identical(na_action, "omit")) {
    avail_req <- intersect(required, names(col_map))
    keep <- !Reduce(`|`, lapply(avail_req, function(k) is.na(data[[col_map[[k]]]])))
    hm_inform(sprintf("liver_markers(): omitting %d rows with NA in required inputs", sum(!keep)), level = if (isTRUE(verbose)) "inform" else "debug")
    data <- data[keep, , drop = FALSE]
  } # "keep" leaves NA as-is

  # Optional extreme scan/cap
  capped_n <- 0L
  if (isTRUE(check_extreme)) {
    rules <- if (is.null(extreme_rules)) .lm_default_extreme_rules() else extreme_rules
    ex <- .lm_extreme_scan(data, col_map, rules, required)
    if (ex$count > 0L) {
      if (extreme_action == "error") {
        rlang::abort(
          sprintf("liver_markers(): detected %d extreme input values.", ex$count),
          class = "healthmarkers_liver_error_extremes"
        )
      } else if (extreme_action == "cap") {
        data <- .lm_cap_inputs(data, ex$flags, col_map, rules)
        capped_n <- ex$count
        rlang::warn(sprintf("liver_markers(): capped %d extreme input values into allowed ranges.", ex$count))
      } else if (extreme_action == "warn") {
        rlang::warn(sprintf("liver_markers(): detected %d extreme input values (not altered).", ex$count))
      } else if (extreme_action == "NA") {
        # Set flagged extreme inputs to NA in-place
        for (nm in names(ex$flags)) {
          bad <- ex$flags[[nm]]
          if (!is.null(col_map[[nm]])) {
            cn <- col_map[[nm]]
            if (!is.null(cn) && cn %in% names(data)) {
              xi <- data[[cn]]
              xi[bad] <- NA_real_
              data[[cn]] <- xi
            }
          }
        }
      }
      # "ignore": do nothing
    }
  }

  hm_inform("liver_markers(): computing indices", level = "debug")

  # Pull vectors (NA vector when column unavailable)
  BMI        <- .lm_col("BMI")
  waist      <- .lm_col("waist")
  TG         <- .lm_col("TG")
  GGT        <- .lm_col("GGT")
  age        <- .lm_col("age")
  AST        <- .lm_col("AST")
  ALT        <- .lm_col("ALT")
  platelets  <- .lm_col("platelets")
  albumin    <- .lm_col("albumin")
  bilirubin  <- .lm_col("bilirubin")
  creatinine <- .lm_col("creatinine")

  # Diabetes: default to 0 (no diabetes) when column absent
  diabetes_x <- {
    cn <- col_map[["diabetes"]]
    if (is.null(cn)) rep(0L, nrow(data)) else data[[cn]]
  }

  # Diabetes coercion with diagnostics (preserve prior behavior: as.integer)
  diab_ok <- is.logical(diabetes_x) || all(diabetes_x %in% c(0,1,NA))
  if (!diab_ok) {
    rlang::warn("liver_markers(): `diabetes` not in {0,1,TRUE,FALSE}; coercing with as.integer().")
  }
  diabetes <- as.integer(diabetes_x)

  # Denominator/transform diagnostics
  denom_info <- list(
    platelets = list(name = "platelets (APRI/FIB4 denominators)", vec = platelets)
  )
  .lm_warn_zero_denoms(denom_info)
  .lm_warn_nonpositive_for_log(list(
    TG = TG,
    GGT = GGT,
    bilirubin = bilirubin,
    creatinine = creatinine
  ))
  .lm_warn_nonpositive_for_sqrt(list(ALT = ALT))

  # Compute markers (unchanged formulas)
  # FLI
  L <- 0.953 * log(TG) + 0.139 * BMI + 0.718 * log(GGT) + 0.053 * waist - 15.745
  FLI <- exp(L) / (1 + exp(L)) * 100

  # NFS
  NFS <- -1.675 +
    0.037 * age +
    0.094 * BMI +
    1.13 * diabetes +
    0.99 * (AST / ALT) -
    0.013 * platelets -
    0.66 * albumin

  # APRI
  APRI <- (AST / 40) / platelets * 100

  # FIB-4
  FIB4 <- (age * AST) / (platelets * sqrt(ALT))

  # BARD
  BARD <- as.integer((BMI >= 28) + (AST / ALT >= 0.8) + (diabetes == 1))

  # ALBI
  bili_umol <- bilirubin * 17.1
  ALBI <- log10(bili_umol) * 0.66 + albumin * -0.0852

  # MELD-XI
  MELD_XI <- 5.11 * log(bilirubin) + 11.76 * log(creatinine) + 9.44

  out <- tibble::tibble(
    FLI     = FLI,
    NFS     = NFS,
    APRI    = APRI,
    FIB4    = FIB4,
    BARD    = BARD,
    ALBI    = ALBI,
    MELD_XI = MELD_XI
  )

  hm_inform(level = if (isTRUE(verbose)) "inform" else "debug",
            msg = hm_result_summary(out, "liver_markers"))

  return(out)
}

# ---- internal helpers (not exported) -----------------------------------------

.lm_validate_args <- function(data, col_map, na_warn_prop, extreme_rules) {
  if (!is.data.frame(data)) {
    rlang::abort("liver_markers(): `data` must be a data.frame or tibble.",
                 class = "healthmarkers_liver_error_data_type")
  }
  if (!is.list(col_map) || is.null(names(col_map)) || any(names(col_map) == "")) {
    rlang::abort("liver_markers(): `col_map` must be a named list.",
                 class = "healthmarkers_liver_error_colmap_type")
  }
  if (!(is.numeric(na_warn_prop) && length(na_warn_prop) == 1L &&
        is.finite(na_warn_prop) && na_warn_prop >= 0 && na_warn_prop <= 1)) {
    rlang::abort("liver_markers(): `na_warn_prop` must be a single numeric in [0, 1].",
                 class = "healthmarkers_liver_error_na_warn_prop")
  }
  if (!is.null(extreme_rules)) {
    if (!is.list(extreme_rules)) {
      rlang::abort("liver_markers(): `extreme_rules` must be NULL or a named list of c(min,max).",
                   class = "healthmarkers_liver_error_extreme_rules_type")
    }
    for (nm in names(extreme_rules)) {
      rng <- extreme_rules[[nm]]
      if (!(is.numeric(rng) && length(rng) == 2L && all(is.finite(rng)) && rng[1] <= rng[2])) {
        rlang::abort(sprintf("liver_markers(): `extreme_rules[['%s']]` must be numeric length-2 with min <= max.", nm),
                     class = "healthmarkers_liver_error_extreme_rules_value")
      }
    }
  }
  invisible(TRUE)
}

.lm_default_extreme_rules <- function() {
  list(
    BMI           = c(10, 70),
    waist         = c(40, 200),
    TG            = c(10, 1500),   # mg/dL
    GGT           = c(1, 2000),    # U/L
    age           = c(18, 120),
    AST           = c(1, 5000),
    ALT           = c(1, 5000),
    platelets     = c(10, 1000),   # 10^9/L
    albumin       = c(15, 60),     # g/L
    bilirubin     = c(0.1, 40),    # mg/dL
    creatinine    = c(0.2, 20)     # mg/dL
  )
}

.lm_extreme_scan <- function(df, col_map, rules, required) {
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

.lm_cap_inputs <- function(df, flags, col_map, rules) {
  for (cn in names(flags)) {
    rn <- names(col_map)[match(cn, unlist(col_map, use.names = FALSE))]
    rn <- rn[!is.na(rn)][1]
    if (is.na(rn) || is.null(rules[[rn]])) next
    rng <- rules[[rn]]
    x <- df[[cn]]
    bad <- flags[[cn]]
    x[bad & is.finite(x) & x < rng[1]] <- rng[1]
    x[bad & is.finite(x) & x > rng[2]] <- rng[2]
    df[[cn]] <- x
  }
  df
}

.lm_warn_high_missing <- function(df, cols, na_warn_prop = 0.2) {
  for (cn in cols) {
    x <- df[[cn]]
    n <- length(x)
    if (n == 0L) next
    pna <- sum(is.na(x)) / n
    if (pna >= na_warn_prop && pna > 0) {
      rlang::warn(sprintf("liver_markers(): column '%s' has high missingness (%.1f%%).", cn, 100 * pna))
    }
    # Negative values quick check (vars expected >= 0)
    neg_n <- sum(is.finite(x) & x < 0)
    if (neg_n > 0L) {
      rlang::warn(sprintf("liver_markers(): column '%s' contains %d negative values; check units.", cn, neg_n))
    }
  }
  invisible(TRUE)
}

.lm_warn_zero_denoms <- function(denoms) {
  msgs <- character(0)
  for (nm in names(denoms)) {
    v <- denoms[[nm]]$vec
    lab <- denoms[[nm]]$name
    n0 <- sum(is.finite(v) & v == 0)
    if (n0 > 0L) msgs <- c(msgs, sprintf("%s==0: %d", lab, n0))
  }
  if (length(msgs)) {
    rlang::warn(sprintf("liver_markers(): zero denominators detected -> %s. Ratios may yield Inf/NaN.", paste(msgs, collapse = ", ")))
  }
  invisible(TRUE)
}

.lm_warn_nonpositive_for_log <- function(named_vecs) {
  for (nm in names(named_vecs)) {
    v <- named_vecs[[nm]]
    nbad <- sum(is.finite(v) & v <= 0)
    if (nbad > 0L) {
      rlang::warn(sprintf("liver_markers(): '%s' contains %d non-positive values; log() undefined.", nm, nbad))
    }
  }
  invisible(TRUE)
}

.lm_warn_nonpositive_for_sqrt <- function(named_vecs) {
  for (nm in names(named_vecs)) {
    v <- named_vecs[[nm]]
    nbad <- sum(is.finite(v) & v < 0)
    if (nbad > 0L) {
      rlang::warn(sprintf("liver_markers(): '%s' contains %d negative values; sqrt() undefined for negatives.", nm, nbad))
    }
  }
  invisible(TRUE)
}
