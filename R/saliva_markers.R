# R/saliva_markers.R

#' Calculate saliva-based stress & glycemic markers
#'
#' Computes:
#'  - log_cortisol_wake (log-transformed waking cortisol)
#'  - CAR_AUC           (Cortisol Awakening Response, trapezoidal AUC over 0–60 min by default)
#'  - log_amylase       (log-transformed salivary α-amylase)
#'  - saliva_glucose    (raw salivary glucose)
#'
#' Inputs are validated, missingness handled via `na_action`, logs made safe
#' (<= 0 -> NA), and optional extremes scan/cap is available.
#'
#' @param data A data.frame or tibble with salivary markers.
#' @param col_map Named list mapping required inputs. Defaults assume same names:
#'   - cort1   -> "saliva_cort1" (nmol/L at wake)
#'   - cort2   -> "saliva_cort2" (nmol/L ~30 min)
#'   - cort3   -> "saliva_cort3" (nmol/L ~60 min)
#'   - amylase -> "saliva_amylase" (U/mL)
#'   - glucose -> "saliva_glucose" (mg/dL)
#' @param verbose Logical; if `TRUE`, prints progress messages via hm_inform().
#' @param na_action One of `c("keep","omit","error")` for required-input NA handling. Default "keep".
#' @param na_warn_prop Proportion [0,1] to trigger high-missingness diagnostics (debug). Default 0.2.
#' @param check_extreme Logical; if TRUE, scan inputs for extreme values. Default FALSE.
#' @param extreme_action One of `c("warn","cap","error","ignore")` when extremes detected. Default "warn".
#' @param extreme_rules Optional named list of c(min,max) bounds. If NULL, broad defaults are used (keyed by mapped column names).
#' @param times Numeric vector of sampling times (minutes) for CAR AUC. Must align with cort1/2/3. Default c(0,30,60).
#'
#' @return A tibble with columns:
#'   - `log_cortisol_wake`
#'   - `CAR_AUC`
#'   - `log_amylase`
#'   - `saliva_glucose`
#'
#' @examples
#' df <- tibble::tibble(
#'   saliva_cort1    = 12.5,
#'   saliva_cort2    = 18.0,
#'   saliva_cort3    = 16.2,
#'   saliva_amylase  = 85,
#'   saliva_glucose  = 4.2
#' )
#' saliva_markers(df)  # uses default col_map
#'
#' @references
#'  Original derivations
#'  Pruessner JC, Kirschbaum C, Meinlschmid G, Hellhammer DH. Two formulas for computation of the area under the curve represent measures of total hormone concentration versus time-dependent change. 
#'   Psychoneuroendocrinology. 2003;28(7):916–931. \doi{10.1016/S0306-4530(02)00108-7} (AUC hormone measures)
#'  Kirschbaum C, Hellhammer DH. Salivary cortisol in psychoneuroendocrine research: recent developments and applications. 
#'   Psychoneuroendocrinology. 1994;19(4):313–333. \doi{10.1016/0306-4530(94)90013-2} (Salivary cortisol methods)
#'
#'  Validation and applications
#'  Clow A, Thorn L, Evans P, Hucklebridge F. The awakening cortisol response: methodological issues and significance. 
#'   Stress. 2004;7(1):29–37. \doi{10.1080/10253890410001667205} (Cortisol awakening response)
#'  Nater UM, Rohleder N. Salivary alpha-amylase as a non-invasive biomarker for the sympathetic nervous system: current state of research. 
#'   Psychoneuroendocrinology. 2009;34(4):486–496. \doi{10.1016/j.psyneuen.2009.01.014} (Salivary α-amylase marker)
#'  Scales WE, Freeman EW, McCoy NL, Klerman EB. Salivary glucose as a measure of blood glucose: correlations and applications. 
#'   Diabetes Care. 1987;10(4):414–418. \doi{10.2337/diacare.10.4.414} (Salivary glucose application)
#'
#' @importFrom tibble tibble
#' @importFrom rlang abort warn inform
#' @export
saliva_markers <- function(data,
                           col_map = list(
                             cort1 = "saliva_cort1",
                             cort2 = "saliva_cort2",
                             cort3 = "saliva_cort3",
                             amylase = "saliva_amylase",
                             glucose = "saliva_glucose"
                           ),
                           verbose = FALSE,
                           na_action = c("keep","omit","error"),
                           na_warn_prop = 0.2,
                           check_extreme = FALSE,
                           extreme_action = c("warn","cap","error","ignore"),
                           extreme_rules = NULL,
                           times = c(0, 30, 60)) {
  na_action <- match.arg(na_action)
  extreme_action <- match.arg(extreme_action)
  if (!is.data.frame(data)) {
    rlang::abort("saliva_markers(): `data` must be a data.frame or tibble.",
                 class = "healthmarkers_saliva_error_data_type")
  }

  # HM-CS v2: standardized validation
  required_keys <- c("cort1","cort2","cort3","amylase","glucose")
  hm_validate_inputs(data, col_map, required_keys = required_keys, fn = "saliva_markers")

  if (isTRUE(verbose)) hm_inform(level = "inform", msg = "-> saliva_markers: validating inputs")
  t0 <- Sys.time()

  # Ensure mapped columns exist
  mapped <- unname(unlist(col_map[required_keys], use.names = FALSE))
  missing_cols <- setdiff(mapped, names(data))
  if (length(missing_cols)) {
    rlang::abort(
      paste0("saliva_markers(): missing columns: ", paste(missing_cols, collapse = ", ")),
      class = "healthmarkers_saliva_error_missing_columns"
    )
  }

  # Coerce to numeric where needed and warn on NAs introduced; set non-finite -> NA
  for (cn in mapped) {
    if (!is.numeric(data[[cn]])) {
      old <- data[[cn]]
      suppressWarnings(new <- as.numeric(old))
      introduced <- sum(is.na(new) & !is.na(old))
      if (introduced > 0) {
        rlang::warn(sprintf("saliva_markers(): column '%s' coerced to numeric; NAs introduced: %d", cn, introduced))
      }
      data[[cn]] <- new
    }
    data[[cn]][!is.finite(data[[cn]])] <- NA_real_
  }

  # High-missingness diagnostics (debug)
  for (cn in mapped) {
    x <- data[[cn]]; n <- length(x); if (!n) next
    pna <- sum(is.na(x)) / n
    if (pna >= na_warn_prop && pna > 0) {
      hm_inform(level = "debug", msg = sprintf("saliva_markers(): column '%s' has high missingness (%.1f%%).", cn, 100 * pna))
    }
  }

  # NA policy on required inputs
  if (na_action == "error") {
    has_na <- Reduce(`|`, lapply(mapped, function(cn) is.na(data[[cn]])))
    if (any(has_na)) {
      rlang::abort("saliva_markers(): required inputs contain missing values (na_action='error').",
                   class = "healthmarkers_saliva_error_missing_values")
    }
  } else if (na_action == "omit") {
    keep <- !Reduce(`|`, lapply(mapped, function(cn) is.na(data[[cn]])))
    if (isTRUE(verbose)) hm_inform(level = "inform", msg = sprintf("-> saliva_markers: omitting %d rows with NA in required inputs", sum(!keep)))
    data <- data[keep, , drop = FALSE]
  }

  # Early return if empty
  if (nrow(data) == 0L) {
    return(tibble::tibble(
      log_cortisol_wake = numeric(),
      CAR_AUC           = numeric(),
      log_amylase       = numeric(),
      saliva_glucose    = numeric()
    ))
  }

  # Optional extremes scan/cap on inputs (keyed by mapped column names)
  capped_n <- 0L
  if (isTRUE(check_extreme)) {
    rules <- if (is.null(extreme_rules)) {
      setNames(
        list(c(0, 2000), c(0, 2000), c(0, 2000), c(0, 50000), c(0, 1000)),
        mapped
      )
    } else extreme_rules

    # Allow rules to be keyed by input keys (cort1/cort2/...) or by actual column names
    if (!is.null(names(rules))) {
      key_to_col <- stats::setNames(mapped, required_keys)
      remapped <- list()
      for (nm in names(rules)) {
        col_nm <- if (nm %in% names(key_to_col)) key_to_col[[nm]] else nm
        remapped[[col_nm]] <- rules[[nm]]
      }
      rules <- remapped
    }

    total_ex <- 0L
    for (nm in names(rules)) {
      if (!nm %in% names(data)) next
      rng <- rules[[nm]]
      x <- data[[nm]]
      bad <- is.finite(x) & (x < rng[1] | x > rng[2])
      nbad <- sum(bad, na.rm = TRUE)
      total_ex <- total_ex + nbad
      if (extreme_action == "cap" && nbad > 0) {
        x[bad & is.finite(x) & x < rng[1]] <- rng[1]
        x[bad & is.finite(x) & x > rng[2]] <- rng[2]
        data[[nm]] <- x
      }
    }
    if (total_ex > 0) {
      if (extreme_action == "error") {
        rlang::abort(sprintf("saliva_markers(): detected %d extreme input values.", total_ex),
                     class = "healthmarkers_saliva_error_extremes")
      } else if (extreme_action == "cap") {
        capped_n <- total_ex
        rlang::warn(sprintf("saliva_markers(): capped %d extreme input values into allowed ranges.", total_ex))
      } else if (extreme_action == "warn") {
        rlang::warn(sprintf("saliva_markers(): detected %d extreme input values (not altered).", total_ex))
      }
      # "ignore": no-op
    }
  }

  if (isTRUE(verbose)) hm_inform(level = "inform", msg = "-> saliva_markers: computing markers")

  # Helpers
  safe_log <- function(x) {
    y <- x
    y[!(is.finite(y) & y > 0)] <- NA_real_
    out <- suppressWarnings(log(y))
    out[!is.finite(out)] <- NA_real_
    out
  }

  # Extract mapped inputs
  c1 <- data[[col_map$cort1]]
  c2 <- data[[col_map$cort2]]
  c3 <- data[[col_map$cort3]]
  amy <- data[[col_map$amylase]]
  glu <- data[[col_map$glucose]]

  # Compute outputs
  log_cortisol_wake <- safe_log(c1)
  log_amylase       <- safe_log(amy)

  # Validate times
  if (!(is.numeric(times) && length(times) == 3L && isTRUE(all(diff(times) >= 0)))) {
    rlang::abort("saliva_markers(): `times` must be a numeric vector of length 3 in non-decreasing order.",
                 class = "healthmarkers_saliva_error_times")
  }
  cort_mat <- cbind(c1, c2, c3)
  dt <- diff(times)
  CAR_AUC <- apply(cort_mat, 1, function(x) {
    if (any(!is.finite(x))) return(NA_real_)
    sum((x[-length(x)] + x[-1]) / 2 * dt)
  })

  out <- tibble::tibble(
    log_cortisol_wake = as.numeric(log_cortisol_wake),
    CAR_AUC           = as.numeric(CAR_AUC),
    log_amylase       = as.numeric(log_amylase),
    saliva_glucose    = as.numeric(glu)
  )

  if (isTRUE(verbose)) {
    na_counts <- vapply(out, function(x) sum(is.na(x) | !is.finite(x)), integer(1))
    elapsed <- as.numeric(difftime(Sys.time(), t0, units = "secs"))
    hm_inform(level = "inform", msg = sprintf(
      "Completed saliva_markers: %d rows; NA/Inf -> %s; capped=%d; elapsed=%.2fs",
      nrow(out),
      paste(sprintf("%s=%d", names(na_counts), na_counts), collapse = ", "),
      capped_n, elapsed
    ))
  }

  out
}
