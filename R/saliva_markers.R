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
#' @param data A data.frame or tibble containing at least:
#'   - `saliva_cort1` (nmol/L at wake)
#'   - `saliva_cort2` (nmol/L ~30 min)
#'   - `saliva_cort3` (nmol/L ~60 min)
#'   - `saliva_amylase` (U/mL)
#'   - `saliva_glucose` (mg/dL)
#' @param verbose Logical; if `TRUE`, prints progress messages.
#' @param na_action One of `c("keep","omit","error")` for required-input NA handling. Default "keep".
#' @param na_warn_prop Proportion [0,1] to trigger high-missingness warnings. Default 0.2.
#' @param check_extreme Logical; if TRUE, scan inputs for extreme values. Default FALSE.
#' @param extreme_action One of `c("warn","cap","error","ignore")` when extremes detected. Default "warn".
#' @param extreme_rules Optional named list of c(min,max) bounds for inputs. If NULL, broad defaults used.
#' @param times Numeric vector of sampling times (minutes) for CAR AUC. Must align with cort1/2/3. Default c(0,30,60).
#'
#' @return A tibble with columns:
#'   - `log_cortisol_wake`
#'   - `CAR_AUC`
#'   - `log_amylase`
#'   - `saliva_glucose`
#' @importFrom tibble tibble
#' @importFrom rlang abort warn inform
#' @export
#' @examples
#' df <- tibble::tibble(
#'   saliva_cort1    = 12.5,
#'   saliva_cort2    = 18.0,
#'   saliva_cort3    = 16.2,
#'   saliva_amylase  = 85,
#'   saliva_glucose  = 4.2
#' )
#' saliva_markers(df)
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
saliva_markers <- function(data,
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
    rlang::abort("saliva_markers(): `data` must be a data.frame or tibble.")
  }

  if (isTRUE(verbose)) rlang::inform("-> saliva_markers: validating inputs")
  t0 <- Sys.time()

  # 1) validate required columns
  req <- c("saliva_cort1","saliva_cort2","saliva_cort3","saliva_amylase","saliva_glucose")
  missing_cols <- setdiff(req, names(data))
  if (length(missing_cols)) {
    rlang::abort(
      paste0("saliva_markers(): missing columns: ", paste(missing_cols, collapse = ", "))
    )
  }

  # 2) Coerce to numeric where needed and warn on NAs introduced
  to_num <- req
  for (cn in to_num) {
    if (!is.numeric(data[[cn]])) {
      old <- data[[cn]]
      suppressWarnings(data[[cn]] <- as.numeric(old))
      introduced <- sum(is.na(data[[cn]]) & !is.na(old))
      if (introduced > 0) {
        rlang::warn(sprintf("saliva_markers(): column '%s' coerced to numeric; NAs introduced: %d", cn, introduced))
      }
    }
  }

  # 3) High-missingness warnings on required inputs
  for (cn in req) {
    x <- data[[cn]]
    n <- length(x)
    if (n == 0L) next
    pna <- sum(is.na(x)) / n
    if (pna >= na_warn_prop && pna > 0) {
      rlang::warn(sprintf("saliva_markers(): column '%s' has high missingness (%.1f%%).", cn, 100 * pna))
    }
  }

  # 4) NA policy on required inputs
  if (na_action == "error") {
    has_na <- Reduce(`|`, lapply(req, function(cn) is.na(data[[cn]])))
    if (any(has_na)) {
      rlang::abort("saliva_markers(): required inputs contain missing values (na_action='error').")
    }
  } else if (na_action == "omit") {
    keep <- !Reduce(`|`, lapply(req, function(cn) is.na(data[[cn]])))
    if (isTRUE(verbose)) rlang::inform(sprintf("-> saliva_markers: omitting %d rows with NA in required inputs", sum(!keep)))
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

  # 5) Optional extremes scan/cap on inputs
  capped_n <- 0L
  if (isTRUE(check_extreme)) {
    rules <- if (is.null(extreme_rules)) {
      list(
        saliva_cort1   = c(0, 2000),   # nmol/L (very broad)
        saliva_cort2   = c(0, 2000),
        saliva_cort3   = c(0, 2000),
        saliva_amylase = c(0, 50000),  # U/mL (broad)
        saliva_glucose = c(0, 1000)    # mg/dL (broad)
      )
    } else extreme_rules

    # detect extremes
    ex_counts <- integer(0)
    for (nm in names(rules)) {
      if (!nm %in% names(data)) next
      rng <- rules[[nm]]
      x <- data[[nm]]
      bad <- is.finite(x) & (x < rng[1] | x > rng[2])
      ex_counts[nm] <- sum(bad, na.rm = TRUE)
      if (extreme_action == "cap") {
        x[bad & is.finite(x) & x < rng[1]] <- rng[1]
        x[bad & is.finite(x) & x > rng[2]] <- rng[2]
        data[[nm]] <- x
      }
    }
    total_ex <- sum(ex_counts, na.rm = TRUE)
    if (total_ex > 0) {
      if (extreme_action == "error") {
        rlang::abort(sprintf("saliva_markers(): detected %d extreme input values.", total_ex))
      } else if (extreme_action == "cap") {
        capped_n <- total_ex
        rlang::warn(sprintf("saliva_markers(): capped %d extreme input values into allowed ranges.", total_ex))
      } else if (extreme_action == "warn") {
        rlang::warn(sprintf("saliva_markers(): detected %d extreme input values (not altered).", total_ex))
      }
    }
  }

  if (isTRUE(verbose)) rlang::inform("-> saliva_markers: computing markers")

  # 6) Helpers
  safe_log <- function(x) {
    y <- x
    y[!(is.finite(y) & y > 0)] <- NA_real_
    out <- suppressWarnings(log(y))
    out[!is.finite(out)] <- NA_real_
    out
  }

  # 7) Compute outputs
  log_cortisol_wake <- safe_log(data$saliva_cort1)
  log_amylase       <- safe_log(data$saliva_amylase)

  # Validate times
  if (!(is.numeric(times) && length(times) == 3L && isTRUE(all(diff(times) >= 0)))) {
    rlang::abort("saliva_markers(): `times` must be a numeric vector of length 3 in non-decreasing order.")
  }
  cort_mat <- cbind(data$saliva_cort1, data$saliva_cort2, data$saliva_cort3)

  # Rowwise trapezoidal AUC; if any cortisol point is NA, return NA
  dt <- diff(times)
  CAR_AUC <- apply(cort_mat, 1, function(x) {
    if (any(!is.finite(x))) return(NA_real_)
    sum((x[-length(x)] + x[-1]) / 2 * dt)
  })

  out <- tibble::tibble(
    log_cortisol_wake = as.numeric(log_cortisol_wake),
    CAR_AUC           = as.numeric(CAR_AUC),
    log_amylase       = as.numeric(log_amylase),
    saliva_glucose    = as.numeric(data$saliva_glucose)
  )

  if (isTRUE(verbose)) {
    na_counts <- vapply(out, function(x) sum(is.na(x) | !is.finite(x)), integer(1))
    elapsed <- as.numeric(difftime(Sys.time(), t0, units = "secs"))
    rlang::inform(sprintf(
      "Completed saliva_markers: %d rows; NA/Inf -> %s; capped=%d; elapsed=%.2fs",
      nrow(out),
      paste(sprintf("%s=%d", names(na_counts), na_counts), collapse = ", "),
      capped_n, elapsed
    ))
  }

  out
}
