# R/sweat_markers.R

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
#' @param data A data.frame or tibble containing at least:
#'   - `sweat_chloride` (mmol/L)
#'   - `sweat_Na`, `sweat_K` (mmol/L)
#'   - `sweat_lactate` (mmol/L)
#'   - `weight_before`, `weight_after` (kg)
#'   - `duration` (h)
#'   - `body_surface_area` (m^2)
#' @param verbose Logical; if `TRUE`, prints progress messages and a completion summary.
#' @param na_action One of `c("keep","omit","error")` for handling missing values in required inputs. Default "keep".
#' @param na_warn_prop Proportion [0,1] to trigger high-missingness warnings for required inputs. Default 0.2.
#' @param check_extreme Logical; if TRUE, scan inputs for extreme values. Default FALSE.
#' @param extreme_action One of `c("warn","cap","error","ignore")` when extremes detected. Default "warn".
#' @param extreme_rules Optional named list of c(min,max) bounds for inputs. If NULL, broad defaults are used.
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
#' Gibson LE, Cooke RE. A test for concentration of electrolytes in sweat in cystic fibrosis of the pancreas utilizing pilocarpine by iontophoresis. Pediatrics. 1959;23(3):545–549. (Sweat chloride test origin)
#' Dill DB, Costill DL. Calculation of percentage changes in volumes of blood, plasma, and red cells in dehydration. J Appl Physiol. 1974;37(2):247–248. \doi{10.1152/jappl.1974.37.2.247}
#' Farrell PM, White TB, Ren CL, et al. Diagnosis of cystic fibrosis: consensus guidelines from the Cystic Fibrosis Foundation. J Pediatr. 2017;181S:S4–S15.e1. \doi{10.1016/j.jpeds.2016.09.064}
#' Sawka MN, Cheuvront SN, Kenefick RW. Hypohydration and human performance: impact of environment and physiological mechanisms. Sports Med. 2015;45(Suppl 1):S51–S60. \doi{10.1007/s40279-015-0395-7}
sweat_markers <- function(data,
                          verbose = FALSE,
                          na_action = c("keep","omit","error"),
                          na_warn_prop = 0.2,
                          check_extreme = FALSE,
                          extreme_action = c("warn","cap","error","ignore"),
                          extreme_rules = NULL) {
  na_action <- match.arg(na_action)
  extreme_action <- match.arg(extreme_action)

  if (!is.data.frame(data)) {
    rlang::abort("sweat_markers(): `data` must be a data.frame or tibble.")
  }
  if (isTRUE(verbose)) rlang::inform("-> sweat_markers: validating inputs")
  t0 <- Sys.time()

  # 1) required columns
  req <- c(
    "sweat_chloride", "sweat_Na", "sweat_K", "sweat_lactate",
    "weight_before", "weight_after", "duration", "body_surface_area"
  )
  missing_cols <- setdiff(req, names(data))
  if (length(missing_cols)) {
    rlang::abort(paste0("sweat_markers(): missing columns: ", paste(missing_cols, collapse = ", ")))
  }

  # 2) Coerce to numeric where needed; warn on NAs introduced
  to_num <- req
  for (cn in to_num) {
    if (!is.numeric(data[[cn]])) {
      old <- data[[cn]]
      suppressWarnings(data[[cn]] <- as.numeric(old))
      introduced <- sum(is.na(data[[cn]]) & !is.na(old))
      if (introduced > 0) {
        rlang::warn(sprintf("sweat_markers(): column '%s' coerced to numeric; NAs introduced: %d", cn, introduced))
      }
    }
  }

  # 3) High-missingness warnings
  for (cn in req) {
    x <- data[[cn]]
    n <- length(x)
    if (n == 0L) next
    pna <- sum(is.na(x)) / n
    if (pna >= na_warn_prop && pna > 0) {
      rlang::warn(sprintf("sweat_markers(): column '%s' has high missingness (%.1f%%).", cn, 100 * pna))
    }
  }

  # 4) NA policy
  if (na_action == "error") {
    has_na <- Reduce(`|`, lapply(req, function(cn) is.na(data[[cn]])))
    if (any(has_na)) {
      rlang::abort("sweat_markers(): required inputs contain missing values (na_action='error').")
    }
  } else if (na_action == "omit") {
    keep <- !Reduce(`|`, lapply(req, function(cn) is.na(data[[cn]])))
    if (isTRUE(verbose)) rlang::inform(sprintf("-> sweat_markers: omitting %d rows with NA in required inputs", sum(!keep)))
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

  # 5) Optional extremes scan/cap
  capped_n <- 0L
  if (isTRUE(check_extreme)) {
    rules <- if (is.null(extreme_rules)) {
      list(
        sweat_chloride = c(0, 200),      # mmol/L, broad
        sweat_Na       = c(0, 200),      # mmol/L
        sweat_K        = c(0, 40),       # mmol/L
        sweat_lactate  = c(0, 50),       # mmol/L
        weight_before  = c(10, 400),     # kg
        weight_after   = c(10, 400),     # kg
        duration       = c(0.05, 48),    # h
        body_surface_area = c(0.3, 3.0)  # m^2
      )
    } else extreme_rules

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
        rlang::abort(sprintf("sweat_markers(): detected %d extreme input values.", total_ex))
      } else if (extreme_action == "cap") {
        capped_n <- total_ex
        rlang::warn(sprintf("sweat_markers(): capped %d extreme input values into allowed ranges.", total_ex))
      } else if (extreme_action == "warn") {
        rlang::warn(sprintf("sweat_markers(): detected %d extreme input values (not altered).", total_ex))
      }
    }
  }

  if (isTRUE(verbose)) rlang::inform("-> sweat_markers: computing markers")

  # 6) Safe division helper with consolidated zero-denominator tracking
  dz_env <- new.env(parent = emptyenv()); dz_env$counts <- list()
  safe_div <- function(num, den, label) {
    out <- num / den
    zero_den <- is.finite(den) & den == 0
    dz_env$counts[[label]] <- sum(zero_den, na.rm = TRUE)
    out[!is.finite(out)] <- NA_real_
    out
  }

  # 7) Compute metrics
  Na_K_ratio <- safe_div(data$sweat_Na, data$sweat_K, "Na_K_ratio")

  mass_loss_kg <- data$weight_before - data$weight_after
  rate_kg_per_h <- safe_div(mass_loss_kg, data$duration, "sweat_rate_duration")
  sweat_rate <- safe_div(rate_kg_per_h, data$body_surface_area, "sweat_rate_bsa")
  # 1 kg ~ 1 L water; units become L/m^2/h

  out <- tibble::tibble(
    sweat_chloride = as.numeric(data$sweat_chloride),
    Na_K_ratio     = as.numeric(Na_K_ratio),
    sweat_lactate  = as.numeric(data$sweat_lactate),
    sweat_rate     = as.numeric(sweat_rate)
  )

  # 8) Consolidated zero-denominator warning
  dz <- dz_env$counts
  dz_total <- if (length(dz)) sum(unlist(dz), na.rm = TRUE) else 0L
  if (dz_total > 0L) {
    nz <- unlist(dz); nz <- nz[nz > 0]
    lbl <- paste(sprintf("%s=%d", names(nz), nz), collapse = ", ")
    rlang::warn(sprintf("sweat_markers(): zero denominators detected in %d cases (%s).", dz_total, lbl))
  }

  if (isTRUE(verbose)) {
    na_counts <- vapply(out, function(x) sum(is.na(x) | !is.finite(x)), integer(1))
    elapsed <- as.numeric(difftime(Sys.time(), t0, units = "secs"))
    rlang::inform(sprintf(
      "Completed sweat_markers: %d rows; NA/Inf -> %s; capped=%d; denom_zero=%d; elapsed=%.2fs",
      nrow(out),
      paste(sprintf("%s=%d", names(na_counts), na_counts), collapse = ", "),
      capped_n, dz_total, elapsed
    ))
  }

  out
}
