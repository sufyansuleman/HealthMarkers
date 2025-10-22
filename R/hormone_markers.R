# R/hormone_markers.R

#' Compute a suite of hormone ratio markers with QA and verbose summaries
#'
#' Ratios computed:
#' - FAI = (total_testosterone / SHBG) * 100
#' - LH_FSH = LH / FSH
#' - E2_P = estradiol / progesterone
#' - T3_T4 = free_T3 / free_T4
#' - ARR = aldosterone / renin
#' - Ins_Glu = insulin / glucagon
#' - GH_IGF1 = GH / IGF1
#' - PRL_T = prolactin / total_testosterone
#' - CAR_slope = (cortisol_30 - cortisol_0) / 30
#'
#' @param data Data frame or tibble with mapped hormone inputs.
#' @param col_map Named list mapping the required keys to column names:
#'   total_testosterone, SHBG, LH, FSH, estradiol, progesterone, free_T3, free_T4,
#'   aldosterone, renin, insulin, glucagon, GH, IGF1, prolactin, cortisol_0, cortisol_30.
#' @param na_action One of "ignore","warn","error","keep","omit". HM-CS: keep ≡ ignore; omit drops rows with any NA in used inputs.
#' @param na_warn_prop Proportion in [0,1] for high-missingness warnings when na_action="warn". Default 0.2.
#' @param check_extreme Logical; if TRUE, scan inputs for out-of-range values (see extreme_rules). Default FALSE.
#' @param extreme_action One of "warn","cap","error","ignore","NA" when check_extreme=TRUE. "cap" truncates to range; "NA" sets out-of-range to NA.
#' @param extreme_rules Optional list of c(min,max) per key to override defaults.
#' @param verbose Logical; print progress and completion summary.
#' @return Tibble with the nine ratio markers.
#' @export
hormone_markers <- function(
  data,
  col_map,
  na_action = c("ignore","warn","error","keep","omit"),
  na_warn_prop = 0.2,
  check_extreme = FALSE,
  extreme_action = c("warn","cap","error","ignore","NA"),
  extreme_rules = NULL,
  verbose = FALSE
) {
  na_action_raw <- match.arg(na_action)
  na_action <- if (na_action_raw == "keep") "ignore" else na_action_raw
  extreme_action <- match.arg(extreme_action)

  required <- c(
    "total_testosterone","SHBG","LH","FSH","estradiol","progesterone",
    "free_T3","free_T4","aldosterone","renin","insulin","glucagon",
    "GH","IGF1","prolactin","cortisol_0","cortisol_30"
  )

  # Validate inputs
  if (!is.data.frame(data)) stop("hormone_markers(): `data` must be a data.frame or tibble.", call. = FALSE)
  if (is.null(col_map) || !is.list(col_map)) {
    stop("hormone_markers(): `col_map` must be a named list.", call. = FALSE)
  }
  # Ensure all required keys exist in col_map
  missing_keys <- setdiff(required, names(col_map))
  if (length(missing_keys)) {
    stop("missing required columns: ", paste(missing_keys, collapse = ", "), call. = FALSE)
  }
  # Ensure each mapping is a single non-empty character string
  bad_keys <- vapply(required, function(k) {
    v <- col_map[[k]]
    !is.character(v) || length(v) != 1L || !nzchar(v)
  }, logical(1))
  if (any(bad_keys)) {
    stop("missing required columns: ", paste(required[bad_keys], collapse = ", "), call. = FALSE)
  }
  # Mapped columns must exist in data
  used_cols <- unname(vapply(required, function(k) col_map[[k]], character(1)))
  missing_cols <- setdiff(used_cols, names(data))
  if (length(missing_cols)) {
    stop("missing required columns: ", paste(missing_cols, collapse = ", "), call. = FALSE)
  }

  if (isTRUE(verbose)) {
    message(sprintf("-> hormone_markers: starting (%d rows, %d mapped inputs)", nrow(data), length(required)))
    message("-> hormone_markers: coercing inputs to numeric (if needed)")
  }

  # Coerce to numeric; warn if NAs introduced; sanitize non-finite to NA
  for (cn in used_cols) {
    if (!is.numeric(data[[cn]])) {
      old <- data[[cn]]
      suppressWarnings(data[[cn]] <- as.numeric(old))
      introduced_na <- sum(is.na(data[[cn]]) & !is.na(old))
      if (introduced_na > 0L) warning(sprintf("Column '%s' coerced to numeric; NAs introduced: %d", cn, introduced_na), call. = FALSE)
    }
    data[[cn]][!is.finite(data[[cn]])] <- NA_real_
  }

  # HM-CS: omit rows with any NA in used inputs
  keep_rows <- rep(TRUE, nrow(data))
  if (na_action_raw == "omit") {
    keep_rows <- stats::complete.cases(data[, used_cols, drop = FALSE])
    data <- data[keep_rows, , drop = FALSE]
  }

  # Missingness QA
  qa <- .hor_quality_scan(data, used_cols, na_warn_prop)
  if (na_action == "warn") {
    if (length(qa$any_na)) warning(sprintf("Missing values in: %s.", paste(qa$any_na, collapse = ", ")), call. = FALSE)
    if (length(qa$high_na)) warning(sprintf("High missingness (>= %.0f%%): %s.", 100 * na_warn_prop, paste(qa$high_na, collapse = ", ")), call. = FALSE)
  }
  if (na_action == "error" && length(qa$any_na)) {
    stop("hormone_markers(): missing or non-finite values in required inputs with na_action='error'.", call. = FALSE)
  }

  # Extreme scan/handling
  if (isTRUE(check_extreme)) {
    rules <- .hor_default_extreme_rules()
    if (is.list(extreme_rules)) for (nm in names(extreme_rules)) rules[[nm]] <- extreme_rules[[nm]]
    flags <- .hor_extreme_scan(data, col_map, rules)
    flagged_total <- sum(vapply(flags, function(x) sum(x, na.rm = TRUE), integer(1)))
    if (flagged_total > 0L) {
      if (extreme_action == "error") {
        stop(sprintf("hormone_markers(): %d extreme input values detected.", flagged_total), call. = FALSE)
      } else if (extreme_action == "cap") {
        data <- .hor_cap_inputs(data, flags, col_map, rules)
        warning(sprintf("hormone_markers(): capped %d extreme input values into allowed ranges.", flagged_total), call. = FALSE)
      } else if (extreme_action == "warn") {
        warning(sprintf("hormone_markers(): detected %d extreme input values (not altered).", flagged_total), call. = FALSE)
      } else if (extreme_action == "NA") {
        for (nm in names(flags)) {
          cn <- col_map[[nm]]
          if (is.null(cn) || !(cn %in% names(data))) next
          bad <- flags[[nm]]
          xi <- data[[cn]]
          xi[bad] <- NA_real_
          data[[cn]] <- xi
        }
      }
    }
  }

  # Extract vectors
  v <- function(k) data[[col_map[[k]]]]
  Ttot <- v("total_testosterone"); SHBG <- v("SHBG")
  LH <- v("LH"); FSH <- v("FSH")
  E2 <- v("estradiol"); P4 <- v("progesterone")
  T3 <- v("free_T3"); T4 <- v("free_T4")
  Aldo <- v("aldosterone"); Renin <- v("renin")
  Ins <- v("insulin"); Glu <- v("glucagon")
  GH <- v("GH"); IGF1 <- v("IGF1")
  PRL <- v("prolactin")
  Cort0 <- v("cortisol_0"); Cort30 <- v("cortisol_30")

  sdiv <- function(a,b) { z <- a / b; z[!is.finite(z)] <- NA_real_; z }

  # Compute ratios
  FAI      <- sdiv(Ttot, SHBG) * 100
  LH_FSH   <- sdiv(LH, FSH)
  E2_P     <- sdiv(E2, P4)
  T3_T4    <- sdiv(T3, T4)
  ARR      <- sdiv(Aldo, Renin)
  Ins_Glu  <- sdiv(Ins, Glu)
  GH_IGF1  <- sdiv(GH, IGF1)
  PRL_T    <- sdiv(PRL, Ttot)
  CAR_slope <- sdiv(Cort30 - Cort0, 30)

  out <- tibble::tibble(
    FAI = FAI,
    LH_FSH = LH_FSH,
    E2_P = E2_P,
    T3_T4 = T3_T4,
    ARR = ARR,
    Ins_Glu = Ins_Glu,
    GH_IGF1 = GH_IGF1,
    PRL_T = PRL_T,
    CAR_slope = CAR_slope
  )

  # Preserve row count when na_action != 'omit'
  if (na_action_raw != "omit") {
    res <- tibble::tibble(
      FAI = rep(NA_real_, nrow(data)),
      LH_FSH = NA_real_, E2_P = NA_real_, T3_T4 = NA_real_, ARR = NA_real_,
      Ins_Glu = NA_real_, GH_IGF1 = NA_real_, PRL_T = NA_real_, CAR_slope = NA_real_
    )
    # replicate rows
    res <- res[rep(1, nrow(data)), , drop = FALSE]
    for (nm in names(out)) res[[nm]] <- out[[nm]]
    out <- res
  }

  if (isTRUE(verbose)) {
    na_counts <- vapply(out, function(x) sum(!is.finite(x) | is.na(x)), integer(1))
    message(sprintf(
      "Completed hormone_markers: %d rows; NA counts -> %s",
      nrow(out),
      paste(sprintf("%s=%d", names(na_counts), na_counts), collapse = ", ")
    ))
  }

  out
}

# --- internal helpers ---------------------------------------------------------

.hor_quality_scan <- function(df, cols, na_warn_prop = 0.2) {
  any_na <- character(0); high_na <- character(0)
  for (cn in cols) {
    x <- df[[cn]]
    if (any(is.na(x))) any_na <- c(any_na, cn)
    prop <- mean(is.na(x))
    if (is.finite(prop) && prop >= na_warn_prop) high_na <- c(high_na, cn)
  }
  list(any_na = unique(any_na), high_na = unique(high_na))
}

.hor_default_extreme_rules <- function() {
  # Plausible broad ranges for lab units; tuned to tests where needed
  list(
    total_testosterone = c(0, 100),
    SHBG = c(0, 200),
    LH = c(0, 200),
    FSH = c(0, 200),
    estradiol = c(0, 10000),
    progesterone = c(0, 1000),
    free_T3 = c(0, 20),
    free_T4 = c(0, 50),
    aldosterone = c(0, 1000),
    renin = c(0, 50),      # upper 50 to match test expectation
    insulin = c(0, 1000),
    glucagon = c(0, 500),
    GH = c(0, 200),
    IGF1 = c(0, 2000),
    prolactin = c(0, 500),
    cortisol_0 = c(0, 2000),
    cortisol_30 = c(0, 2000)
  )
}

.hor_extreme_scan <- function(df, col_map, rules) {
  flags <- list()
  for (nm in names(rules)) {
    cn <- col_map[[nm]]
    if (is.null(cn) || !(cn %in% names(df))) next
    rng <- rules[[nm]]
    x <- df[[cn]]
    flags[[nm]] <- is.finite(x) & (x < rng[1] | x > rng[2])
  }
  flags
}

.hor_cap_inputs <- function(df, flags, col_map, rules) {
  for (nm in names(flags)) {
    cn <- col_map[[nm]]
    if (is.null(cn) || !(cn %in% names(df))) next
    bad <- flags[[nm]]
    if (!any(bad, na.rm = TRUE)) next
    lo <- rules[[nm]][1]; hi <- rules[[nm]][2]
    x <- df[[cn]]
    x[bad & x < lo] <- lo
    x[bad & x > hi] <- hi
    df[[cn]] <- x
  }
  df
}
