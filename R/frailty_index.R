
#' Compute Frailty (Deficit) Index using di::di with QA and verbose summaries
#'
#' Thin wrapper around the di package's di() that:
#' - Validates inputs and arguments.
#' - Coerces tibbles to base data.frames (for di()'s class checks).
#' - Auto-selects numeric deficit columns when cols = NULL (excluding age if supplied).
#' - Optionally scans for missing/out-of-range values with warnings or errors.
#' - Provides step-by-step verbose output and a completion summary.
#' - Optionally returns a tidy tibble instead of the original list.
#'
#' Background
#' The Frailty Index (FI) is computed as the proportion of health deficits present
#' in an individual across a set of candidate deficits. The approach was introduced
#' and formalized by Rockwood and Mitnitski and subsequently standardized for
#' construction and reporting.
#'
#' @param data A data.frame or tibble of health deficits (ideally binary/logical or
#'   scaled to \eqn{[0,1]}). Non-binary numeric columns can be rescaled by di::di when
#'   rescale = TRUE.
#' @param cols Character vector of deficit column names to use. If NULL (default),
#'   all numeric columns are used except age (if supplied).
#' @param invert Character vector of column names whose values should be inverted
#'   by di::di (e.g., where higher values indicate better health).
#' @param rescale Logical; if TRUE, non-binary columns will be rescaled to \eqn{[0,1]}
#'   by di::di. Default TRUE.
#' @param age Optional name of the column holding age (used by di for plotting and
#'   optional age-binned outputs; excluded from auto-selected cols).
#' @param rescale.custom Advanced argument passed through to di::di.
#'   See di::di documentation for syntax.
#' @param rescale.avoid Advanced argument passed through to di::di;
#'   see di::di documentation for syntax.
#' @param bins Integer; number of age bins for FI-by-age plots. Default 7.
#' @param visible Logical; if TRUE and age is provided, di will draw a plot (via
#'   plot.di()). Default FALSE.
#' @param na_action One of:
#'   - Legacy: "ignore","warn","error"
#'   - HM-CS:  "keep","omit" (keep == ignore; omit drops rows with any NA in selected deficits)
#'   Default "ignore".
#' @param na_warn_prop Proportion in \eqn{[0,1]} above which a high-missingness warning
#'   is emitted (per column) when na_action = "warn". Default 0.2.
#' @param check_extreme NULL/TRUE/FALSE gate for out-of-range scan:
#'   - NULL (default): legacy behavior (scan only when rescale = FALSE)
#'   - TRUE: always scan selected deficits for values < 0 or > 1 before di::di
#'   - FALSE: never scan for extremes
#' @param extreme_action One of "warn","ignore","error","cap","NA" for out-of-range handling when scanning is enabled.
#'   - "cap": truncate to \eqn{[0,1]}
#'   - "NA": set out-of-range to NA
#'   Default "warn".
#' @param return One of c("list","data"). "list" (default) returns the original
#'   di::di result (backward compatible). "data" returns a tibble with one row
#'   per individual, columns: di (the frailty index) plus the selected deficit
#'   columns (post-capping if applied). Age is included if present.
#' @param verbose Logical; if TRUE, prints progress and a completion summary. Default FALSE.
#'
#' @return
#' - If return = "list" (default): the object returned by di::di (typically a list
#'   with di and columns).
#' - If return = "data": a tibble with di and the selected columns.
#'
#' @examples
#' # Minimal example (runs only if the 'di' package is installed)
#' if (requireNamespace("di", quietly = TRUE)) {
#'   df <- data.frame(
#'     age = c(70, 75, 80),
#'     d1 = c(0, 1, 1),
#'     d2 = c(0.2, 0.8, 1.0),
#'     d3 = c(TRUE, FALSE, TRUE)
#'   )
#'   # Auto-select numeric deficits; returns list (di, columns)
#'   res <- frailty_index(df, cols = NULL, age = "age", verbose = TRUE)
#'   # Tidy tibble return
#'   tb  <- frailty_index(df, cols = c("d1","d2","d3"), age = "age",
#'                        return = "data", verbose = TRUE)
#' }
#'
#' @references
#' \insertRef{mitnitski2001deficit}{HealthMarkers}
#' \insertRef{rockwood2007frailty}{HealthMarkers}
#' \insertRef{searle2008frailtyindex}{HealthMarkers}
#' \insertRef{rockwood2020cfs}{HealthMarkers}
#' \insertRef{cesari2014frailtyphenotype}{HealthMarkers}
#'
#' @export
frailty_index <- function(data,
                          cols = NULL,
                          invert = NULL,
                          rescale = TRUE,
                          age = NULL,
                          rescale.custom = NULL,
                          rescale.avoid = NULL,
                          bins = 7,
                          visible = FALSE,
                          na_action = c("ignore","warn","error","keep","omit"),
                          na_warn_prop = 0.2,
                          check_extreme = NULL,
                          extreme_action = c("warn","ignore","error","cap","NA"),
                          return = c("list","data"),
                          verbose = FALSE) {
  na_action <- match.arg(na_action)
  extreme_action <- match.arg(extreme_action)
  return <- match.arg(return)

  .need_pkg_di()
  .assert_df(data, "data")
  .assert_scalar_logical(rescale, "rescale")
  .assert_scalar_logical(visible, "visible")
  .assert_scalar_logical(verbose, "verbose")
  .assert_scalar_number(bins, "bins", integer_ok = TRUE, must_be_positive = TRUE)
  .assert_prop(na_warn_prop, "na_warn_prop")

  # Coerce to base data.frame to satisfy di::di checks
  df <- as.data.frame(data, stringsAsFactors = FALSE)

  # Validate columns
  if (!is.null(age)) .assert_cols_exist(df, age, what = "age")
  if (is.null(cols)) {
    # auto-select numeric columns; exclude age if present
    cols <- names(df)[vapply(df, is.numeric, logical(1))]
    if (!is.null(age)) cols <- setdiff(cols, age)
  } else {
    .assert_character(cols, "cols")
    .assert_cols_exist(df, cols, what = "cols")
    if (!is.null(age)) cols <- setdiff(unique(cols), age)
  }
  if (length(cols) == 0L) stop("No deficit columns selected. Provide cols or supply numeric deficit columns in `data`.")

  if (!is.null(invert)) {
    .assert_character(invert, "invert")
    # Only keep invert entries that are in cols (di will also check)
    invert <- intersect(invert, cols)
  }

  if (!is.null(rescale.custom)) .assert_character(rescale.custom, "rescale.custom")
  if (!is.null(rescale.avoid))  .assert_character(rescale.avoid,  "rescale.avoid")

  # HM-CS v2: debug-only notice when aliases are used
  if (na_action %in% c("keep","omit") || identical(extreme_action, "NA")) {
    hm_inform(
      sprintf("frailty_index(): HM-CS v2 aliases active (na_action=%s%s)",
              na_action, if (identical(extreme_action, "NA")) ", extreme_action=NA" else ""),
      level = "debug"
    )
  }

  # Select deficits for scanning
  sel_df <- df[, cols, drop = FALSE]

  # HM-CS: na_action = 'omit' drops rows with any NA in selected deficits
  if (na_action == "omit") {
    keep_rows <- stats::complete.cases(sel_df)
    df <- df[keep_rows, , drop = FALSE]
    sel_df <- sel_df[keep_rows, , drop = FALSE]
  }

  # Run QA scans
  qa_all <- .fi_scan(sel_df, na_warn_prop = na_warn_prop, consider_range = TRUE)
  # Legacy behavior: only flag range when not rescaling
  qa_legacy <- .fi_scan(sel_df, na_warn_prop = na_warn_prop, consider_range = !isTRUE(rescale))

  # Start messages
  if (verbose) {
    hm_inform(sprintf("frailty_index(): preparing inputs (%d rows, %d deficits%s)",
                      nrow(df), length(cols),
                      if (!is.null(age)) ", age provided" else ""),
              level = "inform")
    hm_inform(sprintf("frailty_index(): column map: %s%s",
                      paste(head(cols, 8), collapse = ", "),
                      if (length(cols) > 8) sprintf(" ... and %d more", length(cols) - 8) else ""),
              level = "inform")
    hm_inform(sprintf("frailty_index(): NA scan: %d col(s) with any NA; %d col(s) high-NA (>= %.0f%%)",
                      length(qa_all$any_na_cols), length(qa_all$high_na_cols), 100 * na_warn_prop),
              level = "debug")
    if (!isTRUE(rescale)) {
      hm_inform(sprintf("frailty_index(): out-of-range scan (<0 or >1): %d col(s) flagged",
                        length(qa_all$out_of_range_cols)),
                level = "debug")
    }
  } else {
    hm_inform("frailty_index(): computing", level = "debug")
  }

  # NA handling (legacy + HM-CS keep/omit)
  if (na_action == "warn" && (length(qa_all$any_na_cols) > 0L || length(qa_all$high_na_cols) > 0L)) {
    if (length(qa_all$high_na_cols) > 0L) {
      warning(sprintf("High missingness (>= %.0f%%) in: %s.",
                      100 * na_warn_prop, paste(qa_all$high_na_cols, collapse = ", ")), call. = FALSE)
    } else {
      warning(sprintf("Missing values detected in: %s.",
                      paste(qa_all$any_na_cols, collapse = ", ")), call. = FALSE)
    }
  }
  if (na_action %in% c("error") && length(qa_all$any_na_cols) > 0L) {
    stop(sprintf("Missing values present in selected deficits: %s.", paste(qa_all$any_na_cols, collapse = ", ")))
  }

  # Extremes gating: HM-CS check_extreme overrides legacy rescale gating
  do_extreme <- if (is.null(check_extreme)) !isTRUE(rescale) else isTRUE(check_extreme)
  oor_cols <- if (do_extreme) qa_all$out_of_range_cols else qa_legacy$out_of_range_cols

  if (do_extreme && length(oor_cols) > 0L) {
    if (extreme_action == "error") {
      stop(sprintf("Out-of-range values (<0 or >1) found in: %s.", paste(oor_cols, collapse = ", ")))
    } else if (extreme_action == "warn") {
      warning(sprintf("Out-of-range values (<0 or >1) found in: %s (not altered).",
                      paste(oor_cols, collapse = ", ")), call. = FALSE)
    } else if (extreme_action == "cap") {
      sel_df <- .cap_01(sel_df)
      df[, cols] <- sel_df
      if (verbose) hm_inform("frailty_index(): capped out-of-range values to [0,1]", level = "debug")
    } else if (extreme_action == "NA") {
      for (v in oor_cols) {
        x <- sel_df[[v]]
        x[is.finite(x) & (x < 0 | x > 1)] <- NA_real_
        sel_df[[v]] <- x
      }
      df[, cols] <- sel_df
      if (verbose) hm_inform("frailty_index(): set out-of-range values to NA", level = "debug")
    } # "ignore": do nothing
  }

  # Call di::di() safely
  di_res <- try(
    di::di(
      dat             = df,
      cols            = cols,
      invert          = invert,
      rescale         = rescale,
      age             = age,
      rescale.custom  = rescale.custom,
      rescale.avoid   = rescale.avoid,
      bins            = bins,
      visible         = visible
    ),
    silent = TRUE
  )

  if (inherits(di_res, "try-error")) {
    stop("di::di() failed. Check inputs or update the 'di' package.")
  }

  # Completion summary
  if (return == "data") {
    di_vec <- tryCatch(di_res$di, error = function(e) NULL)
    out_tbl <- {
      out_tmp <- data.frame(di = as.numeric(di_vec), stringsAsFactors = FALSE)
      keep_cols <- unique(c(cols, age))
      if (length(keep_cols)) out_tmp <- cbind(out_tmp, df[, keep_cols, drop = FALSE])
      tibble::as_tibble(out_tmp)
    }
    hm_inform(level = if (isTRUE(verbose)) "inform" else "debug",
              msg = hm_result_summary(out_tbl["di"], "frailty_index"))
    return(out_tbl)
  } else {
    if (verbose) {
      di_vec <- tryCatch(di_res$di, error = function(e) NULL)
      if (is.numeric(di_vec)) {
        rng <- range(di_vec, na.rm = TRUE)
        hm_inform(sprintf("frailty_index(): results: di range [%.3f, %.3f] (%d rows, %d deficits)",
                          ifelse(is.finite(rng[1]), rng[1], NA_real_),
                          ifelse(is.finite(rng[2]), rng[2], NA_real_),
                          length(di_vec), length(cols)),
                  level = "inform")
      } else {
        hm_inform("frailty_index(): completed", level = "inform")
      }
    } else {
      hm_inform("frailty_index(): completed", level = "debug")
    }
    return(di_res)
  }
}

#' Plot FI vs age (convenience wrapper)
#'
#' Calls frailty_index() with visible = TRUE; see frailty_index() for arguments,
#' validation, and references.
#'
#' @inheritParams frailty_index
#' @return The object returned by frailty_index() (di::di object if return="list").
#' @examples
#' if (requireNamespace("di", quietly = TRUE)) {
#'   df <- data.frame(age = c(70, 75, 80), d1 = c(0, 1, 1),
#'     d2 = c(0.2, 0.8, 1.0), d3 = c(TRUE, FALSE, TRUE))
#'   plot_frailty_age(df, cols = c("d1", "d2", "d3"), age = "age")
#' }
#' @export
plot_frailty_age <- function(data,
                             cols = NULL,
                             invert = NULL,
                             rescale = TRUE,
                             age = NULL,
                             rescale.custom = NULL,
                             rescale.avoid = NULL,
                             bins = 7,
                             na_action = c("ignore","warn","error","keep","omit"),
                             na_warn_prop = 0.2,
                             check_extreme = NULL,
                             extreme_action = c("warn","ignore","error","cap","NA"),
                             return = c("list","data"),
                             verbose = FALSE) {
  na_action <- match.arg(na_action)
  extreme_action <- match.arg(extreme_action)
  return <- match.arg(return)
  frailty_index(
    data            = data,
    cols            = cols,
    invert          = invert,
    rescale         = rescale,
    age             = age,
    rescale.custom  = rescale.custom,
    rescale.avoid   = rescale.avoid,
    bins            = bins,
    visible         = TRUE,
    na_action       = na_action,
    na_warn_prop    = na_warn_prop,
    check_extreme   = check_extreme,
    extreme_action  = extreme_action,
    return          = return,
    verbose         = verbose
  )
}

# ---- internal helpers (not exported) ----------------------------------------

.need_pkg_di <- function() {
  ok <- suppressMessages(suppressWarnings(requireNamespace("di", quietly = TRUE)))
  if (!ok) stop("Please install the 'di' package to use frailty_index().")
  invisible(TRUE)
}

.assert_df <- function(x, nm) {
  if (!is.data.frame(x)) stop(sprintf("`%s` must be a data.frame or tibble.", nm))
}

.assert_scalar_logical <- function(x, nm) {
  if (!(is.logical(x) && length(x) == 1L && !is.na(x))) {
    stop(sprintf("`%s` must be a single logical value.", nm))
  }
}

.assert_scalar_number <- function(x, nm, integer_ok = FALSE, must_be_positive = FALSE) {
  ok <- is.numeric(x) && length(x) == 1L && is.finite(x)
  if (ok && integer_ok) ok <- (abs(x - round(x)) < .Machine$double.eps^0.5)
  if (ok && must_be_positive) ok <- x > 0
  if (!ok) {
    stop(sprintf("`%s` must be a %s%s%s.",
                 nm,
                 if (integer_ok) "single integer" else "single numeric",
                 if (must_be_positive) " > 0" else "",
                 " and finite"))
  }
}

.assert_character <- function(x, nm) {
  if (!is.character(x)) stop(sprintf("`%s` must be a character vector.", nm))
}

.assert_cols_exist <- function(df, cols, what = "columns") {
  miss <- setdiff(cols, names(df))
  if (length(miss)) stop(sprintf("Missing %s in `data`: %s", what, paste(miss, collapse = ", ")))
}

.assert_prop <- function(x, nm) {
  if (!(is.numeric(x) && length(x) == 1L && is.finite(x) && x >= 0 && x <= 1)) {
    stop(sprintf("`%s` must be a single numeric in [0, 1].", nm))
  }
}

.fi_scan <- function(df, na_warn_prop = 0.2, consider_range = TRUE) {
  any_na_cols <- character(0)
  high_na_cols <- character(0)
  oor_cols <- character(0)
  for (v in names(df)) {
    x <- df[[v]]
    if (anyNA(x)) any_na_cols <- c(any_na_cols, v)
    n <- length(x)
    if (n > 0L) {
      pna <- sum(is.na(x)) / n
      if (pna >= na_warn_prop && pna > 0) high_na_cols <- c(high_na_cols, v)
    }
    if (isTRUE(consider_range)) {
      # Consider only finite values
      xf <- x[is.finite(x)]
      if (length(xf) && (min(xf) < 0 || max(xf) > 1)) {
        # Flag columns that look binary or logical
        if (is.logical(x) || setequal(sort(unique(na.omit(x))), c(0,1))) {
          oor_cols <- c(oor_cols, v)
        }
      }
    }
  }
  list(any_na_cols = unique(any_na_cols),
       high_na_cols = unique(high_na_cols),
       out_of_range_cols = unique(oor_cols))
}

.cap_01 <- function(df) {
  for (v in names(df)) {
    x <- df[[v]]
    if (is.numeric(x)) {
      x[x < 0 & is.finite(x)] <- 0
      x[x > 1 & is.finite(x)] <- 1
      df[[v]] <- x
    }
  }
  df
}
