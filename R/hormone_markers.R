
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
#' @param na_action One of "ignore","warn","error","keep","omit". HM-CS: keep == ignore; omit drops rows with any NA in used inputs.
#' @param na_warn_prop Proportion in \eqn{[0,1]} for high-missingness warnings when na_action="warn". Default 0.2.
#' @param check_extreme Logical; if TRUE, scan inputs for out-of-range values (see extreme_rules). Default FALSE.
#' @param extreme_action One of "warn","cap","error","ignore","NA" when check_extreme=TRUE. "cap" truncates to range; "NA" sets out-of-range to NA.
#' @param extreme_rules Optional list of c(min,max) per key to override defaults.
#' @param verbose Logical; print progress and completion summary.
#' @return Tibble with the nine ratio markers.
#'
#' @examples
#' df <- data.frame(
#'   TT = c(15, 12), SHBG = c(40, 35), LH = c(5, 6), FSH = c(4, 5),
#'   E2 = c(100, 120), Prog = c(0.5, 0.6), fT3 = c(4.5, 4.2),
#'   fT4 = c(15, 14), Aldo = c(200, 180), Renin = c(10, 12),
#'   Ins = c(60, 70), Gluc = c(8, 9), GH = c(1.2, 1.0),
#'   IGF1 = c(180, 160), Prl = c(10, 12), Cort0 = c(400, 380),
#'   Cort30 = c(600, 580)
#' )
#' col_map <- list(
#'   total_testosterone = "TT", SHBG = "SHBG", LH = "LH", FSH = "FSH",
#'   estradiol = "E2", progesterone = "Prog", free_T3 = "fT3",
#'   free_T4 = "fT4", aldosterone = "Aldo", renin = "Renin",
#'   insulin = "Ins", glucagon = "Gluc", GH = "GH", IGF1 = "IGF1",
#'   prolactin = "Prl", cortisol_0 = "Cort0", cortisol_30 = "Cort30"
#' )
#' hormone_markers(df, col_map = col_map)
#' @export
hormone_markers <- function(
  data,
  col_map = NULL,
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

  # Per-ratio input dependencies
  ratio_defs <- list(
    FAI       = c("total_testosterone", "SHBG"),
    LH_FSH    = c("LH", "FSH"),
    E2_P      = c("estradiol", "progesterone"),
    E2_T      = c("estradiol", "total_testosterone"),
    T3_T4     = c("free_T3", "free_T4"),
    TSH_fT4   = c("TSH", "free_T4"),
    ARR       = c("aldosterone", "renin"),
    Ins_Glu   = c("insulin", "glucagon"),
    GH_IGF1   = c("GH", "IGF1"),
    PRL_T     = c("prolactin", "total_testosterone"),
    CAR_slope = c("cortisol_0", "cortisol_30")
  )

  # Inference rules: derive a missing key from available mapped keys.
  # Each entry: list(target_key  = name to inject into col_map,
  #                   needs       = mapped keys required,
  #                   compute     = function(data, col_map) -> vector,
  #                   label       = short description for messaging)
  infer_rules <- list(
    list(
      target  = "free_T3",
      needs   = c("TSH", "free_T4"),
      compute = function(d, cm) {
        # Jostel/Midgley estimate: fT3 ~ fT4 * 0.33 * TSH^(-0.20)
        # doi:10.1530/eje-10-0825
        fT4 <- d[[cm[["free_T4"]]]]
        tsh <- d[[cm[["TSH"]]]]
        ifelse(is.finite(tsh) & tsh > 0 & is.finite(fT4),
               fT4 * 0.33 * tsh^(-0.20), NA_real_)
      },
      label   = "free_T3 estimated from TSH + free_T4 (Jostel/Midgley)"
    ),
    list(
      target  = "GH",
      needs   = c("IGF1"),
      compute = function(d, cm) {
        # Rough equivalence: median GH ~ IGF1 / 22.5 (nmol-based scale)
        # Use only as a last resort; flag as estimated
        d[[cm[["IGF1"]]]] / 22.5
      },
      label   = "GH estimated from IGF1 (IGF1 / 22.5, approximate)"
    )
  )

  # Validate inputs (HM-CS v3)
  if (!is.data.frame(data)) {
    rlang::abort("hormone_markers(): `data` must be a data.frame or tibble.", class = "healthmarkers_horm_error_data_type")
  }

  col_map <- .hm_autofill_col_map(col_map, data,
    c("total_testosterone","SHBG","LH","FSH","estradiol","progesterone",
      "free_T3","free_T4","TSH","aldosterone","renin",
      "insulin","IGF1","prolactin","cortisol_0","cortisol_30"),
    fn = "hormone_markers")

  if (is.null(col_map) || !is.list(col_map)) {
    rlang::abort("hormone_markers(): `col_map` must be a named list.", class = "healthmarkers_horm_error_colmap_type")
  }

  # --- Inference pass: derive missing keys from available mapped data --------
  inferred_keys <- character(0)
  for (rule in infer_rules) {
    if (rule$target %in% names(col_map)) next          # already explicitly mapped
    if (!all(rule$needs %in% names(col_map))) next      # prerequisites not available
    tmp_col <- paste0(".hm_inferred_", rule$target)
    data[[tmp_col]] <- rule$compute(data, col_map)
    col_map[[rule$target]] <- tmp_col
    inferred_keys <- c(inferred_keys, rule$target)
    hm_inform(level = if (isTRUE(verbose)) "inform" else "debug",
              msg   = sprintf("hormone_markers(): inferred '%s' \u2014 %s", rule$target, rule$label))
  }
  # --------------------------------------------------------------------------

  # Determine which ratios can be computed from the supplied col_map
  avail_ratios <- vapply(ratio_defs, function(keys) all(keys %in% names(col_map)), logical(1))
  skipped_ratios <- names(ratio_defs)[!avail_ratios]
  if (length(skipped_ratios) > 0L)
    hm_inform(level = if (isTRUE(verbose)) "inform" else "debug",
              msg = sprintf("hormone_markers(): skipping %d ratio(s) with unmapped inputs: %s",
                            length(skipped_ratios), paste(skipped_ratios, collapse = ", ")))
  if (!any(avail_ratios))
    rlang::abort("hormone_markers(): no computable ratios; supply at least one pair of mapped inputs.",
                 class = "healthmarkers_horm_error_no_ratios")

  used_keys <- unique(unlist(ratio_defs[avail_ratios], use.names = FALSE))

  # Ensure each used mapping is a single non-empty character string
  bad_keys <- vapply(used_keys, function(k) {
    v <- col_map[[k]]
    !is.character(v) || length(v) != 1L || !nzchar(v)
  }, logical(1))
  if (any(bad_keys))
    rlang::abort(paste0("missing required columns: ", paste(used_keys[bad_keys], collapse = ", ")),
                 class = "healthmarkers_horm_error_bad_map_values")

  # Mapped columns must exist in data
  used_cols <- unname(vapply(used_keys, function(k) col_map[[k]], character(1)))
  missing_cols <- setdiff(used_cols, names(data))
  if (length(missing_cols))
    rlang::abort(paste0("missing required columns: ", paste(missing_cols, collapse = ", ")),
                 class = "healthmarkers_horm_error_missing_columns")
  # na_warn_prop sanity
  if (!(is.numeric(na_warn_prop) && length(na_warn_prop) == 1L && is.finite(na_warn_prop) && na_warn_prop >= 0 && na_warn_prop <= 1)) {
    rlang::abort("hormone_markers(): `na_warn_prop` must be a single number in [0,1].",
                 class = "healthmarkers_horm_error_na_warn_prop")
  }

  hm_inform("hormone_markers(): preparing inputs", level = if (isTRUE(verbose)) "inform" else "debug")
  hm_inform(level = if (isTRUE(verbose)) "inform" else "debug",
            msg = hm_fmt_col_map(col_map[used_keys], "hormone_markers"))

  # Coerce to numeric; warn if NAs introduced; sanitize non-finite to NA
  for (cn in used_cols) {
    if (!is.numeric(data[[cn]])) {
      old <- data[[cn]]
      suppressWarnings(data[[cn]] <- as.numeric(old))
      introduced_na <- sum(is.na(data[[cn]]) & !is.na(old))
      if (introduced_na > 0L) {
        rlang::warn(sprintf("Column '%s' coerced to numeric; NAs introduced: %d", cn, introduced_na),
                    class = "healthmarkers_horm_warn_na_coercion")
      }
    }
    data[[cn]][!is.finite(data[[cn]])] <- NA_real_
  }

  # HM-CS: omit rows with any NA in used inputs
  if (na_action_raw == "omit") {
    keep_rows <- stats::complete.cases(data[, used_cols, drop = FALSE])
    data <- data[keep_rows, , drop = FALSE]
  }

  # Missingness QA
  qa <- .hor_quality_scan(data, used_cols, na_warn_prop)
  if (na_action == "warn") {
    if (length(qa$any_na)) {
      rlang::warn(sprintf("Missing values in: %s.", paste(qa$any_na, collapse = ", ")),
                  class = "healthmarkers_horm_warn_any_na")
    }
    if (length(qa$high_na)) {
      rlang::warn(sprintf("High missingness (>= %.0f%%): %s.", 100 * na_warn_prop, paste(qa$high_na, collapse = ", ")),
                  class = "healthmarkers_horm_warn_high_na")
    }
  }
  if (na_action == "error" && length(qa$any_na)) {
    rlang::abort("hormone_markers(): missing or non-finite values in required inputs with na_action='error'.",
                 class = "healthmarkers_horm_error_missing_values")
  }

  # Extreme scan/handling
  if (isTRUE(check_extreme)) {
    rules <- .hor_default_extreme_rules()
    if (is.list(extreme_rules)) for (nm in names(extreme_rules)) rules[[nm]] <- extreme_rules[[nm]]
    flags <- .hor_extreme_scan(data, col_map, rules)
    flagged_total <- sum(vapply(flags, function(x) sum(x, na.rm = TRUE), integer(1)))
    if (flagged_total > 0L) {
      if (extreme_action == "error") {
        rlang::abort(sprintf("hormone_markers(): %d extreme input values detected.", flagged_total),
                     class = "healthmarkers_horm_error_extremes")
      } else if (extreme_action == "cap") {
        data <- .hor_cap_inputs(data, flags, col_map, rules)
        rlang::warn(sprintf("hormone_markers(): capped %d extreme input values into allowed ranges.", flagged_total),
                    class = "healthmarkers_horm_warn_extremes_capped")
      } else if (extreme_action == "warn") {
        rlang::warn(sprintf("hormone_markers(): detected %d extreme input values (not altered).", flagged_total),
                    class = "healthmarkers_horm_warn_extremes_detected")
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

  # Extract only the vectors actually needed
  v <- function(k) data[[col_map[[k]]]]

  sdiv <- function(a, b) { z <- a / b; z[!is.finite(z)] <- NA_real_; z }

  # Compute only the available ratios
  out_list <- list()
  if (avail_ratios["FAI"])       out_list$FAI       <- sdiv(v("total_testosterone"), v("SHBG")) * 100
  if (avail_ratios["LH_FSH"])    out_list$LH_FSH    <- sdiv(v("LH"), v("FSH"))
  if (avail_ratios["E2_P"])      out_list$E2_P      <- sdiv(v("estradiol"), v("progesterone"))
  if (avail_ratios["E2_T"])      out_list$E2_T      <- sdiv(v("estradiol"), v("total_testosterone"))
  if (avail_ratios["T3_T4"])     out_list$T3_T4     <- sdiv(v("free_T3"), v("free_T4"))
  if (avail_ratios["TSH_fT4"])   out_list$TSH_fT4   <- sdiv(v("TSH"), v("free_T4"))
  if (avail_ratios["ARR"])       out_list$ARR       <- sdiv(v("aldosterone"), v("renin"))
  if (avail_ratios["Ins_Glu"])   out_list$Ins_Glu   <- sdiv(v("insulin"), v("glucagon"))
  if (avail_ratios["GH_IGF1"])   out_list$GH_IGF1   <- sdiv(v("GH"), v("IGF1"))
  if (avail_ratios["PRL_T"])     out_list$PRL_T     <- sdiv(v("prolactin"), v("total_testosterone"))
  if (avail_ratios["CAR_slope"]) out_list$CAR_slope <- sdiv(v("cortisol_30") - v("cortisol_0"), 30)

  # Tag inferred columns so downstream callers know
  out <- tibble::as_tibble(out_list)
  if (length(inferred_keys) > 0L) {
    inferred_ratios <- names(ratio_defs)[vapply(ratio_defs, function(keys) any(keys %in% inferred_keys), logical(1))]
    inferred_ratios <- intersect(inferred_ratios, names(out))
    attr(out, "inferred_inputs") <- inferred_keys
    attr(out, "inferred_ratios") <- inferred_ratios
  }

  # Completion summary
  hm_inform(level = if (isTRUE(verbose)) "inform" else "debug",
            msg = hm_result_summary(out, "hormone_markers"))

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
    TSH = c(0, 100),
    free_T3 = c(0, 20),
    free_T4 = c(0, 50),
    aldosterone = c(0, 1000),
    renin = c(0, 50),
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
