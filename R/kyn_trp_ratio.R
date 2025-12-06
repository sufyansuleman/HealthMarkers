#' Kynurenine/Tryptophan Ratio (KTR)
#'
#' Computes the ratio of kynurenine to tryptophan, a marker of IDO activity and immune activation.
#'
#' @details
#' KTR is calculated as Kyn (nmol/L) divided by Trp (mumol/L). Elevated KTR indicates
#' increased tryptophan catabolism via the kynurenine pathway, often reflecting
#' inflammation and cell-mediated immune activation.
#'
#' Inputs should already be in Kyn (nmol/L) and Trp (mumol/L).
#'
#' @param data A data.frame or tibble with kynurenine and tryptophan concentrations.
#' @param col_map Named list with:
#'   - kynurenine: column for kynurenine (nmol/L)
#'   - tryptophan: column for tryptophan (mumol/L)
#' @param na_action One of c("keep","omit","error","ignore","warn").
#' @param check_extreme Logical; if TRUE, scan inputs for plausible ranges.
#' @param extreme_action One of c("warn","cap","error","ignore","NA").
#' @param extreme_rules Optional overrides; defaults:
#'   list(kynurenine_nmolL = c(100, 20000), tryptophan_umolL = c(10, 150), ratio = c(0, 200)).
#' @param verbose Logical; if TRUE, emits progress via rlang::inform.
#' @return A tibble with one column: kyn_trp_ratio (numeric).
#'
#' @references
#' Fuchs D, Moller AA, Reibnegger G, Werner ER, Werner-Felmayer G, Dierich MP, Wachter H. (1998).
#' Serum kynurenine-to-tryptophan ratio increases with disease progression in HIV-1 infection.
#' Clin Chem. 44(4):858-862. PMID:9555676
#'
#' Damerell V, Midttun O, Ulvik A, et al. (2025).
#' Circulating tryptophan-kynurenine metabolites and mortality.
#' Int J Cancer. 156(3):552-565. doi:10.1002/ijc.35183
#'
#' @export
kyn_trp_ratio <- function(
  data,
  col_map = list(kynurenine = "Kyn_nM", tryptophan = "Trp_uM"),
  na_action = c("keep","omit","error","ignore","warn"),
  check_extreme = FALSE,
  extreme_action = c("warn","cap","error","ignore","NA"),
  extreme_rules = NULL,
  verbose = FALSE
) {
  na_action_raw <- match.arg(na_action)
  na_action_eff <- if (na_action_raw %in% c("ignore","warn")) "keep" else na_action_raw
  extreme_action <- match.arg(extreme_action)

  # Validate
  if (!is.data.frame(data)) {
    rlang::abort("kyn_trp_ratio(): `data` must be a data.frame or tibble.",
                 class = "healthmarkers_ktr_error_data_type")
  }
  if (!is.list(col_map) || is.null(names(col_map))) {
    rlang::abort("kyn_trp_ratio(): `col_map` must be a named list.",
                 class = "healthmarkers_ktr_error_colmap_type")
  }
  req <- c("kynurenine","tryptophan")
  missing_keys <- setdiff(req, names(col_map))
  if (length(missing_keys)) {
    rlang::abort(paste0("kyn_trp_ratio(): missing col_map entries for: ",
                        paste(missing_keys, collapse = ", ")),
                 class = "healthmarkers_ktr_error_missing_map")
  }
  mapped <- unname(unlist(col_map[req], use.names = FALSE))
  if (any(!nzchar(mapped))) {
    bad <- req[!nzchar(unname(unlist(col_map[req])))]
    rlang::abort(paste0("kyn_trp_ratio(): missing col_map entries for: ",
                        paste(bad, collapse = ", ")),
                 class = "healthmarkers_ktr_error_bad_map_values")
  }
  missing_cols <- setdiff(mapped, names(data))
  if (length(missing_cols)) {
    rlang::abort(paste0("kyn_trp_ratio(): missing required columns in data: ",
                        paste(missing_cols, collapse = ", ")),
                 class = "healthmarkers_ktr_error_missing_columns")
  }

  if (isTRUE(verbose)) rlang::inform("-> kyn_trp_ratio: preparing inputs")
  else hm_inform("kyn_trp_ratio(): preparing inputs", level = "debug")

  # Coerce numeric; warn if NAs introduced; sanitize
  for (cn in mapped) {
    if (!is.numeric(data[[cn]])) {
      old <- data[[cn]]
      suppressWarnings(new <- as.numeric(old))
      intro <- sum(is.na(new) & !is.na(old))
      if (intro > 0L) {
        rlang::warn(sprintf("Column '%s' coerced to numeric; NAs introduced: %d", cn, intro),
                    class = "healthmarkers_ktr_warn_na_coercion")
      }
      data[[cn]] <- new
    }
    data[[cn]][!is.finite(data[[cn]])] <- NA_real_
  }

  kyn <- data[[col_map$kynurenine]]
  trp <- data[[col_map$tryptophan]]

  # NA policy
  any_na <- is.na(kyn) | is.na(trp)
  if (na_action_raw == "warn" && any(any_na)) {
    rlang::warn("Missing kynurenine or tryptophan; ratio will be NA for those entries.",
                class = "healthmarkers_ktr_warn_missing_inputs")
  }
  if (na_action_eff == "error" && any(any_na)) {
    rlang::abort("kyn_trp_ratio(): required inputs contain missing values (na_action='error').",
                 class = "healthmarkers_ktr_error_missing_values")
  }
  keep <- if (na_action_eff == "omit") !any_na else rep(TRUE, length(kyn))

  d_kyn <- kyn[keep]
  d_trp <- trp[keep]

  # Domain warnings: nonpositive tryptophan
  if (any(is.finite(d_trp) & d_trp <= 0)) {
    rlang::warn("kyn_trp_ratio(): nonpositive tryptophan detected; ratios set to NA for those rows.",
                class = "healthmarkers_ktr_warn_nonpositive_trp")
    d_trp[is.finite(d_trp) & d_trp <= 0] <- NA_real_
  }

  # Optional extreme scan
  if (isTRUE(check_extreme)) {
    rules_def <- list(kynurenine_nmolL = c(100, 20000), tryptophan_umolL = c(10, 150), ratio = c(0, 200))
    if (is.list(extreme_rules)) {
      for (nm in intersect(names(extreme_rules), names(rules_def))) rules_def[[nm]] <- extreme_rules[[nm]]
    }
    total <- 0L
    cap_vec <- function(x, lo, hi) {
      bad <- is.finite(x) & (x < lo | x > hi)
      total <<- total + sum(bad)
      if (extreme_action == "cap") {
        x[bad & x < lo] <- lo
        x[bad & x > hi] <- hi
      } else if (extreme_action == "NA") {
        x[bad] <- NA_real_
      }
      list(x = x, bad = bad)
    }
    ck <- cap_vec(d_kyn, rules_def$kynurenine_nmolL[1], rules_def$kynurenine_nmolL[2]); d_kyn <- ck$x
    ct <- cap_vec(d_trp, rules_def$tryptophan_umolL[1], rules_def$tryptophan_umolL[2]); d_trp <- ct$x

    if (total > 0L) {
      if (extreme_action == "error") {
        rlang::abort(sprintf("kyn_trp_ratio(): %d extreme input values detected.", total),
                     class = "healthmarkers_ktr_error_extremes")
      } else if (extreme_action == "warn") {
        rlang::warn(sprintf("kyn_trp_ratio(): detected %d extreme input values (not altered).", total),
                    class = "healthmarkers_ktr_warn_extremes_detected")
      } else if (extreme_action == "cap") {
        rlang::warn(sprintf("kyn_trp_ratio(): capped %d extreme input values into allowed ranges.", total),
                    class = "healthmarkers_ktr_warn_extremes_capped")
      }
    }
  }

  if (isTRUE(verbose)) rlang::inform("-> kyn_trp_ratio: computing")
  else hm_inform("kyn_trp_ratio(): computing", level = "debug")

  ratio <- d_kyn / d_trp
  ratio[!is.finite(ratio)] <- NA_real_

  # High ratio warning
  if (any(is.finite(ratio) & ratio > 100)) {
    rlang::warn("kyn_trp_ratio(): very high ratios (>100) detected; check units (Kyn nmol/L, Trp mumol/L).",
                class = "healthmarkers_ktr_warn_ratio_high")
  }

  out <- tibble::tibble(kyn_trp_ratio = ratio)

  # Pad if not omitting
  if (na_action_eff != "omit") {
    res <- tibble::tibble(kyn_trp_ratio = rep(NA_real_, length(kyn)))
    res$kyn_trp_ratio[keep] <- out$kyn_trp_ratio
    out <- res
  }

  if (isTRUE(verbose)) rlang::inform(sprintf("Completed kyn_trp_ratio: %d rows.", nrow(out)))
  else hm_inform(sprintf("kyn_trp_ratio(): completed (%d rows)", nrow(out)), level = "debug")

  out
}
