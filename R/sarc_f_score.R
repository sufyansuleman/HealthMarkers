#' SARC-F Sarcopenia Screening Score
#'
#' Computes the SARC-F questionnaire score, a quick screening tool for sarcopenia risk.
#'
#' @details
#' SARC-F has 5 items: Strength, Assistance in walking, Rise from a chair,
#' Climb stairs, and Falls. Each item is scored 0 (no difficulty) to 2 (high difficulty).
#' Total SARC-F score ranges 0–10. A score >= 4 indicates high risk of sarcopenia
#' and suggests further assessment.
#'
#' @param data A data.frame or tibble with SARC-F questionnaire responses.
#' @param col_map Named list mapping the five SARC-F components to columns:
#'   strength, walking, chair, stairs, falls.
#' @param verbose Logical; if TRUE, emits progress messages.
#' @param na_action One of c("keep","omit","error","ignore","warn").
#' @param check_extreme Logical; if TRUE, scan inputs for plausible ranges (0–2).
#' @param extreme_action One of c("warn","cap","error","ignore","NA").
#' @param extreme_rules Optional overrides; default caps each item to c(0,2).
#'
#' @return A tibble with:
#'   - sarc_f_score (numeric 0–10; NA if any component is NA)
#'   - sarc_f_high_risk (logical; TRUE if score >= 4, NA if score is NA)
#'#' @references
#' Malmstrom TK, Morley JE. (2013). SARC-F: a simple questionnaire to rapidly diagnose sarcopenia.
#' J Am Med Dir Assoc. 14(8):531–532. doi:10.1016/j.jamda.2013.05.018
#'
#' Malmstrom TK, Miller DK, Simonsick EM, Ferrucci L, Morley JE. (2016).
#' SARC-F: a symptomatic measure of sarcopenia predictive of poor function, disability, and mortality.
#' J Cachexia Sarcopenia Muscle. 7(1):28–36. doi:10.1002/jcsm.12048
#' @export
sarc_f_score <- function(
  data,
  col_map = list(
    strength = "Strength", walking = "Walking", chair = "Chair",
    stairs = "Stairs", falls = "Falls"
  ),
  verbose = FALSE,
  na_action = c("keep","omit","error","ignore","warn"),
  check_extreme = FALSE,
  extreme_action = c("warn","cap","error","ignore","NA"),
  extreme_rules = NULL
) {
  na_action_raw <- match.arg(na_action)
  na_action_eff <- if (na_action_raw %in% c("ignore","warn")) "keep" else na_action_raw
  extreme_action <- match.arg(extreme_action)

  if (!is.data.frame(data)) {
    rlang::abort(
      "sarc_f_score(): `data` must be a data.frame or tibble.",
      class = "healthmarkers_sarcf_error_data_type"
    )
  }

  if (!is.list(col_map)) {
    rlang::abort(
      "sarc_f_score(): `col_map` must be a named list.",
      class = "healthmarkers_sarcf_error_colmap_type"
    )
  }

  if (length(col_map) == 0L || is.null(names(col_map))) {
    rlang::abort(
      "sarc_f_score(): missing or empty `col_map`.",
      class = "healthmarkers_sarcf_error_missing_map"
    )
  }

  req <- c("strength","walking","chair","stairs","falls")
  missing_keys <- setdiff(req, names(col_map))
  if (length(missing_keys)) {
    rlang::abort(
      paste0("sarc_f_score(): missing col_map entries for: ",
             paste(missing_keys, collapse = ", ")),
      class = "healthmarkers_sarcf_error_missing_map"
    )
  }

  mapped <- unname(unlist(col_map[req], use.names = FALSE))
  if (any(!nzchar(mapped))) {
    bad <- req[!nzchar(unname(unlist(col_map[req])))]
    rlang::abort(
      paste0("sarc_f_score(): missing col_map entries for: ",
             paste(bad, collapse = ", ")),
      class = "healthmarkers_sarcf_error_bad_map_values"
    )
  }

  missing_cols <- setdiff(mapped, names(data))
  if (length(missing_cols)) {
    rlang::abort(
      paste0("sarc_f_score(): missing required columns in data: ",
             paste(missing_cols, collapse = ", ")),
      class = "healthmarkers_sarcf_error_missing_columns"
    )
  }

  if (isTRUE(verbose)) {
    rlang::inform("-> sarc_f_score: preparing inputs")
  } else if (exists("hm_inform", mode = "function")) {
    hm_inform("sarc_f_score(): preparing inputs", level = "debug")
  }

  for (cn in mapped) {
    if (!is.numeric(data[[cn]])) {
      old <- data[[cn]]
      suppressWarnings(new <- as.numeric(old))
      intro <- sum(is.na(new) & !is.na(old))
      if (intro > 0L) {
        rlang::warn(
          sprintf("Column '%s' coerced to numeric; NAs introduced: %d", cn, intro),
          class = "healthmarkers_sarcf_warn_na_coercion"
        )
      }
      data[[cn]] <- new
    }
    data[[cn]][!is.finite(data[[cn]])] <- NA_real_
  }

  xs <- data[[col_map$strength]]
  xw <- data[[col_map$walking]]
  xc <- data[[col_map$chair]]
  xt <- data[[col_map$stairs]]
  xf <- data[[col_map$falls]]

  any_na <- is.na(xs) | is.na(xw) | is.na(xc) | is.na(xt) | is.na(xf)

  if (na_action_raw == "warn" && any(any_na)) {
    rlang::warn(
      "Missing SARC-F item values; total score will be NA for those entries.",
      class = "healthmarkers_sarcf_warn_missing_inputs"
    )
  }

  if (na_action_eff == "error" && any(any_na)) {
    rlang::abort(
      "sarc_f_score(): required inputs contain missing values (na_action='error').",
      class = "healthmarkers_sarcf_error_missing_values"
    )
  }

  keep <- if (na_action_eff == "omit") !any_na else rep(TRUE, length(xs))

  d_xs <- xs[keep]; d_xw <- xw[keep]; d_xc <- xc[keep]; d_xt <- xt[keep]; d_xf <- xf[keep]

  out_of_range <- function(x) is.finite(x) & (x < 0 | x > 2)
  if (any(out_of_range(d_xs) | out_of_range(d_xw) |
          out_of_range(d_xc) | out_of_range(d_xt) | out_of_range(d_xf))) {
    rlang::warn(
      "sarc_f_score(): SARC-F items should be 0, 1, or 2; values outside this range detected.",
      class = "healthmarkers_sarcf_warn_out_of_range"
    )
  }

  # ---- fixed extreme scan block ----
  if (isTRUE(check_extreme)) {
    rules_def <- list(
      strength = c(0, 2),
      walking  = c(0, 2),
      chair    = c(0, 2),
      stairs   = c(0, 2),
      falls    = c(0, 2)
    )
    if (is.list(extreme_rules)) {
      for (nm in intersect(names(rules_def), names(extreme_rules))) {
        rules_def[[nm]] <- extreme_rules[[nm]]
      }
    }

    total_extreme <- 0L
    cap_vec <- function(x, lo, hi) {
      bad <- is.finite(x) & (x < lo | x > hi)
      total_extreme <<- total_extreme + sum(bad)
      if (extreme_action == "cap") {
        x[bad & x < lo] <- lo
        x[bad & x > hi] <- hi
      } else if (extreme_action == "NA") {
        x[bad] <- NA_real_
      }
      x
    }

    d_xs <- cap_vec(d_xs, rules_def$strength[1], rules_def$strength[2])
    d_xw <- cap_vec(d_xw, rules_def$walking[1],  rules_def$walking[2])
    d_xc <- cap_vec(d_xc, rules_def$chair[1],    rules_def$chair[2])
    d_xt <- cap_vec(d_xt, rules_def$stairs[1],   rules_def$stairs[2])
    d_xf <- cap_vec(d_xf, rules_def$falls[1],    rules_def$falls[2])

    if (total_extreme > 0L) {
      if (extreme_action == "error") {
        rlang::abort(
          sprintf("sarc_f_score(): %d extreme input values detected.", total_extreme),
          class = "healthmarkers_sarcf_error_extremes"
        )
      } else if (extreme_action == "warn") {
        rlang::warn(
          sprintf("sarc_f_score(): detected %d extreme input values (not altered).", total_extreme),
          class = "healthmarkers_sarcf_warn_extremes_detected"
        )
      } else if (extreme_action == "cap") {
        rlang::warn(
          sprintf("sarc_f_score(): capped %d extreme input values into allowed ranges.", total_extreme),
          class = "healthmarkers_sarcf_warn_extremes_capped"
        )
      }
      # extreme_action == "NA": silently set to NA, no extra warning
    }
  }
  # ---- end extreme scan block ----

  if (isTRUE(verbose)) {
    rlang::inform("-> sarc_f_score: computing")
  }

  total_score <- d_xs + d_xw + d_xc + d_xt + d_xf
  high_risk   <- ifelse(is.na(total_score), NA, total_score >= 4)

  out_core <- tibble::tibble(
    sarc_f_score     = total_score,
    sarc_f_high_risk = as.logical(high_risk)
  )

  if (na_action_eff != "omit") {
    out <- tibble::tibble(
      sarc_f_score     = rep(NA_real_, length(xs)),
      sarc_f_high_risk = rep(NA, length(xs))
    )
    out[keep, ] <- out_core
  } else {
    out <- out_core
  }

  if (isTRUE(verbose)) {
    rlang::inform(sprintf("Completed sarc_f_score: %d rows.", nrow(out)))
  } else if (exists("hm_inform", mode = "function")) {
    hm_inform(sprintf("sarc_f_score(): completed (%d rows)", nrow(out)), level = "debug")
  }

  out
}