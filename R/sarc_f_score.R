#' SARC-F Sarcopenia Screening Score
#'
#' Computes the SARC-F questionnaire score, a quick screening tool for sarcopenia risk.
#'
#' @details
#' SARC-F has 5 items: Strength, Assistance in walking, Rise from a chair,
#' Climb stairs, and Falls. Each item is scored 0 (no difficulty) to 2 (high difficulty).
#' Total SARC-F score ranges 0-10. A score >= 4 indicates high risk of sarcopenia
#' and suggests further assessment.
#'
#' @param data A data.frame or tibble with SARC-F questionnaire responses.
#' @param col_map Named list mapping the five SARC-F components to columns:
#'   strength, walking, chair, stairs, falls.
#' @param verbose Logical; if TRUE (default), emits progress messages.
#' @param na_action One of c("keep","omit","error","ignore","warn").
#'
#' @return A tibble with:
#'   - sarc_f_score (numeric 0-10; NA if any component is NA)
#'   - sarc_f_high_risk (logical; TRUE if score >= 4, NA if score is NA)
#' @references \insertRef{malmstrom2013sarcf}{HealthMarkers}; \insertRef{malmstrom2016sarcf}{HealthMarkers}
#'
#' @examples
#' df <- data.frame(Strength = c(1, 2, 0), Walking = c(0, 1, 2),
#'                  Chair = c(1, 1, 2), Stairs = c(0, 2, 2), Falls = c(0, 1, 1))
#' sarc_f_score(df)
#' @export
sarc_f_score <- function(
  data,
  col_map = NULL,
  na_action = c("keep","omit","error","ignore","warn"),
  verbose = TRUE
) {
  fn_name <- "sarc_f_score"
  .na <- .hm_normalize_na_action(match.arg(na_action))
  na_action_raw <- .na$na_action_raw
  na_action_eff <- .na$na_action_eff
  id_col <- .hm_detect_id_col(data)

  if (!is.data.frame(data)) {
    rlang::abort(
      "sarc_f_score(): `data` must be a data.frame or tibble.",
      class = "healthmarkers_sarcf_error_data_type"
    )
  }

  if (!is.list(col_map) && !is.null(col_map)) {
    rlang::abort(
      "sarc_f_score(): `col_map` must be a named list.",
      class = "healthmarkers_sarcf_error_colmap_type"
    )
  }

  col_map <- .hm_autofill_col_map(col_map, data,
    c("strength","walking","chair","stairs","falls"),
    fn = "sarc_f_score")
  if (is.null(col_map)) col_map <- list()

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
    map_parts <- vapply(req, function(k) sprintf("%s -> '%s'", k, col_map[[k]]), character(1))
    hm_inform(sprintf("%s(): column mapping: %s", fn_name, paste(map_parts, collapse = ", ")), level = "inform")
    hm_inform(sprintf("%s(): computing markers:\n  sarc_f_score      [0-10 sum]\n  sarc_f_high_risk  [score >= 4]", fn_name), level = "inform")
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

  hm_inform("sarc_f_score(): computing markers", level = "debug")

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

  if (!is.null(id_col)) {
    id_vec <- if (na_action_eff == "omit") data[[id_col]][keep] else data[[id_col]]
    out[[id_col]] <- id_vec
    out <- out[, c(id_col, setdiff(names(out), id_col)), drop = FALSE]
    out <- tibble::as_tibble(out)
  }
  if (isTRUE(verbose)) { hm_inform(hm_result_summary(out, fn_name), level = "inform") }

  out
}
