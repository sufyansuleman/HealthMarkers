#' Oxidative stress markers
#'
#' Computes GSH_GSSG_Ratio = reduced glutathione (GSH) / oxidized glutathione (GSSG).
#'
#' @param data Data frame with columns for GSH and GSSG (per col_map).
#' @param col_map Named list with required keys GSH and GSSG. Defaults assume same names.
#' @param na_action One of c("keep","omit","error").
#' @param verbose Logical; if TRUE, emits progress via hm_inform().
#' @return A tibble with column GSH_GSSG_Ratio.
#'
#' @references
#' - Glutathione redox status as an oxidative stress indicator: standard laboratory/clinical biochemistry references.
#'
#' @examples
#' df <- data.frame(GSH = c(5, 3), GSSG = c(1, 0.5))
#' oxidative_markers(df, col_map = list(GSH="GSH", GSSG="GSSG"))
#'
#' @importFrom rlang abort warn inform
#' @importFrom tibble tibble
#' @export
oxidative_markers <- function(
  data,
  col_map = list(GSH = "GSH", GSSG = "GSSG"),
  na_action = c("keep","omit","error"),
  verbose = FALSE
) {
  na_action <- match.arg(na_action)

  if (isTRUE(verbose)) hm_inform(level = "inform", msg = "-> oxidative_markers: validating inputs")

  # HM-CS v2 validation
  hm_validate_inputs(
    data, col_map,
    required_keys = c("GSH","GSSG"),
    fn = "oxidative_markers"
  )

  # Ensure mapped columns exist in data
  mapped <- unname(unlist(col_map[c("GSH","GSSG")], use.names = FALSE))
  missing_cols <- setdiff(mapped, names(data))
  if (length(missing_cols)) {
    rlang::abort(
      paste0("oxidative_markers(): missing columns in `data`: ", paste(missing_cols, collapse = ", ")),
      class = "healthmarkers_oxidative_error_missing_columns"
    )
  }

  # Coerce required columns to numeric; warn if NAs introduced; set non-finite to NA
  for (cn in mapped) {
    if (!is.numeric(data[[cn]])) {
      old <- data[[cn]]
      suppressWarnings(new <- as.numeric(old))
      introduced_na <- sum(is.na(new) & !is.na(old))
      if (introduced_na > 0L) rlang::warn(sprintf("oxidative_markers(): column '%s' coerced to numeric; NAs introduced: %d", cn, introduced_na))
      data[[cn]] <- new
    }
    data[[cn]][!is.finite(data[[cn]])] <- NA_real_
  }

  # High-missingness diagnostics (debug)
  for (cn in mapped) {
    x <- data[[cn]]; n <- length(x); if (!n) next
    pna <- sum(is.na(x)) / n
    if (pna >= 0.2 && pna > 0) {
      hm_inform(level = "debug", msg = sprintf("oxidative_markers(): column '%s' has high missingness (%.1f%%).", cn, 100 * pna))
    }
  }

  # NA policy
  if (na_action == "error") {
    has_na <- Reduce(`|`, lapply(mapped, function(cn) is.na(data[[cn]])))
    if (any(has_na)) {
      rlang::abort("oxidative_markers(): required inputs contain missing values (na_action='error').",
                   class = "healthmarkers_oxidative_error_missing_values")
    }
  } else if (na_action == "omit") {
    keep <- !Reduce(`|`, lapply(mapped, function(cn) is.na(data[[cn]])))
    if (isTRUE(verbose)) hm_inform(level = "inform", msg = sprintf("-> oxidative_markers: omitting %d rows with NA in required inputs", sum(!keep)))
    data <- data[keep, , drop = FALSE]
  }

  if (isTRUE(verbose)) hm_inform(level = "inform", msg = "-> oxidative_markers: computing markers")

  # Accessors (post-filter)
  GSH  <- data[[col_map$GSH]]
  GSSG <- data[[col_map$GSSG]]

  # Safe division: NA on non-finite or zero denom
  sdiv <- function(a, b) {
    out <- a / b
    out[!is.finite(out)] <- NA_real_
    out
  }

  out <- tibble::tibble(GSH_GSSG_Ratio = sdiv(GSH, GSSG))

  if (isTRUE(verbose)) {
    na_count <- sum(is.na(out$GSH_GSSG_Ratio) | !is.finite(out$GSH_GSSG_Ratio))
    hm_inform(level = "inform", msg = sprintf(
      "Completed oxidative_markers: %d rows; NA/Inf in GSH_GSSG_Ratio=%d",
      nrow(out), na_count
    ))
  }

  out
}