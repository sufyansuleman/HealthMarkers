#' Classify Chronic Kidney Disease (CKD) Stage and Albuminuria Grade
#'
#' This function categorises individuals into Chronic Kidney Disease (CKD)
#' stages G1–G5 based on estimated glomerular filtration rate (eGFR) and
#' classifies albuminuria into KDIGO A1–A3 categories based on the urine
#' albumin‑to‑creatinine ratio (UACR).  Optionally, an overall KDIGO risk
#' category is returned by combining G and A stages following the KDIGO
#' heat‑map recommendations.  The function does not compute eGFR or UACR; it
#' assumes these values have been calculated previously (see `renal_markers()`
#' or other helpers).
#'
#' @param data Data frame or tibble containing at least an eGFR column and
#'   optionally a UACR column.
#' @param col_map Named list mapping `eGFR` and `UACR` to column names in
#'   `data`.  `eGFR` is required; `UACR` is optional but needed for
#'   albuminuria staging and KDIGO risk categorisation.
#' @param include_kdigo Logical; if `TRUE` (default) returns an additional
#'   column `KDIGO_risk` indicating overall risk (low, moderate, high, very
#'   high or extremely high) based on the cross‑classification of G and A
#'   stages.
#' @param verbose Logical; if `TRUE` prints progress messages.
#' @param na_action Handling of missing values: `"keep"` (default) retains
#'   missing results; `"omit"` drops rows with missing eGFR or UACR; `"error"`
#'   aborts if missing values are present.
#' @param na_warn_prop Proportion threshold for missingness warnings.  Default
#'   `0.2`.
#'
#' @return A tibble containing `CKD_stage` and, if `UACR` is supplied, an
#'   `Albuminuria_stage` column and optionally `KDIGO_risk`.  Individuals with
#'   missing eGFR or UACR will have `NA` in the corresponding output.
#'
#' @examples
#' df <- data.frame(eGFR = c(95, 70, 50, 35, 10), UACR = c(10, 50, 200, 600, 100))
#' ckd_stage(df, col_map = list(eGFR = "eGFR", UACR = "UACR"))
#'
#' @references
#' Kidney Disease: Improving Global Outcomes (KDIGO) CKD Work Group. KDIGO
#' clinical practice guideline for the evaluation and management of chronic
#' kidney disease. Kidney Int Suppl. 2013;3(1):1–150.
#' \doi{10.1038/kisup.2012.73}
#' @importFrom tibble tibble
#' @importFrom rlang abort warn inform
#' @export
ckd_stage <- function(data, col_map, include_kdigo = TRUE,
                      verbose = FALSE,
                      na_action = c("keep", "omit", "error"),
                      na_warn_prop = 0.2) {
  na_action <- match.arg(na_action)

  # Validate col_map (no external helpers)
  if (!is.list(col_map) || is.null(names(col_map))) {
    rlang::abort("ckd_stage(): 'col_map' must be a named list.")
  }
  if (is.null(col_map$eGFR)) {
    rlang::abort("ckd_stage(): 'eGFR' must be specified in col_map.")
  }
  have_uacr <- !is.null(col_map$UACR)
  req_vars <- c("eGFR", if (have_uacr) "UACR" else NULL)

  # Resolve mapped columns
  mapped <- vapply(req_vars, function(nm) as.character(col_map[[nm]]), character(1))
  if (anyNA(mapped) || any(!nzchar(mapped))) {
    rlang::abort("ckd_stage(): 'col_map' contains empty or NA column names.")
  }
  if (anyDuplicated(mapped)) {
    rlang::abort("ckd_stage(): 'col_map' has duplicated target columns.")
  }
  missing_in_data <- setdiff(mapped, names(data))
  if (length(missing_in_data)) {
    rlang::abort(sprintf("ckd_stage(): columns not found in 'data': %s",
                         paste(missing_in_data, collapse = ", ")))
  }

  # Subset and standardize names
  df <- data[, mapped, drop = FALSE]
  names(df) <- req_vars

  # Missingness warnings
  if (na_warn_prop > 0) {
    for (cn in req_vars) {
      pna <- mean(is.na(df[[cn]]))
      if (isTRUE(pna >= na_warn_prop) && pna > 0) {
        rlang::warn(sprintf("ckd_stage(): column '%s' has high missingness (%.1f%%).",
                            cn, 100 * pna))
      }
    }
  }

  # Handle missing values
  if (na_action == "error") {
    if (any(is.na(df$eGFR))) rlang::abort("ckd_stage(): missing eGFR values (na_action='error').")
    if (have_uacr && any(is.na(df$UACR))) rlang::abort("ckd_stage(): missing UACR values (na_action='error').")
  } else if (na_action == "omit") {
    keep <- !is.na(df$eGFR) & (!have_uacr | !is.na(df$UACR))
    if (isTRUE(verbose)) {
      rlang::inform(sprintf("ckd_stage(): omitting %d rows with missing required inputs", sum(!keep)))
    }
    df <- df[keep, , drop = FALSE]
  }

  if (nrow(df) == 0L) {
    out <- tibble::tibble(CKD_stage = character())
    if (have_uacr) out$Albuminuria_stage <- character()
    if (include_kdigo && have_uacr) out$KDIGO_risk <- character()
    return(out)
  }

  # Coerce to numeric
  df$eGFR <- suppressWarnings(as.numeric(df$eGFR))
  if (have_uacr) df$UACR <- suppressWarnings(as.numeric(df$UACR))

  # CKD stage by eGFR
  ckdstage <- rep(NA_character_, nrow(df))
  ckdstage[df$eGFR >= 90] <- "G1"
  ckdstage[df$eGFR >= 60 & df$eGFR < 90] <- "G2"
  ckdstage[df$eGFR >= 45 & df$eGFR < 60] <- "G3a"
  ckdstage[df$eGFR >= 30 & df$eGFR < 45] <- "G3b"
  ckdstage[df$eGFR >= 15 & df$eGFR < 30] <- "G4"
  ckdstage[df$eGFR < 15] <- "G5"

  out <- tibble::tibble(CKD_stage = ckdstage)

  # Albuminuria and KDIGO risk if UACR provided
  if (have_uacr) {
    albumstage <- rep(NA_character_, nrow(df))
    albumstage[df$UACR < 30] <- "A1"
    albumstage[df$UACR >= 30 & df$UACR < 300] <- "A2"
    albumstage[df$UACR >= 300] <- "A3"
    out$Albuminuria_stage <- albumstage

    if (include_kdigo) {
      risk <- rep(NA_character_, nrow(df))
      # Low risk: G1–G2 & A1
      ix <- out$CKD_stage %in% c("G1", "G2") & albumstage == "A1"
      risk[ix] <- "low"
      # Moderate: G1–G2 & A2 or G3a & A1
      ix <- (out$CKD_stage %in% c("G1", "G2") & albumstage == "A2") |
            (out$CKD_stage == "G3a" & albumstage == "A1")
      risk[ix] <- "moderate"
      # High: G1–G2 & A3 or G3a & A2 or G3b & A1
      ix <- (out$CKD_stage %in% c("G1", "G2") & albumstage == "A3") |
            (out$CKD_stage == "G3a" & albumstage == "A2") |
            (out$CKD_stage == "G3b" & albumstage == "A1")
      risk[ix] <- "high"
      # Very high: G3b & A2 or G4 & A1
      ix <- (out$CKD_stage == "G3b" & albumstage == "A2") |
            (out$CKD_stage == "G4" & albumstage == "A1")
      risk[ix] <- "very high"
      # Extremely high: remaining defined combinations (not NA)
      ix <- is.na(risk) & !is.na(out$CKD_stage) & !is.na(albumstage)
      risk[ix] <- "extremely high"

      out$KDIGO_risk <- risk
    }
  }

  if (isTRUE(verbose)) {
    rlang::inform(sprintf(
      "ckd_stage(): classified %d rows; missing eGFR: %d; missing UACR: %d",
      nrow(out), sum(is.na(df$eGFR)), if (have_uacr) sum(is.na(df$UACR)) else 0
    ))
  }
  out
}