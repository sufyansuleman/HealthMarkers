#' CKD staging (GFR and albuminuria) and KDIGO risk
#'
#' @description
#' Categorizes eGFR into G1–G5, albuminuria into A1–A3 (by UACR mg/g), and maps KDIGO risk.
#'
#' @param data Data frame with renal measures.
#' @param col_map Named list with required key: eGFR; optional key: UACR.
#' @param na_action One of:
#'   - "keep"  (retain rows; stages become NA where inputs missing)
#'   - "omit"  (drop rows with any missing eGFR/UACR that are mapped)
#'   - "error" (abort if any mapped input missing)
#' @param check_extreme Logical; if TRUE, screen eGFR/UACR for extreme values.
#' @param extreme_action One of:
#'   - "cap"   – winsorize to bounds (eGFR: [0, 200]; UACR: [0, 5000] mg/g)
#'   - "NA"    – replace out-of-range with NA
#'   - "error" – abort on any out-of-range value
#'
#' @return Tibble with CKD_stage, Albuminuria_stage, KDIGO_risk.
#' @references
#' KDIGO 2012 CKD guideline. \doi{10.1038/kisup.2012.73}
#' @examples
#' df <- data.frame(eGFR = c(95, 50), UACR = c(10, 200))
#' ckd_stage(df, list(eGFR = "eGFR", UACR = "UACR"))
#' @export
ckd_stage <- function(
  data,
  col_map,
  na_action = c("keep","omit","error"),
  check_extreme = FALSE,
  extreme_action = c("cap","NA","error")
) {
  na_action <- match.arg(na_action)
  extreme_action <- match.arg(extreme_action)

  # Validate mapping and columns (HM-CS centralized)
  if (!is.list(col_map) || is.null(names(col_map))) {
    rlang::abort("ckd_stage(): `col_map` must be a named list.", class = "healthmarkers_ckd_error_colmap_type")
  }
  req <- c("eGFR")
  hm_validate_inputs(data, col_map, req, fn = "ckd_stage")

  # Identify optional present inputs to include in NA/extreme policies
  opt_keys <- character()
  if (!is.null(col_map$UACR) && col_map$UACR %in% names(data)) opt_keys <- c(opt_keys, "UACR")
  keys_for_policy <- c(req, opt_keys)

  hm_inform("ckd_stage(): computing stages", level = "inform")

  # Coerce to numeric quietly
  eGFR <- suppressWarnings(as.numeric(data[[col_map$eGFR]]))
  UACR <- if ("UACR" %in% keys_for_policy) suppressWarnings(as.numeric(data[[col_map$UACR]])) else rep(NA_real_, length(eGFR))

  # NA policy over mapped inputs (required + present optional)
  if (length(keys_for_policy)) {
    vals <- list(eGFR = eGFR)
    if ("UACR" %in% keys_for_policy) vals$UACR <- UACR
    rows_with_na <- Reduce(`|`, lapply(vals, function(x) is.na(x) | !is.finite(x)))
  } else {
    rows_with_na <- rep(FALSE, length(eGFR))
  }

  if (na_action == "error" && any(rows_with_na)) {
    rlang::abort("ckd_stage(): missing/non-finite values in mapped inputs (na_action='error').",
                 class = "healthmarkers_ckd_error_missing_values")
  } else if (na_action == "omit" && any(rows_with_na)) {
    keep <- !rows_with_na
    eGFR <- eGFR[keep]
    UACR <- UACR[keep]
  }

  # Optional extremes handling
  if (isTRUE(check_extreme)) {
    rules <- list(eGFR = c(0, 200), UACR = c(0, 5000))
    if (length(eGFR)) {
      bad_g <- is.finite(eGFR) & (eGFR < rules$eGFR[1] | eGFR > rules$eGFR[2])
      if (any(bad_g)) {
        if (extreme_action == "error") {
          rlang::abort("ckd_stage(): eGFR out of range.", class = "healthmarkers_ckd_error_extreme")
        } else if (extreme_action == "NA") {
          eGFR[bad_g] <- NA_real_
        } else if (extreme_action == "cap") {
          eGFR[bad_g & eGFR < rules$eGFR[1]] <- rules$eGFR[1]
          eGFR[bad_g & eGFR > rules$eGFR[2]] <- rules$eGFR[2]
        }
      }
    }
    if ("UACR" %in% keys_for_policy && length(UACR)) {
      bad_a <- is.finite(UACR) & (UACR < rules$UACR[1] | UACR > rules$UACR[2])
      if (any(bad_a)) {
        if (extreme_action == "error") {
          rlang::abort("ckd_stage(): UACR out of range.", class = "healthmarkers_ckd_error_extreme")
        } else if (extreme_action == "NA") {
          UACR[bad_a] <- NA_real_
        } else if (extreme_action == "cap") {
          UACR[bad_a & UACR < rules$UACR[1]] <- rules$UACR[1]
          UACR[bad_a & UACR > rules$UACR[2]] <- rules$UACR[2]
        }
      }
    }
  }

  # G stage
  G <- cut(
    eGFR,
    breaks = c(-Inf, 15, 30, 45, 60, 90, Inf),
    labels = c("G5","G4","G3b","G3a","G2","G1"),
    right = FALSE
  )

  # A stage (if UACR provided; else NA)
  A <- cut(
    UACR,
    breaks = c(-Inf, 30, 300, Inf),
    labels = c("A1","A2","A3"),
    right = FALSE
  )

  # For risk mapping, treat missing albuminuria as A1 by convention
  A_filled <- as.character(A); A_filled[is.na(A_filled)] <- "A1"

  kdigo_map <- function(g, a) {
    if (is.na(g) || is.na(a)) return(NA_character_)
    if (g %in% c("G1","G2") && a == "A1") return("Low")
    if (g %in% c("G1","G2") && a %in% c("A2","A3")) return("Moderate")
    if (g == "G3a" && a == "A1") return("Moderate")
    if (g == "G3a" && a %in% c("A2","A3")) return("High")
    if (g == "G3b") return(ifelse(a == "A1","High","Very High"))
    if (g %in% c("G4","G5")) return("Very High")
    "Moderate"
  }
  KDIGO <- mapply(kdigo_map, as.character(G), A_filled, USE.NAMES = FALSE)

  out <- tibble::tibble(
    CKD_stage = factor(G, levels = c("G1","G2","G3a","G3b","G4","G5")),
    Albuminuria_stage = factor(A, levels = c("A1","A2","A3")),
    KDIGO_risk = factor(KDIGO, levels = c("Low","Moderate","High","Very High"))
  )

  hm_inform("ckd_stage(): completed", level = "inform")
  out
}