# File: R/corrected_calcium.R

#' Corrected Calcium (Payne's Formula)
#'
#' @description
#' Computes corrected (albumin-adjusted) calcium using Payne's formula:
#' Corrected Ca = Total Ca + 0.8 * (4.0 - Albumin)
#'
#' The function auto-detects conventional (mg/dL) vs. SI (mmol/L) units for calcium
#' and g/dL vs. g/L for albumin, converts to conventional internally, then
#' standardizes the output to the requested unit system.
#'
#' @param data A data.frame or tibble containing calcium and albumin measurements.
#' @param col_map Named list mapping keys to column names. Required keys:
#'   - `Ca`: total calcium
#'   - `Albumin`: serum albumin
#'   Defaults map to same-named columns.
#' @param units One of "conventional" (mg/dL) or "SI" (mmol/L). If NULL (default),
#'   output units match auto-detected input units.
#' @param na_action One of "keep", "warn", "omit", "error", "ignore" controlling
#'   how missing/non-finite values are handled. Default "keep".
#' @param na_warn_prop Proportion (0-1) threshold for missingness warnings when
#'   `na_action = "warn"`. Default 0.2.
#' @param check_extreme Logical; if TRUE, scan for extreme values based on
#'   `extreme_rules`. Default FALSE.
#' @param extreme_action One of "warn", "cap", "NA", "error", "ignore" for handling
#'   detected extremes. Default "warn".
#' @param extreme_rules Named list of c(min, max) ranges for Ca and Albumin.
#'   Defaults: Ca c(5, 15) mg/dL, Albumin c(2, 5) g/dL (conventional).
#' @param verbose Logical; if TRUE, prints progress messages. Default FALSE.
#'
#' @return A tibble with one column: `corrected_calcium` in requested units.
#'
#' @details
#' Unit auto-detection heuristics:
#' - Calcium: values typically <5 mmol/L (SI) vs. 7-12 mg/dL (conventional)
#' - Albumin: values typically 35-50 g/L vs. 3.5-5.0 g/dL
#'
#' Conversion factors:
#' - Ca: mmol/L to mg/dL multiply by 4.0
#' - Albumin: g/L to g/dL divide by 10
#'
#' @references
#' Payne RB, et al. (1973). Interpretation of serum calcium in patients with
#' abnormal serum proteins. Br Med J, 4(5893):643-646.
#'
#' @importFrom tibble tibble
#' @importFrom rlang abort warn inform
#' @export
#'
#' @examples
#' library(tibble)
#' df <- tibble(Ca = c(9.0, 8.5), Albumin = c(3.5, 3.0))
#' cm <- list(Ca = "Ca", Albumin = "Albumin")
#' corrected_calcium(df, cm)
corrected_calcium <- function(
  data,
  col_map = list(Ca = "Ca", Albumin = "Albumin"),
  units = NULL,
  na_action = c("keep", "warn", "omit", "error", "ignore"),
  na_warn_prop = 0.2,
  check_extreme = FALSE,
  extreme_action = c("warn", "cap", "NA", "error", "ignore"),
  extreme_rules = NULL,
  verbose = FALSE
) {
  na_action <- match.arg(na_action)
  extreme_action <- match.arg(extreme_action)
  
  # Verbose: preparing inputs
  if (isTRUE(verbose)) {
    message("-> corrected_calcium: preparing inputs")
  } else {
    hm_inform("corrected_calcium(): preparing inputs", level = "debug")
  }
  
  # Validation
  if (!is.data.frame(data)) {
    rlang::abort("corrected_calcium(): `data` must be a data.frame or tibble.",
                 class = "healthmarkers_calcium_error_data_type")
  }
  
  required <- c("Ca", "Albumin")
  if (!is.list(col_map) || is.null(names(col_map))) {
    rlang::abort("corrected_calcium(): `col_map` must be a named list.",
                 class = "healthmarkers_calcium_error_colmap_type")
  }
  
  missing_map <- setdiff(required, names(col_map))
  if (length(missing_map)) {
    rlang::abort(
      sprintf("corrected_calcium(): missing col_map entries for: %s",
              paste(missing_map, collapse = ", ")),
      class = "healthmarkers_calcium_error_missing_map"
    )
  }
  
  # Check columns exist
  mapped_cols <- unname(unlist(col_map[required]))
  missing_cols <- setdiff(mapped_cols, names(data))
  if (length(missing_cols)) {
    rlang::abort(
      sprintf("corrected_calcium(): missing required columns: %s",
              paste(missing_cols, collapse = ", ")),
      class = "healthmarkers_calcium_error_missing_cols"
    )
  }
  
  # Handle empty input
  if (nrow(data) == 0) {
    if (isTRUE(verbose)) message("Completed corrected_calcium: 0 rows.")
    return(tibble::tibble(corrected_calcium = numeric(0)))
  }
  
  # Extract columns
  ca_col <- col_map$Ca
  alb_col <- col_map$Albumin
  
  # Coerce to numeric with warning for NA coercion
  ca_orig <- data[[ca_col]]
  alb_orig <- data[[alb_col]]
  
  if (!is.numeric(ca_orig)) {
    suppressWarnings(ca_numeric <- as.numeric(ca_orig))
    introduced_ca <- sum(is.na(ca_numeric) & !is.na(ca_orig))
    if (introduced_ca > 0) {
      rlang::warn(
        sprintf("corrected_calcium(): column '%s' coerced to numeric; NAs introduced: %d",
                ca_col, introduced_ca),
        class = "healthmarkers_calcium_warn_na_coercion"
      )
    }
    ca_orig <- ca_numeric
  }
  
  if (!is.numeric(alb_orig)) {
    suppressWarnings(alb_numeric <- as.numeric(alb_orig))
    introduced_alb <- sum(is.na(alb_numeric) & !is.na(alb_orig))
    if (introduced_alb > 0) {
      rlang::warn(
        sprintf("corrected_calcium(): column '%s' coerced to numeric; NAs introduced: %d",
                alb_col, introduced_alb),
        class = "healthmarkers_calcium_warn_na_coercion"
      )
    }
    alb_orig <- alb_numeric
  }
  
  # Make non-finite -> NA
  ca_orig[!is.finite(ca_orig)] <- NA_real_
  alb_orig[!is.finite(alb_orig)] <- NA_real_
  
  # Auto-detect units (use non-NA values for heuristic)
  ca_median <- median(ca_orig, na.rm = TRUE)
  alb_median <- median(alb_orig, na.rm = TRUE)
  
  # Heuristic: Ca typically 2-3 mmol/L (SI) vs 8-11 mg/dL (conventional)
  # Albumin typically 35-45 g/L vs 3.5-4.5 g/dL
  ca_is_SI <- !is.na(ca_median) && ca_median < 5
  alb_is_gL <- !is.na(alb_median) && alb_median > 10
  
  # Convert to conventional units internally (mg/dL for Ca, g/dL for Alb)
  ca_conv <- if (ca_is_SI) ca_orig * 4.0 else ca_orig
  alb_conv <- if (alb_is_gL) alb_orig / 10.0 else alb_orig
  
  # Emit unit detection warning
  ca_unit_str <- if (ca_is_SI) "mmol/L" else "mg/dL"
  alb_unit_str <- if (alb_is_gL) "g/L" else "g/dL"
  
  # Determine output unit
  if (is.null(units)) {
    # Match input unit
    output_is_SI <- ca_is_SI
  } else {
    output_is_SI <- (units == "SI")
  }
  
  output_unit_str <- if (output_is_SI) "mmol/L" else "mg/dL"
  
  rlang::warn(
    sprintf("corrected_calcium(): auto-detected units Ca=%s, Alb=%s; output standardized to %s.",
            ca_unit_str, alb_unit_str, output_unit_str),
    class = "healthmarkers_calcium_warn_unit_assumption"
  )
  
  # NA handling
  na_action_eff <- if (na_action %in% c("ignore", "keep")) "keep" else na_action
  
  if (na_action == "warn") {
    # Check for missingness
    ca_na_count <- sum(is.na(ca_conv))
    alb_na_count <- sum(is.na(alb_conv))
    total_na <- sum(is.na(ca_conv) | is.na(alb_conv))
    if (total_na > 0) {
      rlang::warn(
        sprintf("corrected_calcium(): %d rows with missing Ca or Albumin.",
                total_na),
        class = "healthmarkers_calcium_warn_missing"
      )
    }
  } else if (na_action_eff == "error") {
    has_na <- is.na(ca_conv) | is.na(alb_conv)
    if (any(has_na)) {
      rlang::abort(
        "corrected_calcium(): missing values present (na_action='error').",
        class = "healthmarkers_calcium_error_missing"
      )
    }
  } else if (na_action_eff == "omit") {
    has_na <- is.na(ca_conv) | is.na(alb_conv)
    if (any(has_na)) {
      keep <- !has_na
      ca_conv <- ca_conv[keep]
      alb_conv <- alb_conv[keep]
    }
  }
  
  # Verbose: computing
  if (isTRUE(verbose)) {
    message("-> corrected_calcium: computing result")
  } else {
    hm_inform("corrected_calcium(): computing result", level = "debug")
  }
  
  # Compute corrected calcium (Payne's formula)
  # Corrected Ca = Ca + 0.8 * (4.0 - Albumin)
  corrected_ca_conv <- ca_conv + 0.8 * (4.0 - alb_conv)
  
  # Check for domain warnings (conventional units)
  if (!check_extreme) {
    # Domain warnings when not doing extreme scan
    if (any(!is.na(corrected_ca_conv) & (corrected_ca_conv < 5 | corrected_ca_conv > 15))) {
      rlang::warn(
        "corrected_calcium(): corrected calcium outside typical 5–15 mg/dL range detected.",
        class = "healthmarkers_calcium_warn_domain_corrected"
      )
    }
    if (any(!is.na(alb_conv) & (alb_conv < 2 | alb_conv > 5))) {
      rlang::warn(
        "corrected_calcium(): albumin outside typical 2–5 g/dL range; correction accuracy may be affected.",
        class = "healthmarkers_calcium_warn_domain_albumin"
      )
    }
  }
  
  # Extreme value handling
  if (isTRUE(check_extreme)) {
    # Default rules in conventional units
    if (is.null(extreme_rules)) {
      extreme_rules <- list(
        Ca = c(5, 15),
        Albumin = c(2, 5),
        corrected_calcium = c(5, 15)
      )
    }
    
    # Check extremes
    extremes_detected <- FALSE
    extremes_count <- 0
    
    # Check Ca
    if ("Ca" %in% names(extreme_rules)) {
      ca_range <- extreme_rules$Ca
      ca_bad <- !is.na(ca_conv) & (ca_conv < ca_range[1] | ca_conv > ca_range[2])
      extremes_count <- extremes_count + sum(ca_bad)
      if (any(ca_bad)) extremes_detected <- TRUE
    }
    
    # Check Albumin
    if ("Albumin" %in% names(extreme_rules)) {
      alb_range <- extreme_rules$Albumin
      alb_bad <- !is.na(alb_conv) & (alb_conv < alb_range[1] | alb_conv > alb_range[2])
      extremes_count <- extremes_count + sum(alb_bad)
      if (any(alb_bad)) extremes_detected <- TRUE
    }
    
    # Check corrected calcium
    if ("corrected_calcium" %in% names(extreme_rules)) {
      cc_range <- extreme_rules$corrected_calcium
      cc_bad <- !is.na(corrected_ca_conv) & (corrected_ca_conv < cc_range[1] | corrected_ca_conv > cc_range[2])
      if (any(cc_bad)) {
        extremes_detected <- TRUE
        
        if (extreme_action == "warn") {
          rlang::warn(
            sprintf("corrected_calcium(): %d extreme values detected outside %g-%g mg/dL.",
                    sum(cc_bad), cc_range[1], cc_range[2]),
            class = "healthmarkers_calcium_warn_extremes_detected"
          )
        } else if (extreme_action == "cap") {
          corrected_ca_conv[cc_bad & corrected_ca_conv < cc_range[1]] <- cc_range[1]
          corrected_ca_conv[cc_bad & corrected_ca_conv > cc_range[2]] <- cc_range[2]
          rlang::warn(
            sprintf("corrected_calcium(): %d extreme values capped to %g-%g mg/dL.",
                    sum(cc_bad), cc_range[1], cc_range[2]),
            class = "healthmarkers_calcium_warn_extremes_capped"
          )
        } else if (extreme_action == "NA") {
          corrected_ca_conv[cc_bad] <- NA_real_
          rlang::warn(
            sprintf("corrected_calcium(): %d extreme values set to NA.",
                    sum(cc_bad)),
            class = "healthmarkers_calcium_warn_extremes_na"
          )
        } else if (extreme_action == "error") {
          rlang::abort(
            sprintf("corrected_calcium(): %d extreme values detected (extreme_action='error').",
                    sum(cc_bad)),
            class = "healthmarkers_calcium_error_extremes"
          )
        }
      }
    }
  }
  
  # Convert to output units
  if (output_is_SI) {
    corrected_calcium <- corrected_ca_conv / 4.0
  } else {
    corrected_calcium <- corrected_ca_conv
  }
  
  # Build result
  result <- tibble::tibble(corrected_calcium = corrected_calcium)
  
  # Verbose: completion
  if (isTRUE(verbose)) {
    message(sprintf("Completed corrected_calcium: %d rows.", nrow(result)))
  } else {
    hm_inform(
      sprintf("corrected_calcium(): completed %d rows", nrow(result)),
      level = "debug"
    )
  }
  
  result
}
