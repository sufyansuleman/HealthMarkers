#' Albumin-Corrected Calcium
#'
#' Calculates the albumin-adjusted (corrected) serum calcium level, accounting for hypoalbuminemia,
#' using the Payne formula.
#'
#' @details
#' Payne formula (conventional units): Corrected Ca (mg/dL) = measured Ca (mg/dL) + 0.8 * (4.0 - albumin (g/dL)).
#' If inputs appear to be in SI units (calcium mmol/L, albumin g/L), they are converted to mg/dL and g/dL
#' (using 1 mmol/L ~= 4 mg/dL; 1 g/L = 0.1 g/dL) for the correction and converted back to mmol/L for output.
#'
#' @param data A data.frame or tibble containing serum calcium and albumin.
#' @param col_map Named list with `calcium` and `albumin` indicating column names.
#' @param verbose Logical; if TRUE, emits progress via hm_inform().
#' @param na_action One of c("keep","omit","error","ignore","warn").
#' @param check_extreme Logical; if TRUE, scan inputs for plausible ranges (applied on working units).
#' @param extreme_action One of c("warn","cap","error","ignore","NA").
#' @param extreme_rules Optional overrides for defaults in working units:
#'   list(ca_mgdl = c(4, 15), alb_gdl = c(2, 5)).
#' @param units One of c("auto","conventional","si"). "auto" attempts unit detection.
#'
#' @return A tibble with one column: corrected_calcium (numeric, in mg/dL for
#'   conventional input or mmol/L for SI / auto-SI input).
#'
#' @export
corrected_calcium <- function(
  data,
  col_map = list(calcium = "Ca", albumin = "Alb"),
  units = c("auto", "conventional", "si"),
  na_action = c("keep", "omit", "error", "ignore", "warn"),
  check_extreme = FALSE,
  extreme_action = c("warn", "cap", "error", "ignore", "NA"),
  extreme_rules = NULL,
  verbose = FALSE
) {
  units         <- match.arg(units)
  na_action_raw <- match.arg(na_action)
  na_action_eff <- if (na_action_raw %in% c("ignore", "warn")) "keep" else na_action_raw
  extreme_action <- match.arg(extreme_action)

  ## --- Basic validation -----------------------------------------------------
  if (!is.data.frame(data)) {
    rlang::abort(
      "corrected_calcium(): `data` must be a data.frame or tibble.",
      class = "healthmarkers_calcium_error_data_type"
    )
  }
  if (!is.list(col_map)) {
    rlang::abort(
      "corrected_calcium(): `col_map` must be a named list.",
      class = "healthmarkers_calcium_error_colmap_type"
    )
  }

  ca_key  <- intersect(names(col_map), c("calcium", "ca"))
  alb_key <- intersect(names(col_map), c("albumin", "alb"))
  if (length(ca_key) == 0 || length(alb_key) == 0) {
    rlang::abort(
      "corrected_calcium(): missing col_map entries for calcium and/or albumin.",
      class = "healthmarkers_calcium_error_missing_map"
    )
  }

  ca_col  <- as.character(col_map[[ca_key[1]]])
  alb_col <- as.character(col_map[[alb_key[1]]])

  if (!nzchar(ca_col) || !nzchar(alb_col)) {
    rlang::abort(
      "corrected_calcium(): empty mapping values supplied.",
      class = "healthmarkers_calcium_error_bad_map_values"
    )
  }

  missing_cols <- setdiff(c(ca_col, alb_col), names(data))
  if (length(missing_cols) > 0L) {
    rlang::abort(
      paste0(
        "corrected_calcium(): missing required columns in data: ",
        paste(missing_cols, collapse = ", ")
      ),
      class = "healthmarkers_calcium_error_missing_columns"
    )
  }

  if (isTRUE(verbose)) message("-> corrected_calcium: preparing inputs")

  ## --- Coercion to numeric --------------------------------------------------
  coerce_num <- function(x, nm) {
    if (!is.numeric(x)) {
      old <- x
      suppressWarnings(xn <- as.numeric(old))
      introduced <- sum(is.na(xn) & !is.na(old))
      if (introduced > 0L) {
        rlang::warn(
          sprintf("Column '%s' coerced to numeric; NAs introduced: %d", nm, introduced),
          class = "healthmarkers_calcium_warn_na_coercion"
        )
      }
      x <- xn
    }
    x[!is.finite(x)] <- NA_real_
    x
  }

  ca_raw  <- coerce_num(data[[ca_col]],  ca_col)
  alb_raw <- coerce_num(data[[alb_col]], alb_col)

  ## --- NA handling ----------------------------------------------------------
  row_missing <- is.na(ca_raw) | is.na(alb_raw)

  if (na_action_raw == "warn" && any(row_missing)) {
    rlang::warn(
      "Missing calcium or albumin values; corrected_calcium will be NA for those rows.",
      class = "healthmarkers_calcium_warn_missing_inputs"
    )
  }

  if (na_action_eff == "error" && any(row_missing)) {
    rlang::abort(
      "corrected_calcium(): required inputs contain missing values (na_action='error').",
      class = "healthmarkers_calcium_error_missing_values"
    )
  }

  keep_idx <- if (na_action_eff == "omit") !row_missing else rep(TRUE, length(row_missing))
  d_ca  <- ca_raw[keep_idx]
  d_alb <- alb_raw[keep_idx]

  ## --- Unit resolution / auto detection ------------------------------------
  med_ca  <- stats::median(d_ca,  na.rm = TRUE)
  med_alb <- stats::median(d_alb, na.rm = TRUE)

  if (units == "conventional") {
    ca_unit  <- "mg/dL"
    alb_unit <- "g/dL"
  } else if (units == "si") {
    ca_unit  <- "mmol/L"
    alb_unit <- "g/L"
  } else { # units == "auto"
    ca_unit  <- if (is.finite(med_ca)  && med_ca  <= 4)  "mmol/L" else "mg/dL"
    alb_unit <- if (is.finite(med_alb) && med_alb > 20) "g/L"    else "g/dL"
    si_inferred <- (ca_unit == "mmol/L" || alb_unit == "g/L")
    if (si_inferred) {
      rlang::warn(
        sprintf(
          "corrected_calcium(): auto-detected SI units Ca=%s, Alb=%s; output standardized to mmol/L.",
          ca_unit, alb_unit
        ),
        class = "healthmarkers_calcium_warn_unit_assumption"
      )
    }
  }

  si_inferred <- (ca_unit == "mmol/L" || alb_unit == "g/L")

  ## --- Convert to working conventional units (mg/dL, g/dL) -----------------
  ca_work_mgdl <- if (ca_unit == "mmol/L") d_ca * 4.0 else d_ca
  alb_work_gdl <- if (alb_unit == "g/L")  d_alb / 10.0 else d_alb

  ## --- Extreme scan in working units ---------------------------------------
  if (isTRUE(check_extreme)) {
    limits <- list(ca_mgdl = c(4, 15), alb_gdl = c(2, 5))
    if (is.list(extreme_rules)) {
      if (!is.null(extreme_rules$ca_mgdl)) limits$ca_mgdl <- extreme_rules$ca_mgdl
      if (!is.null(extreme_rules$alb_gdl)) limits$alb_gdl <- extreme_rules$alb_gdl
    }

    bad_ca  <- is.finite(ca_work_mgdl)  & (ca_work_mgdl  < limits$ca_mgdl[1]  | ca_work_mgdl  > limits$ca_mgdl[2])
    bad_alb <- is.finite(alb_work_gdl)  & (alb_work_gdl  < limits$alb_gdl[1]  | alb_work_gdl  > limits$alb_gdl[2])
    n_bad <- sum(bad_ca) + sum(bad_alb)

    if (n_bad > 0L) {
      if (extreme_action == "error") {
        rlang::abort(
          sprintf("corrected_calcium(): %d extreme input values detected.", n_bad),
          class = "healthmarkers_calcium_error_extremes"
        )
      } else if (extreme_action == "warn") {
        rlang::warn(
          sprintf("corrected_calcium(): detected %d extreme input values (not altered).", n_bad),
          class = "healthmarkers_calcium_warn_extremes_detected"
        )
      } else if (extreme_action == "cap") {
        if (any(bad_ca)) {
          ca_work_mgdl[bad_ca & ca_work_mgdl < limits$ca_mgdl[1]] <- limits$ca_mgdl[1]
          ca_work_mgdl[bad_ca & ca_work_mgdl > limits$ca_mgdl[2]] <- limits$ca_mgdl[2]
        }
        if (any(bad_alb)) {
          alb_work_gdl[bad_alb & alb_work_gdl < limits$alb_gdl[1]] <- limits$alb_gdl[1]
          alb_work_gdl[bad_alb & alb_work_gdl > limits$alb_gdl[2]] <- limits$alb_gdl[2]
        }
        rlang::warn(
          sprintf("corrected_calcium(): capped %d extreme input values into allowed ranges.", n_bad),
          class = "healthmarkers_calcium_warn_extremes_capped"
        )
      } else if (extreme_action == "NA") {
        ca_work_mgdl[bad_ca]  <- NA_real_
        alb_work_gdl[bad_alb] <- NA_real_
      }
    }
  }

  ## --- Payne correction in mg/dL -------------------------------------------
  corr_mgdl <- ca_work_mgdl + 0.8 * (4.0 - alb_work_gdl)

  ## --- Domain warnings (explicit conventional only, no extreme scan) -------
  if (units == "conventional" && !si_inferred && !isTRUE(check_extreme)) {
    if (any(is.finite(alb_work_gdl) & (alb_work_gdl < 2 | alb_work_gdl > 5))) {
      rlang::warn(
        "corrected_calcium(): albumin outside typical 2-5 g/dL range; correction accuracy may be affected.",
        class = "healthmarkers_calcium_warn_albumin_range"
      )
    }
    if (any(is.finite(corr_mgdl) & (corr_mgdl < 5 | corr_mgdl > 15))) {
      rlang::warn(
        "corrected_calcium(): corrected calcium outside typical 5-15 mg/dL range detected.",
        class = "healthmarkers_calcium_warn_corrected_range"
      )
    }
  }

  ## --- Output scaling -------------------------------------------------------
  # SI or auto-SI => mmol/L
  corr_out <- if (units == "si" || (units == "auto" && si_inferred)) corr_mgdl / 4.0 else corr_mgdl

  if (isTRUE(verbose)) message("-> corrected_calcium: computing result")

  result <- tibble::tibble(corrected_calcium = corr_out)

  if (na_action_eff != "omit") {
    padded <- tibble::tibble(corrected_calcium = rep(NA_real_, length(ca_raw)))
    padded$corrected_calcium[keep_idx] <- result$corrected_calcium
    result <- padded
  }

  if (isTRUE(verbose)) {
    message(sprintf("Completed corrected_calcium: %d rows.", nrow(result)))
  }

  return(result)
}
