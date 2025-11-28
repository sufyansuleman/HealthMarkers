#' Appendicular Lean Mass to BMI Index
#'
#' Calculates the ratio of appendicular lean mass (ALM) to body mass index (BMI),
#' and flags low muscle mass based on FNIH Sarcopenia Project cut-points.
#'
#' @details
#' The ALM/BMI index indicates muscle mass relative to body size. Low values suggest sarcopenia.
#' FNIH Sarcopenia Project cut-points:
#' - Men: ALM/BMI < 0.789
#' - Women: ALM/BMI < 0.512
#'
#' ALM should be in kilograms and BMI in kg/m^2.
#'
#' @param data A data.frame or tibble with columns for ALM, BMI, and sex.
#' @param col_map Named list with:
#'   - alm: appendicular lean mass (kg)
#'   - bmi: body mass index (kg/m^2)
#'   - sex: "Male"/"Female" (case-insensitive; also accepts first letter m/f)
#' @param na_action One of c("keep","omit","error","ignore","warn").
#' @param check_extreme Logical; if TRUE, scan inputs for plausible ranges.
#' @param extreme_action One of c("warn","cap","error","ignore","NA").
#' @param extreme_rules Optional overrides; defaults: list(alm=c(5,40), bmi=c(10,60)).
#' @param verbose Logical; if TRUE, emits progress via rlang::inform.
#' @return A tibble with:
#'   - alm_bmi_ratio (numeric)
#'   - low_muscle_mass (logical; TRUE if below sex-specific cut-point; NA if sex unknown or ratio NA)
#'
#' @references
#' McLean RR, Shardell MD, Alley DE, et al. (2014).
#' Criteria for clinically relevant weakness and low lean mass: FNIH Sarcopenia Project.
#' J Gerontol A Biol Sci Med Sci. 69(5):576–583. doi:10.1093/gerona/glu012
#'
#' Studenski SA, Peters KW, Alley DE, et al. (2014).
#' The FNIH Sarcopenia Project: Rationale, Study Description, Conference Recommendations, and Final Estimates.
#' J Gerontol A Biol Sci Med Sci. 69(5):564–570. doi:10.1093/gerona/glu013
#'
#' @export
alm_bmi_index <- function(
  data,
  col_map = list(alm = "ALM_kg", bmi = "BMI", sex = "Sex"),
  na_action = c("keep","omit","error","ignore","warn"),
  check_extreme = FALSE,
  extreme_action = c("warn","cap","error","ignore","NA"),
  extreme_rules = NULL,
  verbose = FALSE
) {
  na_action_raw <- match.arg(na_action)
  na_action_eff <- if (na_action_raw %in% c("ignore","warn")) "keep" else na_action_raw
  extreme_action <- match.arg(extreme_action)

  # Validate mapping and columns
  if (!is.data.frame(data)) {
    rlang::abort("alm_bmi_index(): `data` must be a data.frame or tibble.",
                 class = "healthmarkers_alm_bmi_error_data_type")
  }
  if (!is.list(col_map) || is.null(names(col_map))) {
    rlang::abort("alm_bmi_index(): `col_map` must be a named list.",
                 class = "healthmarkers_alm_bmi_error_colmap_type")
  }
  req <- c("alm","bmi","sex")
  missing_keys <- setdiff(req, names(col_map))
  if (length(missing_keys)) {
    rlang::abort(paste0("alm_bmi_index(): missing col_map entries for: ",
                        paste(missing_keys, collapse = ", ")),
                 class = "healthmarkers_alm_bmi_error_missing_map")
  }
  mapped <- unname(unlist(col_map[req], use.names = FALSE))
  if (any(!nzchar(mapped))) {
    bad <- req[!nzchar(unname(unlist(col_map[req])))]
    rlang::abort(paste0("alm_bmi_index(): missing col_map entries for: ",
                        paste(bad, collapse = ", ")),
                 class = "healthmarkers_alm_bmi_error_bad_map_values")
  }
  missing_cols <- setdiff(mapped, names(data))
  if (length(missing_cols)) {
    rlang::abort(paste0("alm_bmi_index(): missing required columns in data: ",
                        paste(missing_cols, collapse = ", ")),
                 class = "healthmarkers_alm_bmi_error_missing_columns")
  }

  if (isTRUE(verbose)) rlang::inform("-> alm_bmi_index: preparing inputs")
  else hm_inform("alm_bmi_index(): preparing inputs", level = "debug")

  # Coerce ALM and BMI to numeric; keep sex as character
  num_cols <- unname(unlist(col_map[c("alm","bmi")]))
  for (cn in num_cols) {
    if (!is.numeric(data[[cn]])) {
      old <- data[[cn]]
      suppressWarnings(new <- as.numeric(old))
      intro <- sum(is.na(new) & !is.na(old))
      if (intro > 0L) {
        rlang::warn(sprintf("Column '%s' coerced to numeric; NAs introduced: %d", cn, intro),
                    class = "healthmarkers_alm_bmi_warn_na_coercion")
      }
      data[[cn]] <- new
    }
    data[[cn]][!is.finite(data[[cn]])] <- NA_real_
  }

  alm <- data[[col_map$alm]]
  bmi <- data[[col_map$bmi]]
  sex_raw <- data[[col_map$sex]]

  # NA policy
  any_na_req <- is.na(alm) | is.na(bmi) | is.na(sex_raw)
  if (na_action_raw == "warn" && any(any_na_req)) {
    rlang::warn("Missing ALM, BMI, or sex; outputs will be NA for those entries.",
                class = "healthmarkers_alm_bmi_warn_missing_inputs")
  }
  if (na_action_eff == "error" && any(any_na_req)) {
    rlang::abort("alm_bmi_index(): required inputs contain missing values (na_action='error').",
                 class = "healthmarkers_alm_bmi_error_missing_values")
  }
  keep <- if (na_action_eff == "omit") !any_na_req else rep(TRUE, length(alm))

  d_alm <- alm[keep]
  d_bmi <- bmi[keep]
  d_sex_raw <- sex_raw[keep]

  # Normalize sex: "male"/"female" by first letter; unknown -> NA and warn
  sex_chr <- tolower(as.character(d_sex_raw))
  sex_norm <- ifelse(substr(sex_chr, 1, 1) %in% c("m","1"), "male",
                ifelse(substr(sex_chr, 1, 1) %in% c("f","0"), "female", NA_character_))
  if (any(is.na(sex_norm) & !is.na(d_sex_raw))) {
    rlang::warn("alm_bmi_index(): unrecognized sex entries; expected male/female.",
                class = "healthmarkers_alm_bmi_warn_sex_unknown")
  }

  # Domain warnings
  if (any(is.finite(d_bmi) & (d_bmi < 10 | d_bmi > 60))) {
    rlang::warn("alm_bmi_index(): BMI outside realistic range (10–60 kg/m^2) detected.",
                class = "healthmarkers_alm_bmi_warn_bmi_range")
  }
  if (any(is.finite(d_alm) & (d_alm < 5 | d_alm > 40))) {
    rlang::warn("alm_bmi_index(): ALM outside plausible range (5–40 kg) detected.",
                class = "healthmarkers_alm_bmi_warn_alm_range")
  }
  if (any(is.finite(d_bmi) & d_bmi <= 0)) {
    rlang::warn("alm_bmi_index(): nonpositive BMI encountered; ratio set to NA for those rows.",
                class = "healthmarkers_alm_bmi_warn_bmi_nonpositive")
  }

  # Optional extreme scan/handling
  if (isTRUE(check_extreme)) {
    rules_def <- list(alm = c(5, 40), bmi = c(10, 60))
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
    ca <- cap_vec(d_alm, rules_def$alm[1], rules_def$alm[2]); d_alm <- ca$x
    cb <- cap_vec(d_bmi, rules_def$bmi[1], rules_def$bmi[2]); d_bmi <- cb$x

    if (total > 0L) {
      if (extreme_action == "error") {
        rlang::abort(sprintf("alm_bmi_index(): %d extreme input values detected.", total),
                     class = "healthmarkers_alm_bmi_error_extremes")
      } else if (extreme_action == "warn") {
        rlang::warn(sprintf("alm_bmi_index(): detected %d extreme input values (not altered).", total),
                    class = "healthmarkers_alm_bmi_warn_extremes_detected")
      } else if (extreme_action == "cap") {
        rlang::warn(sprintf("alm_bmi_index(): capped %d extreme input values into allowed ranges.", total),
                    class = "healthmarkers_alm_bmi_warn_extremes_capped")
      }
    }
  }

  if (isTRUE(verbose)) rlang::inform("-> alm_bmi_index: computing")
  else hm_inform("alm_bmi_index(): computing", level = "debug")

  # Compute ratio; protect against nonpositive BMI
  ratio <- d_alm / d_bmi
  ratio[!is.finite(ratio)] <- NA_real_

  # Sex-specific thresholds
  thr <- ifelse(sex_norm == "male", 0.789,
         ifelse(sex_norm == "female", 0.512, NA_real_))

  low_flag <- ifelse(is.na(ratio) | is.na(thr), NA, ratio < thr)

  out <- tibble::tibble(
    alm_bmi_ratio = as.numeric(ratio),
    low_muscle_mass = as.logical(low_flag)
  )

  # Pad back if not omitting
  if (na_action_eff != "omit") {
    res <- tibble::tibble(
      alm_bmi_ratio = rep(NA_real_, length(alm)),
      low_muscle_mass = rep(NA, length(alm))
    )
    res[keep, ] <- out
    out <- res
  }

  if (isTRUE(verbose)) rlang::inform(sprintf("Completed alm_bmi_index: %d rows.", nrow(out)))
  else hm_inform(sprintf("alm_bmi_index(): completed (%d rows)", nrow(out)), level = "debug")

  out
}