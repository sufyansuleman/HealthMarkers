#' BODE Index (BMI, Obstruction, Dyspnea, Exercise capacity)
#'
#' Computes the BODE index (0-10) using FEV1 % predicted, 6-minute walk distance (6MWD),
#' mMRC dyspnea scale, and BMI. Higher scores indicate worse prognosis in COPD.
#'
#' @details
#' Scoring components:
#' FEV1 % predicted: >=65 = 0; 50-64 = 1; 36-49 = 2; <=35 = 3
#' 6MWD (meters): >=350 = 0; 250-349 = 1; 150-249 = 2; <=149 = 3
#' mMRC dyspnea: 0-1 = 0; 2 = 1; 3 = 2; 4 = 3
#' BMI: >21 = 0; <=21 = 1
#'
#' @param data data.frame/tibble with required columns.
#' @param col_map named list with keys:
#'   fev1_pct OR (fev1 and fev1_pred) OR fev1_pp; plus sixmwd, mmrc, bmi.
#'   Example minimal: list(fev1_pct="FEV1pct", sixmwd="Walk_m", mmrc="mMRC", bmi="BMI")
#'   Example derive: list(fev1="FEV1", fev1_pred="FEV1_pred", sixmwd="Walk_m", mmrc="mMRC", bmi="BMI")
#'   Example from spirometry_markers: list(fev1_pp="fev1_pp", sixmwd="Walk_m", mmrc="mMRC", bmi="BMI")
#' @param na_action one of c("keep","omit","error","ignore","warn").
#' @param check_extreme logical; if TRUE scan for extreme values.
#' @param extreme_action one of c("warn","cap","error","ignore","NA").
#' @param extreme_rules named list of bounds (c(lo,hi)) for fev1_pct,sixmwd,mmrc,bmi.
#'                      Defaults: fev1_pct c(10,140), sixmwd c(50,800), mmrc c(0,4), bmi c(10,60).
#' @param verbose logical; TRUE emits messages.
#' @return tibble with bode_index (integer). NA if any required input missing (unless omitted).
#' @references Celli BR et al. (2004). The BODE index in COPD. N Engl J Med. 350:1005-1012.
#' @export
bode_index <- function(
  data,
  col_map = list(fev1_pct = "FEV1pct", sixmwd = "Walk_m", mmrc = "mMRC", bmi = "BMI",
                 fev1 = NULL, fev1_pred = NULL, fev1_pp = NULL),
  na_action = c("keep","omit","error","ignore","warn"),
  check_extreme = FALSE,
  extreme_action = c("warn","cap","error","ignore","NA"),
  extreme_rules = NULL,
  verbose = FALSE
) {
  na_action_raw <- match.arg(na_action)
  na_action_eff <- if (na_action_raw %in% c("ignore","warn")) "keep" else na_action_raw
  extreme_action <- match.arg(extreme_action)

  # Validate data
  if (!is.data.frame(data)) {
    rlang::abort("bode_index(): `data` must be a data.frame or tibble.",
                 class = "healthmarkers_bode_error_data_type")
  }
  if (!is.list(col_map) || is.null(names(col_map))) {
    rlang::abort("bode_index(): `col_map` must be a named list.",
                 class = "healthmarkers_bode_error_colmap_type")
  }

  # Mandatory non-FEV1 keys
  base_keys <- c("sixmwd","mmrc","bmi")
  missing_base <- setdiff(base_keys, names(col_map))
  if (length(missing_base)) {
    rlang::abort(paste0("bode_index(): missing col_map entries for: ",
                        paste(missing_base, collapse = ", ")),
                 class = "healthmarkers_bode_error_missing_map")
  }

  # Resolve FEV1 percent predicted source
  has_pct <- !is.null(col_map$fev1_pct) && nzchar(col_map$fev1_pct)
  has_raw <- !is.null(col_map$fev1) && nzchar(col_map$fev1)
  has_pred <- !is.null(col_map$fev1_pred) && nzchar(col_map$fev1_pred)
  has_pp <- !is.null(col_map$fev1_pp) && nzchar(col_map$fev1_pp)

  fev1_source <- if (has_pct) "pct" else if (has_raw && has_pred) "derive" else if (has_pp) "pp" else "none"
  if (fev1_source == "none") {
    rlang::abort("bode_index(): must supply either fev1_pct OR (fev1 & fev1_pred) OR fev1_pp.",
                 class = "healthmarkers_bode_error_missing_fev1_source")
  }

  # Collect column names to check exist
  cols_needed <- c(
    if (fev1_source == "pct") col_map$fev1_pct,
    if (fev1_source == "derive") c(col_map$fev1, col_map$fev1_pred),
    if (fev1_source == "pp") col_map$fev1_pp,
    col_map$sixmwd,
    col_map$mmrc,
    col_map$bmi
  )
  cols_needed <- cols_needed[nzchar(cols_needed)]
  miss_cols <- setdiff(cols_needed, names(data))
  if (length(miss_cols)) {
    rlang::abort(paste0("bode_index(): missing required columns in data: ",
                        paste(miss_cols, collapse = ", ")),
                 class = "healthmarkers_bode_error_missing_columns")
  }

  if (isTRUE(verbose)) rlang::inform("-> bode_index: preparing inputs")
  else hm_inform("bode_index(): preparing inputs", level = "debug")

  # Coercion helper
  coerce_col <- function(cn) {
    if (is.null(cn) || !nzchar(cn)) return(NULL)
    if (!is.numeric(data[[cn]])) {
      old <- data[[cn]]
      suppressWarnings(new <- as.numeric(old))
      intro <- sum(is.na(new) & !is.na(old))
      if (intro > 0) {
        rlang::warn(sprintf("Column '%s' coerced to numeric; NAs introduced: %d", cn, intro),
                    class = "healthmarkers_bode_warn_na_coercion")
      }
      data[[cn]] <- new
    }
    data[[cn]][!is.finite(data[[cn]])] <- NA_real_
    data[[cn]]
  }

  fev1_pct <- switch(
    fev1_source,
    pct = coerce_col(col_map$fev1_pct),
    derive = {
      raw <- coerce_col(col_map$fev1)
      pred <- coerce_col(col_map$fev1_pred)
      out <- 100 * raw / pred
      out[!is.finite(out)] <- NA_real_
      out
    },
    pp = {
      pp <- coerce_col(col_map$fev1_pp)
      pp[!is.finite(pp)] <- NA_real_
      pp
    }
  )

  sixmwd <- coerce_col(col_map$sixmwd)
  mmrc   <- coerce_col(col_map$mmrc)
  bmi    <- coerce_col(col_map$bmi)

  row_na <- is.na(fev1_pct) | is.na(sixmwd) | is.na(mmrc) | is.na(bmi)
  if (na_action_raw == "warn" && any(row_na)) {
    rlang::warn("Missing BODE components; bode_index will be NA for those rows.",
                class = "healthmarkers_bode_warn_missing_inputs")
  }
  if (na_action_eff == "error" && any(row_na)) {
    rlang::abort("bode_index(): required inputs contain missing values (na_action='error').",
                 class = "healthmarkers_bode_error_missing_values")
  }
  keep <- if (na_action_eff == "omit") !row_na else rep(TRUE, length(fev1_pct))

  d_fev1 <- fev1_pct[keep]
  d_walk <- sixmwd[keep]
  d_mmrc <- mmrc[keep]
  d_bmi  <- bmi[keep]

  # Domain warnings (skip if check_extreme to reduce noise)
  if (!isTRUE(check_extreme)) {
    if (any(is.finite(d_mmrc) & (d_mmrc < 0 | d_mmrc > 4))) {
      rlang::warn("bode_index(): mMRC values outside 0-4 detected.",
                  class = "healthmarkers_bode_warn_mmrc_range")
    }
    if (any(is.finite(d_fev1) & (d_fev1 < 0 | d_fev1 > 150))) {
      rlang::warn("bode_index(): FEV1 % predicted values outside 0-150 detected.",
                  class = "healthmarkers_bode_warn_fev1pct_range")
    }
    if (any(is.finite(d_walk) & (d_walk < 0 | d_walk > 1500))) {
      rlang::warn("bode_index(): 6MWD values outside plausible range detected.",
                  class = "healthmarkers_bode_warn_sixmwd_range")
    }
    if (any(is.finite(d_bmi) & (d_bmi < 10 | d_bmi > 80))) {
      rlang::warn("bode_index(): BMI values outside plausible range detected.",
                  class = "healthmarkers_bode_warn_bmi_range")
    }
  }

  # Extreme scan
  if (isTRUE(check_extreme)) {
    rules_def <- list(fev1_pct = c(10,140), sixmwd = c(50,800), mmrc = c(0,4), bmi = c(10,60))
    if (is.list(extreme_rules)) {
      for (nm in intersect(names(extreme_rules), names(rules_def))) {
        rules_def[[nm]] <- extreme_rules[[nm]]
      }
    }
    total <- 0L
    cap_or_na <- function(x, bounds) {
      bad <- is.finite(x) & (x < bounds[1] | x > bounds[2])
      nb <- sum(bad)
      if (nb > 0) {
        if (extreme_action == "cap") {
          x[bad & x < bounds[1]] <- bounds[1]
          x[bad & x > bounds[2]] <- bounds[2]
        } else if (extreme_action == "NA") {
          x[bad] <- NA_real_
        }
      }
      list(x = x, n_bad = nb)
    }
    r1 <- cap_or_na(d_fev1, rules_def$fev1_pct); d_fev1 <- r1$x; total <- total + r1$n_bad
    r2 <- cap_or_na(d_walk, rules_def$sixmwd);    d_walk <- r2$x; total <- total + r2$n_bad
    r3 <- cap_or_na(d_mmrc, rules_def$mmrc);      d_mmrc <- r3$x; total <- total + r3$n_bad
    r4 <- cap_or_na(d_bmi,  rules_def$bmi);       d_bmi  <- r4$x; total <- total + r4$n_bad
    if (total > 0L) {
      if (extreme_action == "error") {
        rlang::abort(sprintf("bode_index(): %d extreme input values detected.", total),
                     class = "healthmarkers_bode_error_extremes")
      } else if (extreme_action == "warn") {
        rlang::warn(sprintf("bode_index(): detected %d extreme input values (not altered).", total),
                    class = "healthmarkers_bode_warn_extremes_detected")
      } else if (extreme_action == "cap") {
        rlang::warn(sprintf("bode_index(): capped %d extreme input values into allowed ranges.", total),
                    class = "healthmarkers_bode_warn_extremes_capped")
      }
    }
  }

  if (isTRUE(verbose)) rlang::inform("-> bode_index: computing score")
  else hm_inform("bode_index(): computing score", level = "debug")

  fev1_score <- ifelse(is.na(d_fev1), NA_integer_,
                       ifelse(d_fev1 >= 65, 0L,
                       ifelse(d_fev1 >= 50, 1L,
                       ifelse(d_fev1 >= 36, 2L, 3L))))
  walk_score <- ifelse(is.na(d_walk), NA_integer_,
                       ifelse(d_walk >= 350, 0L,
                       ifelse(d_walk >= 250, 1L,
                       ifelse(d_walk >= 150, 2L, 3L))))
  mmrc_score <- ifelse(is.na(d_mmrc), NA_integer_,
                       ifelse(d_mmrc <= 1, 0L,
                       ifelse(d_mmrc == 2, 1L,
                       ifelse(d_mmrc == 3, 2L, 3L))))
  bmi_score  <- ifelse(is.na(d_bmi), NA_integer_,
                       ifelse(d_bmi > 21, 0L, 1L))

  total_score <- fev1_score + walk_score + mmrc_score + bmi_score
  any_comp_na <- is.na(fev1_score) | is.na(walk_score) | is.na(mmrc_score) | is.na(bmi_score)
  total_score[any_comp_na] <- ifelse(na_action_eff == "omit", total_score[any_comp_na], NA_integer_)

  out <- tibble::tibble(
    bode_index = total_score,
    fev1_pct = d_fev1,
    fev1_score = fev1_score,
    walk_score = walk_score,
    mmrc_score = mmrc_score,
    bmi_score  = bmi_score
  )

  if (na_action_eff != "omit") {
    res <- tibble::tibble(
      bode_index = rep(NA_integer_, length(fev1_pct)),
      fev1_pct = rep(NA_real_, length(fev1_pct)),
      fev1_score = NA_integer_,
      walk_score = NA_integer_,
      mmrc_score = NA_integer_,
      bmi_score  = NA_integer_
    )
    res[keep, ] <- out
    out <- res
  }

  if (isTRUE(verbose)) rlang::inform(sprintf("Completed bode_index: %d rows.", nrow(out)))
  else hm_inform(sprintf("bode_index(): completed (%d rows)", nrow(out)), level = "debug")

  out
}
