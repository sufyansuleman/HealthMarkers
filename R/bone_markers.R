# File: R/bone_markers.R

#' Compute Bone Health & Body-Composition Markers (HM-CS v3)
#'
#' Given DXA, anthropometry, and optional bone-turnover markers, computes:
#' - OSTA: (weight - age) x 0.2
#' - ALMI: Appendicular Lean Mass Index = ALM / height^2
#' - FMI: Fat Mass Index = FM / height^2
#' - BMD_Tscore: (BMD - ref_mean) / ref_sd
#' and (if in `col_map` + data) passes through: TBS, HSA, PINP, CTX, BSAP, Osteocalcin.
#'
#' Notes:
#' - Units: height in meters; ALM, FM, weight in kilograms; BMD in g/cm^2; ALMI/FMI in kg/m^2.
#' - Non-finite values are treated as NA; division by zero is prevented by input checks.
#'
#' @param data A `data.frame` or tibble with subject-level DXA/anthropometry data.
#' @param col_map Named list mapping keys to column names. Required keys:
#'   - `age`, `weight`, `height`, `ALM`, `FM`, `BMD`, `BMD_ref_mean`, `BMD_ref_sd`
#'   Optional (passed-through if present and found in data): `TBS`, `HSA`, `PINP`, `CTX`, `BSAP`, `Osteocalcin`.
#' @param na_action One of "keep", "omit", or "error" controlling how
#'   missing/non-finite input values are treated.
#' @param check_extreme Logical; if `TRUE`, scan mapped columns whose key
#'   names contain "sds" (case-insensitive) for absolute values > `sds_limit`.
#' @param sds_limit Positive numeric; SDS magnitude limit used when `check_extreme` is TRUE.
#' @param extreme_action One of "cap", "NA", or "error" for handling extreme SDS-like values.
#'
#' @return A tibble with columns: `OSTA`, `ALMI`, `FMI`, `BMD_Tscore`, and
#'   optionally `TBS`, `HSA`, `PINP`, `CTX`, `BSAP`, `Osteocalcin` (in that order).
#'
#' @examples
#' library(tibble)
#' df <- tibble(
#'   age = c(60, 72), weight = c(65, 50), height = c(1.65, 1.58),
#'   ALM = c(18.2, 14.7), FM = c(22.0, 20.5),
#'   BMD = c(0.95, 0.80), BMD_ref_mean = c(1.00, 1.00), BMD_ref_sd = c(0.12, 0.12)
#' )
#' col_map <- list(
#'   age = "age", weight = "weight", height = "height",
#'   ALM = "ALM", FM = "FM", BMD = "BMD",
#'   BMD_ref_mean = "BMD_ref_mean", BMD_ref_sd = "BMD_ref_sd"
#' )
#' bone_markers(df, col_map)
#'
#' @references
#' Woo J, Leung J, Lau E, et al. (2002). The Osteoporosis Self-Assessment Tool (OSTA): a simple screening tool for osteoporosis. Maturitas, 41(2):227-232.
#' Kelly TL, Wilson KE, Heymsfield SB (2009). Dual energy X-ray absorptiometry body composition reference values from NHANES. Int J Obes, 33(6):783-789.
#' World Health Organization (1994). Assessment of fracture risk and its application to screening for postmenopausal osteoporosis. WHO Tech Rep Ser, 843:1-129.
#'
#' @importFrom tibble tibble
#' @export
bone_markers <- function(
  data,
  col_map,
  na_action = c("keep", "omit", "error"),
  check_extreme = FALSE,
  sds_limit = 6,
  extreme_action = c("cap", "NA", "error")
) {
  na_action <- match.arg(na_action)
  extreme_action <- match.arg(extreme_action)

  # Validate mapping object
  if (!is.list(col_map) || is.null(names(col_map))) {
    rlang::abort("bone_markers(): `col_map` must be a named list mapping keys to column names.",
                 class = "healthmarkers_bone_error_colmap_type")
  }
  required <- c("age", "weight", "height", "ALM", "FM", "BMD", "BMD_ref_mean", "BMD_ref_sd")
  optional <- c("TBS", "HSA", "PINP", "CTX", "BSAP", "Osteocalcin")

  # Required keys present in col_map
  missing_map <- setdiff(required, names(col_map))
  if (length(missing_map)) {
    rlang::abort(
      paste0("bone_markers(): missing col_map entries for: ", paste(missing_map, collapse = ", ")),
      class = "healthmarkers_bone_error_colmap_missing"
    )
  }

  # Required columns exist in data
  req_cols <- unname(unlist(col_map[required], use.names = FALSE))
  missing_cols <- setdiff(req_cols, names(data))
  if (length(missing_cols)) {
    rlang::abort(
      paste0("bone_markers(): missing required columns in data: ", paste(missing_cols, collapse = ", ")),
      class = "healthmarkers_bone_error_missing_columns"
    )
  }

  hm_inform("bone_markers(): computing bone markers", level = "inform")

  # Coerce required and present optional columns to numeric; warn if NAs introduced
  present_opt_cols <- intersect(unname(unlist(col_map[intersect(optional, names(col_map))], use.names = FALSE)), names(data))
  for (cn in unique(c(req_cols, present_opt_cols))) {
    if (!is.numeric(data[[cn]])) {
      old <- data[[cn]]
      suppressWarnings(data[[cn]] <- as.numeric(old))
      introduced <- sum(is.na(data[[cn]]) & !is.na(old))
      if (introduced > 0) {
        rlang::warn(
          sprintf("bone_markers(): column '%s' coerced to numeric; NAs introduced: %d", cn, introduced),
          class = "healthmarkers_bone_warn_na_coercion"
        )
      }
    }
    data[[cn]][!is.finite(data[[cn]])] <- NA_real_
  }

  n <- nrow(data)
  # extract required
  age <- data[[col_map$age]]
  weight <- data[[col_map$weight]]
  height <- data[[col_map$height]]
  ALM <- data[[col_map$ALM]]
  FM <- data[[col_map$FM]]
  BMD <- data[[col_map$BMD]]
  ref_mean <- data[[col_map$BMD_ref_mean]]
  ref_sd <- data[[col_map$BMD_ref_sd]]

  # Constraints
  if (any(ref_sd <= 0, na.rm = TRUE)) {
    rlang::abort("bone_markers(): 'BMD_ref_sd' must be positive for non-missing rows.",
                 class = "healthmarkers_bone_error_refsd_positive")
  }
  if (any(height <= 0, na.rm = TRUE)) {
    rlang::abort("bone_markers(): 'height' must be positive for non-missing rows.",
                 class = "healthmarkers_bone_error_height_positive")
  }

  # NA row policy across required variables (NA or non-finite)
  req_df <- data[, unname(unlist(col_map[required])), drop = FALSE]
  rows_with_na <- !stats::complete.cases(req_df)
  if (na_action == "error" && any(rows_with_na)) {
    rlang::abort("bone_markers(): missing/non-finite values present in required inputs (na_action='error').",
                 class = "healthmarkers_bone_error_missing_values")
  } else if (na_action == "omit" && any(rows_with_na)) {
    keep <- !rows_with_na
    data <- data[keep, , drop = FALSE]
    age <- age[keep]; weight <- weight[keep]; height <- height[keep]
    ALM <- ALM[keep]; FM <- FM[keep]; BMD <- BMD[keep]
    ref_mean <- ref_mean[keep]; ref_sd <- ref_sd[keep]
    n <- nrow(data)
  }

  # Optional extreme SDS-like check for mapped keys containing 'sds'
  if (isTRUE(check_extreme)) {
    sds_keys <- names(col_map)[grepl("sds", names(col_map), ignore.case = TRUE)]
    total_adj <- 0L
    for (k in sds_keys) {
      cn <- col_map[[k]]
      if (!is.null(cn) && cn %in% names(data)) {
        x <- data[[cn]]
        if (!is.numeric(x)) suppressWarnings(x <- as.numeric(x))
        bad <- is.finite(x) & abs(x) > sds_limit
        nbad <- sum(bad, na.rm = TRUE)
        if (nbad > 0) {
          if (extreme_action == "error") {
            rlang::abort(
              sprintf("bone_markers(): extreme SDS-like values detected: %s(%d values > |%g|).",
                      k, nbad, sds_limit),
              class = "healthmarkers_bone_error_extreme_sds"
            )
          } else if (extreme_action == "NA") {
            x[bad] <- NA_real_; data[[cn]] <- x; total_adj <- total_adj + nbad
          } else if (extreme_action == "cap") {
            x[bad & x > 0] <-  sds_limit
            x[bad & x < 0] <- -sds_limit
            data[[cn]] <- x; total_adj <- total_adj + nbad
          }
        }
      }
    }
    if (total_adj > 0 && extreme_action %in% c("cap","NA")) {
      hm_inform(sprintf("bone_markers(): adjusted %d SDS-like extreme values (%s).",
                        total_adj, extreme_action), level = "inform")
    }
  }

  # compute core indices
  OSTA <- (weight - age) * 0.2
  ALMI <- ALM / (height^2)
  FMI <- FM / (height^2)
  BMD_Tscore <- (BMD - ref_mean) / ref_sd

  # helper for optional pass-through
  get_opt <- function(key) {
    if (key %in% names(col_map) && col_map[[key]] %in% names(data)) {
      as.numeric(data[[ col_map[[key]] ]])
    } else {
      rep(NA_real_, n)
    }
  }
  TBS <- get_opt("TBS")
  HSA <- get_opt("HSA")
  PINP <- get_opt("PINP")
  CTX <- get_opt("CTX")
  BSAP <- get_opt("BSAP")
  Osteocalcin <- get_opt("Osteocalcin")

  result <- tibble::tibble(
    OSTA = as.numeric(OSTA),
    ALMI = as.numeric(ALMI),
    FMI = as.numeric(FMI),
    BMD_Tscore = as.numeric(BMD_Tscore),
    TBS = TBS,
    HSA = HSA,
    PINP = PINP,
    CTX = CTX,
    BSAP = BSAP,
    Osteocalcin = Osteocalcin
  )

  hm_inform("bone_markers(): completed", level = "inform")
  result
}

# ---- internal helpers (not exported) ----
.bm_validate_misc_args <- function(verbose, na_warn_prop, check_extreme_sds, sds_limit) {
  invisible(TRUE)
}
