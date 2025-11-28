#' Compute atherogenic indices
#'
#' Calculates:
#' - AIP: Atherogenic Index of Plasma = log10(TG / HDL_c)
#' - CRI_I: Castelli Risk Index I = TC / HDL_c
#' - CRI_II: Castelli Risk Index II = LDL_c / HDL_c
#'
#' Behavior:
#' - Required keys: TG, HDL_c. Optional: TC, LDL_c.
#' - NA policy via `na_action`: "keep" (default), "omit" (drop rows with any NA in used lipids), "error".
#' - Extreme screening via `check_extreme` and `extreme_action` ("warn","cap","error","ignore","NA").
#'   Default bounds (mg/dL) used only for screening: TG [0, 10000], HDL_c [0, 1000], LDL_c [0, 10000], TC [0, 10000].
#'   Note: All indices are unitless ratios; units cancel in computations.
#' - Emits progress via `hm_inform()` when `verbose = TRUE` or when package option enables logs.
#'
#' @param data data.frame/tibble with lipid columns.
#' @param col_map named list mapping keys to columns, e.g. list(TG="TG", HDL_c="HDL_c", TC="TC", LDL_c="LDL_c").
#' @param na_action one of c("keep","omit","error").
#' @param check_extreme logical; if TRUE, screen inputs for extremes using `extreme_rules`.
#' @param extreme_action one of c("warn","cap","error","ignore","NA").
#' @param extreme_rules optional named list of bounds per key or column, each c(min, max).
#' @param normalize one of c("none","log10"). Reserved; AIP always uses log10(TG/HDL_c).
#' @param verbose logical; prints step messages via hm_inform when TRUE.
#'
#' @return tibble with columns AIP, CRI_I, CRI_II
#'
#' @references
#' Dobiasova M (2004). Atherogenic Index of Plasma (AIP) [log(TG/HDL-C)]: theoretical and practical implications. Clin Chem 50(7):1113–1115. \doi{10.1373/clinchem.2004.035220}
#'
#' Castelli WP, et al. (1977). HDL cholesterol and other lipids in coronary heart disease: The Framingham Study. Am J Med 62(5):707–714. \doi{10.1016/0002-9343(77)90874-9}
#'
#' @examples
#' df <- tibble::tibble(
#'   TG = c(150, 200),
#'   HDL_c = c(50, 40),
#'   TC = c(200, 220),
#'   LDL_c = c(120, 150)
#' )
#' cm <- list(TG = "TG", HDL_c = "HDL_c", TC = "TC", LDL_c = "LDL_c")
#' atherogenic_indices(df, col_map = cm, verbose = FALSE)
#'
#' @export
atherogenic_indices <- function(data,
                                col_map,
                                na_action = c("keep","omit","error"),
                                check_extreme = FALSE,
                                extreme_action = c("warn","cap","error","ignore","NA"),
                                extreme_rules = NULL,
                                normalize = c("none","log10"),
                                verbose = FALSE) {
  na_action <- match.arg(na_action)

  # Validate choices with clearer messages than base match.arg errors
  allowed_ext <- c("warn","cap","error","ignore","NA")
  if (length(extreme_action) == 0L) {
    rlang::abort("`extreme_action` must be one of: warn, cap, error, ignore, NA",
                 class = "healthmarkers_atherogenic_indices_error_extreme_action")
  }
  extreme_action <- extreme_action[1L]
  if (!(extreme_action %in% allowed_ext)) {
    rlang::abort("`extreme_action` must be one of: warn, cap, error, ignore, NA",
                 class = "healthmarkers_atherogenic_indices_error_extreme_action")
  }

  allowed_norm <- c("none","log10")
  if (length(normalize) == 0L) {
    rlang::abort("`normalize` must be one of: 'none', 'log10'",
                 class = "healthmarkers_atherogenic_indices_error_normalize")
  }
  normalize <- normalize[1L]
  if (!(normalize %in% allowed_norm)) {
    rlang::abort("`normalize` must be one of: 'none', 'log10'",
                 class = "healthmarkers_atherogenic_indices_error_normalize")
  }

  if (!is.data.frame(data)) {
    rlang::abort("atherogenic_indices(): `data` must be a data.frame or tibble.",
                 class = "healthmarkers_atherogenic_indices_error_data_type")
  }
  if (!is.list(col_map) || is.null(names(col_map)) || any(names(col_map) == "")) {
    rlang::abort("atherogenic_indices(): `col_map` must be a named list of required keys -> column names.",
                 class = "healthmarkers_atherogenic_indices_error_colmap_type")
  }

  req <- c("TG", "HDL_c")
  opt <- c("TC", "LDL_c")

  # Explicit required key presence in col_map (replace hm_validate_inputs)
  missing_keys <- setdiff(req, names(col_map))
  if (length(missing_keys)) {
    rlang::abort(
      paste0("atherogenic_indices(): missing col_map entries for: ", paste(missing_keys, collapse = ", ")),
      class = "healthmarkers_atherogenic_indices_error_missing_map"
    )
  }
  if (isTRUE(verbose)) hm_inform(level = "inform", msg = "-> atherogenic_indices: validating inputs")

  # Required columns presence
  req_cols <- unname(unlist(col_map[req], use.names = FALSE))
  missing_req <- setdiff(req_cols, names(data))
  if (length(missing_req)) {
    rlang::abort(
      paste0("atherogenic_indices(): missing required columns in data: ", paste(missing_req, collapse = ", ")),
      class = "healthmarkers_atherogenic_indices_error_missing_columns"
    )
  }

  # Optional columns present in data
  opt_cols <- unname(unlist(col_map[intersect(opt, names(col_map))], use.names = FALSE))
  opt_cols <- intersect(opt_cols, names(data))

  # Coerce used columns to numeric; NAs introduced are warned
  used_cols <- unique(c(req_cols, opt_cols))
  for (cn in used_cols) {
    if (!is.numeric(data[[cn]])) {
      old <- data[[cn]]
      suppressWarnings(data[[cn]] <- as.numeric(old))
      introduced <- sum(is.na(data[[cn]]) & !is.na(old))
      if (introduced > 0) {
        rlang::warn(sprintf("atherogenic_indices(): column '%s' coerced to numeric; NAs introduced: %d", cn, introduced),
                    class = "healthmarkers_atherogenic_indices_warn_na_coercion")
      }
    }
    data[[cn]][!is.finite(data[[cn]])] <- NA_real_
  }

  # NA handling across all used lipids
  if (na_action == "error") {
    has_na <- Reduce(`|`, lapply(used_cols, function(cn) is.na(data[[cn]])))
    if (any(has_na)) {
      rlang::abort("atherogenic_indices(): missing values in required inputs (na_action='error').",
                   class = "healthmarkers_atherogenic_indices_error_missing_values")
    }
  } else if (na_action == "omit") {
    keep <- !Reduce(`|`, lapply(used_cols, function(cn) is.na(data[[cn]])))
    if (isTRUE(verbose)) {
      hm_inform(level = "inform",
                msg = sprintf("-> atherogenic_indices: omitting %d rows with NA in lipid inputs", sum(!keep)))
    }
    data <- data[keep, , drop = FALSE]
  }

  # Early empty
  if (nrow(data) == 0L) {
    hm_inform("atherogenic_indices(): computed atherogenic indices", level = "inform")
    return(tibble::tibble(AIP = numeric(), CRI_I = numeric(), CRI_II = numeric()))
  }

  # Extreme screening
  if (isTRUE(check_extreme)) {
    default_rules <- list(
      TG = c(0, 10000),
      HDL_c = c(0, 1000),
      LDL_c = c(0, 10000),
      TC = c(0, 10000)
    )
    rules <- if (is.null(extreme_rules)) default_rules else extreme_rules

    # Robust key->column map (avoid setNames(NULL, ...))
    key_to_col <- list()
    for (i in seq_along(req)) key_to_col[[req[i]]] <- req_cols[i]
    opt_keys <- intersect(opt, names(col_map))
    if (length(opt_keys)) {
      for (k in opt_keys) {
        key_to_col[[k]] <- col_map[[k]]
      }
    }

    # Remap any rule names that match keys to actual column names
    if (!is.null(names(rules))) {
      remapped <- list()
      for (nm in names(rules)) {
        col_nm <- if (!is.null(key_to_col[[nm]])) key_to_col[[nm]] else nm
        remapped[[col_nm]] <- rules[[nm]]
      }
      rules <- remapped
    }

    total_ex <- 0L
    for (nm in intersect(names(rules), names(data))) {
      rng <- rules[[nm]]
      x <- data[[nm]]
      bad <- is.finite(x) & (x < rng[1] | x > rng[2])
      n_bad <- sum(bad, na.rm = TRUE)
      if (n_bad > 0) {
        total_ex <- total_ex + n_bad
        if (identical(extreme_action, "cap")) {
          x[bad & x < rng[1]] <- rng[1]
          x[bad & x > rng[2]] <- rng[2]
          data[[nm]] <- x
        } else if (identical(extreme_action, "NA")) {
          x[bad] <- NA_real_
          data[[nm]] <- x
        }
      }
    }
    if (total_ex > 0) {
      if (identical(extreme_action, "error")) {
        rlang::abort("atherogenic_indices(): values out of range.",
                     class = "healthmarkers_atherogenic_indices_error_extremes")
      } else if (identical(extreme_action, "warn")) {
        rlang::warn(sprintf("atherogenic_indices(): detected %d extreme input values (not altered).", total_ex),
                    class = "healthmarkers_atherogenic_indices_warn_extremes")
      }
      # cap/NA are silent; 'ignore' makes no changes and no warning
    }
  }

  if (isTRUE(verbose)) hm_inform(level = "inform", msg = "-> atherogenic_indices: computing indices")

  # Accessor and safe division
  g <- function(key) data[[col_map[[key]]]]
  dz_env <- new.env(parent = emptyenv()); dz_env$counts <- list()
  safe_div <- function(num, den, label) {
    out <- num / den
    zero_den <- is.finite(den) & den == 0
    dz_env$counts[[label]] <- sum(zero_den, na.rm = TRUE)
    out[!is.finite(out)] <- NA_real_
    out
  }

  # Compute indices
  AIP   <- log10(safe_div(g("TG"), g("HDL_c"), "AIP_denHDL"))
  CRI_I <- if ("TC" %in% names(col_map) && col_map[["TC"]] %in% names(data)) {
    safe_div(g("TC"), g("HDL_c"), "CRI_I_denHDL")
  } else rep(NA_real_, nrow(data))
  CRI_II <- if ("LDL_c" %in% names(col_map) && col_map[["LDL_c"]] %in% names(data)) {
    safe_div(g("LDL_c"), g("HDL_c"), "CRI_II_denHDL")
  } else rep(NA_real_, nrow(data))

  out <- tibble::tibble(
    AIP = as.numeric(AIP),
    CRI_I = as.numeric(CRI_I),
    CRI_II = as.numeric(CRI_II)
  )

  # Zero denominators warning
  dz <- dz_env$counts
  if (length(dz)) {
    dz_total <- sum(unlist(dz), na.rm = TRUE)
    if (dz_total > 0L) {
      nz <- unlist(dz); nz <- nz[nz > 0]
      lbl <- paste(sprintf("%s=%d", names(nz), nz), collapse = ", ")
      rlang::warn(sprintf("atherogenic_indices(): zero denominators detected in %d cases (%s).", dz_total, lbl),
                  class = "healthmarkers_atherogenic_indices_warn_zero_denominator")
    }
  }

  hm_inform("atherogenic_indices(): computed atherogenic indices", level = "inform")

  out
}