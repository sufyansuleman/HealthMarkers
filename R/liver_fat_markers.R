#' Compute Hepatic Steatosis Index (HSI) and NAFLD‑Liver Fat Score (LFS)
#'
#' This function calculates two non‑invasive liver fat scores. The
#' Hepatic Steatosis Index (HSI) is defined as 8 × (ALT/AST) + BMI plus a
#' sex and diabetes term (+2 if female; +2 if type 2 diabetes). It has been
#' validated as a simple screening tool for hepatic steatosis. The
#' NAFLD‑Liver Fat Score (LFS) combines metabolic syndrome status,
#' diabetes, fasting insulin, AST and the AST/ALT ratio to estimate liver fat
#' accumulation. Both indices complement the existing fatty
#' liver and fibrosis scores.
#'
#' @param data Data frame or tibble containing the variables listed above.
#' @param col_map Named list mapping required variable names to columns in
#'   `data`. See Required variables. Unused entries are ignored.
#' @param verbose Logical; if `TRUE` prints diagnostic messages.
#' @param na_action How to handle missing values in required inputs. One of
#'   `"keep"` (default), `"omit"` or `"error"`.
#' @param na_warn_prop Proportion (0–1) above which a warning is issued
#'   about missingness in any required column. Default `0.2`.
#' @param check_extreme Logical; if `TRUE` scan for implausible AST, ALT,
#'   BMI and insulin values. See `extreme_rules` and `extreme_action`.
#' @param extreme_action One of `"warn"`, `"cap"`, `"error"` or
#'   `"ignore"`. When `"cap"`, out‑of‑range values are truncated to
#'   plausible limits.
#' @param extreme_rules Optional named list specifying plausible ranges for
#'   `ALT`, `AST`, `BMI`, `insulin`. Each element is a length‑2 numeric
#'   vector `c(min, max)`. Default ranges are ALT 5–200 U/L, AST 5–200 U/L,
#'   BMI 15–60 kg/m², and insulin 1–200 mU/L.
#'
#' @return A tibble with two columns: `HSI` and `NAFLD_LFS`. `NA` is
#'   returned when required inputs are missing or invalid.
#'
#' @examples
#' df <- data.frame(ALT = c(30, 40), AST = c(20, 35), BMI = c(25, 32),
#'                  sex = c(1, 2), diabetes = c(0, 1), MetS = c(1, 0),
#'                  insulin = c(15, 20))
#' liver_fat_markers(df, col_map = list(ALT = "ALT", AST = "AST", BMI = "BMI",
#'                                     sex = "sex", diabetes = "diabetes",
#'                                     MetS = "MetS", insulin = "insulin"))
#'
#' @references
#' Lee JH, Kim D, Kim HJ, et al. Hepatic steatosis index: a simple screening
#' tool reflecting nonalcoholic fatty liver disease. Dig Liver Dis.
#' 2010;42(7):503–508. \doi{10.1016/j.dld.2009.08.002}
#' Bedogni G, Bellentani S, Miglioli L, et al. The fatty liver index: a simple
#' and accurate predictor of hepatic steatosis in the general population.
#' BMC Gastroenterol. 2006;6:33. \doi{10.1186/1471-230X-6-33}
#' Kotronen A, Peltonen M, Hakkarainen A, et al. Prediction of non-alcoholic
#' fatty liver disease and liver fat using metabolic and genetic factors.
#' Gastroenterology. 2009;137(3):865–872. \doi{10.1053/j.gastro.2009.06.005}
#' Karczewski J, Grzebyk M, Pokorski J. Liver fat scores for noninvasive
#' diagnosis of nonalcoholic fatty liver disease: A clinical review. Rev Med
#' Chil. 2023;151(2):256–266. \doi{10.1007/s11739-023-03282-9}
#' @importFrom tibble tibble
#' @importFrom rlang abort warn inform
#' @export
liver_fat_markers <- function(data, col_map, verbose = FALSE,
                              na_action = c("keep", "omit", "error"),
                              na_warn_prop = 0.2,
                              check_extreme = FALSE,
                              extreme_action = c("warn", "cap", "error", "ignore"),
                              extreme_rules = NULL) {
  na_action <- match.arg(na_action)
  extreme_action <- match.arg(extreme_action)

  # required variables
  req_vars <- c("ALT", "AST", "BMI", "sex", "diabetes", "MetS", "insulin")

  # Validate col_map (no external helper)
  if (!is.list(col_map) || is.null(names(col_map))) {
    rlang::abort("liver_fat_markers(): 'col_map' must be a named list.")
  }
  missing_names <- setdiff(req_vars, names(col_map))
  if (length(missing_names)) {
    rlang::abort(sprintf(
      "liver_fat_markers(): 'col_map' missing required names: %s",
      paste(missing_names, collapse = ", ")
    ))
  }
  mapped_cols <- vapply(req_vars, function(nm) as.character(col_map[[nm]]), character(1))
  if (anyNA(mapped_cols) || any(!nzchar(mapped_cols))) {
    rlang::abort("liver_fat_markers(): 'col_map' contains empty or NA column names.")
  }
  if (anyDuplicated(mapped_cols)) {
    rlang::abort("liver_fat_markers(): 'col_map' has duplicated target columns.")
  }
  missing_in_data <- setdiff(mapped_cols, names(data))
  if (length(missing_in_data)) {
    rlang::abort(sprintf(
      "liver_fat_markers(): columns not found in 'data': %s",
      paste(missing_in_data, collapse = ", ")
    ))
  }

  # Subset and standardize names
  df <- data[, mapped_cols, drop = FALSE]
  names(df) <- req_vars

  # missingness warning
  if (na_warn_prop > 0) {
    for (cn in req_vars) {
      pna <- mean(is.na(df[[cn]]))
      if (pna >= na_warn_prop && pna > 0) {
        rlang::warn(sprintf("liver_fat_markers(): column '%s' has high missingness (%.1f%%).",
                            cn, 100 * pna))
      }
    }
  }

  # missing value handling
  if (na_action == "error") {
    if (any(vapply(df[req_vars], function(x) any(is.na(x)), logical(1)))) {
      rlang::abort("liver_fat_markers(): required inputs contain missing values (na_action='error').")
    }
  } else if (na_action == "omit") {
    keep <- Reduce(`&`, lapply(df[req_vars], function(x) !is.na(x)))
    if (isTRUE(verbose)) {
      rlang::inform(sprintf("liver_fat_markers(): omitting %d rows with missing required inputs",
                            sum(!keep)))
    }
    df <- df[keep, , drop = FALSE]
  }
  if (nrow(df) == 0L) {
    return(tibble::tibble(HSI = numeric(), NAFLD_LFS = numeric()))
  }

  # coerce numeric variables except sex and diabetes and MetS
  num_vars <- setdiff(req_vars, c("sex", "diabetes", "MetS"))
  for (cn in num_vars) {
    if (!is.numeric(df[[cn]])) {
      old <- df[[cn]]
      suppressWarnings(df[[cn]] <- as.numeric(old))
      introduced <- sum(is.na(df[[cn]]) & !is.na(old))
      if (introduced > 0) {
        rlang::warn(sprintf("liver_fat_markers(): column '%s' coerced to numeric; NAs introduced: %d",
                            cn, introduced))
      }
    }
  }

  # convert sex/diabetes/MetS to numeric indicators
  for (bn in c("sex", "diabetes", "MetS")) {
    if (is.character(df[[bn]]) || is.factor(df[[bn]])) {
      lv <- trimws(toupper(as.character(df[[bn]])))
      if (bn == "sex") {
        df[[bn]] <- ifelse(lv %in% c("F", "FEMALE", "2"), 2,
                           ifelse(lv %in% c("M", "MALE", "1"), 1, NA_real_))
      } else {
        df[[bn]] <- ifelse(lv %in% c("YES", "TRUE", "1", "T"), 1,
                           ifelse(lv %in% c("NO", "FALSE", "0", "F"), 0, NA_real_))
      }
    }
    if (!is.numeric(df[[bn]])) df[[bn]] <- as.numeric(df[[bn]])
  }

  # check extremes
  capped_n <- 0L
  if (isTRUE(check_extreme)) {
    if (is.null(extreme_rules)) {
      extreme_rules <- list(ALT = c(5, 200), AST = c(5, 200), BMI = c(15, 60), insulin = c(1, 200))
    }
    ex_counts <- integer(0)
    for (nm in names(extreme_rules)) {
      if (!nm %in% names(df)) next
      rng <- extreme_rules[[nm]]
      x <- df[[nm]]
      bad <- is.finite(x) & (x < rng[1] | x > rng[2])
      ex_counts[nm] <- sum(bad, na.rm = TRUE)
      if (extreme_action == "cap") {
        x[bad & x < rng[1] & is.finite(x)] <- rng[1]
        x[bad & x > rng[2] & is.finite(x)] <- rng[2]
        df[[nm]] <- x
      }
    }
    total_ex <- sum(ex_counts, na.rm = TRUE)
    if (total_ex > 0) {
      if (extreme_action == "error") {
        rlang::abort(sprintf("liver_fat_markers(): detected %d extreme input values.", total_ex))
      } else if (extreme_action == "cap") {
        capped_n <- total_ex
        rlang::warn(sprintf("liver_fat_markers(): capped %d extreme input values into allowed ranges.", total_ex))
      } else if (extreme_action == "warn") {
        rlang::warn(sprintf("liver_fat_markers(): detected %d extreme input values (not altered).", total_ex))
      }
    }
  }

  # safe division
  sdv <- function(a, b) {
    res <- a / b
    res[is.na(a) | is.na(b) | b == 0] <- NA_real_
    res
  }

  # compute HSI: 8 × (ALT/AST) + BMI + 2*I(sex == 2) + 2*I(diabetes == 1)
  alt_ast <- sdv(df$ALT, df$AST)
  HSI <- 8 * alt_ast + df$BMI + 2 * as.numeric(df$sex == 2) + 2 * as.numeric(df$diabetes == 1)

  # compute NAFLD-LFS: -2.89 + 1.18*MetS + 0.45*diabetes + 0.15*insulin + 0.04*AST – 0.94*(AST/ALT)
  ast_alt <- sdv(df$AST, df$ALT)
  NAFLD_LFS <- -2.89 + 1.18 * df$MetS + 0.45 * df$diabetes + 0.15 * df$insulin + 0.04 * df$AST - 0.94 * ast_alt

  out <- tibble::tibble(HSI = as.numeric(HSI), NAFLD_LFS = as.numeric(NAFLD_LFS))
  if (isTRUE(verbose)) {
    bad <- sum(is.na(out$HSI) | !is.finite(out$HSI)) + sum(is.na(out$NAFLD_LFS) | !is.finite(out$NAFLD_LFS))
    rlang::inform(sprintf(
      "liver_fat_markers(): computed for %d rows; NA/Inf in outputs: %d; capped extremes: %d",
      nrow(out), bad, capped_n
    ))
  }
  out
}