#’ @title   Compute all insulin‐sensitivity (and optionally resistance) indices
#’ @description
#’ `all_insulin_indices()` calls the four IS calculators
#’ (`fasting_is()`, `ogtt_is()`, `adipo_is()`, `tracer_dxa_is()`) and then
#’ optionally inverts them to IR measures.
#’
#’ @param data A data.frame or tibble of raw measurements.
#’ @param col_map Named list with keys G0,I0,G30,I30,G120,I120,TG,HDL_c,FFA,waist,weight,bmi,age,sex,rate_palmitate,rate_glycerol,fat_mass.
#’ @param normalize One of `c("none","z","inverse","range","robust")`.
#’ @param mode  One of `c("IS","IR","both")`.  `"IR"` returns only inverted IR, `"IS"` only the original IS, `"both"` returns both with IR_ prefix.
#’ @param verbose Logical.
#’ @return A tibble of IS (and/or IR_) columns.
#’ @export
all_insulin_indices <- function(data,
                                col_map,
                                normalize = c("none", "z", "inverse", "range", "robust"),
                                mode      = c("both", "IS", "IR"),
                                verbose   = TRUE) {
  normalize <- match.arg(normalize)
  mode      <- match.arg(mode)
  
  # 1) compute all four IS sets
  out <- data
  if (verbose)
    message("→ fasting")
  out_is1 <- fasting_is(out, col_map, normalize, verbose)
  if (verbose)
    message("→ OGTT")
  out_is2 <- ogtt_is(out, col_map, normalize, verbose)
  if (verbose)
    message("→ adipose")
  out_is3 <- adipo_is(out, col_map, normalize, verbose)
  if (verbose)
    message("→ tracer/DXA")
  out_is4 <- tracer_dxa_is(out, col_map, normalize, verbose)
  
  is_tbl <- dplyr::bind_cols(out_is1, out_is2, out_is3, out_is4)
  
  # 2) optionally invert to IR
  if (mode == "IS")
    return(is_tbl)
  
  # pick all numeric IS columns
  is_cols <- names(is_tbl)
  ir_tbl <- purrr::set_names(is_cols, function(col)
    paste0("IR_", col)) %>%
    purrr::imap_dfr(function(new_nm, old_nm)
      1 / is_tbl[[old_nm]])
  
  if (mode == "IR")
    return(ir_tbl)
  
  # both
  dplyr::bind_cols(is_tbl, ir_tbl)
}

#’ @title   Compute core metabolic markers (insulin + liver, lipid, cardio, etc.)
#’ @description
#’ `metabolic_markers()` builds on `all_insulin_indices()` and then adds:
#’  - adiposity_sds
#’  - cardio_advanced
#’  - lipid_markers
#’  - liver_markers
#’  - glycemic_markers
#’  - mets
#’
#’ @param data A data.frame or tibble of raw measurements.
#’ @param col_map As above, for insulin calculators.
#’ @param which  Character vector choosing subsets; default all:
#’   `c("insulin","adiposity_sds","cardio","lipid","liver","glycemic","mets")`.
#’ @param normalize One of `c("none","z","inverse","range","robust")`.
#’ @param mode      Passed to `all_insulin_indices()`.
#’ @param verbose Logical.
#’ @return A tibble: original `data` plus requested marker columns.
#’ @export
metabolic_markers <- function(data,
                              col_map,
                              which     = c("insulin",
                                            "adiposity_sds",
                                            "cardio",
                                            "lipid",
                                            "liver",
                                            "glycemic",
                                            "mets"),
                              normalize = c("none", "z", "inverse", "range", "robust"),
                              mode      = c("both", "IS", "IR"),
                              verbose   = TRUE) {
  normalize <- match.arg(normalize)
  mode      <- match.arg(mode)
  which     <- match.arg(which, several.ok = TRUE)
  
  out <- data
  
  if ("insulin" %in% which) {
    if (verbose)
      message("→ insulin indices")
    out <- dplyr::bind_cols(out,
                            all_insulin_indices(out, col_map, normalize, mode, verbose))
  }
  if ("adiposity_sds" %in% which) {
    if (verbose)
      message("→ adiposity SDS")
    out <- dplyr::bind_cols(out, adiposity_sds(out, verbose = verbose))
  }
  if ("cardio" %in% which) {
    if (verbose)
      message("→ cardio markers")
    out <- dplyr::bind_cols(out, cardio_advance(out))
  }
  if ("lipid" %in% which) {
    if (verbose)
      message("→ lipid markers")
    out <- dplyr::bind_cols(out, lipid_markers(out, verbose = verbose))
  }
  if ("liver" %in% which) {
    if (verbose)
      message("→ liver markers")
    out <- dplyr::bind_cols(out, liver_markers(out))
  }
  if ("glycemic" %in% which) {
    if (verbose)
      message("→ glycemic markers")
    out <- dplyr::bind_cols(out, glycemic_markers(out, verbose = verbose))
  }
  if ("mets" %in% which) {
    if (verbose)
      message("→ MetS severity score")
    out <- dplyr::bind_cols(out, metss(out, verbose = verbose))
  }
  
  out
}

#’ @title   Compute **all** HealthMarkers in one go
#’ @description
#’ Convenience wrapper that runs **every** calculator in the package:
#’ insulin sensitivity/resistance, anthropometry‐SDS, cardio, lipid,
#’ liver, glycemic, MetS, pulmonary, saliva, sweat, urine.
#’
#’ @param data A data.frame or tibble of raw measurements.
#’ @param col_map Passed to insulin calculators.
#’ @param normalize One of `c("none","z","inverse","range","robust")`.
#’ @param mode      One of `c("both","IS","IR")`.
#’ @param verbose Logical.
#’ @return A tibble: the original data plus every marker column.
#’ @export
all_health_markers <- function(data,
                               col_map,
                               normalize = c("none", "z", "inverse", "range", "robust"),
                               mode      = c("both", "IS", "IR"),
                               verbose   = TRUE) {
  normalize <- match.arg(normalize)
  mode      <- match.arg(mode)
  out <- metabolic_markers(
    data,
    col_map,
    which = c(
      "insulin",
      "adiposity_sds",
      "cardio",
      "lipid",
      "liver",
      "glycemic",
      "mets"
    ),
    normalize,
    mode,
    verbose
  )
  
  if (verbose)
    message("→ pulmonary markers")
  out <- dplyr::bind_cols(out, pulmo_markers(out))
  
  if (verbose)
    message("→ salivary markers")
  out <- dplyr::bind_cols(out, saliva_markers(out))
  
  if (verbose)
    message("→ sweat markers")
  out <- dplyr::bind_cols(out, sweat_markers(out))
  
  if (verbose)
    message("→ urine markers")
  out <- dplyr::bind_cols(out, urine_markers(out))
  
  out
}
