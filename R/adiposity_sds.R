# R/adiposity_sds.R

#’ Calculate sex-stratified standardized scores (SDS) for adiposity measures
#’
#’ Given raw anthropometry and sex, compute z-scores relative to sex-specific
#’ reference means & SDs.
#’
#’ @param data A data.frame or tibble containing your raw measures *and* a
#’   `sex` column coded `"M"` or `"F"`.
#’ @param ref A named list with elements `"M"` and `"F"`.  Each is itself a
#’   named list of named numeric vectors `c(mean=…, sd=…)`.  E.g.
#’   ```r
#’   list(
#’     M = list(BMI=c(mean=23,sd=3.5), waist=c(mean=85,sd=10)),
#’     F = list(BMI=c(mean=21,sd=3),  waist=c(mean=75,sd=9))
#’   )
#’   ```
#’ @param sex_col Name of the sex column in `data` (must be `"M"`/`"F"`).
#’ @param verbose Logical; if `TRUE`, prints a progress message.
#’
#’ @return A tibble with one `<var>_SDS` column per reference variable.
#’ @importFrom purrr imap
#’ @importFrom tibble tibble
#’ @export
#’ @examples
#’ df <- tibble::tibble(
#’   sex   = c("M","F","M"),
#’   BMI   = c(25,20,22),
#’   waist = c(90,70,85)
#’ )
#’ ref <- list(
#’   M = list(BMI=c(mean=23,sd=3.5), waist=c(mean=85,sd=10)),
#’   F = list(BMI=c(mean=21,sd=3),  waist=c(mean=75,sd=9))
#’ )
#’ adiposity_sds_strat(df, ref, sex_col="sex")
adiposity_sds <- function(data, ref, sex_col = "sex", verbose = FALSE) {
  # 1) validate ref structure
  if (!is.list(ref) || !all(c("M","F") %in% names(ref)))
    stop("`ref` must have elements 'M' and 'F'")
  if (!identical(sort(names(ref$M)), sort(names(ref$F))))
    stop("Both ref$M and ref$F must define the same set of variables")
  for (sex in c("M","F")) {
    lst <- ref[[sex]]
    if (!is.list(lst) || is.null(names(lst)))
      stop("ref[['",sex,"']] must be a named list")
    purrr::imap(lst, function(stats,var) {
      if (!(is.numeric(stats) && length(stats)==2 && all(c("mean","sd") %in% names(stats))))
        stop("ref[['",sex,"']][['",var,"']] must be c(mean=…, sd=…)")
      if (stats["sd"] <= 0)
        stop("sd for ",sex,"/",var," must be > 0")
      NULL
    })
  }
  
  # 2) validate data
  if (!sex_col %in% names(data))
    stop("Column '", sex_col, "' not found in data")
  if (!all(data[[sex_col]] %in% c("M","F")))
    stop("All values in '", sex_col, "' must be 'M' or 'F'")
  
  # 3) compute, vectorized by sex
  vars <- names(ref$M)
  sds_tables <- purrr::imap(ref$M, function(stats_M, var) {
    stats_F <- ref$F[[var]]
    mu_M  <- stats_M["mean"]; sd_M  <- stats_M["sd"]
    mu_F  <- stats_F["mean"]; sd_F  <- stats_F["sd"]
    sex_vec <- data[[sex_col]]
    mu_vec  <- ifelse(sex_vec=="M", mu_M, mu_F)
    sd_vec  <- ifelse(sex_vec=="M", sd_M, sd_F)
    vals    <- data[[var]]
    tibble::tibble(!!paste0(var, "_SDS") := (vals - mu_vec) / sd_vec)
  })
  
  out <- do.call(dplyr::bind_cols, sds_tables)
  
  if (verbose) {
    message("→ computed SDS for: ", paste(vars, collapse=", "),
            " stratified by ", sex_col)
  }
  out
}
