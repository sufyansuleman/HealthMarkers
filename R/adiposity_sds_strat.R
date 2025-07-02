# R/adiposity_sds_strat.R

#’ Calculate sex‐stratified standardized‐score (SDS) for adiposity measures
#’
#’ Given raw anthropometry and a sex column, compute z‐scores relative to
#’ sex‐specific reference means & SDs.
#’
#’ @param data A data.frame or tibble containing your raw measures *and* a
#’   `sex` column coded `"M"` or `"F"`.
#’ @param ref A named list with elements `"M"` and `"F"`. Each is itself a
#’   named list of numeric vectors `c(mean=…, sd=…)`. E.g.:
#’   ```r
#’   list(
#’     M = list(BMI = c(mean=23, sd=3.5), waist = c(mean=85, sd=10)),
#’     F = list(BMI = c(mean=21, sd=3  ), waist = c(mean=75, sd= 9))
#’   )
#’   ```
#’ @param sex_col Character; name of the sex column in `data` (values `"M"`/`"F"`).
#’ @param verbose Logical; if `TRUE`, prints a progress message.
#’
#’ @return A tibble with one `<var>_SDS` column per reference variable.
#’ @export
#’
adiposity_sds_strat <- function(data, ref, sex_col = "sex", verbose = FALSE) {
  # 1) validate ref structure
  if (!is.list(ref) || !all(c("M","F") %in% names(ref))) {
    stop("`ref` must be a named list with elements 'M' and 'F'")
  }
  # both must define the same variables
  vars_M <- names(ref$M)
  vars_F <- names(ref$F)
  if (!identical(sort(vars_M), sort(vars_F))) {
    stop("ref$M and ref$F must contain the same variable names")
  }
  for (sex in c("M","F")) {
    lst <- ref[[sex]]
    if (!is.list(lst) || is.null(names(lst))) {
      stop(sprintf("ref[['%s']] must be a named list", sex))
    }
    for (var in names(lst)) {
      stats <- lst[[var]]
      if (!(is.numeric(stats) && length(stats) == 2 && all(c("mean","sd") %in% names(stats)))) {
        stop(sprintf("ref[['%s']][['%s']] must be c(mean=…, sd=…)", sex, var))
      }
      if (stats["sd"] <= 0) {
        stop(sprintf("sd for %s/%s must be > 0", sex, var))
      }
    }
  }
  
  # 2) validate data
  if (!(sex_col %in% names(data))) {
    stop("Column '", sex_col, "' not found in data")
  }
  if (!all(data[[sex_col]] %in% c("M","F"))) {
    stop("All values in '", sex_col, "' must be 'M' or 'F'")
  }
  
  if (verbose) {
    message("→ adiposity_sds_strat: computing sex‐stratified SDS for: ",
            paste(vars_M, collapse = ", "))
  }
  
  # 3) compute SDS for each var
  out_list <- lapply(vars_M, function(var) {
    stats_M <- ref$M[[var]]
    stats_F <- ref$F[[var]]
    # vectorize per row
    mu_vec <- ifelse(data[[sex_col]] == "M", stats_M["mean"], stats_F["mean"])
    sd_vec <- ifelse(data[[sex_col]] == "M", stats_M["sd"],   stats_F["sd"])
    (data[[var]] - mu_vec) / sd_vec
  })
  names(out_list) <- paste0(vars_M, "_SDS")
  
  # 4) assemble tibble
  tibble::as_tibble(out_list)
}
