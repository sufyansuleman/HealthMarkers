# R/adiposity_sds.R

#’ Calculate standardized‐score (SDS) for adiposity measures
#’
#’ Given raw anthropometry, compute z‐scores relative to supplied reference means & SDs.
#’
#’ @param data A data.frame or tibble containing your raw measures.
#’ @param ref A named list of reference parameters. Each element must be
#’   a numeric vector of length 2, named **mean** and **sd**, e.g.:
#’   ```r
#’   list(
#’     BMI           = c(mean = 18.5, sd = 4.2),
#’     waist         = c(mean = 75,   sd = 10),
#’     body_fat_pct  = c(mean = 25,   sd = 8)
#’   )
#’   ```
#’ @param verbose Logical; if `TRUE`, prints a progress message.
#’
#’ @return A tibble with one column per reference variable, named `<var>_SDS`.
#’ @export
#’
adiposity_sds <- function(data, ref, verbose = FALSE) {
  # 1) validate ref
  if (!is.list(ref) || is.null(names(ref))) {
    stop("`ref` must be a named list, e.g. list(BMI = c(mean=…, sd=…), …)")
  }
  for (var in names(ref)) {
    stats <- ref[[var]]
    if (!(is.numeric(stats) && length(stats) == 2 && all(c("mean","sd") %in% names(stats)))) {
      stop(sprintf("`ref[[\"%s\"]]` must be a numeric vector c(mean=…, sd=…)", var))
    }
    if (stats["sd"] <= 0) {
      stop(sprintf("`ref[[\"%s\"]][\"sd\"]` must be > 0", var))
    }
  }
  
  # 2) check data has each var
  missing_vars <- setdiff(names(ref), names(data))
  if (length(missing_vars)) {
    stop("adiposity_sds(): missing required columns: ",
         paste(missing_vars, collapse = ", "))
  }
  
  if (verbose) {
    message("→ adiposity_sds: computing SDS for: ", paste(names(ref), collapse = ", "))
  }
  
  # 3) compute SDS
  out_list <- lapply(names(ref), function(var) {
    m <- ref[[var]]["mean"]
    s <- ref[[var]]["sd"]
    (data[[var]] - m) / s
  })
  names(out_list) <- paste0(names(ref), "_SDS")
  
  # 4) assemble tibble
  tibble::as_tibble(out_list)
}
