# R/adiposity_sds.R

#' Calculate standardized‐score (SDS) for adiposity measures
#'
#' Given raw anthropometry, compute z‐scores relative to supplied reference means & SDs.
#'
#' @param data A data.frame or tibble containing your raw measures.
#' @param ref A named list of reference parameters. Each element must be
#'   a numeric vector of length 2, named **mean** and **sd**, e.g.:
#'   ```r
#'   list(
#'     BMI           = c(mean = 18.5, sd = 4.2),
#'     waist         = c(mean = 75,   sd = 10),
#'     body_fat_pct  = c(mean = 25,   sd = 8)
#'   )
#'   ```
#' @param verbose Logical; if `TRUE`, prints a progress message.
#'
#' @return A tibble with one column per reference variable, named `<var>_SDS`.
#' @export
#'
adiposity_sds <- function(data, ref, verbose = FALSE) {
  # - validate ref structure -
  if (!is.list(ref) || is.null(names(ref))) {
    stop("`ref` must be a named list, e.g. list(BMI = c(mean=…, sd=…), …)")
  }
  for (nm in names(ref)) {
    stats <- ref[[nm]]
    if (!(is.numeric(stats) && length(stats) == 2 && all(c("mean","sd") %in% names(stats)))) {
      stop(sprintf("ref[[\"%s\"]] must be a numeric vector with names 'mean' and 'sd'", nm))
    }
    if (stats["sd"] <= 0) {
      stop(sprintf("`ref[[\"%s\"]][\"sd\"]` must be > 0", nm))
    }
  }
  
  # - check data has each variable -
  missing_vars <- setdiff(names(ref), names(data))
  if (length(missing_vars)) {
    stop("adiposity_sds(): missing required columns: ",
         paste(missing_vars, collapse = ", "))
  }
  
  if (verbose) {
    message("-> computing SDS for: ", paste(names(ref), collapse = ", "))
  }
  
  # - compute SDS into a named list -
  out_list <- lapply(names(ref), function(var) {
    stats <- ref[[var]]
    
    unname((data[[var]] - stats["mean"]) / stats["sd"])
    
  })
  names(out_list) <- paste0(names(ref), "_SDS")
  
  # - return tibble, even if length(data)==1 -
  tibble::as_tibble(out_list)
}
