#' Generate a marker function (HM-VS v2 template)
#'
#' Creates and assigns a marker function with consistent validation, NA/extreme
#' handling, and hm_inform logging. Intended for internal scaffolding of new
#' HealthMarkers functions.
#'
#' @param name Function name to create (string).
#' @param required_keys Character vector of required col_map keys.
#' @param compute_body List containing `expr` (quoted expression computing `out`),
#'   `empty_out` (returned when zero rows), and `default_rules` (optional extreme
#'   rules list) used when `include_extremes = TRUE`.
#' @param defaults Named list of default arguments to merge into the generated
#'   function formals (e.g., col_map, normalize).
#' @param include_extremes Logical; if TRUE, the generated function wires in
#'   extreme-value scanning/capping using `compute_body$default_rules`.
#' @return Invisibly returns the symbol name after assigning the function into
#'   the parent frame.
#' @export
make_marker_function <- function(name,
                                 required_keys,
                                 compute_body,
                                 defaults = list(),
                                 include_extremes = TRUE) {
  stopifnot(is.character(name), length(name) == 1L)

  body_expr <- bquote({
    na_action <- match.arg(na_action)
    extreme_action <- match.arg(extreme_action)
    t0 <- Sys.time()

    if (!is.data.frame(data)) {
      rlang::abort(.(paste0(name, "(): `data` must be a data.frame or tibble.")),
                   class = .(paste0("healthmarkers_", name, "_error_data_type")))
    }
    if (!is.list(col_map) || is.null(names(col_map)) || any(names(col_map) == "")) {
      rlang::abort(.(paste0(name, "(): `col_map` must be a named list of required keys -> column names.")),
                   class = .(paste0("healthmarkers_", name, "_error_colmap_type")))
    }

    hm_validate_inputs(data, col_map, required_keys = .(required_keys), fn = .(name))
    if (isTRUE(verbose)) hm_inform(level = "inform", msg = .(paste0("-> ", name, ": validating inputs")))

    req_cols <- hm_require_columns(data, col_map, .(required_keys), fn = .(name))

    # Coerce to numeric and sanitize non-finite
    for (key in .(required_keys)) {
      cn <- col_map[[key]]
      if (!is.numeric(data[[cn]])) {
        old <- data[[cn]]
        suppressWarnings(data[[cn]] <- as.numeric(old))
        introduced <- sum(is.na(data[[cn]]) & !is.na(old))
        if (introduced > 0) {
          rlang::warn(sprintf("%s(): column '%s' coerced to numeric; NAs introduced: %d", .(name), cn, introduced))
        }
      }
      data[[cn]][!is.finite(data[[cn]])] <- NA_real_
    }

    # NA handling on required inputs
    if (na_action == "error") {
      has_na <- Reduce(`|`, lapply(req_cols, function(cn) is.na(data[[cn]])))
      if (any(has_na)) {
        rlang::abort(.(paste0(name, "(): required inputs contain missing values (na_action='error').")),
                     class = .(paste0("healthmarkers_", name, "_error_missing_values")))
      }
    } else if (na_action == "omit") {
      keep <- !Reduce(`|`, lapply(req_cols, function(cn) is.na(data[[cn]])))
      if (isTRUE(verbose)) hm_inform(level = "inform", msg = sprintf("-> %s: omitting %d rows with NA in required inputs", .(name), sum(!keep)))
      data <- data[keep, , drop = FALSE]
    }

    if (nrow(data) == 0L) {
      return(.((compute_body$empty_out)))
    }

    capped_n <- 0L
    if (.(include_extremes) && isTRUE(check_extreme)) {
      rules <- if (is.null(extreme_rules)) .(compute_body$default_rules) else extreme_rules
      key_to_col <- stats::setNames(req_cols, .(required_keys))
      if (!is.null(names(rules))) {
        remapped <- list()
        for (nm in names(rules)) {
          col_nm <- if (nm %in% names(key_to_col)) key_to_col[[nm]] else nm
          remapped[[col_nm]] <- rules[[nm]]
        }
        rules <- remapped
      }
      ex_counts <- integer(0)
      for (nm in names(rules)) {
        if (!nm %in% names(data)) next
        rng <- rules[[nm]]
        x <- data[[nm]]
        bad <- is.finite(x) & (x < rng[1] | x > rng[2])
        ex_counts[nm] <- sum(bad, na.rm = TRUE)
        if (extreme_action == "cap" && any(bad, na.rm = TRUE)) {
          x[bad & is.finite(x) & x < rng[1]] <- rng[1]
          x[bad & is.finite(x) & x > rng[2]] <- rng[2]
          data[[nm]] <- x
        }
      }
      total_ex <- sum(ex_counts, na.rm = TRUE)
      if (total_ex > 0) {
        if (extreme_action == "error") {
          rlang::abort(sprintf("%s(): detected %d extreme input values.", .(name), total_ex),
                       class = .(paste0("healthmarkers_", name, "_error_extremes")))
        } else if (extreme_action == "cap") {
          capped_n <- total_ex
          rlang::warn(sprintf("%s(): capped %d extreme input values into allowed ranges.", .(name), total_ex))
        } else if (extreme_action == "warn") {
          rlang::warn(sprintf("%s(): detected %d extreme input values (not altered).", .(name), total_ex))
        }
      }
    }

    if (isTRUE(verbose)) hm_inform(level = "inform", msg = .(paste0("-> ", name, ": computing markers")))

    dz_env <- new.env(parent = emptyenv()); dz_env$counts <- list()
    safe_div <- function(num, den, label) {
      out <- num / den
      zero_den <- is.finite(den) & den == 0
      dz_env$counts[[label]] <- sum(zero_den, na.rm = TRUE)
      out[!is.finite(out)] <- NA_real_
      out
    }

    .(compute_body$expr)

    dz <- dz_env$counts
    dz_total <- if (length(dz)) sum(unlist(dz), na.rm = TRUE) else 0L
    if (dz_total > 0L) {
      nz <- unlist(dz); nz <- nz[nz > 0]
      lbl <- paste(sprintf("%s=%d", names(nz), nz), collapse = ", ")
      rlang::warn(sprintf("%s(): zero denominators detected in %d cases (%s).", .(name), dz_total, lbl))
    }

    if (isTRUE(verbose)) {
      na_counts <- vapply(out, function(x) sum(is.na(x) | !is.finite(x)), integer(1))
      elapsed <- as.numeric(difftime(Sys.time(), t0, units = "secs"))
      hm_inform(level = "inform", msg = sprintf(
        "Completed %s: %d rows; NA/Inf -> %s; capped=%d; denom_zero=%d; elapsed=%.2fs",
        .(name),
        nrow(out),
        paste(sprintf("%s=%d", names(na_counts), na_counts), collapse = ", "),
        capped_n, dz_total, elapsed
      ))
    }

    out
  })

  formals_list <- modifyList(
    alist(
      data = , col_map = , verbose = FALSE,
      na_action = c("keep","omit","error"),
      na_warn_prop = 0.2,
      check_extreme = FALSE,
      extreme_action = c("warn","cap","error","ignore"),
      extreme_rules = NULL
    ),
    defaults
  )

  fn <- eval(call("function", as.pairlist(formals_list), body_expr))
  assign(name, fn, envir = parent.frame())
  invisible(as.symbol(name))
}