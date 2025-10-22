#' Generate a marker function skeleton
#'
#' @description
#' Utility to scaffold a new marker function with validation and documentation (HM-CS v2).
#' @param name Function name (string)
#' @param required_keys Character vector of required col_map keys
#' @param formula_text Character description of the calculation
#' @return Character vector with function code you can write into a file
#' @export
make_marker_function <- function(name, required_keys, formula_text) {
  stopifnot(is.character(name), length(name) == 1)
  stopifnot(is.character(required_keys), length(required_keys) >= 1)
  keys_vec <- paste(sprintf('"%s"', required_keys), collapse = ", ")
  colmap_defaults <- paste(sprintf('%s = "%s"', required_keys, required_keys), collapse = ",\n                   ")
  glue <- function(...) paste(..., sep = "")
  glue(
    "#' ", name, "\n",
    "#'\n",
    "#' @description\n",
    "#' ", formula_text, "\n",
    "#'\n",
    "#' @param data A data.frame or tibble.\n",
    "#' @param col_map Named list mapping required inputs. Defaults map keys to same-named columns.\n",
    "#' @param na_action One of c(\"keep\",\"omit\",\"error\",\"ignore\",\"warn\").\n",
    "#' @param na_warn_prop Proportion threshold for high-missingness warnings when na_action='warn'.\n",
    "#' @param check_extreme Logical; if TRUE, enable extreme-value handling.\n",
    "#' @param extreme_action One of c(\"warn\",\"cap\",\"error\",\"ignore\",\"NA\").\n",
    "#' @param extreme_rules Optional named list of c(min,max) per key.\n",
    "#' @param verbose Logical; if TRUE, prints progress messages.\n",
    "#' @return A tibble\n",
    "#' @export\n",
    name, " <- function(data,\n",
    "                 col_map = list(\n",
    "                   ", colmap_defaults, "\n",
    "                 ),\n",
    "                 na_action = c(\"keep\",\"omit\",\"error\",\"ignore\",\"warn\"),\n",
    "                 na_warn_prop = 0.2,\n",
    "                 check_extreme = FALSE,\n",
    "                 extreme_action = c(\"warn\",\"cap\",\"error\",\"ignore\",\"NA\"),\n",
    "                 extreme_rules = NULL,\n",
    "                 verbose = FALSE) {\n",
    "  na_action_raw <- match.arg(na_action)\n",
    "  na_action_eff <- if (na_action_raw %in% c(\"ignore\",\"warn\")) \"keep\" else na_action_raw\n",
    "  extreme_action <- match.arg(extreme_action)\n",
    "\n",
    "  if (!is.data.frame(data)) rlang::abort(\"", name, "(): `data` must be a data.frame or tibble.\")\n",
    "  if (isTRUE(verbose)) rlang::inform(\"-> ", name, ": validating inputs\")\n",
    "\n",
    "  req <- c(", keys_vec, ")\n",
    "  # Optional package-level validation (non-fatal)\n",
    "  if (is.function(get0(\"hm_validate_inputs\", envir = asNamespace(\"HealthMarkers\"), inherits = TRUE))) {\n",
    "    try(hm_validate_inputs(data = data, col_map = col_map, required_keys = req, fn = \"", name, "\"), silent = TRUE)\n",
    "  }\n",
    "\n",
    "  # Required mapping and columns\n",
    "  missing_map <- setdiff(req, names(col_map))\n",
    "  if (length(missing_map)) rlang::abort(paste0(\"", name, "(): you must supply col_map entries for: \", paste(missing_map, collapse = \", \")))\n",
    "  mapped <- vapply(req, function(k) {\n",
    "    val <- col_map[[k]]\n",
    "    if (is.null(val) || is.na(val)) \"\" else as.character(val)\n",
    "  }, character(1))\n",
    "  bad_idx <- (is.na(mapped) | !nzchar(mapped))\n",
    "  if (any(bad_idx)) {\n",
    "    bad <- req[bad_idx]\n",
    "    rlang::abort(paste0(\"", name, "(): you must supply col_map entries for: \", paste(bad, collapse = \", \")))\n",
    "  }\n",
    "  missing_cols <- setdiff(unname(mapped), names(data))\n",
    "  if (length(missing_cols)) rlang::abort(paste0(\"missing required columns: \", paste(missing_cols, collapse = \", \")))\n",
    "\n",
    "  # Optional: numeric coercion for required inputs (customize numeric_keys as needed)\n",
    "  numeric_keys <- req\n",
    "  for (k in numeric_keys) {\n",
    "    cn <- col_map[[k]]\n",
    "    if (!is.numeric(data[[cn]])) {\n",
    "      old <- data[[cn]]\n",
    "      suppressWarnings(new <- as.numeric(old))\n",
    "      introduced_na <- sum(is.na(new) & !is.na(old))\n",
    "      if (introduced_na > 0L) warning(sprintf(\"Column '%s' coerced to numeric; NAs introduced: %d\", cn, introduced_na), call. = FALSE)\n",
    "      data[[cn]] <- new\n",
    "    }\n",
    "    data[[cn]][!is.finite(data[[cn]])] <- NA_real_\n",
    "  }\n",
    "\n",
    "  # NA policy (warn/omit/error)\n",
    "  if (na_action_raw == \"warn\") {\n",
    "    dfp <- data[, mapped, drop = FALSE]\n",
    "    any_na_cols <- names(which(colSums(is.na(dfp)) > 0))\n",
    "    if (length(any_na_cols)) warning(sprintf(\"Missing values in: %s.\", paste(any_na_cols, collapse = \", \")), call. = FALSE)\n",
    "  }\n",
    "  if (na_action_eff == \"error\") {\n",
    "    cc <- stats::complete.cases(data[, mapped, drop = FALSE])\n",
    "    if (!all(cc)) rlang::abort(\"", name, "(): required inputs contain missing values (na_action='error').\")\n",
    "  } else if (na_action_eff == \"omit\") {\n",
    "    cc <- stats::complete.cases(data[, mapped, drop = FALSE])\n",
    "    if (isTRUE(verbose)) rlang::inform(sprintf(\"-> ", name, ": omitting %d rows with NA in required inputs\", sum(!cc)))\n",
    "    data <- data[cc, , drop = FALSE]\n",
    "  }\n",
    "\n",
    "  # Extremes (provide rules to enable)\n",
    "  if (isTRUE(check_extreme) && length(mapped)) {\n",
    "    # TODO: supply `extreme_rules` as named list like list(key = c(min,max)) to activate scanning\n",
    "    if (!is.null(extreme_rules) && is.list(extreme_rules)) {\n",
    "      total <- 0L\n",
    "      flags <- list()\n",
    "      for (k in intersect(names(extreme_rules), req)) {\n",
    "        cn <- col_map[[k]]; rng <- extreme_rules[[k]]; x <- data[[cn]]\n",
    "        bad <- is.finite(x) & (x < rng[1] | x > rng[2])\n",
    "        flags[[k]] <- bad; total <- total + sum(bad)\n",
    "      }\n",
    "      if (total > 0L) {\n",
    "        if (extreme_action == \"error\") {\n",
    "          rlang::abort(sprintf(\"", name, "(): %d extreme input values detected.\", total))\n",
    "        } else if (extreme_action == \"cap\") {\n",
    "          for (k in names(flags)) {\n",
    "            cn <- col_map[[k]]; rng <- extreme_rules[[k]]; x <- data[[cn]]; bad <- flags[[k]]\n",
    "            x[bad & is.finite(x) & x < rng[1]] <- rng[1]\n",
    "            x[bad & is.finite(x) & x > rng[2]] <- rng[2]\n",
    "            data[[cn]] <- x\n",
    "          }\n",
    "          rlang::warn(sprintf(\"", name, "(): capped %d extreme input values into allowed ranges.\", total))\n",
    "        } else if (extreme_action == \"warn\") {\n",
    "          rlang::warn(sprintf(\"", name, "(): detected %d extreme input values (not altered).\", total))\n",
    "        } else if (extreme_action == \"NA\") {\n",
    "          for (k in names(flags)) {\n",
    "            cn <- col_map[[k]]; x <- data[[cn]]; x[flags[[k]]] <- NA_real_; data[[cn]] <- x\n",
    "          }\n",
    "        }\n",
    "      }\n",
    "    }\n",
    "  }\n",
    "\n",
    "  # TODO: implement the calculation using mapped columns\n",
    "  tibble::tibble(result = rep(NA_real_, nrow(data)))\n",
    "}\n"
  )
}

# HM-VS v2 template: uses hm_validate_inputs + hm_inform consistently
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