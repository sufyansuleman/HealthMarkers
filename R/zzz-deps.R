# R/zzz-deps.R

# List ALL optional (Suggests) namespaces in one place
.optional_deps <- c(
  "CVrisk", "PooledCohort", "QRISK3", "RiskScorescvd",
  "whoishRisk", "minfi", "methylclock", "di"
)

# Require exactly one package (runtime gate)
.need_pkg <- function(pkg) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    rlang::abort(sprintf(
      "Package '%s' is required for this feature. Install it first.",
      pkg
    ))
  }
}

# Require a set of packages (useful for multi-dep features)
.need_pkgs <- function(pkgs) {
  miss <- pkgs[!vapply(pkgs, requireNamespace, FUN.VALUE = TRUE, quietly = TRUE)]
  if (length(miss)) {
    rlang::abort(sprintf(
      "Missing required package(s): %s. Install them first.",
      paste(miss, collapse = ", ")
    ))
  }
}

# Run an expression only if a package is available; otherwise return NA or a fallback
.with_pkg <- function(pkg, expr, otherwise = NULL) {
  if (requireNamespace(pkg, quietly = TRUE)) {
    force(expr)
  } else {
    if (!is.null(otherwise)) otherwise else rlang::abort(
      sprintf("Optional package '%s' not installed.", pkg)
    )
  }
}


utils::globalVariables(c(
  "chol_total","chol_ldl","chol_hdl","triglycerides","age_year","z_HOMA",
  "glucose","HbA1c","bp_sys_z","bp_dia_z","weight_kg","height_m",
  "BMI","Avignon_Si0","Avignon_Si120"
))
