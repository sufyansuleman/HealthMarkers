% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/cvd_risk.R
\name{cvd_risk_mesa}
\alias{cvd_risk_mesa}
\title{MESA 10-year CHD risk}
\usage{
cvd_risk_mesa(data, ...)
}
\arguments{
\item{data}{A tibble with columns: \code{race}, \code{sex}, \code{age}, \code{totchol}, \code{HDL_c}, \code{sbp}, \code{bp_treated}, \code{smoker}, \code{diabetes}.}

\item{...}{Additional arguments for chd_10y_mesa.}
}
\value{
A tibble with columns \code{model}, \code{year} (10), and \code{risk}.
}
\description{
Wrapper around CVrisk::chd_10y_mesa.
}
\examples{
\dontrun{
library(tibble)
df <- tibble(race="white", sex=1, age=55, totchol=200, HDL_c=50,
sbp=140, bp_treated=FALSE, smoker=FALSE, diabetes=FALSE)
cvd_risk_mesa(df)
}
}
