% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/metabolic_risk_features.R
\name{metabolic_risk_features}
\alias{metabolic_risk_features}
\title{Calculate metabolic risk feature flags}
\usage{
metabolic_risk_features(data)
}
\arguments{
\item{data}{A data.frame or tibble containing at least:
\itemize{
\item \code{chol_total}, \code{chol_ldl}, \code{chol_hdl}, \code{triglycerides} (numeric)
\item \code{age_year}     (numeric, years)
\item \code{z_HOMA}       (numeric, standardized HOMA‐IR)
\item \code{glucose}      (numeric, mmol/L)
\item \code{HbA1c}        (numeric, mmol/mol)
\item \code{bp_sys_z}, \code{bp_dia_z} (numeric, systolic/diastolic BP z‐scores)
}}
}
\value{
A tibble with one column per marker-each a factor with levels \code{0}/\code{1}.
}
\description{
Given routine labs and z‐scores, computes four key binary risk markers:
\itemize{
\item dyslipidemia
\item insulin_resistance
\item hyperglycemia
\item hypertension
}
}
\examples{
df <- tibble::tibble(
  chol_total   = 6.0,
  chol_ldl     = 3.5,
  chol_hdl     = 1.0,
  triglycerides= 1.2,
  age_year     = 25,
  z_HOMA       = 1.5,
  glucose      = 5.8,
  HbA1c        = 40,
  bp_sys_z     = 1.7,
  bp_dia_z     = 1.0
)
metabolic_risk_features(df)
}
