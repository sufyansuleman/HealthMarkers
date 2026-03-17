## R CMD check results

0 errors | 0 warnings | 2 notes

### Note 1: Suggested package not on CRAN

One package in `Suggests` is not on CRAN. An
`Additional_repositories: https://sufyansuleman.r-universe.dev` field has been
added to DESCRIPTION so it can be found automatically.

- **whoishRisk**: WHO cardiovascular risk scoring. Available on r-universe at
  https://sufyansuleman.r-universe.dev. Used in `cvd_risk_scorescvd()`,
  guarded with `requireNamespace()` and degrades gracefully when not installed.

All other suggested packages (`QRISK3`, `RiskScorescvd`, `di`, `rspiro`, etc.) are on CRAN.

### Note 2: New submission

This is the first CRAN submission of the HealthMarkers package.

## Test environments

- Windows 11 x64, R 4.4.0 (local)
- win-builder (R-devel and R-release)

## Downstream dependencies

None — this is a new package.

## Additional notes

- "Unable to verify current time" is a transient network issue on the local test
  machine and does not reflect a package problem.
- "Non-staged installation was used" is specific to the local R installation
  and does not affect the package.
