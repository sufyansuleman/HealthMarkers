## R CMD check results

0 errors | 0 warnings | 2 notes

### Note 1: Suggested packages not on CRAN

The following packages listed in `Suggests` are not currently on CRAN. An
`Additional_repositories: https://sufyansuleman.r-universe.dev` field has been
added to DESCRIPTION so `install.packages()` can find them automatically.

- **whoishRisk**: WHO cardiovascular risk scoring. Used in `cvd_risk_scorescvd()`, guarded with `requireNamespace()`.
- **QRISK3**: QRISK3 cardiovascular risk scoring. Used in `cvd_risk_qrisk3()`, guarded with `requireNamespace()`.
- **RiskScorescvd**: Additional CVD risk functions. Used in `cvd_risk_scorescvd()`, guarded with `requireNamespace()`.
- **di**: Disability Index utilities. Optional in `frailty_index()`.
- **rspiro**: Spirometry reference equations. Optional in `spirometry_markers()` and `pulmo_markers()`.

All usages are optional and degrade gracefully with an informative message
when the suggested package is not installed.

### Note 2: New submission

This is the first CRAN submission of the HealthMarkers package.

## Test environments

- Windows 11 x64, R 4.4.0 (local)
- win-builder (R-devel and R-release)

## Downstream dependencies

None — this is a new package.
