## HealthMarkers 0.1.4

This release restores the package after its CRAN archival (2026-06-01). It fixes
the check failure that caused the archival and corrects a number of biomarker
formula/unit bugs found in a full, script-by-script audit of every
marker-calculation function against its original publication. No exported
function signatures changed.

### Formula & unit bug fixes

- **`fasting_is()`** — `HOMA_IR_inv` and `FIRI` now use raw mmol/L glucose (were 18× too large); `QUICKI` now uses `log10` (Katz 2000).
- **`ogtt_is()`** — `Cederholm_index` uses raw mmol/L glucose, removing a double unit conversion.
- **`glycemic_markers()`** — `SPISE` and `METS_IR` now convert inputs to mg/dL internally, as their definitions and cut-offs require.
- **`liver_fat_markers()`** — `NAFLD_LFS` codes type-2 diabetes as yes = 2 (Kotronen 2009).
- **`renal_markers()`** — `eGFR_combined` uses the Inker 2012 creatinine α and the correct female/Black multipliers.
- **`ckd_stage()`** — KDIGO risk heatmap: G1/G2 + A3 → High; G3a + A3 → Very High.
- **`kidney_failure_risk()`** — KFRE reimplemented per Tangri 2011 (non-North-American calibration).
- **`obesity_indices()`** — `BRI` uses `π` (not `2π`) in the eccentricity ratio (Thomas 2013).

### Documentation

- AIP unit clarifications for `atherogenic_indices()` and `cvd_marker_aip()` (mmol/L inputs).

### Internal

- Fixed the Fedora CRAN check failure in `test-frailty_index` (now mocks the internal `di` gatekeeper directly).
- Removed a deprecated `dplyr::case_when()` pattern in `obesity_indices()` (dplyr ≥ 1.2.0).

Full details and references are in [NEWS.md](https://github.com/sufyansuleman/HealthMarkers/blob/main/NEWS.md).
