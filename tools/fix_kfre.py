fname = 'C:/R_packages/HealthMarkers/R/kidney_kfre.R'
with open(fname, 'rb') as f:
    raw = f.read()

text = raw.decode('utf-8')

# 1. Remove the @param extreme_rules block (lines 83-85 area)
text = text.replace(
    "#' @param extreme_rules Optional named list of numeric c(min,max) ranges for c(age, eGFR, UACR).\r\n\r\n#'   If NULL, broad defaults are used: age (18, 120), eGFR (1, 200), UACR (0.1, 10000) (mg/g).\r\n\r\n",
    ""
)

# 2. Remove the stale verbose bullet points that don't belong under @param verbose
# Lines 73-81: the "-warn/-cap/-error/-ignore/-NA" bullets that belong to the removed extreme param
text = text.replace(
    "#'   - \"warn\": only warn about out-of-range values (default).\r\n\r\n#'   - \"cap\": truncate to range and warn.\r\n\r\n#'   - \"error\": abort if any out-of-range is detected.\r\n\r\n#'   - \"ignore\": do nothing.\r\n\r\n#'   - \"NA\": set out-of-range input values to NA before computation.\r\n\r\n",
    ""
)

# 3. Fix the broken example: remove check_extreme = TRUE, extreme_action = "cap"
text = text.replace(
    "#'   check_extreme = TRUE, extreme_action = \"cap\", verbose = TRUE\r\n",
    "#'   verbose = TRUE\r\n"
)

# Also fix comment above it
text = text.replace(
    "#' # With diagnostics and capping\r\n",
    "#' # With verbose output\r\n"
)

with open(fname, 'wb') as f:
    f.write(text.encode('utf-8'))

print("Done")

# Verify
with open(fname, 'rb') as f:
    t = f.read().decode('utf-8')
for bad in ['check_extreme', 'extreme_action', 'extreme_rules']:
    count = t.count(bad)
    print(f'{bad}: {count} occurrences')
