fname = 'C:/R_packages/HealthMarkers/R/kidney_kfre.R'
with open(fname, 'rb') as f:
    raw = f.read()

# Normalize to LF for processing, remember original ending style
crlf = b'\r\n' in raw
text = raw.decode('utf-8').replace('\r\n', '\n').replace('\r', '\n')

lines = text.split('\n')
out = []
skip_next_blank = False

i = 0
while i < len(lines):
    line = lines[i]
    stripped = line.strip()
    
    # Remove @param extreme_rules block (2 lines: param + description)
    if stripped == "#' @param extreme_rules Optional named list of numeric c(min,max) ranges for c(age, eGFR, UACR).":
        i += 1  # skip this line
        # skip following blank and continuation line
        while i < len(lines) and (lines[i].strip() == '' or lines[i].strip().startswith("#'   If NULL")):
            i += 1
        continue
    
    # Remove stale bullet points under what was the extreme_action param
    if stripped in (
        "#'   - \"warn\": only warn about out-of-range values (default).",
        "#'   - \"cap\": truncate to range and warn.",
        "#'   - \"error\": abort if any out-of-range is detected.",
        "#'   - \"ignore\": do nothing.",
        "#'   - \"NA\": set out-of-range input values to NA before computation.",
    ):
        i += 1
        continue
    
    # Fix broken example line
    if stripped == "#'   check_extreme = TRUE, extreme_action = \"cap\", verbose = TRUE":
        out.append(line.replace(
            'check_extreme = TRUE, extreme_action = "cap", verbose = TRUE',
            'verbose = TRUE'
        ))
        i += 1
        continue
    
    # Fix comment above broken example
    if stripped == "#' # With diagnostics and capping":
        out.append(line.replace("# With diagnostics and capping", "# With verbose output"))
        i += 1
        continue
    
    out.append(line)
    i += 1

result = '\n'.join(out)
if crlf:
    result = result.replace('\n', '\r\n')

with open(fname, 'wb') as f:
    f.write(result.encode('utf-8'))

print("Done")

# Verify
with open(fname, 'rb') as f:
    t = f.read().decode('utf-8')
for bad in ['check_extreme', 'extreme_action', 'extreme_rules']:
    # Find context
    idx = t.find(bad)
    if idx >= 0:
        ctx = t[max(0,idx-40):idx+60].replace('\r','').replace('\n','\\n')
        print(f'REMAINING {bad}: ...{ctx}...')
    else:
        print(f'OK: {bad} not found')
