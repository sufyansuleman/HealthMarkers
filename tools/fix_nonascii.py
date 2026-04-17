"""Fix non-ASCII characters in R source files for CRAN compliance."""
import os

BASE = 'C:/R_packages/HealthMarkers/R/'

# ---- kidney_kfre.R: em-dash in sprintf string ----
with open(BASE + 'kidney_kfre.R', 'rb') as f:
    txt = f.read().decode('utf-8')
txt = txt.replace('\u2014', '--')
with open(BASE + 'kidney_kfre.R', 'w', encoding='ascii', errors='backslashreplace') as f:
    f.write(txt)
print('kidney_kfre.R done')

# ---- utils_helpers.R: em-dash and arrows in comments ----
with open(BASE + 'utils_helpers.R', 'rb') as f:
    txt = f.read().decode('utf-8')
txt = txt.replace('\u2014', '--')
txt = txt.replace('\u2192', '->')
txt = txt.replace('\u2194', '<->')
txt = txt.replace('\u00b5', 'mu')
with open(BASE + 'utils_helpers.R', 'w', encoding='ascii', errors='backslashreplace') as f:
    f.write(txt)
print('utils_helpers.R done')

# ---- utils_infer-cols.R ----
with open(BASE + 'utils_infer-cols.R', 'rb') as f:
    txt = f.read().decode('utf-8')

# String literals: preserve the Unicode but as \uXXXX escape so the file is ASCII
# The order matters: do string literals BEFORE bulk-replacing the chars in comments

# "ikä" -> "ik\u00e4"  (Finnish age column name)
txt = txt.replace('"ik\u00e4"', '"ik\\u00e4"')
# "kjønn" -> "kj\u00f8nn"  (Norwegian sex)
txt = txt.replace('"kj\u00f8nn"', '"kj\\u00f8nn"')
# "kön" -> "k\u00f6n"  (Swedish sex)
txt = txt.replace('"k\u00f6n"', '"k\\u00f6n"')
# "kropsvægt" -> "kropsv\u00e6gt"  (Danish weight)
txt = txt.replace('"kropsv\u00e6gt"', '"kropsv\\u00e6gt"')
# "vyotaronympärys" -> "vyotaronymp\u00e4rys"  (Finnish waist)
txt = txt.replace('"vyotaronymp\u00e4rys"', '"vyotaronymp\\u00e4rys"')
# "lantionympärys" and "lantionympärys_cm"  (Finnish hip)
txt = txt.replace('"lantionymp\u00e4rys_cm"', '"lantionymp\\u00e4rys_cm"')
txt = txt.replace('"lantionymp\u00e4rys"', '"lantionymp\\u00e4rys"')

# Now replace remaining non-ASCII (in comments) with ASCII equivalents
txt = txt.replace('\u00f8', 'o')   # ø -> o  (Tromsø -> Tromso)
txt = txt.replace('\u00e4', 'a')   # ä -> a
txt = txt.replace('\u00f6', 'o')   # ö -> o
txt = txt.replace('\u00e6', 'ae')  # æ -> ae
txt = txt.replace('\u2014', '--')  # em-dash -> --
txt = txt.replace('\u00e5', 'a')   # å -> a
txt = txt.replace('\u00c5', 'A')   # Å -> A

with open(BASE + 'utils_infer-cols.R', 'w', encoding='ascii', errors='backslashreplace') as f:
    f.write(txt)
print('utils_infer-cols.R done')

# Verify no non-ASCII remains
for fname in ['kidney_kfre.R', 'utils_helpers.R', 'utils_infer-cols.R']:
    with open(BASE + fname, 'rb') as f:
        content = f.read()
    non_ascii = [b for b in content if b > 127]
    if non_ascii:
        print(f'WARNING: {fname} still has {len(non_ascii)} non-ASCII bytes!')
    else:
        print(f'OK: {fname} is clean')
