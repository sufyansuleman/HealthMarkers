Set-Location "c:\R_packages\HealthMarkers"
$bib = [System.IO.File]::ReadAllText("inst\REFERENCES.bib", [System.Text.Encoding]::UTF8)

# ── Fix 1: glutathione_redox_review (placeholder → real paper) ──────────────
$old = "@article{glutathione_redox_review,`r`n  author  = {General, Reference and Review Authors},`r`n  title   = {Glutathione redox status as an oxidative stress indicator: clinical laboratory perspectives},`r`n  journal = {Clinical Biochemistry (placeholder)},`r`n  year    = {2010},`r`n  note    = {placeholder â€" replace with canonical review/reference if desired}`r`n}"
$new = "@article{glutathione_redox_review,`r`n  author  = {Forman, Henry Jay and Zhang, Hongqiao and Rinna, Alessandra},`r`n  title   = {Glutathione: Overview of its protective roles, measurement, and biosynthesis},`r`n  journal = {Molecular Aspects of Medicine},`r`n  year    = {2009},`r`n  volume  = {30},`r`n  number  = {1--2},`r`n  pages   = {1--12},`r`n  doi     = {10.1016/j.mam.2008.08.006}`r`n}"
if ($bib.Contains($old)) { $bib = $bib.Replace($old, $new); Write-Host "Fix 1 OK: glutathione_redox_review" } else { Write-Host "WARN Fix 1 not matched" }

# ── Fix 2: payne1973 (placeholder → real Payne 1973 BMJ paper) ───────────────
$old = "@misc{payne1973,`r`n  title = {Payne formula for albumin-corrected calcium},`r`n  author = {Payne, R.},`r`n  year = {1973},`r`n  note = {Original Payne correction; formula: Corrected Ca = measured Ca + 0.8 * (4.0 - albumin). Placeholder entry â€" replace with canonical citation if available.}`r`n}"
$new = "@article{payne1973,`r`n  author  = {Payne, R. B. and Little, A. J. and Williams, R. B. and Milner, J. R.},`r`n  title   = {Interpretation of serum calcium in patients with abnormal serum proteins},`r`n  journal = {British Medical Journal},`r`n  year    = {1973},`r`n  volume  = {4},`r`n  number  = {5893},`r`n  pages   = {643--646},`r`n  doi     = {10.1136/bmj.4.5893.643}`r`n}"
if ($bib.Contains($old)) { $bib = $bib.Replace($old, $new); Write-Host "Fix 2 OK: payne1973" } else { Write-Host "WARN Fix 2 not matched" }

# ── Fix 3: lee2010hsi (placeholder → real Lee et al 2010 DLD paper) ──────────
$old = "@article{lee2010hsi,`r`n  author  = {Lee, J. H. and others},`r`n  title   = {Hepatic Steatosis Index (HSI): original description and validation},`r`n  journal = {Clinical and Experimental Hepatology (placeholder)},`r`n  year    = {2010},`r`n  note    = {placeholder entry â€" replace with canonical citation if available}`r`n}"
$new = "@article{lee2010hsi,`r`n  author  = {Lee, Jae-Hun and Kim, Donghee and Kim, Hyun Jung and Lee, Chang Hyeong and Yang, Jong In and Kim, Won and Kim, Yoon Jun and Yoon, Jung-Hwan and Cho, Sang-Heon and Sung, Myung-Whun and Lee, Hyo-Suk},`r`n  title   = {Hepatic steatosis index: a simple screening tool reflecting nonalcoholic fatty liver disease},`r`n  journal = {Digestive and Liver Disease},`r`n  year    = {2010},`r`n  volume  = {42},`r`n  number  = {7},`r`n  pages   = {503--508},`r`n  doi     = {10.1016/j.dld.2009.08.002}`r`n}"
if ($bib.Contains($old)) { $bib = $bib.Replace($old, $new); Write-Host "Fix 3 OK: lee2010hsi" } else { Write-Host "WARN Fix 3 not matched" }

# ── Fix 4: kotronen2009nafldlfs (placeholder → real Kotronen 2009 paper) ─────
$old = "@article{kotronen2009nafldlfs,`r`n  author  = {Kotronen, Anna and others},`r`n  title   = {NAFLD Liver Fat Score: development and validation},`r`n  journal = {Journal (placeholder)},`r`n  year    = {2009},`r`n  note    = {placeholder entry â€" replace with canonical citation if available}`r`n}"
$new = "@article{kotronen2009nafldlfs,`r`n  author  = {Kotronen, Anna and Peltonen, Markku and Hakkarainen, Antti and Sevastianova, Ksenia and Bergholm, Robert and Johansson, Lovisa M and Lundbom, Nina and Rissanen, Aila and Ridderstrale, Martin and Groop, Leif and Orho-Melander, Marju and {Yki-J{""a}rvinen}, Hannele},`r`n  title   = {Prediction of non-alcoholic fatty liver disease and liver fat using metabolic and genetic factors},`r`n  journal = {Gastroenterology},`r`n  year    = {2009},`r`n  volume  = {137},`r`n  number  = {3},`r`n  pages   = {865--872},`r`n  doi     = {10.1053/j.gastro.2009.06.005}`r`n}"
if ($bib.Contains($old)) { $bib = $bib.Replace($old, $new); Write-Host "Fix 4 OK: kotronen2009nafldlfs" } else { Write-Host "WARN Fix 4 not matched" }

# ── Fix 5: dobiasova2001atherogenic — 2004 content → correct 2001 paper ──────
$old = "@article{dobiasova2001atherogenic,`r`n  title   = {Atherogenic Index of Plasma (Log(Triglycerides/HDL-Cholesterol)): Theoretical and Practical Implications},`r`n  author  = {Dobi{\'a}\v{s}ov{\'a}, Michaela},`r`n  journal = {Clinical Chemistry and Laboratory Medicine},`r`n  year    = {2004},`r`n  volume  = {42},`r`n  number  = {12},`r`n  pages   = {1355--1363},`r`n  doi     = {10.1515/CCLM.2004.258}`r`n}"
$new = "@article{dobiasova2001atherogenic,`r`n  title   = {The Plasma Parameter Log(TG/HDL-C) as an Atherogenic Index: Correlation with Lipoprotein Particle Size and Esterification Rate in ApoB-Lipoprotein-Depleted Plasma},`r`n  author  = {Dobi{\'a}\v{s}ov{\'a}, Michaela and Frohlich, Jiri J.},`r`n  journal = {Clinical Biochemistry},`r`n  year    = {2001},`r`n  volume  = {34},`r`n  number  = {7},`r`n  pages   = {583--588},`r`n  doi     = {10.1016/S0009-9120(01)00263-6}`r`n}"
if ($bib.Contains($old)) { $bib = $bib.Replace($old, $new); Write-Host "Fix 5 OK: dobiasova2001atherogenic" } else { Write-Host "WARN Fix 5 not matched" }

# ── Fix 6: quanjer2012 — mojibake in title (3â€"95 → 3--95) ──────────────────
$old = "3â€"95-yr age range"
$new = "3--95-yr age range"
if ($bib.Contains($old)) { $bib = $bib.Replace($old, $new); Write-Host "Fix 6 OK: quanjer2012 mojibake" } else { Write-Host "WARN Fix 6 not matched" }

# ── Fix 7: Add DOI to sayed2021iage ──────────────────────────────────────────
$old = "@article{sayed2021iage,`r`n  title   = {An inflammatory aging clock (iAge) predicts multimorbidity, immunosenescence, frailty and cardiovascular aging},`r`n  author  = {Sayed, N and others},`r`n  journal = {Nature Aging},`r`n  year    = {2021},`r`n  volume  = {1},`r`n  pages   = {598--610},`r`n}"
$new = "@article{sayed2021iage,`r`n  title   = {An inflammatory aging clock ({iAge}) based on deep learning predicts multimorbidity, immunosenescence, frailty and cardiovascular aging},`r`n  author  = {Sayed, Nazish and Huang, Yingxiang and Nguyen, Khiem and others},`r`n  journal = {Nature Aging},`r`n  year    = {2021},`r`n  volume  = {1},`r`n  pages   = {598--615},`r`n  doi     = {10.1038/s43587-021-00082-y}`r`n}"
if ($bib.Contains($old)) { $bib = $bib.Replace($old, $new); Write-Host "Fix 7 OK: sayed2021iage DOI" } else { Write-Host "WARN Fix 7 not matched" }

# ── Fix 8: Add DOI to celli2004bode ──────────────────────────────────────────
$old = "@article{celli2004bode,`r`n  title = {The {BODE} Index in Chronic Obstructive Pulmonary Disease},`r`n  author = {Celli, Burton R and Cote, Cecile G and Marin, Jose M and Casanova, C and Montes de Oca, Miguel and Mendez, R and Pinto-Plata, Victor and Cabral, Hector J},`r`n  journal = {The New England Journal of Medicine},`r`n  year = {2004},`r`n  volume = {350},`r`n  pages = {1005--1012},`r`n}"
$new = "@article{celli2004bode,`r`n  title = {The {BODE} Index in Chronic Obstructive Pulmonary Disease},`r`n  author = {Celli, Burton R and Cote, Cecile G and Marin, Jose M and Casanova, C and Montes de Oca, Miguel and Mendez, R and Pinto-Plata, Victor and Cabral, Hector J},`r`n  journal = {The New England Journal of Medicine},`r`n  year = {2004},`r`n  volume = {350},`r`n  number = {10},`r`n  pages = {1005--1012},`r`n  doi   = {10.1056/NEJMoa021322}`r`n}"
if ($bib.Contains($old)) { $bib = $bib.Replace($old, $new); Write-Host "Fix 8 OK: celli2004bode DOI" } else { Write-Host "WARN Fix 8 not matched" }

# ── Fix 9: Add DOI to quanjer2012 ────────────────────────────────────────────
$old = "  pages = {1324--1343},`r`n}"
$new = "  pages = {1324--1343},`r`n  doi   = {10.1183/09031936.00080312}`r`n}"
# Be specific to quanjer entry only by matching surrounding context too
$old = "  author = {Quanjer, P. H. and Stanojevic, S. and Cole, T. J. and Baur, X. and Hall, G. L. and Culver, B. H. and et al.},`r`n  title = {Multi-ethnic reference values for spirometry for the 3--95-yr age range: the global lung function 2012 equations},`r`n  journal = {European Respiratory Journal},`r`n  year = {2012},`r`n  volume = {40},`r`n  pages = {1324--1343},`r`n}"
$new = "  author = {Quanjer, P. H. and Stanojevic, S. and Cole, T. J. and Baur, X. and Hall, G. L. and Culver, B. H. and et al.},`r`n  title = {Multi-ethnic reference values for spirometry for the 3--95-yr age range: the global lung function 2012 equations},`r`n  journal = {European Respiratory Journal},`r`n  year = {2012},`r`n  volume = {40},`r`n  pages = {1324--1343},`r`n  doi   = {10.1183/09031936.00080312}`r`n}"
if ($bib.Contains($old)) { $bib = $bib.Replace($old, $new); Write-Host "Fix 9 OK: quanjer2012 DOI" } else { Write-Host "WARN Fix 9 not matched" }

# ── Fix 10: Add DOI to gurka2014metsss ───────────────────────────────────────
$old = "  title   = {Metabolic syndrome severity score and cardiovascular disease risk},`r`n  journal = {Journal of Clinical Endocrinology & Metabolism},`r`n  year    = {2014},`r`n  volume  = {99},`r`n  number  = {3},`r`n  pages   = {1073--1081},`r`n}"
$new = "  title   = {Metabolic syndrome severity score and cardiovascular disease risk},`r`n  journal = {Journal of Clinical Endocrinology \& Metabolism},`r`n  year    = {2014},`r`n  volume  = {99},`r`n  number  = {3},`r`n  pages   = {1073--1081},`r`n  doi     = {10.1210/jc.2013-4116}`r`n}"
if ($bib.Contains($old)) { $bib = $bib.Replace($old, $new); Write-Host "Fix 10 OK: gurka2014metsss DOI" } else { Write-Host "WARN Fix 10 not matched" }

# ── Fix 11: Add DOI to deboer2015metss ───────────────────────────────────────
$old = "  title   = {Severity of metabolic syndrome and its association with risk for type 2 diabetes and cardiovascular disease},`r`n  journal = {Diabetologia},`r`n  year    = {2015},`r`n  volume  = {58},`r`n  number  = {12},`r`n  pages   = {2745--2752},`r`n}"
$new = "  title   = {Severity of metabolic syndrome and its association with risk for type 2 diabetes and cardiovascular disease},`r`n  journal = {Diabetologia},`r`n  year    = {2015},`r`n  volume  = {58},`r`n  number  = {12},`r`n  pages   = {2745--2752},`r`n  doi     = {10.1007/s00125-015-3782-5}`r`n}"
if ($bib.Contains($old)) { $bib = $bib.Replace($old, $new); Write-Host "Fix 11 OK: deboer2015metss DOI" } else { Write-Host "WARN Fix 11 not matched" }

# ── Fix 12: Add DOI to gurka2017metss ────────────────────────────────────────
$old = "  title   = {Independent associations between metabolic syndrome severity and future coronary heart disease by sex and race},`r`n  journal = {Diabetes Care},`r`n  year    = {2017},`r`n  volume  = {40},`r`n  number  = {11},`r`n  pages   = {1693--1701},`r`n}"
$new = "  title   = {Independent associations between metabolic syndrome severity and future coronary heart disease by sex and race},`r`n  journal = {Diabetes Care},`r`n  year    = {2017},`r`n  volume  = {40},`r`n  number  = {11},`r`n  pages   = {1693--1701},`r`n  doi     = {10.2337/dc17-0520}`r`n}"
if ($bib.Contains($old)) { $bib = $bib.Replace($old, $new); Write-Host "Fix 12 OK: gurka2017metss DOI" } else { Write-Host "WARN Fix 12 not matched" }

# ── Fix 13: Add DOI to goff2014accaha ────────────────────────────────────────
$old = "  note    = {Pooled Cohort Equations; ACC/AHA Task Force on Practice Guidelines}`r`n}"
$new = "  doi     = {10.1161/01.cir.0000437741.48606.98},`r`n  note    = {Pooled Cohort Equations; ACC/AHA Task Force on Practice Guidelines}`r`n}"
if ($bib.Contains($old)) { $bib = $bib.Replace($old, $new); Write-Host "Fix 13 OK: goff2014accaha DOI" } else { Write-Host "WARN Fix 13 not matched" }

# ── Fix 14: Add DOI to dobiasova2004aip ──────────────────────────────────────
$old = "@article{dobiasova2004aip,`r`n  title   = {Atherogenic Index of Plasma (AIP) log(Triglycerides/HDL-Cholesterol): Theoretical and Practical Implications},`r`n  author  = {Dobi{\'á}\v{s}ov{\'á}, Michaela},`r`n  journal = {Clinical Chemistry},`r`n  year    = {2004},`r`n  volume  = {50},`r`n  number  = {7},`r`n  pages   = {1113--1115},`r`n}"
$new = "@article{dobiasova2004aip,`r`n  title   = {Atherogenic Index of Plasma (AIP) log(Triglycerides/HDL-Cholesterol): Theoretical and Practical Implications},`r`n  author  = {Dobi{\'á}\v{š}ov{\'á}, Michaela},`r`n  journal = {Clinical Chemistry},`r`n  year    = {2004},`r`n  volume  = {50},`r`n  number  = {7},`r`n  pages   = {1113--1115},`r`n  doi     = {10.1373/clinchem.2004.033142}`r`n}"
if ($bib.Contains($old)) { $bib = $bib.Replace($old, $new); Write-Host "Fix 14a OK: dobiasova2004aip DOI" } else {
  # Try simpler approach - find the pages line unique to this entry
  $pattern = "(@article{dobiasova2004aip[^}]+pages\s+=\s+\{1113--1115\},)\r\n}"
  if ($bib -match $pattern) { Write-Host "Pattern 14 found" } else { Write-Host "WARN Fix 14 not matched either" }
}

# Write the updated file
[System.IO.File]::WriteAllText("inst\REFERENCES.bib", $bib, [System.Text.Encoding]::UTF8)
Write-Host "`nAll done. File written."
