bib_path <- "c:/R_packages/HealthMarkers/inst/REFERENCES.bib"
bib <- readLines(bib_path, encoding = "UTF-8")

cat("Total lines:", length(bib), "\n")

# Helper: replace a block identified by start pattern + end pattern
replace_block <- function(lines, start_pat, end_after_start = NULL, new_block) {
  start_i <- which(grepl(start_pat, lines, fixed = FALSE))
  if (length(start_i) == 0) {
    cat("WARN: pattern not found:", start_pat, "\n")
    return(lines)
  }
  if (length(start_i) > 1) {
    cat("WARN: multiple matches for:", start_pat, "\n")
    return(lines)
  }
  # Find closing }
  i <- start_i
  while (i <= length(lines) && !grepl("^\\}", lines[i])) i <- i + 1
  cat("Replacing lines", start_i, "to", i, "\n")
  c(lines[seq_len(start_i - 1)], new_block, lines[seq(i + 1, length(lines))])
}

# ── Fix 1: glutathione_redox_review ─────────────────────────────────────────
bib <- replace_block(bib, "^@article\\{glutathione_redox_review",
  new_block = c(
    "@article{glutathione_redox_review,",
    "  author  = {Forman, Henry Jay and Zhang, Hongqiao and Rinna, Alessandra},",
    "  title   = {Glutathione: Overview of its protective roles, measurement, and biosynthesis},",
    "  journal = {Molecular Aspects of Medicine},",
    "  year    = {2009},",
    "  volume  = {30},",
    "  number  = {1--2},",
    "  pages   = {1--12},",
    "  doi     = {10.1016/j.mam.2008.08.006}",
    "}"
  )
)
cat("Fix 1 done (glutathione_redox_review)\n")

# ── Fix 2: payne1973 ─────────────────────────────────────────────────────────
bib <- replace_block(bib, "^@misc\\{payne1973",
  new_block = c(
    "@article{payne1973,",
    "  author  = {Payne, R. B. and Little, A. J. and Williams, R. B. and Milner, J. R.},",
    "  title   = {Interpretation of serum calcium in patients with abnormal serum proteins},",
    "  journal = {British Medical Journal},",
    "  year    = {1973},",
    "  volume  = {4},",
    "  number  = {5893},",
    "  pages   = {643--646},",
    "  doi     = {10.1136/bmj.4.5893.643}",
    "}"
  )
)
cat("Fix 2 done (payne1973)\n")

# ── Fix 3: lee2010hsi ────────────────────────────────────────────────────────
bib <- replace_block(bib, "^@article\\{lee2010hsi",
  new_block = c(
    "@article{lee2010hsi,",
    "  author  = {Lee, Jae-Hun and Kim, Donghee and Kim, Hyun Jung and Lee, Chang Hyeong and",
    "             Yang, Jong In and Kim, Won and Kim, Yoon Jun and Yoon, Jung-Hwan and",
    "             Cho, Sang-Heon and Sung, Myung-Whun and Lee, Hyo-Suk},",
    "  title   = {Hepatic steatosis index: a simple screening tool reflecting nonalcoholic fatty liver disease},",
    "  journal = {Digestive and Liver Disease},",
    "  year    = {2010},",
    "  volume  = {42},",
    "  number  = {7},",
    "  pages   = {503--508},",
    "  doi     = {10.1016/j.dld.2009.08.002}",
    "}"
  )
)
cat("Fix 3 done (lee2010hsi)\n")

# ── Fix 4: kotronen2009nafldlfs ──────────────────────────────────────────────
bib <- replace_block(bib, "^@article\\{kotronen2009nafldlfs",
  new_block = c(
    "@article{kotronen2009nafldlfs,",
    "  author  = {Kotronen, Anna and Peltonen, Markku and Hakkarainen, Antti and Sevastianova, Ksenia and",
    "             Bergholm, Robert and Johansson, Lovisa M and Lundbom, Nina and Rissanen, Aila and",
    "             Ridderstrale, Martin and Groop, Leif and Orho-Melander, Marju and {Yki-J{\\\"a}rvinen}, Hannele},",
    "  title   = {Prediction of non-alcoholic fatty liver disease and liver fat using metabolic and genetic factors},",
    "  journal = {Gastroenterology},",
    "  year    = {2009},",
    "  volume  = {137},",
    "  number  = {3},",
    "  pages   = {865--872},",
    "  doi     = {10.1053/j.gastro.2009.06.005}",
    "}"
  )
)
cat("Fix 4 done (kotronen2009nafldlfs)\n")

# ── Fix 5: dobiasova2001atherogenic — correct content to actual 2001 paper ───
bib <- replace_block(bib, "^@article\\{dobiasova2001atherogenic",
  new_block = c(
    "@article{dobiasova2001atherogenic,",
    "  title   = {The Plasma Parameter Log(TG/HDL-C) as an Atherogenic Index: Correlation with",
    "             Lipoprotein Particle Size and Esterification Rate in ApoB-Lipoprotein-Depleted Plasma},",
    "  author  = {Dobi{\\'a}\\v{s}ov{\\'a}, Michaela and Frohlich, Jiri J.},",
    "  journal = {Clinical Biochemistry},",
    "  year    = {2001},",
    "  volume  = {34},",
    "  number  = {7},",
    "  pages   = {583--588},",
    "  doi     = {10.1016/S0009-9120(01)00263-6}",
    "}"
  )
)
cat("Fix 5 done (dobiasova2001atherogenic)\n")

# ── Fix 6: quanjer2012 — mojibake in title ───────────────────────────────────
idx <- grep("3.*95-yr age range", bib)
if (length(idx) > 0) {
  bib[idx] <- gsub("\u00e2\u0080\u0093|â€\"", "--", bib[idx])
  # Also check for raw bytes interpreted as UTF-8 mojibake
  bib[idx] <- gsub("3\u00e2\u0080\u009395", "3--95", bib[idx])
  cat("Fix 6 done (quanjer2012 title), line", idx, ":", bib[idx], "\n")
} else {
  cat("WARN Fix 6: quanjer2012 title line not found\n")
}

# ── Fix 7: Add DOI to celli2004bode ─────────────────────────────────────────
idx_celli_pages <- grep("^  pages = \\{1005--1012\\}", bib)
if (length(idx_celli_pages) == 1) {
  bib[idx_celli_pages] <- "  pages = {1005--1012},"
  bib <- append(bib, "  doi   = {10.1056/NEJMoa021322}", after = idx_celli_pages)
  # Remove trailing comma from old closing line
  idx_close <- idx_celli_pages + 1
  bib <- append(bib, "}", after = idx_close)
  bib <- bib[bib != bib[idx_close + 1] | seq_along(bib) != idx_close + 1]
  cat("Fix 7 attempted (celli2004bode DOI)\n")
} else {
  cat("WARN Fix 7: celli pages line matches:", length(idx_celli_pages), "\n")
}

# ── Fix 8: Add DOI to quanjer2012 ────────────────────────────────────────────
idx_q <- grep("^@article\\{quanjer2012", bib)
if (length(idx_q) == 1) {
  # Find the closing } for this entry
  j <- idx_q + 1
  while (j <= length(bib) && !grepl("^\\}", bib[j])) j <- j + 1
  # Insert DOI line before closing }
  bib <- append(bib, "  doi   = {10.1183/09031936.00080312}", after = j - 1)
  cat("Fix 8 done (quanjer2012 DOI)\n")
} else {
  cat("WARN Fix 8 not found\n")
}

# ── Fix 9: Add DOI to gurka2014metsss ────────────────────────────────────────
idx_g <- grep("^@article\\{gurka2014metsss", bib)
if (length(idx_g) == 1) {
  j <- idx_g + 1
  while (j <= length(bib) && !grepl("^\\}", bib[j])) j <- j + 1
  bib <- append(bib, "  doi     = {10.1210/jc.2013-4116}", after = j - 1)
  cat("Fix 9 done (gurka2014metsss DOI)\n")
} else { cat("WARN Fix 9 not found\n") }

# ── Fix 10: Add DOI to deboer2015metss ───────────────────────────────────────
idx_d <- grep("^@article\\{deboer2015metss", bib)
if (length(idx_d) == 1) {
  j <- idx_d + 1
  while (j <= length(bib) && !grepl("^\\}", bib[j])) j <- j + 1
  bib <- append(bib, "  doi     = {10.1007/s00125-015-3782-5}", after = j - 1)
  cat("Fix 10 done (deboer2015metss DOI)\n")
} else { cat("WARN Fix 10 not found\n") }

# ── Fix 11: Add DOI to gurka2017metss ────────────────────────────────────────
idx_g2 <- grep("^@article\\{gurka2017metss", bib)
if (length(idx_g2) == 1) {
  j <- idx_g2 + 1
  while (j <= length(bib) && !grepl("^\\}", bib[j])) j <- j + 1
  bib <- append(bib, "  doi     = {10.2337/dc17-0520}", after = j - 1)
  cat("Fix 11 done (gurka2017metss DOI)\n")
} else { cat("WARN Fix 11 not found\n") }

# ── Fix 12: Add DOI to goff2014accaha ────────────────────────────────────────
idx_acc <- grep("^@article\\{goff2014accaha", bib)
if (length(idx_acc) == 1) {
  j <- idx_acc + 1
  while (j <= length(bib) && !grepl("^\\}", bib[j])) j <- j + 1
  bib <- append(bib, "  doi     = {10.1161/01.cir.0000437741.48606.98}", after = j - 1)
  cat("Fix 12 done (goff2014accaha DOI)\n")
} else { cat("WARN Fix 12 not found\n") }

# ── Fix 13: Add DOI to dobiasova2004aip ──────────────────────────────────────
idx_aip <- grep("^@article\\{dobiasova2004aip", bib)
if (length(idx_aip) == 1) {
  j <- idx_aip + 1
  while (j <= length(bib) && !grepl("^\\}", bib[j])) j <- j + 1
  bib <- append(bib, "  doi     = {10.1373/clinchem.2004.033142}", after = j - 1)
  cat("Fix 13 done (dobiasova2004aip DOI)\n")
} else { cat("WARN Fix 13 not found\n") }

# ── Fix 14: Add DOI to sayed2021iage ─────────────────────────────────────────
idx_s <- grep("^@article\\{sayed2021iage", bib)
if (length(idx_s) == 1) {
  j <- idx_s + 1
  while (j <= length(bib) && !grepl("^\\}", bib[j])) j <- j + 1
  bib <- append(bib, "  doi     = {10.1038/s43587-021-00082-y}", after = j - 1)
  cat("Fix 14 done (sayed2021iage DOI)\n")
} else { cat("WARN Fix 14 not found\n") }

# ── Write back ────────────────────────────────────────────────────────────────
writeLines(bib, bib_path, useBytes = FALSE)
cat("\nDone. Total lines now:", length(bib), "\n")
