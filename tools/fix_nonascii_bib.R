bib_path <- "c:/R_packages/HealthMarkers/inst/REFERENCES.bib"
bib <- readLines(bib_path, encoding = "UTF-8")
Encoding(bib) <- "UTF-8"

# The mojibake for en-dash: U+00E2 U+20AC U+201C (double-encoded Windows-1252)
mojibake_endash <- "\u00e2\u20ac\u201c"
# Also check for em-dash mojibake: U+00E2 U+20AC U+201D
mojibake_emdash <- "\u00e2\u20ac\u201d"

idx <- grep(mojibake_endash, bib, fixed = TRUE)
cat("Lines with en-dash mojibake:", length(idx), "\n")
if (length(idx)) {
  for (i in idx) cat("  Line", i, ":", substr(bib[i], 1, 80), "\n")
  bib[idx] <- gsub(mojibake_endash, "--", bib[idx], fixed = TRUE)
  cat("Fixed en-dash mojibake\n")
}

idx2 <- grep(mojibake_emdash, bib, fixed = TRUE)
cat("Lines with em-dash mojibake:", length(idx2), "\n")
if (length(idx2)) {
  for (i in idx2) cat("  Line", i, ":", substr(bib[i], 1, 80), "\n")
  bib[idx2] <- gsub(mojibake_emdash, "---", bib[idx2], fixed = TRUE)
  cat("Fixed em-dash mojibake\n")
}

# Also check for the other common pattern: "â€" = U+00E2 U+20AC U+201C
# (same as above - already handled)

# Check for any remaining non-ASCII in used entries
cat("\nChecking for remaining non-ASCII in used entries...\n")
# These are the keys used in R/ files
used_keys <- c("katz2000quicki","amato2010vai","kahn2005lap","guerreroromero2018tyg",
  "dobiasova2001atherogenic","belfiore1998insulin","raynaud1999fasting","cole1992lms",
  "who1995anthro","mcewen1993allostatic","mclean2014fnih","dobiasova2004aip",
  "castelli1977framingham","celli2004bode","woo2002osta","deonis2006who","kdigo2012ckd",
  "payne1973","hippisleycox2017qrisk3","goff2014accaha","matthews1985homa",
  "avignon1999ogtt","sluiter1976gtolerance","hanson2000evaluation","anderson1995exploration",
  "mitnitski2001deficit","rockwood2007frailty","searle2008frailtyindex","rockwood2020cfs",
  "cesari2014frailtyphenotype","kanis2008frax","kanis2020fraxoverview","paulmichl2016spise",
  "bellogaytan2018metsir","fruhbeck2018adiponectinleptin","simentalmendia2008tyg",
  "yang2006adiponectininsulin","rubin1987mi","stekhoven2012missforest","sayed2021iage",
  "zahorec2001","tangri2011kfre","fuchs1998ktr","friedewald1972","lee2016tyg","lee2020tygbmi",
  "lee2010hsi","kotronen2009nafldlfs","bedogni2006fli","angulo2007nfs","wai2003apri",
  "sterling2006fib4","harrison2008bard","johnson2015albi","heuman2007meldxi",
  "gurka2014metsss","deboer2015metss","gurka2017metss","deboer2018metss","simren2022nfl",
  "harris2004omega3","koga2010glyalb","block1998calcp","waikar2009creak","quetelet1842",
  "who1995physicalstatus","guerrero1999avi","bergman2011bai","krakauer2012absi",
  "thomas2013bri","valdez1991ci","woolcott2018rfm","calle1999bmi","freedman2012bai",
  "he2013absi","maessen2014bri","matsuda1999ogtt","gutt2000isi","stumvoll2000ogtt",
  "hansen2007bigtt","glutathione_redox_review","kroenke2001phq9","spitzer2006gad7",
  "prochaska2012k6","topp2015who5","bastien2001isi","hirschfeld2000mdq","adler2006asrs",
  "patton1995bis","raine1991spq","quanjer2012","hankinson1999spirometry","bowerman2023gli",
  "levey2009ckdepi","inker2012cys","inker2021racefree","waikar2009feu","parikh2011ngal",
  "vaidya2010kim1","portilla2008lfabp","malmstrom2013sarcf","miller2005spirometry",
  "groop1989tracer","steele1959glucose","boston2003minmod","roden1996ffa","gastaldelli2004betacell",
  "karpe2011ffa","petersen2007muscleinsulin","santomauro1999acipimox","iomm2011calciumvitd",
  "kuczmarski2000cdc","woo2002osta","holick2011vitd")

# Find each key's block and check for non-ASCII
for (k in used_keys) {
  ki <- grep(paste0("^@.*\\{", k, ","), bib)
  if (length(ki) == 0) next
  j <- ki + 1
  while (j <= length(bib) && !grepl("^\\}", bib[j])) j <- j + 1
  block <- bib[ki:j]
  # Check for non-ASCII outside of LaTeX escapes
  # Remove LaTeX escape sequences first
  clean <- gsub("\\\\['\"`^~=.][a-zA-Z]|\\\\[a-zA-Z]+\\{[^}]*\\}|\\{[^}]*\\}", "", block)
  has_nonascii <- any(grepl("[^\x01-\x7F]", clean))
  if (has_nonascii) {
    bad <- block[grepl("[^\x01-\x7F]", block)]
    cat("NON-ASCII in", k, ":", paste(bad, collapse=" | "), "\n")
  }
}

writeLines(bib, bib_path, useBytes = FALSE)
cat("\nDone. File written with", length(bib), "lines.\n")
