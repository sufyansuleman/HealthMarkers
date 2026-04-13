devtools::load_all(quiet=TRUE)
d <- readRDS("inst/extdata/simulated_hm_data.rds")

# Test each group individually and capture results
groups_to_test <- c(
  "lipid", "atherogenic", "cvd_aip", "cvd_ldl_particles",
  "liver", "liver_fat", "glycemic", "mets", "renal", "urine",
  "nutrient", "vitamin", "vitamin_d_status", "calcium_corrected",
  "hormone", "inflammatory", "bone", "oxidative",
  "frailty_index", "pulmo", "sweat", "saliva",
  "kyn_trp", "nfl", "tracer_dxa",
  "obesity", "insulin_fasting", "insulin_ogtt",
  "ckd_kfre", "psych", "charlson", "frax",
  "cvd_qrisk3", "cvd_ascvd", "cvd_scorescvd", "cvd_stroke",
  "iage", "allostatic_load"
)

for (g in groups_to_test) {
  tryCatch({
    r <- suppressWarnings(all_health_markers(d, which=g, include_insulin=FALSE, verbose=FALSE))
    new_n <- ncol(r) - ncol(d)
    cat(sprintf("  OK  %-25s +%d cols\n", g, new_n))
  }, error=function(e) {
    cat(sprintf(" ERR  %-25s %s\n", g, conditionMessage(e)))
  })
}
