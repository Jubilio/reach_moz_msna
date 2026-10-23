# =============================================================================
# Mozambique MSNA 2026 — Analysis Configuration (English)
# =============================================================================

cfg <- list(
  country_code = "MOZ",
  assessment   = "MSNA 2026",

  # Directory paths
  moz_outputs_dir = here::here("outputs", "moz"),
  output_dir      = here::here("analysis", "output"),
  reports_dir     = here::here("analysis", "reports"),

  # Education Sector Settings (EduAnalysisTool & Mozambique National Curriculum - SNE)
  education = list(
    age_min = 5,
    age_max = 17,
    ece_age = 5,
    primary_age_min = 6,
    primary_age_max = 11,
    lower_sec_age_min = 12,
    lower_sec_age_max = 15,
    upper_sec_age_min = 16,
    upper_sec_age_max = 17,
    isced_mapping_path = here::here("analysis", "config", "moz_isced_mapping.csv")
  ),

  # Food Security & Livelihoods (FSL) Settings
  fsl = list(
    fcs_cutoffs = "normal", # normal: <=21 Poor, 21.5-35 Borderline, >35 Acceptable
    lcsi_mapping_path = here::here("analysis", "config", "lcsi_strategies.csv")
  ),

  # Standard Disaggregation Dimensions
  disaggregations = list(
    admin1           = "admin1",
    admin2           = "admin2",
    population_group = "population_group",
    hoh_gender       = "hoh_gender"
  )
)
