# =============================================================================
# Mozambique MSNA 2026 — Analysis Configuration
# =============================================================================

cfg <- list(
  country_code = "MOZ",
  assessment   = "MSNA 2026",

  # Caminhos
  moz_outputs_dir = here::here("outputs", "moz"),
  output_dir      = here::here("analysis", "output"),

  # Configurações de Educação (EduAnalysisTool & SNE Moçambique)
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

  # Configurações de Segurança Alimentar (FSL)
  fsl = list(
    fcs_cutoffs = "normal", # normal: <=21 Poor, 21.5-35 Borderline, >35 Acceptable
    lcsi_mapping_path = here::here("analysis", "config", "lcsi_strategies.csv")
  ),

  # Variáveis de Desagregação Padrão
  disaggregations = list(
    admin1       = "admin1",
    admin2       = "admin2",
    disp_status  = "disp_status",
    hoh_gender   = "hoh_gender"
  )
)
