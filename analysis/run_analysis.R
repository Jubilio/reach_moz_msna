# =============================================================================
# Mozambique MSNA 2026 — Integrated Analysis Pipeline
# EduAnalysisTool (martivit/IMPACT) & Humind MSNI Workflow (IMPACT Initiatives)
# =============================================================================

suppressPackageStartupMessages({
  library(dplyr)
  library(readr)
  library(openxlsx)
  library(here)
  library(humind)
  library(fs)
})

message("==================================================================")
message("   MOÇAMBIQUE MSNA 2026 — PIPELINE DE ANÁLISE INTEGRADA (EDU & MSNI)")
message("==================================================================")

# 1. Carregar Configurações e Módulos -----------------------------------------
source(here::here("analysis", "config", "config.R"))
source(here::here("analysis", "R", "00_utils.R"))
source(here::here("analysis", "R", "01_edu_indicators.R"))
source(here::here("analysis", "R", "02_msni_indicators.R"))
source(here::here("analysis", "R", "03_analysis_engine.R"))
source(here::here("analysis", "R", "04_export_outputs.R"))

# 2. Carregar Dados Limpos mais Recentes ---------------------------------------
data_tables <- load_latest_clean_data(outputs_dir = cfg$moz_outputs_dir)
main_raw    <- data_tables$Submissions
edu_raw     <- data_tables$edu_ind
health_raw  <- data_tables$health_ind

message("Dados carregados:")
message("  - Domicílios (Submissions): ", nrow(main_raw), " registos")
message("  - Indivíduos em Educação (edu_ind): ", nrow(edu_raw), " registos")
if (!is.null(health_raw)) {
  message("  - Indivíduos em Saúde (health_ind): ", nrow(health_raw), " registos")
}

# 3. Executar Módulo de Educação (EduAnalysisTool & Humind) -------------------
edu_out <- compute_education_indicators(
  edu_loop = edu_raw,
  main_df  = main_raw,
  cfg      = cfg
)
edu_processed <- edu_out$edu_loop
main_with_edu <- edu_out$main

# 4. Executar Módulo de Compósitos Setoriais & MSNI (Humind MSNI) -------------
main_processed <- compute_msni_indicators(
  main_df     = main_with_edu,
  health_loop = health_raw,
  cfg         = cfg
)

# 5. Executar Motor de Análise Estatística (Desagregações) --------------------
analysis_results <- run_full_analysis(
  edu_loop = edu_processed,
  main_df  = main_processed,
  cfg      = cfg
)

# 6. Exportar Resultados em Excel (.xlsx) e RDS -------------------------------
exports <- export_analysis_results(
  analysis_results = analysis_results,
  main_df          = main_processed,
  edu_df           = edu_processed,
  cfg              = cfg
)

# 7. Resumo Final no Console --------------------------------------------------
message("\n==================================================================")
message("              RESUMO DOS RESULTADOS DA ANÁLISE")
message("==================================================================")
nat_msni <- analysis_results$msni$MSNI_National
nat_edu  <- analysis_results$education$Edu_National

cat("DOMICÍLIOS ANALISADOS: ", format(nat_msni$Sample_Size, big.mark = "."), "\n")
cat("CRIANÇAS EM IDADE ESCOLAR (5-17): ", format(nat_edu$Sample_Size, big.mark = "."), "\n\n")

cat("ÍNDICE MULTISSETORIAL DE NECESSIDADES (MSNI):\n")
cat("  - Domicílios em Necessidade (Score >= 3): ", nat_msni$msni_in_need__mean, "%\n", sep = "")
cat("  - Domicílios em Necessidade Severa (Score >= 4): ", nat_msni$msni_in_severe_need__mean, "%\n\n", sep = "")

cat("COMPÓSITOS SETORIAIS (% EM NECESSIDADE >= 3):\n")
cat("  - Segurança Alimentar (FSL LSG): ", nat_msni$comp_foodsec_in_need__mean, "%\n", sep = "")
cat("  - Água, Saneamento e Higiene (WASH LSG): ", nat_msni$comp_wash_in_need__mean, "%\n", sep = "")
cat("  - Educação (Edu LSG): ", nat_msni$comp_edu_in_need__mean, "%\n", sep = "")
cat("  - Saúde (Health LSG): ", nat_msni$comp_health_in_need__mean, "%\n", sep = "")
cat("  - Abrigo e Artigos Não-Alimentares (SNFI LSG): ", nat_msni$comp_snfi_in_need__mean, "%\n", sep = "")
cat("  - Proteção (Prot LSG): ", nat_msni$comp_prot_in_need__mean, "%\n\n", sep = "")

cat("INDICADORES PRINCIPAIS DE EDUCAÇÃO (EduAnalysisTool):\n")
cat("  - Taxa de Crianças Fora da Escola (OOS Rate): ", nat_edu$is_out_of_school__mean, "%\n", sep = "")
cat("  - Taxa de Sobre-Idade (>= 2 anos acima da idade prevista): ", nat_edu$is_overage_2plus__mean, "%\n", sep = "")
cat("  - Interrupção Escolar Recente (Qualquer Evento): ", nat_edu$any_disruption__mean, "%\n", sep = "")
cat("  - Barreira Financeira/Custos: ", nat_edu$barrier_cost__mean, "%\n", sep = "")
cat("  - Barreira de Distância até à Escola: ", nat_edu$barrier_distance__mean, "%\n", sep = "")
cat("  - Barreira de Segurança / Proteção no Trajeto: ", nat_edu$barrier_safety__mean, "%\n", sep = "")

message("==================================================================")
message("Resultados exportados em:")
message("  Excel: ", exports$xlsx)
message("  RDS:   ", exports$rds)
message("==================================================================")
