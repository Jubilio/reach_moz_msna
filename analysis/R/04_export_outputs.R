# =============================================================================
# 04_export_outputs.R — Excel Workbook & RDS Export Engine
# =============================================================================

suppressPackageStartupMessages({
  library(openxlsx)
  library(fs)
})

#' Exporta todas as tabelas analíticas para um livro Excel (.xlsx) profissional
#' e salva os datasets enriquecidos em formato .rds
export_analysis_results <- function(analysis_results, main_df, edu_df, cfg) {
  out_dir <- cfg$output_dir
  fs::dir_create(out_dir)

  timestamp <- format(Sys.time(), "%Y%m%d_%H%M%S")
  xlsx_file <- file.path(out_dir, paste0("MSNA_2026_MOZ_Analysis_Results_", timestamp, ".xlsx"))
  rds_file  <- file.path(out_dir, paste0("MSNA_2026_MOZ_Analyzed_Data_", timestamp, ".rds"))

  message("--> [Export Engine] Exportando resultados para: ", xlsx_file)

  wb <- openxlsx::createWorkbook()

  # Estilos do Excel
  header_style <- openxlsx::createStyle(
    fontName = "Arial", fontSize = 11, fontColour = "#FFFFFF",
    fgFill = "#002060", halign = "center", valign = "center", textDecoration = "bold",
    border = "TopBottomLeftRight", borderColour = "#000000"
  )
  body_style <- openxlsx::createStyle(
    fontName = "Arial", fontSize = 10, halign = "left", valign = "center",
    border = "TopBottomLeftRight", borderColour = "#D9D9D9"
  )
  num_style <- openxlsx::createStyle(
    fontName = "Arial", fontSize = 10, halign = "right", valign = "center",
    border = "TopBottomLeftRight", borderColour = "#D9D9D9"
  )

  add_formatted_sheet <- function(wb, sheet_name, data) {
    if (is.null(data)) return(invisible(NULL))
    data <- as.data.frame(data)
    if (ncol(data) == 0 || nrow(data) == 0) return(invisible(NULL))

    # Limitar nome da folha a 31 caracteres (requisito Excel)
    clean_name <- substr(gsub("[^A-Za-z0-9_]", "_", sheet_name), 1, 31)
    openxlsx::addWorksheet(wb, clean_name)
    openxlsx::writeData(wb, clean_name, data, startRow = 1, startCol = 1)
    openxlsx::addStyle(wb, clean_name, header_style, rows = 1, cols = 1:ncol(data), gridExpand = TRUE)
    openxlsx::addStyle(wb, clean_name, body_style, rows = 2:(nrow(data) + 1), cols = 1:ncol(data), gridExpand = TRUE)
    openxlsx::setColWidths(wb, clean_name, cols = 1:ncol(data), widths = "auto")
  }

  # Folha de Capa
  cover_data <- data.frame(
    Metadado = c("País", "Avaliação", "Data de Geração", "Pacotes Utilizados", "Status"),
    Valor = c(
      "Moçambique (MOZ)",
      "MSNA 2026 (REACH Initiatives)",
      as.character(Sys.time()),
      "EduAnalysisTool (martivit/IMPACT) & Humind MSNI Workflow (IMPACT Initiatives)",
      "Análise Preliminar Validada"
    ),
    stringsAsFactors = FALSE
  )
  add_formatted_sheet(wb, "Cover", cover_data)

  # Adicionar tabelas de MSNI
  add_formatted_sheet(wb, "MSNI_National", analysis_results$msni$MSNI_National)
  add_formatted_sheet(wb, "MSNI_Provincial", analysis_results$msni$MSNI_Admin1)
  add_formatted_sheet(wb, "MSNI_District", analysis_results$msni$MSNI_Admin2)
  if ("MSNI_DispStatus" %in% names(analysis_results$msni)) {
    add_formatted_sheet(wb, "MSNI_Displacement", analysis_results$msni$MSNI_DispStatus)
  }
  if ("MSNI_HoHGender" %in% names(analysis_results$msni)) {
    add_formatted_sheet(wb, "MSNI_HoHGender", analysis_results$msni$MSNI_HoHGender)
  }
  add_formatted_sheet(wb, "MSNI_Phases_Prov", analysis_results$msni_distribution)

  # Adicionar tabelas de Educação
  add_formatted_sheet(wb, "Edu_National", analysis_results$education$Edu_National)
  add_formatted_sheet(wb, "Edu_Provincial", analysis_results$education$Edu_Admin1)
  add_formatted_sheet(wb, "Edu_Gender_Prov", analysis_results$education$Edu_Gender_Admin1)
  add_formatted_sheet(wb, "Edu_Age_Prov", analysis_results$education$Edu_AgeCat_Admin1)
  add_formatted_sheet(wb, "Edu_District", analysis_results$education$Edu_Admin2)

  openxlsx::saveWorkbook(wb, xlsx_file, overwrite = TRUE)

  # Salvar pacote RDS completo
  message("--> [Export Engine] Salvando dados enriquecidos em: ", rds_file)
  saveRDS(
    list(
      main_with_indicators = main_df,
      edu_with_indicators  = edu_df,
      tables               = analysis_results,
      exported_at          = Sys.time()
    ),
    rds_file
  )

  message("--> [Export Engine] Exportação concluída com sucesso!")
  return(list(xlsx = xlsx_file, rds = rds_file))
}
