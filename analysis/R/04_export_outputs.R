# =============================================================================
# 04_export_outputs.R — Excel Workbook, HTML Report & RDS Export Engine (English)
# =============================================================================

suppressPackageStartupMessages({
  library(openxlsx)
  library(rmarkdown)
  library(fs)
  library(here)
})

#' Exports analytical tables to a formatted Excel workbook with human-readable English labels,
#' renders the interactive HTML report, and saves enriched datasets in RDS format.
export_analysis_results <- function(analysis_results, main_df, edu_df, cfg) {
  out_dir <- cfg$output_dir
  fs::dir_create(out_dir)

  timestamp <- format(Sys.time(), "%Y%m%d_%H%M%S")
  xlsx_file <- file.path(out_dir, paste0("MSNA_2026_MOZ_Analysis_Results_", timestamp, ".xlsx"))
  rds_file  <- file.path(out_dir, paste0("MSNA_2026_MOZ_Analyzed_Data_", timestamp, ".rds"))
  html_file <- file.path(out_dir, paste0("MSNA_2026_MOZ_Analysis_Report_", timestamp, ".html"))

  message("--> [Export Engine] Exporting Excel workbook with human-readable English labels...")

  wb <- openxlsx::createWorkbook()

  # Workbook Styles
  header_style <- openxlsx::createStyle(
    fontName = "Arial", fontSize = 11, fontColour = "#FFFFFF",
    fgFill = "#002060", halign = "center", valign = "center", textDecoration = "bold",
    border = "TopBottomLeftRight", borderColour = "#000000", wrapText = TRUE
  )
  body_style <- openxlsx::createStyle(
    fontName = "Arial", fontSize = 10, halign = "left", valign = "center",
    border = "TopBottomLeftRight", borderColour = "#D9D9D9"
  )

  add_formatted_sheet <- function(wb, sheet_name, data, apply_labels = TRUE) {
    if (is.null(data)) return(invisible(NULL))
    data <- as.data.frame(data)
    if (ncol(data) == 0 || nrow(data) == 0) return(invisible(NULL))

    if (apply_labels) {
      data <- apply_human_readable_labels(data)
    }

    # Limit worksheet name to 31 characters (Excel constraint)
    clean_name <- substr(gsub("[^A-Za-z0-9_]", "_", sheet_name), 1, 31)
    openxlsx::addWorksheet(wb, clean_name)
    openxlsx::writeData(wb, clean_name, data, startRow = 1, startCol = 1)
    openxlsx::addStyle(wb, clean_name, header_style, rows = 1, cols = 1:ncol(data), gridExpand = TRUE)
    openxlsx::addStyle(wb, clean_name, body_style, rows = 2:(nrow(data) + 1), cols = 1:ncol(data), gridExpand = TRUE)
    openxlsx::setColWidths(wb, clean_name, cols = 1:ncol(data), widths = "auto")
    openxlsx::setRowHeights(wb, clean_name, rows = 1, heights = 28)
  }

  # 1. Cover Sheet (Metadata)
  cover_data <- data.frame(
    Metadata = c(
      "Country", "Assessment", "Generation Date",
      "Core Frameworks", "Status", "Contact"
    ),
    Value = c(
      "Mozambique (MOZ)",
      "Multi-Sector Needs Assessment (MSNA 2026)",
      format(Sys.time(), "%Y-%m-%d %H:%M:%S %Z"),
      "EduAnalysisTool (martivit/IMPACT) & Humind MSNI Workflow (IMPACT Initiatives)",
      "Validated Integrated Analysis",
      "REACH Initiatives / IMPACT Initiatives"
    ),
    stringsAsFactors = FALSE
  )
  add_formatted_sheet(wb, "Cover", cover_data, apply_labels = FALSE)

  # 2. Variable Dictionary Sheet (Human-Readable Mapping)
  dict_path <- here::here("analysis", "config", "variable_labels_en.csv")
  if (file.exists(dict_path)) {
    var_dict <- readr::read_csv(dict_path, show_col_types = FALSE)
    add_formatted_sheet(wb, "Variable_Dictionary", var_dict, apply_labels = FALSE)
  }

  # 3. MSNI Tables
  add_formatted_sheet(wb, "MSNI_National", analysis_results$msni$MSNI_National)
  add_formatted_sheet(wb, "MSNI_Provincial", analysis_results$msni$MSNI_Admin1)
  add_formatted_sheet(wb, "MSNI_District", analysis_results$msni$MSNI_Admin2)
  if ("MSNI_DispStatus" %in% names(analysis_results$msni)) {
    add_formatted_sheet(wb, "MSNI_Displacement", analysis_results$msni$MSNI_DispStatus)
  }
  if ("MSNI_HoHGender" %in% names(analysis_results$msni)) {
    add_formatted_sheet(wb, "MSNI_HoHGender", analysis_results$msni$MSNI_HoHGender)
  }
  add_formatted_sheet(wb, "MSNI_Phases_Prov", analysis_results$msni_distribution, apply_labels = FALSE)

  # 4. Education Tables
  add_formatted_sheet(wb, "Edu_National", analysis_results$education$Edu_National)
  add_formatted_sheet(wb, "Edu_Provincial", analysis_results$education$Edu_Admin1)
  add_formatted_sheet(wb, "Edu_Gender_Prov", analysis_results$education$Edu_Gender_Admin1)
  add_formatted_sheet(wb, "Edu_Age_Prov", analysis_results$education$Edu_AgeCat_Admin1)
  add_formatted_sheet(wb, "Edu_District", analysis_results$education$Edu_Admin2)

  openxlsx::saveWorkbook(wb, xlsx_file, overwrite = TRUE)

  # 5. Save Enriched Datasets to RDS
  message("--> [Export Engine] Saving enriched datasets to: ", rds_file)
  saveRDS(
    list(
      main_with_indicators = main_df,
      edu_with_indicators  = edu_df,
      tables               = analysis_results,
      exported_at          = Sys.time()
    ),
    rds_file
  )

  # 6. Render Interactive HTML Report (English)
  html_template <- here::here("analysis", "reports", "msna_moz_2026_analysis_report.Rmd")
  if (file.exists(html_template)) {
    message("--> [Export Engine] Rendering interactive HTML report: ", html_file)
    tryCatch({
      rmarkdown::render(
        input       = html_template,
        output_file = basename(html_file),
        output_dir  = out_dir,
        params = list(
          analyzed_data = list(
            main_with_indicators = main_df,
            edu_with_indicators  = edu_df,
            tables               = analysis_results
          )
        ),
        quiet = TRUE
      )
      message("--> [Export Engine] HTML report generated successfully!")
    }, error = function(e) {
      warning("HTML report generation failed: ", e$message)
    })
  }

  message("--> [Export Engine] All outputs exported successfully!")
  return(list(xlsx = xlsx_file, rds = rds_file, html = html_file))
}
