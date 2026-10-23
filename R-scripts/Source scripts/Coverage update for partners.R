cat("\014")
print_step("7. Exporting coverage update for partners")

coverage_list <- make_coverage_summary(data_hh_main)
coverage_list$repeat_count_summary <- repeat_count_summary
if (length(coverage_list) == 0) coverage_list <- list(message = tibble::tibble(message = "No coverage summaries could be created."))

coverage_path <- export_excel_list(coverage_list, paste0("Coverage_update_MSNA2026_Uganda_", format(Sys.time(), "%Y-%m-%d_at_%Hh%M"), ".xlsx"), path_cleaning_folder_today)
cat("Exported: ", coverage_path, "\n", sep = "")
print_step("Coverage update file exported")
