

# =============================================================================
# PART 2: Daily district cleaning logs -> one main cleaning log
# =============================================================================
day_folders <- list.dirs(daily_logs_path, recursive = FALSE)
day_folders <- day_folders[grepl("MSNA2026 Mozambique data cleaning", basename(day_folders), ignore.case = TRUE)]
message("\nDaily folders found: ", length(day_folders))

log_summary <- list()
all_logs    <- list()

for (day_folder in day_folders) {
  day_name <- basename(day_folder)
  log_day  <- sub(".*cleaning\\s*", "", day_name)          # e.g. "2026-Aug-27"
  
  district_root <- list.dirs(day_folder, recursive = FALSE)
  district_root <- district_root[grepl("district cleaning logs", basename(district_root), ignore.case = TRUE)]
  if (length(district_root) == 0) {
    message("  ! No 'District cleaning logs' folder in: ", day_name)
    next
  }
  
  district_folders <- list.dirs(district_root[1], recursive = FALSE)
  
  for (district_folder in district_folders) {
    district <- basename(district_folder)
    files <- list_excel(district_folder, pattern = "^Cleaning_log_MSNA2026_Mozambique_.*\\.xlsx?$")
    
    for (f in files) {
      df <- tryCatch(read_cleaning_log(f, cols_main), error = function(e) {
        message("  ! Could not read: ", f, " (", conditionMessage(e), ")"); NULL
      })
      status <- if (is.null(df)) "empty / skipped" else "loaded"
      log_summary[[length(log_summary) + 1]] <- data.frame(
        day_folder = day_name, district = district, file = basename(f),
        rows = if (is.null(df)) 0 else nrow(df), status = status,
        missing_cols = if (is.null(df)) "" else attr(df, "missing_cols"),
        extra_cols   = if (is.null(df)) "" else attr(df, "extra_cols")
      )
      if (!is.null(df)) {
        df <- df |>
          mutate(log_day = log_day, district_folder = district,
                 source_file = basename(f), .before = 1)
        all_logs[[length(all_logs) + 1]] <- df
      }
    }
  }
}

log_summary <- bind_rows(log_summary)
Cleaning_log_main <- bind_rows(all_logs)
# Note: all columns are kept as text so values like "01" or uuids are not altered.

# ---- Quick checks -----------------------------------------------------------
message("\nFiles checked: ", nrow(log_summary),
        " | loaded: ", sum(log_summary$status == "loaded"),
        " | empty/skipped: ", sum(log_summary$status != "loaded"))
message("Main cleaning log rows: ", nrow(Cleaning_log_main))
print(count(Cleaning_log_main, log_day, district_folder))

# =============================================================================
# PART 3: Export
# =============================================================================
today <- format(Sys.Date(), "%Y-%m-%d")

writexl::write_xlsx(list(cleaning_log = Cleaning_log_main),
                    file.path(output_path, paste0("Cleaning_log_main_combined_", today, ".xlsx")))
writexl::write_xlsx(list(cleaning_log = Cleaning_log_specify_others_1),
                    file.path(output_path, paste0("Cleaning_log_specify_others_1_", today, ".xlsx")))
writexl::write_xlsx(list(cleaning_log = Cleaning_log_specify_others_2),
                    file.path(output_path, paste0("Cleaning_log_specify_others_2_", today, ".xlsx")))
writexl::write_xlsx(list(files_checked = log_summary),
                    file.path(output_path, paste0("Cleaning_log_file_summary_", today, ".xlsx")))

message("\nDone. Files exported to: ", output_path)






















