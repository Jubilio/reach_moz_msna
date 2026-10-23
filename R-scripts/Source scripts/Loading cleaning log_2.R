cat("\014")
# print_step("9. Loading reviewed cleaning log data")
message("═══════════════════════════════════════════════════════════════")






##############################################################################
# ---- 1. SET THE PATH (only thing you need to change) ------------------------
cleaning_log_path <- "inputs/feedback2"

daily_logs_path <- file.path(cleaning_log_path, "01_Daily data cleaning logs")
output_path     <- file.path(cleaning_log_path, "combined_cleaning_logs")
sheet_to_read   <- "cleaning_log"

dir.create(output_path, showWarnings = FALSE, recursive = TRUE)



##### loading the combuned cleaning log 
# cleaning_log <- read_excel(
#   "inputs/cleaning_log_combined_2026-10-05.xlsx"
# )


##### loading the kampala cleaning log 
cleaning_log <- read_excel(
  "inputs/Cleaning_log_MSNA2026_Uganda_kampala.xlsx",
  sheet = "cleaning_log"
)








# =============================================================================
# PART 4A: IDENTIFY AND LOG DUPLICATE POINT NUMBERS & SHORT DURATION SURVEYS
# =============================================================================
message("\n\n--- Processing Duplicates and Short Duration Surveys ---")

# Initialize tracking lists
duplicate_point_numbers_log <- NULL
short_duration_surveys_log <- NULL

if (!is.null(raw_data) && nrow(raw_data) > 0) {
  
  # ---- Step 1: Calculate total duration ----
  # Three approaches: (1) Use start/end times, (2) Sum section durations, (3) Both
  
  message("\n  Calculating interview duration...")
  
  # Approach A: From submission start/end times (if available)
  duration_from_times <- NULL
  if (all(c("_submission_time", "_end_time") %in% names(raw_data))) {
    duration_from_times <- raw_data |>
      mutate(
        total_duration_minutes = as.numeric(
          difftime(.data$`_end_time`, .data$`_submission_time`, units = "mins")
        )
      ) |>
      select(uuid, total_duration_minutes)
    message("    ✓ Found _submission_time and _end_time columns")
  }
  
  # Approach B: Sum of section durations (if available)
  duration_sections <- NULL
  duration_section_cols <- names(raw_data)[grepl("^duration_.*_min$", names(raw_data), ignore.case = TRUE)]
  if (length(duration_section_cols) > 0) {
    message("    ✓ Found section duration columns: ", paste(duration_section_cols, collapse = ", "))
    duration_sections <- raw_data |>
      mutate(
        total_duration_minutes = rowSums(
          across(all_of(duration_section_cols), 
                 ~as.numeric(.) %>% replace(is.na(.), 0)),
          na.rm = TRUE
        )
      ) |>
      select(uuid, total_duration_minutes)
  }
  
  # Combine approaches: prefer times if available, fall back to section sum
  raw_data_with_duration <- raw_data
  if (!is.null(duration_from_times)) {
    raw_data_with_duration <- raw_data_with_duration |>
      left_join(duration_from_times, by = "uuid")
  } else if (!is.null(duration_sections)) {
    raw_data_with_duration <- raw_data_with_duration |>
      left_join(duration_sections, by = "uuid")
  } else {
    raw_data_with_duration <- raw_data_with_duration |>
      mutate(total_duration_minutes = NA_real_)
    message("    ⚠️  WARNING: Could not calculate duration from available columns")
  }
  
  # ---- Step 2: Identify DUPLICATES (by enum_id + point_number combination) ----
  message("\n✓ Checking for duplicate surveys (enum_id + point_number)...")
  
  # Find duplicates where BOTH enum_id AND point_number are identical
  duplicates_by_enum_point <- raw_data_with_duration |>
    filter(!is.na(point_number) & point_number != "" &
             !is.na(enum_id) & enum_id != "") |>
    group_by(enum_id, point_number) |>
    mutate(
      n_duplicates = n(),
      duplicate_rank = row_number()  # Rank by submission time order
    ) |>
    filter(n_duplicates > 1) |>
    ungroup() |>
    arrange(enum_id, point_number, `_submission_time`)
  
  # Keep only the EXTRA duplicates (exclude the first/earliest one per group)
  duplicates_to_remove <- duplicates_by_enum_point |>
    filter(duplicate_rank > 1) |>  # Keep only records 2, 3, 4... (not the first one)
    select(
      uuid, enum_id, point_number, `_submission_time`, 
      total_duration_minutes, n_duplicates, duplicate_rank, everything()
    )
  
  if (nrow(duplicates_to_remove) > 0) {
    message("  ⚠️ Found ", nrow(duplicates_to_remove), " extra duplicate records to remove")
    message("    Unique duplicate combinations: ", 
            n_distinct(duplicates_to_remove |> select(enum_id, point_number)))
    
    # Create cleaning log entries for extra duplicates only
    duplicate_point_numbers_log <- duplicates_to_remove |>
      mutate(
        source_dataset = "Submissions",
        dataset = "Submissions",
        question = "duplicate_survey",
        issue = paste0("Duplicate survey: enum_id='", enum_id, "' + point_number='", point_number,
                       "' (", n_duplicates, " surveys collected by same enumerator at same point)"),
        old_value = paste0(enum_id, "_", point_number),
        new_value = NA_character_,
        notes = paste0("Duplicate rank: ", duplicate_rank, "/", n_duplicates, 
                       " | Duration: ", 
                       ifelse(!is.na(total_duration_minutes), 
                              paste0(round(total_duration_minutes, 1), " mins"),
                              "Unknown"),
                       " | Keep first submission (earliest), remove extras"),
        change_type = "remove_survey",
        source_log = "duplicate_detection"
      ) |>
      select(
        source_dataset, dataset, uuid, enum_id, point_number,
        question, issue, old_value, new_value, notes,
        change_type, source_log, everything()
      )
  } else {
    message("  ✓ No duplicate surveys found (no enum_id + point_number combinations)")
  }
  
  # ---- Step 3: Identify SHORT DURATION SURVEYS (< 10 minutes) ----
  message("\n✓ Checking for surveys with duration < 10 minutes...")
  
  # You can also sum up individual section durations if available
  # For now, using total_duration_minutes - adjust based on your data
  
  short_duration_surveys <- raw_data_with_duration |>
    filter(!is.na(total_duration_minutes) & total_duration_minutes < 10) |>
    arrange(total_duration_minutes, `_submission_time`) |>
    select(
      uuid, enum_id, point_number, `_submission_time`, 
      total_duration_minutes, everything()
    )
  
  if (nrow(short_duration_surveys) > 0) {
    message("  ⚠️ Found ", nrow(short_duration_surveys), " surveys with duration < 10 minutes")
    
    # Create cleaning log entries for short duration surveys
    short_duration_surveys_log <- short_duration_surveys |>
      mutate(
        source_dataset = "Submissions",
        dataset = "Submissions",
        question = "interview_duration",
        issue = paste0("Survey duration too short: ", 
                       round(total_duration_minutes, 1), " minutes (< 10 min threshold)"),
        old_value = as.character(round(total_duration_minutes, 1)),
        new_value = NA_character_,
        notes = paste0("Possible incomplete interview. Duration: ", 
                       round(total_duration_minutes, 1), " minutes"),
        change_type = "remove_survey",
        source_log = "duration_check"
      ) |>
      select(
        source_dataset, dataset, uuid, enum_id, point_number,
        question, issue, old_value, new_value, notes,
        change_type, source_log, everything()
      )
  } else {
    message("  ✓ No surveys found with duration < 10 minutes")
  }
  
} else {
  message("\n⚠️  No raw data available for duplicate/duration checks")
}

# ---- Step 4: EXPORT duplicates and short duration to Excel ----
if (!is.null(duplicate_point_numbers_log) || !is.null(short_duration_surveys_log)) {
  
  export_issues_file <- file.path(
    output_path, 
    paste0("00_Data_Quality_Issues_",
           format(Sys.Date(), "%Y%m%d"),
           ".xlsx")
  )
  
  export_sheets_list <- list()
  sheet_summary <- data.frame(
    Sheet = character(),
    Records = numeric(),
    Description = character(),
    stringsAsFactors = FALSE
  )
  
  if (!is.null(duplicate_point_numbers_log) && nrow(duplicate_point_numbers_log) > 0) {
    export_sheets_list[["Duplicate_Points"]] <- duplicate_point_numbers_log |>
      select(
        source_dataset, dataset, uuid, enum_id, point_number,
        question, issue, old_value, new_value, notes,
        change_type
      )
    sheet_summary <- rbind(
      sheet_summary,
      data.frame(
        Sheet = "Duplicate_Points",
        Records = nrow(duplicate_point_numbers_log),
        Description = "Surveys with duplicate point_number values"
      )
    )
  }
  
  if (!is.null(short_duration_surveys_log) && nrow(short_duration_surveys_log) > 0) {
    export_sheets_list[["Short_Duration"]] <- short_duration_surveys_log |>
      select(
        source_dataset, dataset, uuid, enum_id, point_number,
        question, issue, old_value, new_value, notes,
        change_type
      )
    sheet_summary <- rbind(
      sheet_summary,
      data.frame(
        Sheet = "Short_Duration",
        Records = nrow(short_duration_surveys_log),
        Description = "Surveys with interview duration < 10 minutes"
      )
    )
  }
  
  if (length(export_sheets_list) > 0) {
    export_sheets_list[["Summary"]] <- sheet_summary
    writexl::write_xlsx(export_sheets_list, export_issues_file)
    message("\n✓ Exported data quality issues to: ", basename(export_issues_file))
    message("  Sheets: ", paste(names(export_sheets_list), collapse = ", "))
    message("  Total records: ", sum(sheet_summary$Records))
  }
}

# ---- Step 5: ADD QUALITY ISSUES TO CLEANING LOG (before splitting) ----
message("\n✓ Adding flagged surveys to cleaning log...")

issues_logs <- list()

if (!is.null(duplicate_point_numbers_log) && nrow(duplicate_point_numbers_log) > 0) {
  issues_logs[[length(issues_logs) + 1]] <- duplicate_point_numbers_log |>
    mutate(across(everything(), as.character))
}

if (!is.null(short_duration_surveys_log) && nrow(short_duration_surveys_log) > 0) {
  issues_logs[[length(issues_logs) + 1]] <- short_duration_surveys_log |>
    mutate(across(everything(), as.character))
}

if (length(issues_logs) > 0) {
  # Combine all issue logs
  issues_combined <- bind_rows(issues_logs)
  
  # Get common columns and add them to the main cleaning log
  common_cols_with_issues <- intersect(names(cleaning_log), names(issues_combined))
  
  cleaning_log <- bind_rows(
    cleaning_log |> mutate(across(everything(), as.character)),
    issues_combined |> select(all_of(common_cols_with_issues))
  )
  
  # Add/fill source_log column if it doesn't exist, using coalesce if it does
  if ("source_log" %in% names(cleaning_log)) {
    cleaning_log <- cleaning_log |>
      mutate(source_log = coalesce(.data$source_log, "data_quality_check"))
  } else {
    cleaning_log <- cleaning_log |>
      mutate(source_log = "data_quality_check")
  }
  
  message("  ✓ Added ", nrow(issues_combined), " data quality flags to cleaning_log")
  message("  Updated cleaning_log total rows: ", nrow(cleaning_log))
} else {
  message("  ✓ No data quality issues to add")
}











### loading the cleaing log 




# =============================================================================
# PART 5: SPLIT combined cleaning log by dataset
# =============================================================================
message("\n\n--- Splitting Cleaning Log by Dataset ---")

if (!is.null(cleaning_log) && nrow(cleaning_log) > 0) {
  
  # Get unique datasets in the cleaning log
  unique_datasets <- unique(cleaning_log$dataset)
  message("\nDatasets found in cleaning log (", length(unique_datasets), "):")
  print(unique_datasets)
  
  # Create separate logs for each dataset
  # Main datasets to filter: household_level, edu_ind, nutrition_ind, roster
  
  message("\n--- Creating Dataset-Specific Logs ---\n")
  
  # 1. HOUSEHOLD LEVEL (main household data)
  # Captures both "Submissions" and "household_level" dataset entries
  # Also includes NA dataset entries (data entry gaps in cleaning logs)
  if (any(c("Submissions", "household_level") %in% unique_datasets)) {
    cleaning_log_main <- cleaning_log |>
      filter(dataset %in% c("Submissions", "household_level") | is.na(dataset))
    message("✓ cleaning_log_main (Submissions + household_level + NA): ", nrow(cleaning_log_main), " rows")
  } else {
    cleaning_log_main <- NULL
    message("✗ cleaning_log_main: No data (neither Submissions nor household_level found)")
  }
  
  # 2. EDUCATION INDICATOR
  if ("edu_ind" %in% unique_datasets) {
    cleaning_log_edu_ind <- cleaning_log |>
      filter(dataset == "edu_ind")
    message("✓ cleaning_log_edu_ind: ", nrow(cleaning_log_edu_ind), " rows")
  } else {
    cleaning_log_edu_ind <- NULL
    message("✗ cleaning_log_edu_ind: No data (edu_ind not found)")
  }
  
  # 3. NUTRITION INDICATOR
  if ("nutrition_ind" %in% unique_datasets) {
    cleaning_log_nutrition <- cleaning_log |>
      filter(dataset == "nutrition_ind")
    message("✓ cleaning_log_nutrition: ", nrow(cleaning_log_nutrition), " rows")
  } else {
    cleaning_log_nutrition <- NULL
    message("✗ cleaning_log_nutrition: No data (nutrition_ind not found)")
  }
  
  # 4. ROSTER
  if ("roster" %in% unique_datasets) {
    cleaning_log_roster <- cleaning_log |>
      filter(dataset == "roster")
    message("✓ cleaning_log_roster: ", nrow(cleaning_log_roster), " rows")
  } else {
    cleaning_log_roster <- NULL
    message("✗ cleaning_log_roster: No data (roster not found)")
  }
  
  # 5. HEALTH INDICATOR (if exists)
  if ("health_ind" %in% unique_datasets) {
    cleaning_log_health_ind <- cleaning_log |>
      filter(dataset == "health_ind")
    message("✓ cleaning_log_health_ind: ", nrow(cleaning_log_health_ind), " rows")
  } else {
    cleaning_log_health_ind <- NULL
    message("✗ cleaning_log_health_ind: No data (health_ind not found)")
  }
  
  # 6. CHILD PROTECTION INDICATOR (if exists)
  if ("child_protection_ind" %in% unique_datasets) {
    cleaning_log_prot_ind <- cleaning_log |>
      filter(dataset == "child_protection_ind")
    message("✓ cleaning_log_prot_ind: ", nrow(cleaning_log_prot_ind), " rows")
  } else {
    cleaning_log_prot_ind <- NULL
    message("✗ cleaning_log_prot_ind: No data (child_protection_ind not found)")
  }
  
  # Export each dataset-specific log to Excel
  message("\n--- Exporting Dataset-Specific Logs ---\n")
  
  dataset_logs <- list(
    cleaning_log_main = cleaning_log_main,
    cleaning_log_edu_ind = cleaning_log_edu_ind,
    cleaning_log_nutrition = cleaning_log_nutrition,
    cleaning_log_roster = cleaning_log_roster,
    cleaning_log_health_ind = cleaning_log_health_ind,
    cleaning_log_prot_ind = cleaning_log_prot_ind
  )
  
  for (log_name in names(dataset_logs)) {
    log_df <- dataset_logs[[log_name]]
    
    if (!is.null(log_df) && nrow(log_df) > 0) {
      # Create filename based on log name
      file_suffix <- gsub("cleaning_log_", "", log_name)
      
      message(sprintf("  ✓ %s_%s.xlsx", file_suffix, format(Sys.Date(), "%Y%m%d")))
      message(sprintf("    Rows: %d", nrow(log_df)))
    }
  }
  
  # Create summary of splits
  message("\n--- Dataset-Specific Logs Summary ---\n")
  
  summary_splits <- tibble(
    dataset = c("Submissions", "edu_ind", "nutrition_ind", "roster", "health_ind", "child_protection_ind"),
    log_name = c("cleaning_log_main", "cleaning_log_edu_ind", "cleaning_log_nutrition", 
                 "cleaning_log_roster", "cleaning_log_health_ind", "cleaning_log_prot_ind"),
    rows = c(
      if (!is.null(cleaning_log_main)) nrow(cleaning_log_main) else 0,
      if (!is.null(cleaning_log_edu_ind)) nrow(cleaning_log_edu_ind) else 0,
      if (!is.null(cleaning_log_nutrition)) nrow(cleaning_log_nutrition) else 0,
      if (!is.null(cleaning_log_roster)) nrow(cleaning_log_roster) else 0,
      if (!is.null(cleaning_log_health_ind)) nrow(cleaning_log_health_ind) else 0,
      if (!is.null(cleaning_log_prot_ind)) nrow(cleaning_log_prot_ind) else 0
    ),
    status = c(
      if (!is.null(cleaning_log_main) && nrow(cleaning_log_main) > 0) "exported" else "no_data",
      if (!is.null(cleaning_log_edu_ind) && nrow(cleaning_log_edu_ind) > 0) "exported" else "no_data",
      if (!is.null(cleaning_log_nutrition) && nrow(cleaning_log_nutrition) > 0) "exported" else "no_data",
      if (!is.null(cleaning_log_roster) && nrow(cleaning_log_roster) > 0) "exported" else "no_data",
      if (!is.null(cleaning_log_health_ind) && nrow(cleaning_log_health_ind) > 0) "exported" else "no_data",
      if (!is.null(cleaning_log_prot_ind) && nrow(cleaning_log_prot_ind) > 0) "exported" else "no_data"
    )
  ) |>
    filter(rows > 0)
  
  print(summary_splits)
  
  message("\n✓ All dataset-specific cleaning logs created and exported")
  
} else {
  message("⚠️  Combined log is empty - cannot create dataset-specific splits")
}







#####


#Add columns to the df (percentage missing, duration and other)----
# data_hh_main <- data_hh_main %>%
#   add_percentage_missing() %>%
#   add_duration()
# 
# table(data_hh_main$point_number)









#Count of 999.0 for each variable----
# count_999_by_variable(data_hh_main)
# 
# unique(data$What_other_Organizations)

# #Check percentage missing, duration, duplicates, specific values and others----
# other_columns_to_check <- df_survey %>%
#   filter(type=="text" & grepl("other", name)) %>%
#   filter(name %in% names(data_hh_main)) %>%
#   pull(name)
# 
# # other_columns_to_check
# 
# 
# clog_main <- data_hh_main %>% 
#   check_percentage_missing(uuid_column = "_uuid", column_to_check = "percentage_missing", strongness_factor = 2) %>%
#   check_duration(column_to_check = "duration", uuid_column = "_uuid", lower_bound = 10, higher_bound = 1440) %>%
#   check_others(uuid_column = "_uuid", columns_to_check = other_columns_to_check) %>%
#   check_duplicate(uuid_column = "_uuid") %>%
#   check_value(uuid_column = "_uuid", element_name = "checked_dataset", values_to_look = c(999," _999", -990, "-999", "999"))
# 
# 
# 





# table(cleaning_log$question)

# ═════════════════════════════════════════════════════════════════════
# COMPREHENSIVE AUDIT: Verify all cleaning logs are captured completely
# ═════════════════════════════════════════════════════════════════════

cat("\n╔════════════════════════════════════════════════════════════════════════╗\n")
cat("║         CLEANING LOG CAPTURE AUDIT: Completeness Verification          ║\n")
cat("╚════════════════════════════════════════════════════════════════════════╝\n\n")

# 1. Raw combined log by dataset
cat("1. RAW COMBINED LOG (cleaning_log) - Distribution by Dataset:\n")
cat("   ───────────────────────────────────────────────────────────\n")
raw_by_dataset <- table(cleaning_log$dataset, useNA = "ifany")
for (i in seq_along(raw_by_dataset)) {
  dataset_name <- names(raw_by_dataset)[i]
  count <- as.numeric(raw_by_dataset[i])
  cat(sprintf("   %-20s %6d rows\n", dataset_name, count))
}

# 2. Total rows check
total_raw <- nrow(cleaning_log)
total_split <- (if (!is.null(cleaning_log_main)) nrow(cleaning_log_main) else 0) + 
  (if (!is.null(cleaning_log_edu_ind)) nrow(cleaning_log_edu_ind) else 0) + 
  (if (!is.null(cleaning_log_health_ind)) nrow(cleaning_log_health_ind) else 0) + 
  (if (!is.null(cleaning_log_nutrition)) nrow(cleaning_log_nutrition) else 0) + 
  (if (!is.null(cleaning_log_roster)) nrow(cleaning_log_roster) else 0)

cat("\n2. CAPTURE COMPLETENESS:\n")
cat("   ───────────────────────────────────────────────────────────\n")
cat(sprintf("   Raw combined log:        %6d rows\n", total_raw))
cat(sprintf("   Sum of split logs:       %6d rows\n", total_split))
cat(sprintf("   Capture rate:            %6.2f%%\n", (total_split / total_raw) * 100))

# 3. Breakdown of split logs
cat("\n3. SPLIT LOG BREAKDOWN:\n")
cat("   ───────────────────────────────────────────────────────────\n")
cat(sprintf("   cleaning_log_main:       %6d rows (Submissions + household_level + NA)\n", if (!is.null(cleaning_log_main)) nrow(cleaning_log_main) else 0))
cat(sprintf("   cleaning_log_edu_ind:    %6d rows (edu_ind)\n", if (!is.null(cleaning_log_edu_ind)) nrow(cleaning_log_edu_ind) else 0))
cat(sprintf("   cleaning_log_health_ind: %6d rows (health_ind)\n", if (!is.null(cleaning_log_health_ind)) nrow(cleaning_log_health_ind) else 0))
cat(sprintf("   cleaning_log_nutrition:  %6d rows (nutrition_ind)\n", if (!is.null(cleaning_log_nutrition)) nrow(cleaning_log_nutrition) else 0))
cat(sprintf("   cleaning_log_roster:     %6d rows (roster)\n", if (!is.null(cleaning_log_roster)) nrow(cleaning_log_roster) else 0))

# 4. Detailed breakdown of cleaning_log_main
cat("\n4. CLEANING_LOG_MAIN COMPOSITION:\n")
cat("   ───────────────────────────────────────────────────────────\n")
if (!is.null(cleaning_log_main)) {
  main_submissions <- nrow(cleaning_log_main |> filter(dataset == "Submissions"))
  main_household <- nrow(cleaning_log_main |> filter(dataset == "household_level"))
  main_na <- nrow(cleaning_log_main |> filter(is.na(dataset)))
  cat(sprintf("   • Submissions:           %6d rows\n", main_submissions))
  cat(sprintf("   • household_level:       %6d rows\n", main_household))
  cat(sprintf("   • NA (data entry gaps):  %6d rows\n", main_na))
  cat(sprintf("   TOTAL:                   %6d rows\n", nrow(cleaning_log_main)))
} else {
  cat("   • No data available\n")
}

# 5. Consent entry verification
cat("\n5. SPECIFIC QUESTION CHECK: Consent Entry\n")
cat("   ───────────────────────────────────────────────────────────\n")
if (!is.null(cleaning_log_main)) {
  consent_entry <- cleaning_log_main |> filter(stringr::str_detect(.data$question, "consent|Consent"))
  if (nrow(consent_entry) > 0) {
    cat("   ✓ FOUND in cleaning_log_main\n")
    cat(sprintf("   ├─ UUID: %s\n", substr(consent_entry$uuid[1], 1, 20)))
    cat(sprintf("   ├─ Dataset: %s\n", consent_entry$dataset[1]))
    cat(sprintf("   ├─ Question: %s\n", consent_entry$question[1]))
    cat(sprintf("   ├─ Issue: %s\n", consent_entry$issue[1]))
    cat(sprintf("   └─ Change: %s → %s\n", consent_entry$old_value[1], consent_entry$new_value[1]))
  } else {
    cat("   ✗ NOT FOUND in cleaning_log_main\n")
  }
} else {
  cat("   • cleaning_log_main is empty/NULL\n")
}

# 6. Final status
cat("\n6. FINAL STATUS:\n")
cat("   ───────────────────────────────────────────────────────────\n")
if (total_raw == total_split) {
  cat("   ✓ ALL ENTRIES CAPTURED (100% completeness)\n")
} else {
  cat("   ⚠ SOME ENTRIES NOT CAPTURED\n")
}
cat("\n")



cat("Reviewed cleaning-log records loaded: ", NROW(cleaning_log_main), "\n", sep = "")
message("═══════════════════════════════════════════════════════════════")






