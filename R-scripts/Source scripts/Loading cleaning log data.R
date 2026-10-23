cat("\014")
# print_step("9. Loading reviewed cleaning log data")
message("═══════════════════════════════════════════════════════════════")


output_path
# =============================================================================
# MSNA 2026 Uganda - Load & combine cleaning logs
# -----------------------------------------------------------------------------
# 1. Loads the two "Specify other" cleaning logs separately
#      -> Cleaning_log_specify_others_1, Cleaning_log_specify_others_2
# 2. Loops through every daily folder > "District cleaning logs" > district
#    folders, reads the "cleaning_log" sheet of each
#    "Cleaning_log_MSNA2026_Uganda_*.xlsx" file, keeps only non-empty logs and
#    appends them into one main cleaning log -> Cleaning_log_main
# 3. Exports the three logs to Excel
# =============================================================================

# ---- Packages ---------------------------------------------------------------
packages <- c("readxl", "writexl", "dplyr")
to_install <- packages[!packages %in% rownames(installed.packages())]
if (length(to_install) > 0) install.packages(to_install)
invisible(lapply(packages, library, character.only = TRUE))

# ---- CONFIGURATION: Part 4A (Duplicates & Duration Checks) -------------------
# IMPORTANT: Before running Part 4A, verify these settings match your data:
#
# 1. DURATION COLUMNS: Check the actual column names in your raw_data for:
#    - Start time column (default: "_submission_time")
#    - End time column (default: "_end_time")
#    - Or if you have section durations: columns like "duration_*_min"
#    - Duration threshold: Currently set to 10 minutes (change if needed)
#
# 2. DUPLICATE POINT_NUMBER: The code filters by point_number field
#    - Adjust if your identifier column has a different name
#
# 3. ALTERNATIVE: If you prefer to use sum of section durations instead of
#    total_duration_minutes, uncomment the section duration calculation below
# --------------------------------------------------------------------------

# ---- 1. SET THE PATH (only thing you need to change) ------------------------
cleaning_log_path <- "inputs/feedback"

daily_logs_path <- file.path(cleaning_log_path, "01_Daily data cleaning logs")
output_path     <- file.path(cleaning_log_path, "combined_cleaning_logs")
sheet_to_read   <- "cleaning_log"

dir.create(output_path, showWarnings = FALSE, recursive = TRUE)

# ---- Expected headers -------------------------------------------------------
cols_main <- c("source_dataset", "dataset", "uuid", "enum_id", "point_number",
               "question", "issue", "old_value", "new_value", "notes",
               "change_type", "admin1", "admin2", "admin3", "admin3_setlement",
               "admin4", "hoh_type", "parent_uuid", "repeat_index")

cols_specify <- c("source_dataset", "dataset", "uuid", "enum_id", "point_number",
                  "question", "question_label", "issue", "old_value",
                  "specify_other_text", "new_value", "notes", "change_type",
                  "admin1", "admin2", "New_other_response_category", "admin3",
                  "admin3_setlement", "admin4", "hoh_type", "parent_uuid",
                  "repeat_index", "color")

# ---- Helper: read the "cleaning_log" sheet safely ---------------------------
# - Returns NULL if the sheet is missing or the log has no data rows
# - A row counts as data only if it has a uuid (rows with only admin/formula
#   columns filled in are dropped)
# - Columns are put in the expected order; missing expected columns are added
#   as blank, and any extra columns are kept at the end. Header differences are
#   reported in the console and in the summary file.
read_cleaning_log <- function(file, expected_cols, sheet = sheet_to_read) {
  # Normalize the path to handle encoding issues
  normalized_file <- normalizePath(file, winslash = "/", mustWork = FALSE)

  # Don't check file.exists() due to Windows path encoding issues
  # Just try to read and handle errors
  tryCatch({
    sheets <- readxl::excel_sheets(normalized_file)
    sheet_match <- sheets[tolower(trimws(sheets)) == tolower(sheet)]
    if (length(sheet_match) == 0) {
      message("  ! No '", sheet, "' sheet in: ", basename(file))
      return(NULL)
    }
    # Read everything as text so logs from different days/districts bind cleanly
    df <- readxl::read_excel(normalized_file, sheet = sheet_match[1], col_types = "text",
                             .name_repair = "minimal")
    names(df) <- trimws(names(df))

    # Drop unnamed columns (blank header cells) and duplicated column names
    df <- df[, names(df) != "" & !is.na(names(df)) & !duplicated(names(df)), drop = FALSE]

    # Match headers to the expected names ignoring case (e.g. "UUID" -> "uuid")
    idx <- match(tolower(names(df)), tolower(expected_cols))
    names(df)[!is.na(idx)] <- expected_cols[idx[!is.na(idx)]]

    missing_cols <- setdiff(expected_cols, names(df))
    extra_cols   <- setdiff(names(df), expected_cols)
    if (length(missing_cols) > 0) message("  ! Missing columns in ", basename(file), ": ", paste(missing_cols, collapse = ", "))
    if (length(extra_cols)   > 0) message("  ! Extra columns in ",   basename(file), ": ", paste(extra_cols,   collapse = ", "))

    for (col in missing_cols) df[[col]] <- NA_character_
    df <- df[, c(expected_cols, extra_cols), drop = FALSE]

    # Keep only rows with a uuid
    df <- df[!is.na(df$uuid) & trimws(df$uuid) != "", , drop = FALSE]
    if (nrow(df) == 0) return(NULL)

    attr(df, "missing_cols") <- paste(missing_cols, collapse = ", ")
    attr(df, "extra_cols")   <- paste(extra_cols,   collapse = ", ")
    df
  }, error = function(e) {
    message("  ! Error reading file ", basename(file), ": ", conditionMessage(e))
    NULL
  })
}

# Lists Excel files, skipping temporary "~$" files created when a file is open
list_excel <- function(path, pattern = "\\.xlsx?$", recursive = FALSE) {
  if (!dir.exists(path)) return(character(0))
  files <- list.files(path, pattern = pattern, full.names = TRUE,
                      recursive = recursive, ignore.case = TRUE)
  files[!startsWith(basename(files), "~$")]
}

# =============================================================================
# PART 1: Specify other cleaning logs
# =============================================================================
# Finds the "Specify other cleaning logs" folder, prioritizing the top-level one
all_dirs     <- list.dirs(cleaning_log_path, recursive = TRUE)
matching_dirs <- all_dirs[grepl("specify other", basename(all_dirs), ignore.case = TRUE)]

# Prioritize the folder directly under "01_Daily data cleaning logs" (not nested deeper)
specify_path <- matching_dirs[grepl("01_Daily data cleaning logs/Specify other", matching_dirs, ignore.case = TRUE)][1]

# Fallback to any matching folder if the first preference isn't found
if (is.na(specify_path)) {
  specify_path <- matching_dirs[1]
}

if (is.na(specify_path)) stop("Could not find the 'Specify other cleaning logs' folder.")

specify_files <- sort(list_excel(specify_path))
message("Specify-other files found: ", length(specify_files))
print(basename(specify_files))
if (length(specify_files) < 2) stop("Expected 2 Excel files in: ", specify_path)

Cleaning_log_specify_others_1 <- read_cleaning_log(specify_files[1], cols_specify)
Cleaning_log_specify_others_2 <- read_cleaning_log(specify_files[2], cols_specify)
message("Specify others 1 rows: ", NROW(Cleaning_log_specify_others_1),
        " | Specify others 2 rows: ", NROW(Cleaning_log_specify_others_2))

# =============================================================================
# HARMONIZE: Convert Log 1 question format from "_choice_" to "/" format
# =============================================================================
# Log 1 uses format like: edu_barrier_choice_child_marriage
# Log 2 uses format like: edu_barrier/child_marriage (standard Kobo format)
# This function converts Log 1 to match Log 2's standard format

harmonize_question_format <- function(log_df) {
  # Extract unique questions
  log_questions <- log_df |>
    distinct(question) |>
    pull(question)

  # Create mapping: original -> harmonized
  mapping <- tibble(
    original_question = log_questions,
    harmonized_question = log_questions
  )

  # For questions with _choice_, convert them to / format
  choice_idx <- grepl("_choice_", mapping$original_question)
  if (sum(choice_idx) > 0) {
    for (i in which(choice_idx)) {
      original <- mapping$original_question[i]
      # Replace _choice_ with /
      harmonized <- sub("_choice_", "/", original)
      mapping$harmonized_question[i] <- harmonized
    }
  }

  # Apply the mapping
  log_df |>
    left_join(mapping, by = c("question" = "original_question")) |>
    mutate(question = harmonized_question) |>
    select(-harmonized_question)
}

# Apply harmonization to Log 1
Cleaning_log_specify_others_1 <- harmonize_question_format(Cleaning_log_specify_others_1)

message("\n--- Harmonization Complete ---")
message("Log 1 questions with '_choice_' pattern: ",
        Cleaning_log_specify_others_1 |> filter(grepl("_choice_", question)) |> nrow())
message("Log 1 and Log 2 common questions: ",
        length(intersect(
          Cleaning_log_specify_others_1 |> distinct(question) |> pull(question),
          Cleaning_log_specify_others_2 |> distinct(question) |> pull(question)
        )))




# =============================================================================
# COMBINE: Append both specify others logs into one
# =============================================================================
Cleaning_log_specify_others <- bind_rows(
  Cleaning_log_specify_others_1,
  Cleaning_log_specify_others_2
)





message("\n--- Combined Specify Others Log ---")
message("Log 1 rows: ", NROW(Cleaning_log_specify_others_1))
message("Log 2 rows: ", NROW(Cleaning_log_specify_others_2))
message("Combined rows: ", NROW(Cleaning_log_specify_others))
message("Unique questions: ", n_distinct(Cleaning_log_specify_others$question))

# =============================================================================
# EXPORT: Save combined specify others log to Excel
# =============================================================================
export_specify_file <- file.path(output_path, "Cleaning_log_specify_others_combined.xlsx")
writexl::write_xlsx(Cleaning_log_specify_others, export_specify_file)

message("\n--- Export Summary ---")
message("✓ File exported: Cleaning_log_specify_others_combined.xlsx")
message("  Location: ", output_path)
message("  File size: ", format(file.size(export_specify_file), units = "auto"))
message("  Rows: ", nrow(Cleaning_log_specify_others))
message("  Columns: ", ncol(Cleaning_log_specify_others))

# table(Cleaning_log_specify_others_1$question)
# table(Cleaning_log_specify_others_2$question)




# =============================================================================
# PART 2: Daily district cleaning logs -> one main cleaning log
# =============================================================================
message("\n--- Loading Daily District Cleaning Logs ---")

day_folders <- list.dirs(daily_logs_path, recursive = FALSE)
day_folders <- day_folders[grepl("MSNA2026 Uganda data cleaning", basename(day_folders), ignore.case = TRUE)]
day_folders <- sort(day_folders)

message("Daily folders found: ", length(day_folders))

log_summary <- list()
all_logs    <- list()

# ---- Helper: Extract date label from folder name
extract_date_label <- function(folder_name) {
  sub(".*cleaning\\s*", "", folder_name)  # e.g. "2026-Aug-27"
}

for (day_folder in day_folders) {
  day_name <- basename(day_folder)
  log_day  <- extract_date_label(day_name)

  district_root <- list.dirs(day_folder, recursive = FALSE)
  district_root <- district_root[grepl("district cleaning logs", basename(district_root), ignore.case = TRUE)]

  if (length(district_root) == 0) {
    message("  ! No 'District cleaning logs' folder in: ", day_name)
    next
  }

  district_folders <- list.dirs(district_root[1], recursive = FALSE)
  message("  Processing ", day_name, " (", length(district_folders), " districts)")

  for (district_folder in district_folders) {
    district <- basename(district_folder)

    if (!dir.exists(district_folder)) {
      next
    }

    # Use a more direct approach: list all .xlsx files and filter by name
    tryCatch({
      all_files <- list.files(
        path = district_folder,
        pattern = "\\.(xlsx|xls)$",
        full.names = TRUE,
        ignore.case = TRUE
      )

      # Remove temporary files (open in Excel)
      all_files <- all_files[!grepl("^~\\$", basename(all_files))]

      # Filter to keep only files that look like cleaning logs
      # (more lenient - just check if "cleaning" is in the filename)
      files <- all_files[grepl("cleaning", tolower(basename(all_files)))]

      if (length(files) == 0) {
        next
      }

      for (f in files) {
        # Try to read the file (no need to check if exists first)
        df <- read_cleaning_log(f, cols_main)

        status <- if (is.null(df)) "empty / skipped" else "loaded"
        rows_in_file <- if (is.null(df)) 0 else nrow(df)

        log_summary[[length(log_summary) + 1]] <- data.frame(
          day_folder = day_name,
          log_date = log_day,
          district = district,
          file = basename(f),
          rows = rows_in_file,
          status = status,
          missing_cols = if (is.null(df)) "" else attr(df, "missing_cols"),
          extra_cols   = if (is.null(df)) "" else attr(df, "extra_cols"),
          stringsAsFactors = FALSE
        )

        if (!is.null(df) && nrow(df) > 0) {
          df <- df |>
            mutate(
              log_day = log_day,
              district_folder = district,
              source_file = basename(f),
              .before = 1
            )
          all_logs[[length(all_logs) + 1]] <- df
        }
      }  # End of for loop for files
    }, error = function(e) {
      message("    ! Error processing district ", district, ": ", conditionMessage(e))
    })
    # End of tryCatch for district folder
  }
  # End of district loop
}

# ---- Combine all logs -----------------------------------------------
log_summary <- bind_rows(log_summary)
Cleaning_log_main <- if (length(all_logs) > 0) bind_rows(all_logs) else NULL

# ---- Quality checks -----------------------------------------------------------
if (!is.null(Cleaning_log_main) && nrow(Cleaning_log_main) > 0) {
  message("\n--- Main Cleaning Log Summary ---")
  message("Files checked: ", nrow(log_summary))
  message("Files loaded: ", sum(log_summary$status == "loaded"))
  message("Files skipped/empty: ", sum(log_summary$status != "loaded"))
  message("\nMain cleaning log total rows: ", nrow(Cleaning_log_main))

  message("\n--- Rows by Date ---")
  by_date <- count(Cleaning_log_main, log_day)
  print(by_date)

  message("\n--- Rows by District ---")
  by_district <- count(Cleaning_log_main, district_folder, sort = TRUE)
  print(by_district)

  message("\n--- Coverage Matrix (Districts × Dates) ---")
  coverage_matrix <- count(Cleaning_log_main, district_folder, log_day)
  print(coverage_matrix, n = Inf)
} else {
  message("\n⚠️  WARNING: No cleaning log data was loaded!")
  Cleaning_log_main <- tibble()
}



# =============================================================================
# PART 3: Export
# =============================================================================
message("\n--- Exporting Cleaning Logs ---")

today <- format(Sys.Date(), "%Y-%m-%d")

# Export main cleaning log if not empty
if (!is.null(Cleaning_log_main) && nrow(Cleaning_log_main) > 0) {
  export_main_file <- file.path(output_path, paste0("Cleaning_log_main_combined_", today, ".xlsx"))
  writexl::write_xlsx(
    list(cleaning_log = Cleaning_log_main),
    export_main_file
  )
  message("✓ Exported: Cleaning_log_main_combined_", today, ".xlsx")
  message("  Rows: ", nrow(Cleaning_log_main), " | File size: ",
          format(file.size(export_main_file), units = "auto"))
} else {
  message("⚠️  Skipped: No data to export for main cleaning log")
}


# Export summary of all files processed
export_summary_file <- file.path(output_path, paste0("Cleaning_log_file_summary_", today, ".xlsx"))
writexl::write_xlsx(
  list(files_checked = log_summary),
  export_summary_file
)
message("✓ Exported: Cleaning_log_file_summary_", today, ".xlsx")
message("  Files audited: ", nrow(log_summary))

message("\n╔════════════════════════════════════════════════════════════════╗")
message("║              CLEANING LOG LOADING COMPLETE                     ║")
message("╚════════════════════════════════════════════════════════════════╝")
message("\n📊 SUMMARY:")
message("  Specify Others logs:        ", NROW(Cleaning_log_specify_others), " rows")
message("  Main district logs:         ", NROW(Cleaning_log_main), " rows")
message("  TOTAL:                      ", NROW(Cleaning_log_specify_others) + NROW(Cleaning_log_main), " rows")
message("\n📁 Output folder: ", output_path)



# =============================================================================
# PART 4: COMBINE both cleaning logs (specify others + main) into one
# =============================================================================
message("\n\n--- Combining All Cleaning Logs ---")

# Check if both datasets exist and have data
if (!is.null(Cleaning_log_specify_others) && nrow(Cleaning_log_specify_others) > 0 &&
    !is.null(Cleaning_log_main) && nrow(Cleaning_log_main) > 0) {

  # Get common columns between both datasets
  cols_specify_others <- names(Cleaning_log_specify_others)
  cols_main <- names(Cleaning_log_main)

  # Find columns that exist in both datasets
  common_cols <- intersect(cols_specify_others, cols_main)

  message("\nCommon columns (", length(common_cols), "):")
  message("  ", paste(common_cols, collapse = ", "))

  # Columns unique to each dataset
  only_in_specify <- setdiff(cols_specify_others, cols_main)
  only_in_main <- setdiff(cols_main, cols_specify_others)

  if (length(only_in_specify) > 0) {
    message("\nColumns only in Specify Others (", length(only_in_specify), "):")
    message("  ", paste(only_in_specify, collapse = ", "))
  }

  if (length(only_in_main) > 0) {
    message("\nColumns only in Main logs (", length(only_in_main), "):")
    message("  ", paste(only_in_main, collapse = ", "))
  }

  # Strategy: Select common columns and add a source column to track origin
  # This ensures compatibility when combining

  # Select only common columns from both datasets
  Cleaning_log_specify_others_aligned <- Cleaning_log_specify_others |>
    select(all_of(common_cols)) |>
    mutate(
      source_log = "specify_others",
      .before = 1
    )

  Cleaning_log_main_aligned <- Cleaning_log_main |>
    select(all_of(common_cols)) |>
    mutate(
      source_log = "main_district",
      .before = 1
    )

  # Combine both logs using bind_rows
  cleaning_log <- bind_rows(
    Cleaning_log_specify_others_aligned,
    Cleaning_log_main_aligned
  )









  # ==========================================================================
  # CLEAN UP CLEANING LOG: Remove and fix problematic rows
  # ==========================================================================

  message("\n--- Cleaning Log Cleanup ---")

  # Record initial state
  nrows_before <- nrow(cleaning_log)
  message("Initial cleaning_log rows: ", nrows_before)

  # Task 1: Remove rows where question == "fsl_fcs_score"
  message("\nTask 1: Removing rows with question == 'fsl_fcs_score'")
  rows_removed_task1 <- nrow(cleaning_log |> filter(question == "fsl_fcs_score"))
  cleaning_log <- cleaning_log |>
    filter(question != "fsl_fcs_score")
  message("  ✓ Removed: ", rows_removed_task1, " rows")

  # Task 2: Set new_value to empty string for blank_response change_type
  message("\nTask 2: Setting new_value to empty for blank_response change_type")
  rows_modified_task2 <- nrow(cleaning_log |> filter(change_type == "blank_response"))
  cleaning_log <- cleaning_log |>
    mutate(
      new_value = if_else(change_type == "blank_response", "", new_value)
    )
  message("  ✓ Modified: ", rows_modified_task2, " rows")

  # Task 3: Keep old_value for no_action change_type
  # (this maintains the intent - no changes made, so new_value should match old_value)
  message("\nTask 3: Setting new_value = old_value for no_action change_type")
  rows_modified_task3 <- nrow(cleaning_log |> filter(change_type == "no_action"))
  cleaning_log <- cleaning_log |>
    mutate(
      new_value = if_else(change_type == "no_action", old_value, new_value)
    )
  message("  ✓ Modified: ", rows_modified_task3, " rows")

  # Final summary
  nrows_after <- nrow(cleaning_log)
  message("\n--- Cleanup Summary ---")
  message("  Rows before: ", nrows_before)
  message("  Rows after:  ", nrows_after)
  message("  Net change:  ", nrows_after - nrows_before)
  message("  ✓ Cleaning log cleanup completed")





  message("\n✓ Combined cleaning logs successfully")
  message("  Specify Others rows: ", nrow(Cleaning_log_specify_others_aligned))
  message("  Main logs rows:      ", nrow(Cleaning_log_main_aligned))
  message("  Combined total:      ", nrow(cleaning_log))
  message("  Total columns:       ", ncol(cleaning_log))

} else if (is.null(Cleaning_log_specify_others) || nrow(Cleaning_log_specify_others) == 0) {
  message("\n⚠️  WARNING: Cleaning_log_specify_others is empty or NULL")
  # If specify others is empty, just use main
  if (!is.null(Cleaning_log_main) && nrow(Cleaning_log_main) > 0) {
    cleaning_log <- Cleaning_log_main |>
      mutate(source_log = "main_district", .before = 1)
    message("  Using main logs only: ", nrow(cleaning_log), " rows")
  } else {
    message("  No data to combine!")
    cleaning_log <- NULL
  }
} else if (is.null(Cleaning_log_main) || nrow(Cleaning_log_main) == 0) {
  message("\n⚠️  WARNING: Cleaning_log_main is empty or NULL")
  # If main is empty, just use specify others
  if (!is.null(Cleaning_log_specify_others) && nrow(Cleaning_log_specify_others) > 0) {
    cleaning_log <- Cleaning_log_specify_others |>
      mutate(source_log = "specify_others", .before = 1)
    message("  Using specify others logs only: ", nrow(cleaning_log), " rows")
  } else {
    message("  No data to combine!")
    cleaning_log <- NULL
  }
}

# Export combined log if it has data
if (!is.null(cleaning_log) && nrow(cleaning_log) > 0) {
  export_combined_file <- file.path(output_path, paste0("cleaning_log_combined_", today, ".xlsx"))
  writexl::write_xlsx(cleaning_log, export_combined_file)

  message("\n--- Combined Log Export ---")
  message("✓ File exported: cleaning_log_combined_", today, ".xlsx")
  message("  Location: ", output_path)
  message("  File size: ", format(file.size(export_combined_file), units = "auto"))
  message("  Rows: ", nrow(cleaning_log))
  message("  Columns: ", ncol(cleaning_log))
  message("  Source breakdown:")
  message("    - Specify Others: ", sum(cleaning_log$source_log == "specify_others"), " rows")
  message("    - Main District: ", sum(cleaning_log$source_log == "main_district"), " rows")
} else {
  message("\n⚠️  WARNING: Combined log is empty or NULL - not exported")
}








##############################################################################

##### loading the combuned cleaning log 
cleaning_log <- read_excel(
    "inputs/cleaning_log_combined_2026-10-05.xlsx"
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
  ) |>
    mutate(source_log = coalesce(.data$source_log, "data_quality_check"))
  
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
      # export_file <- file.path(output_path, paste0("cleaning_log_", file_suffix, "_", today, ".xlsx"))
      
      # writexl::write_xlsx(log_df, export_file)
      
      message(sprintf("  ✓ %s_%s.xlsx", file_suffix, today))
      message(sprintf("    Rows: %d | File size: %s", nrow(log_df), 
                     format(file.size(export_file), units = "auto")))
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
      if (!is.null(cleaning_log_main)) "exported" else "no_data",
      if (!is.null(cleaning_log_edu_ind)) "exported" else "no_data",
      if (!is.null(cleaning_log_nutrition)) "exported" else "no_data",
      if (!is.null(cleaning_log_roster)) "exported" else "no_data",
      if (!is.null(cleaning_log_health_ind)) "exported" else "no_data",
      if (!is.null(cleaning_log_prot_ind)) "exported" else "no_data"
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
total_split <- nrow(cleaning_log_main) + nrow(cleaning_log_edu_ind) + 
               nrow(cleaning_log_health_ind) + nrow(cleaning_log_nutrition) + nrow(cleaning_log_roster)

cat("\n2. CAPTURE COMPLETENESS:\n")
cat("   ───────────────────────────────────────────────────────────\n")
cat(sprintf("   Raw combined log:        %6d rows\n", total_raw))
cat(sprintf("   Sum of split logs:       %6d rows\n", total_split))
cat(sprintf("   Capture rate:            %6.2f%%\n", (total_split / total_raw) * 100))

# 3. Breakdown of split logs
cat("\n3. SPLIT LOG BREAKDOWN:\n")
cat("   ───────────────────────────────────────────────────────────\n")
cat(sprintf("   cleaning_log_main:       %6d rows (Submissions + household_level + NA)\n", nrow(cleaning_log_main)))
cat(sprintf("   cleaning_log_edu_ind:    %6d rows (edu_ind)\n", nrow(cleaning_log_edu_ind)))
cat(sprintf("   cleaning_log_health_ind: %6d rows (health_ind)\n", nrow(cleaning_log_health_ind)))
cat(sprintf("   cleaning_log_nutrition:  %6d rows (nutrition_ind)\n", nrow(cleaning_log_nutrition)))
cat(sprintf("   cleaning_log_roster:     %6d rows (roster)\n", nrow(cleaning_log_roster)))

# 4. Detailed breakdown of cleaning_log_main
cat("\n4. CLEANING_LOG_MAIN COMPOSITION:\n")
cat("   ───────────────────────────────────────────────────────────\n")
main_submissions <- nrow(cleaning_log_main |> filter(dataset == "Submissions"))
main_household <- nrow(cleaning_log_main |> filter(dataset == "household_level"))
main_na <- nrow(cleaning_log_main |> filter(is.na(dataset)))
cat(sprintf("   • Submissions:           %6d rows\n", main_submissions))
cat(sprintf("   • household_level:       %6d rows\n", main_household))
cat(sprintf("   • NA (data entry gaps):  %6d rows\n", main_na))
cat(sprintf("   TOTAL:                   %6d rows\n", nrow(cleaning_log_main)))

# 5. Consent entry verification
cat("\n5. SPECIFIC QUESTION CHECK: Consent Entry\n")
cat("   ───────────────────────────────────────────────────────────\n")
consent_entry <- cleaning_log_main |> filter(str_detect(question, "consent|Consent"))
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

# 6. Final status
cat("\n6. FINAL STATUS:\n")
cat("   ───────────────────────────────────────────────────────────\n")
if (total_raw == total_split) {
  cat("   ✓ ALL ENTRIES CAPTURED (100% completeness)\n")
} else {
  cat("   ⚠ SOME ENTRIES NOT CAPTURED\n")
}
cat("\n")



cat("Reviewed cleaning-log records loaded: ", NROW(Cleaning_log_main), "\n", sep = "")
message("═══════════════════════════════════════════════════════════════")



