
# 00_adhoc_analysis_request
# Ad-hoc analysis requests and custom tables

# ==============================================================================
# MSNU Completions for Obongi - September 8, 2026
# With detailed section timings from the form
# ==============================================================================

library(tidyverse)
library(lubridate)
library(flextable)
library(officer)

# ==============================================================================
# PART 1: Comprehensive table with total and section timings
# ==============================================================================

table_obongi_sep8_detailed <- data_hh_main %>%
  mutate(
    interview_date = as.Date(ymd_hms(start)),
    start_dt = ymd_hms(start),
    end_dt = ymd_hms(end),
    start_time = format(start_dt, "%H:%M:%S"),
    end_time = format(end_dt, "%H:%M:%S"),
    total_duration_min = as.numeric(difftime(end_dt, start_dt, units = "mins")),
    # GPS coordinates - use the latitude and longitude columns
    latitude = as.numeric(`_gps_location_latitude`),
    longitude = as.numeric(`_gps_location_longitude`),
    # First section start time (Intro/Consent)
    first_section_start = time_intro_consent_start,
    first_section_start_time = format(ymd_hms(time_intro_consent_start), "%H:%M:%S"),
    # Last section end time (using AAP as the last comprehensive section)
    last_section_end = time_aap_end,
    last_section_end_time = format(ymd_hms(time_aap_end), "%H:%M:%S"),
    # Convert section durations from character to numeric
    across(starts_with("duration_") & ends_with("_min"), 
           ~as.numeric(.))
  ) %>%
  filter(
    admin2 == "obongi",
    interview_date == as.Date("2026-09-08")
  ) %>%
  select(
    `Enumerator` = enum_id,
    `Date` = interview_date,
    `Latitude` = latitude,
    `Longitude` = longitude,
    `Form Start time` = start_time,
    `Form End time` = end_time,
    `First Section Start` = first_section_start_time,
    `Last Section End` = last_section_end_time,
    `Total Duration (Start-End)` = total_duration_min,
    # Section timings
    `Intro/Consent` = duration_intro_consent_min,
    `Location` = duration_location_enumerator_min,
    `Demographics` = duration_respondent_demographics_min,
    `Displacement` = duration_displacement_min,
    `Roster` = duration_demographics_roster_min,
    `Education` = duration_education_min,
    `Health` = duration_health_min,
    `Nutrition` = duration_nutrition_min,
    `MHPSS/CP` = duration_mhpss_child_protection_min,
    `WASH` = duration_wash_min,
    `Food Security` = duration_food_security_min,
    `SNFI/HLP` = duration_snfi_hlp_min,
    `Protection` = duration_protection_min,
    `Livelihoods` = duration_livelihoods_min,
    `Cash/Markets` = duration_cash_markets_min,
    `Shocks/Risks` = duration_shocks_risks_min,
    `AAP` = duration_aap_min,
    `Final Checks` = duration_final_checks_min
  ) %>%
  # Calculate total duration from summing all section durations
  rowwise() %>%
  mutate(
    `Total Duration (Sum of Sections)` = sum(c(
      `Intro/Consent`, `Location`, `Demographics`, `Displacement`,
      `Roster`, `Education`, `Health`, `Nutrition`, `MHPSS/CP`,
      `WASH`, `Food Security`, `SNFI/HLP`, `Protection`, `Livelihoods`,
      `Cash/Markets`, `Shocks/Risks`, `AAP`, `Final Checks`
    ), na.rm = TRUE)
  ) %>%
  ungroup() %>%
  # Now round only the duration and section timing columns, keep GPS precise
  mutate(
    across(starts_with("Total Duration") | starts_with("Intro") | 
           starts_with("Location") | starts_with("Demographics") |
           starts_with("Displacement") | starts_with("Roster") |
           starts_with("Education") | starts_with("Health") |
           starts_with("Nutrition") | starts_with("MHPSS") |
           starts_with("WASH") | starts_with("Food") |
           starts_with("SNFI") | starts_with("Protection") |
           starts_with("Livelihoods") | starts_with("Cash") |
           starts_with("Shocks") | starts_with("AAP") |
           starts_with("Final"), 
           ~round(., 2))
  ) %>%
  arrange(Enumerator)

# Display the main table
cat("MSNU COMPLETIONS - OBONGI DISTRICT - SEPTEMBER 8, 2026\n")
cat("=", rep("=", 80), "\n")
print(table_obongi_sep8_detailed %>% 
      select(Enumerator, Date, Latitude, Longitude, `Form Start time`, `Form End time`, 
             `First Section Start`, `Last Section End`, 
             `Total Duration (Start-End)`, `Total Duration (Sum of Sections)`))

# ==============================================================================
# PART 2: Summary with section timings
# ==============================================================================

cat("\n\nSECTION TIMINGS (minutes):\n")
print(table_obongi_sep8_detailed %>% 
      select(Enumerator, starts_with("Intro"):last_col()))

# ==============================================================================
# PART 3: Summary statistics
# ==============================================================================

cat("\n=== INTERVIEW DURATION SUMMARY ===\n")
cat("Total records:", nrow(table_obongi_sep8_detailed), "\n")
cat("Enumerators:", paste(unique(table_obongi_sep8_detailed$Enumerator), collapse = ", "), "\n\n")

cat("Total Duration Statistics (minutes):\n")
cat("\nFrom Start-End times:\n")
duration_stats_start_end <- table_obongi_sep8_detailed %>%
  pull(`Total Duration (Start-End)`) %>%
  summary()
print(duration_stats_start_end)

cat("\nFrom Sum of Sections:\n")
duration_stats_sum <- table_obongi_sep8_detailed %>%
  pull(`Total Duration (Sum of Sections)`) %>%
  summary()
print(duration_stats_sum)

# ==============================================================================
# PART 4: Export to Excel (with multiple sheets)
# ==============================================================================

excel_file <- file.path(path_cleaning_folder_today, "Obongi_MSNU_Completions_2026-09-08.xlsx")

wb <- openxlsx::createWorkbook()

# Sheet 1: Summary with GPS and key times
summary_table <- table_obongi_sep8_detailed %>%
  select(Enumerator, Date, Latitude, Longitude, `Form Start time`, `Form End time`, 
         `First Section Start`, `Last Section End`, `Total Duration (Start-End)`, 
         `Total Duration (Sum of Sections)`)

openxlsx::addWorksheet(wb, "Summary")
openxlsx::writeData(wb, "Summary", summary_table)
openxlsx::setColWidths(wb, "Summary", cols = 1:10, 
                       widths = c(12, 12, 15, 15, 15, 15, 18, 18, 20, 25))

# Format GPS columns to show full decimal precision (10 decimal places)
gps_style <- openxlsx::createStyle(numFmt = "0.0000000000")
for (col in 3:4) {
  openxlsx::addStyle(wb, "Summary", gps_style, rows = 2:(nrow(summary_table)+1), cols = col)
}

# Sheet 2: Detailed with section timings
openxlsx::addWorksheet(wb, "Detailed Timings")
openxlsx::writeData(wb, "Detailed Timings", table_obongi_sep8_detailed)
# Adjust for 28 columns (Enumerator through Final Checks + Sum of Sections)
col_widths <- c(12, 12, 15, 15, 15, 15, 18, 18, 20, 25, rep(12, 18))
openxlsx::setColWidths(wb, "Detailed Timings", cols = 1:28, widths = col_widths)

# Format GPS columns in Detailed sheet to show full decimal precision
for (col in 3:4) {
  openxlsx::addStyle(wb, "Detailed Timings", gps_style, rows = 2:(nrow(table_obongi_sep8_detailed)+1), cols = col)
}

openxlsx::saveWorkbook(wb, excel_file, overwrite = TRUE)
cat("\n✓ Excel file saved to:", excel_file, "\n")

# ==============================================================================
# PART 5: Export to Word document
# ==============================================================================

word_file <- file.path(path_cleaning_folder_today, "Obongi_MSNU_Completions_2026-09-08.docx")

# Create Word document
doc <- officer::read_docx()

# Add title
doc <- officer::body_add_par(doc, "MSNU Completions - Obongi District", style = "heading 1")
doc <- officer::body_add_par(doc, "Date: September 8, 2026", style = "heading 2")

# Add summary info
doc <- officer::body_add_par(doc, "", style = "Normal")
doc <- officer::body_add_par(doc, 
  paste("Total submissions:", nrow(table_obongi_sep8_detailed)), 
  style = "Normal")
doc <- officer::body_add_par(doc, 
  paste("Enumerators:", paste(unique(table_obongi_sep8_detailed$Enumerator), collapse = ", ")), 
  style = "Normal")
doc <- officer::body_add_par(doc, "", style = "Normal")

# Add summary table
doc <- officer::body_add_par(doc, "Interview Summary", style = "heading 2")
summary_ft <- flextable::flextable(summary_table)
summary_ft <- flextable::autofit(summary_ft)
summary_ft <- flextable::theme_booktabs(summary_ft)
doc <- flextable::body_add_flextable(doc, summary_ft)

# Add section timings table
doc <- officer::body_add_par(doc, "", style = "Normal")
doc <- officer::body_add_par(doc, "Detailed Section Timings", style = "heading 2")
detail_ft <- flextable::flextable(table_obongi_sep8_detailed)
detail_ft <- flextable::autofit(detail_ft)
detail_ft <- flextable::theme_booktabs(detail_ft)
doc <- flextable::body_add_flextable(doc, detail_ft)

# Save Word document
print(doc, target = word_file)
cat("✓ Word document saved to:", word_file, "\n")

cat("\n=== FILES GENERATED ===\n")
cat("Excel:", excel_file, "\n")
cat("Word: ", word_file, "\n")
