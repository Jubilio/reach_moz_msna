cat("\014")

# =================================================================================================
# REACH MSNA 2026 Mozambique - District-level field data collection tracker
# FINAL-TOOL-VARIABLES VERSION
# Purpose: Export a clear Excel tracker with ONE SHEET PER DISTRICT.
# Uses the current final Kobo tool variables as primary inputs:
#   point_number, admin1, admin2, admin3, admin3_setlement, admin4,
#   hoh_type, resp_gender, resp_age, hh_size, enum_id, enum_gender.
# Each district sheet contains enumerator-level daily progress, total forms, household status,
# respondent gender, roster member gender, duration, point coverage, and quality-flag counts.
#
# Expected to run AFTER:
#   1.Load and process the data.R
#   2.Soft duplicate checks.R
#   3.Do the checks.R
#   4.Process the CLs and DFs.R
#
# Main objects used when available:
#   data / data_hh_main / raw_data_list / combined_log / cleaning_log_template / path_cleaning_folder_today
# =================================================================================================

all_dfs_names_before <- Filter(function(x) is(x, "data.frame"), mget(ls())) |> names()
all_lists_names_before <- Filter(function(x) is(x, "list"), mget(ls())) |> names()

suppressPackageStartupMessages({
  library(dplyr)
  library(tidyr)
  library(stringr)
  library(lubridate)
  library(purrr)
  library(openxlsx)
  library(crayon)
  library(naniar)
})

# -------------------------------------------------------------------------------------------------
# 1. Helper functions
# -------------------------------------------------------------------------------------------------

first_existing_col <- function(df, candidates) {
  if (is.null(df) || !is.data.frame(df) || ncol(df) == 0) return(NA_character_)
  hit <- candidates[candidates %in% names(df)]
  if (length(hit) == 0) NA_character_ else hit[[1]]
}

safe_chr <- function(x) {
  out <- as.character(x)
  out[is.na(out) | trimws(out) == ""] <- NA_character_
  out
}

safe_num <- function(x) {
  suppressWarnings(as.numeric(str_replace_all(as.character(x), ",", "")))
}

safe_datetime_tracker <- function(x) {
  if (inherits(x, "POSIXct")) return(x)
  if (inherits(x, "Date")) return(as.POSIXct(x))
  suppressWarnings(lubridate::parse_date_time(
    as.character(x),
    orders = c(
      "Ymd HMS", "Ymd HM", "Ymd", "ymd HMS", "ymd HM", "ymd",
      "dmy HMS", "dmy HM", "dmy", "mdy HMS", "mdy HM", "mdy"
    ),
    tz = "Africa/Kampala"
  ))
}

normalise_status <- function(x) {
  x <- str_to_lower(as.character(x))
  case_when(
    is.na(x) | trimws(x) == "" ~ "Missing",
    str_detect(x, "refugee") ~ "Refugee",
    str_detect(x, "host") ~ "Host community",
    TRUE ~ str_to_sentence(str_replace_all(x, "_", " "))
  )
}

normalise_gender <- function(x) {
  x <- str_to_lower(as.character(x))
  case_when(
    is.na(x) | trimws(x) == "" ~ "Missing",
    x %in% c("m", "male", "man", "boy") ~ "Male",
    x %in% c("f", "female", "woman", "girl") ~ "Female",
    str_detect(x, "female") ~ "Female",
    str_detect(x, "male") & !str_detect(x, "female") ~ "Male",
    str_detect(x, "other") ~ "Other",
    str_detect(x, "pnta|prefer") ~ "Prefer not to answer",
    str_detect(x, "dnk|dont|don't") ~ "Don't know",
    TRUE ~ str_to_sentence(str_replace_all(x, "_", " "))
  )
}

make_clean_sheet_name <- function(x, existing = character()) {
  nm <- as.character(x)
  nm <- str_replace_all(nm, "[\\[\\]\\*\\?/\\\\:]", "_")
  nm <- str_replace_all(nm, "\\s+", " ")
  nm <- str_trim(nm)
  if (is.na(nm) || nm == "") nm <- "Missing"
  nm <- substr(nm, 1, 31)
  original <- nm
  i <- 1
  while (nm %in% existing) {
    suffix <- paste0("_", i)
    nm <- paste0(substr(original, 1, 31 - nchar(suffix)), suffix)
    i <- i + 1
  }
  nm
}

message_df <- function(message) {
  tibble::tibble(Message = message)
}

# -------------------------------------------------------------------------------------------------
# 2. Select the source data objects produced by the cleaning pipeline
# -------------------------------------------------------------------------------------------------

if (exists("data") && is.data.frame(data) && nrow(data) > 0) {
  data_collected <- data
} else if (exists("data_hh_main") && is.data.frame(data_hh_main) && nrow(data_hh_main) > 0) {
  data_collected <- data_hh_main
} else if (exists("raw_data_list") && is.list(raw_data_list) && "Submissions" %in% names(raw_data_list) && is.data.frame(raw_data_list$Submissions)) {
  data_collected <- raw_data_list$Submissions
} else {
  stop("No main submissions dataset found. Expected one of: data, data_hh_main, or raw_data_list$Submissions.", call. = FALSE)
}

if (!exists("raw_data_list") || !is.list(raw_data_list)) {
  raw_data_list <- list(
    Submissions = data_collected,
    roster = if (exists("data_hh_roster") && is.data.frame(data_hh_roster)) data_hh_roster else tibble::tibble(),
    edu_ind = if (exists("data_edu_ind") && is.data.frame(data_edu_ind)) data_edu_ind else tibble::tibble(),
    health_ind = if (exists("data_health_ind") && is.data.frame(data_health_ind)) data_health_ind else tibble::tibble(),
    nutrition_ind = if (exists("data_nutrition_ind") && is.data.frame(data_nutrition_ind)) data_nutrition_ind else tibble::tibble(),
    child_protection_ind = if (exists("data_child_protection_ind") && is.data.frame(data_child_protection_ind)) data_child_protection_ind else tibble::tibble()
  )
}

quality_log <- if (exists("cleaning_log_template") && is.data.frame(cleaning_log_template)) {
  cleaning_log_template
} else if (exists("cleaning_log") && is.data.frame(cleaning_log)) {
  cleaning_log
} else if (exists("combined_log") && is.data.frame(combined_log)) {
  combined_log
} else {
  tibble::tibble()
}

# -------------------------------------------------------------------------------------------------
# 3. Standardise the main submissions data
# -------------------------------------------------------------------------------------------------

uuid_col      <- first_existing_col(data_collected, c("uuid", "_uuid", "instance_name", "_id"))
date_col      <- first_existing_col(data_collected, c("_submission_time", "submission_time", "today", "start", "end"))
start_col     <- first_existing_col(data_collected, c("start", "start_time"))
end_col       <- first_existing_col(data_collected, c("end", "end_time"))
# Final Kobo tool variables are listed first. Older names are kept only as safe fallbacks.
adm1_col      <- first_existing_col(data_collected, c("admin1", "region", "adm1", "district_group"))
district_col  <- first_existing_col(data_collected, c("admin2", "district", "district_name", "ds_plans_district"))
subcounty_col <- first_existing_col(data_collected, c("admin3", "sub_county_division", "subcounty", "sub_county"))
settlement_col<- first_existing_col(data_collected, c("admin3_setlement", "settlement", "refugee_settlement"))
zone_col      <- first_existing_col(data_collected, c("admin4", "zone", "settlement_zone", "refugee_settlement_zone"))
point_col     <- first_existing_col(data_collected, c("point_number", "pt_num", "sample_point", "cluster_id"))
enum_col      <- first_existing_col(data_collected, c("enum_id", "enumerator_id", "enumerator_name", "username", "_submitted_by"))
enum_gender_col <- first_existing_col(data_collected, c("enum_gender", "enumerator_gender"))
hh_status_col <- first_existing_col(data_collected, c("hoh_type", "hh_status", "household_status", "population_group"))
resp_gender_col <- first_existing_col(data_collected, c("resp_gender", "respondent_gender"))
resp_age_col  <- first_existing_col(data_collected, c("resp_age", "respondent_age"))
hh_size_col   <- first_existing_col(data_collected, c("hh_size", "household_size"))
consent_col   <- first_existing_col(data_collected, c("consent", "survey_consent"))
duration_col  <- first_existing_col(data_collected, c("duration_minutes", "duration", "_duration"))

main_tracker <- data_collected |>
  mutate(
    tracker_uuid = if (!is.na(uuid_col)) as.character(.data[[uuid_col]]) else as.character(row_number()),
    tracker_submission_datetime = if (!is.na(date_col)) safe_datetime_tracker(.data[[date_col]]) else as.POSIXct(NA),
    tracker_submission_date = as.Date(tracker_submission_datetime),
    Region = if (!is.na(adm1_col)) safe_chr(.data[[adm1_col]]) else NA_character_,
    District = if (!is.na(district_col)) safe_chr(.data[[district_col]]) else "Missing",
    `Sub-county/division` = if (!is.na(subcounty_col)) safe_chr(.data[[subcounty_col]]) else "Missing",
    Settlement = if (!is.na(settlement_col)) safe_chr(.data[[settlement_col]]) else NA_character_,
    Zone = if (!is.na(zone_col)) safe_chr(.data[[zone_col]]) else NA_character_,
    `Point number` = if (!is.na(point_col)) safe_chr(.data[[point_col]]) else NA_character_,
    Enumerator = if (!is.na(enum_col)) safe_chr(.data[[enum_col]]) else "Missing",
    `Enumerator gender` = if (!is.na(enum_gender_col)) normalise_gender(.data[[enum_gender_col]]) else NA_character_,
    `Household status` = if (!is.na(hh_status_col)) normalise_status(.data[[hh_status_col]]) else "Missing",
    `Respondent gender` = if (!is.na(resp_gender_col)) normalise_gender(.data[[resp_gender_col]]) else "Missing",
    `Respondent age` = if (!is.na(resp_age_col)) safe_num(.data[[resp_age_col]]) else NA_real_,
    `Household size` = if (!is.na(hh_size_col)) safe_num(.data[[hh_size_col]]) else NA_real_,
    Consent = if (!is.na(consent_col)) safe_chr(.data[[consent_col]]) else NA_character_,
    `Duration minutes` = case_when(
      !is.na(duration_col) ~ safe_num(.data[[duration_col]]),
      !is.na(start_col) & !is.na(end_col) ~ as.numeric(difftime(safe_datetime_tracker(.data[[end_col]]), safe_datetime_tracker(.data[[start_col]]), units = "mins")),
      TRUE ~ NA_real_
    )
  ) |>
  mutate(
    District = replace_na(District, "Missing"),
    `Sub-county/division` = replace_na(`Sub-county/division`, "Missing"),
    Zone = replace_na(Zone, "Missing"),
    `Point number` = replace_na(`Point number`, "Missing"),
    Enumerator = replace_na(Enumerator, "Missing"),
    `Household status` = replace_na(`Household status`, "Missing"),
    `Respondent gender` = replace_na(`Respondent gender`, "Missing")
  )

# -------------------------------------------------------------------------------------------------
# 4. Standardise roster data for household-member gender by district/enumerator
# -------------------------------------------------------------------------------------------------

roster_raw <- if ("roster" %in% names(raw_data_list) && is.data.frame(raw_data_list$roster)) raw_data_list$roster else tibble::tibble()

if (nrow(roster_raw) > 0) {
  roster_parent_col <- first_existing_col(roster_raw, c("parent_uuid", "_parent_uuid", "_submission_uuid", "_submission__uuid", "_submission__id", "parent_instance_name"))
  roster_gender_col <- first_existing_col(roster_raw, c("ind_gender", "gender", "member_gender"))
  
  roster_tracker <- roster_raw |>
    mutate(
      tracker_parent_uuid = if (!is.na(roster_parent_col)) as.character(.data[[roster_parent_col]]) else NA_character_,
      `Household member gender` = if (!is.na(roster_gender_col)) normalise_gender(.data[[roster_gender_col]]) else "Missing"
    ) |>
    left_join(
      main_tracker |> select(tracker_uuid, District, Enumerator, `Point number`) |> rename(tracker_parent_uuid = tracker_uuid),
      by = "tracker_parent_uuid"
    ) |>
    mutate(
      District = replace_na(District, "Missing"),
      Enumerator = replace_na(Enumerator, "Missing")
    )
} else {
  roster_tracker <- tibble::tibble()
}

# -------------------------------------------------------------------------------------------------
# 5. Standardise quality log for quality flags by district/enumerator
# -------------------------------------------------------------------------------------------------

if (nrow(quality_log) > 0) {
  q_uuid_col <- first_existing_col(quality_log, c("uuid", "_uuid"))
  q_parent_col <- first_existing_col(quality_log, c("parent_uuid", "_parent_uuid"))
  q_district_col <- first_existing_col(quality_log, c("admin2", "District", "district", "district_name"))
  q_enum_col <- first_existing_col(quality_log, c("enum_id", "Enumerator", "enumerator_id", "enumerator_name", "username", "_submitted_by"))
  q_point_col <- first_existing_col(quality_log, c("point_number", "Point number", "pt_num", "sample_point"))
  q_issue_col <- first_existing_col(quality_log, c("issue", "Issue", "description"))
  
  quality_tracker <- quality_log |>
    mutate(
      q_uuid = if (!is.na(q_uuid_col)) as.character(.data[[q_uuid_col]]) else NA_character_,
      q_parent_uuid = if (!is.na(q_parent_col)) as.character(.data[[q_parent_col]]) else NA_character_,
      q_join_uuid = coalesce(q_parent_uuid, q_uuid),
      District = if (!is.na(q_district_col)) safe_chr(.data[[q_district_col]]) else NA_character_,
      Enumerator = if (!is.na(q_enum_col)) safe_chr(.data[[q_enum_col]]) else NA_character_,
      `Point number` = if (!is.na(q_point_col)) safe_chr(.data[[q_point_col]]) else NA_character_,
      Issue = if (!is.na(q_issue_col)) safe_chr(.data[[q_issue_col]]) else "Quality flag"
    ) |>
    left_join(
      main_tracker |> select(tracker_uuid, District_main = District, Enumerator_main = Enumerator, Point_number_main = `Point number`),
      by = c("q_join_uuid" = "tracker_uuid")
    ) |>
    mutate(
      District = coalesce(District, District_main, "Missing"),
      Enumerator = coalesce(Enumerator, Enumerator_main, "Missing"),
      `Point number` = coalesce(`Point number`, Point_number_main, "Missing"),
      Issue = replace_na(Issue, "Quality flag")
    )
} else {
  quality_tracker <- tibble::tibble()
}

# -------------------------------------------------------------------------------------------------
# 6. Build district-level tracker sheets following the user's coverage_update split format
# -------------------------------------------------------------------------------------------------

# Daily form counts by District + Enumerator, then wide by date.
daily_by_enum <- main_tracker |>
  filter(!is.na(tracker_submission_date)) |>
  count(District, Enumerator, tracker_submission_date, name = "count") |>
  arrange(District, Enumerator, tracker_submission_date) |>
  pivot_wider(names_from = tracker_submission_date, values_from = count, values_fill = 0, names_vary = "slowest")

# Format date columns in chronological order.
date_cols_raw <- names(daily_by_enum)[!names(daily_by_enum) %in% c("District", "Enumerator")]
if (length(date_cols_raw) > 0) {
  date_cols_sorted <- sort(as.Date(date_cols_raw)) |> as.character()
  daily_by_enum <- daily_by_enum |>
    select(District, Enumerator, all_of(date_cols_sorted)) |>
    rename_with(
      .cols = all_of(date_cols_sorted),
      .fn = ~ paste0(format(as.Date(.x), "%b %d"), " forms")
    )
} else {
  daily_by_enum <- main_tracker |>
    distinct(District, Enumerator) |>
    mutate(`No submission date available` = NA_integer_)
}

# Summary columns by District + Enumerator.
summary_by_enum <- main_tracker |>
  group_by(District, Enumerator) |>
  summarise(
    `Sub-counties covered` = n_distinct(`Sub-county/division`, na.rm = TRUE),
    `Settlements covered` = n_distinct(Settlement, na.rm = TRUE),
    `Zones covered` = n_distinct(Zone, na.rm = TRUE),
    `Point numbers covered` = n_distinct(`Point number`, na.rm = TRUE),
    `Total forms` = n(),
    `Refugee HHs` = sum(`Household status` == "Refugee", na.rm = TRUE),
    `Host HHs` = sum(`Household status` == "Host community", na.rm = TRUE),
    `Male respondents` = sum(`Respondent gender` == "Male", na.rm = TRUE),
    `Female respondents` = sum(`Respondent gender` == "Female", na.rm = TRUE),
    `Other/missing respondents` = sum(!`Respondent gender` %in% c("Male", "Female"), na.rm = TRUE),
    `Median duration (min)` = round(median(`Duration minutes`, na.rm = TRUE), 1),
    `Last submission date` = suppressWarnings(max(tracker_submission_date, na.rm = TRUE)),
    .groups = "drop"
  ) |>
  mutate(
    `Median duration (min)` = ifelse(is.infinite(`Median duration (min)`), NA, `Median duration (min)`),
    `Last submission date` = ifelse(is.infinite(as.numeric(`Last submission date`)), NA, as.character(`Last submission date`))
  )

# Add household-member sex composition from roster.
member_gender_by_enum <- if (nrow(roster_tracker) > 0) {
  roster_tracker |>
    count(District, Enumerator, `Household member gender`, name = "n") |>
    pivot_wider(names_from = `Household member gender`, values_from = n, values_fill = 0, names_prefix = "HH members: ") |>
    mutate(`Total HH members` = rowSums(across(starts_with("HH members:")), na.rm = TRUE))
} else {
  main_tracker |> distinct(District, Enumerator) |> mutate(`Total HH members` = NA_integer_)
}

# Add quality flags.
quality_by_enum <- if (nrow(quality_tracker) > 0) {
  quality_tracker |>
    count(District, Enumerator, name = "Quality flags")
} else {
  main_tracker |> distinct(District, Enumerator) |> mutate(`Quality flags` = 0L)
}

# Add lists of areas/points covered.
subcounty_list <- main_tracker |>
  group_by(District, Enumerator) |>
  summarise(
    `Sub-counties covered list` = paste(sort(unique(`Sub-county/division`)), collapse = ", "),
    `Settlements covered list` = paste(sort(unique(na.omit(Settlement))), collapse = ", "),
    `Zones covered list` = paste(sort(unique(na.omit(Zone))), collapse = ", "),
    `Point numbers covered list` = paste(sort(unique(na.omit(`Point number`))), collapse = ", "),
    .groups = "drop"
  )

coverage_update <- summary_by_enum |>
  left_join(subcounty_list, by = c("District", "Enumerator")) |>
  left_join(member_gender_by_enum, by = c("District", "Enumerator")) |>
  left_join(quality_by_enum, by = c("District", "Enumerator")) |>
  left_join(daily_by_enum, by = c("District", "Enumerator")) |>
  mutate(
    `Quality flags` = replace_na(`Quality flags`, 0L),
    across(where(is.numeric), ~ replace_na(.x, 0))
  ) |>
  arrange(District, desc(`Total forms`), Enumerator)

# Split one data frame per district. This is the same logic as the partner-based tracker,
# but now the split variable and sheet names are District.
coverage_update <- coverage_update |>
  group_split(District)

coverage_update <- lapply(coverage_update, \(x) select(x, -where(naniar::all_miss)))
coverage_update <- lapply(coverage_update, \(x) mutate(x, across(where(is.numeric), ~ replace_na(.x, 0))))

# Keep the District column inside the sheet, but name each sheet by district.
district_sheet_names <- main_tracker |> distinct(District) |> arrange(District) |> pull(District)
if (length(district_sheet_names) != length(coverage_update)) {
  district_sheet_names <- vapply(coverage_update, function(x) unique(x$District)[1], character(1))
}

clean_names_out <- character(0)
final_sheet_names <- character(length(district_sheet_names))
for (i in seq_along(district_sheet_names)) {
  final_sheet_names[[i]] <- make_clean_sheet_name(district_sheet_names[[i]], existing = clean_names_out)
  clean_names_out <- c(clean_names_out, final_sheet_names[[i]])
}
names(coverage_update) <- final_sheet_names

# Add national summary sheets before district sheets.
overview_sheet <- tibble::tibble(
  Metric = c(
    "Total submissions",
    "Districts covered",
    "Active enumerators",
    "Sample/point numbers covered",
    "Refugee households",
    "Host community households",
    "Male respondents",
    "Female respondents",
    "Quality flags",
    "Generated on"
  ),
  Value = c(
    nrow(main_tracker),
    n_distinct(main_tracker$District, na.rm = TRUE),
    n_distinct(main_tracker$Enumerator, na.rm = TRUE),
    n_distinct(main_tracker$`Point number`, na.rm = TRUE),
    sum(main_tracker$`Household status` == "Refugee", na.rm = TRUE),
    sum(main_tracker$`Household status` == "Host community", na.rm = TRUE),
    sum(main_tracker$`Respondent gender` == "Male", na.rm = TRUE),
    sum(main_tracker$`Respondent gender` == "Female", na.rm = TRUE),
    nrow(quality_tracker),
    as.character(Sys.time())
  )
)

district_index <- main_tracker |>
  group_by(District) |>
  summarise(
    `Total forms` = n(),
    `Active enumerators` = n_distinct(Enumerator, na.rm = TRUE),
    `Sub-counties covered` = n_distinct(`Sub-county/division`, na.rm = TRUE),
    `Settlements covered` = n_distinct(Settlement, na.rm = TRUE),
    `Zones covered` = n_distinct(Zone, na.rm = TRUE),
    `Point numbers covered` = n_distinct(`Point number`, na.rm = TRUE),
    `Refugee HHs` = sum(`Household status` == "Refugee", na.rm = TRUE),
    `Host HHs` = sum(`Household status` == "Host community", na.rm = TRUE),
    `Male respondents` = sum(`Respondent gender` == "Male", na.rm = TRUE),
    `Female respondents` = sum(`Respondent gender` == "Female", na.rm = TRUE),
    .groups = "drop"
  ) |>
  arrange(desc(`Total forms`))

national_daily_progress <- main_tracker |>
  filter(!is.na(tracker_submission_date)) |>
  count(tracker_submission_date, name = "Daily forms") |>
  arrange(tracker_submission_date) |>
  mutate(`Cumulative forms` = cumsum(`Daily forms`))

if (nrow(national_daily_progress) == 0) national_daily_progress <- message_df("No submission date available.")

tracker_workbook <- c(
  list(
    Overview = overview_sheet,
    District_Index = district_index,
    National_Daily_Progress = national_daily_progress
  ),
  coverage_update
)

# -------------------------------------------------------------------------------------------------
# 7. Export formatted workbook - simple row-1 headers like the user's requested format
# -------------------------------------------------------------------------------------------------

export_coverage <- function(df_list, export_file_name, export_directory = "") {
  if (!is.list(df_list) || length(df_list) == 0) {
    stop("df_list must be a non-empty list of dataframes")
  }
  
  if (!dir.exists(export_directory) && export_directory != "") {
    dir.create(export_directory, recursive = TRUE)
  }
  
  wb <- createWorkbook()
  
  header_style <- createStyle(
    fontName = "Arial Narrow", fontSize = 11, fontColour = "white",
    border = "TopBottomLeftRight", borderColour = "black",
    halign = "center", fgFill = "#E34443", textDecoration = "bold",
    wrapText = TRUE, valign = "center"
  )
  
  table_body_style <- createStyle(
    fontName = "Arial Narrow", fontSize = 8,
    border = "TopBottomLeftRight", borderColour = "grey26", borderStyle = "thin",
    wrapText = FALSE, valign = "top"
  )
  
  total_style <- createStyle(
    fontName = "Arial Narrow", fontSize = 8,
    border = "TopBottomLeftRight", borderColour = "grey26", borderStyle = "thin",
    fgFill = "#FCE4D6", textDecoration = "bold"
  )
  
  for (df_name in names(df_list)) {
    df <- df_list[[df_name]]
    
    if (!is.data.frame(df)) next
    if (nrow(df) == 0 && ncol(df) == 0) df <- message_df("No records available for this sheet.")
    if (ncol(df) == 0) df <- message_df("No columns available for this sheet.")
    
    if (any(sapply(df, inherits, "POSIXct"))) {
      df <- df %>% mutate(across(where(is.POSIXct), as.character))
    }
    
    addWorksheet(wb, sheetName = df_name, gridLines = FALSE)
    writeData(wb, sheet = df_name, df, rowNames = FALSE, colNames = TRUE)
    
    setColWidths(wb, sheet = df_name, cols = 1:ncol(df), widths = rep(16, ncol(df)))
    setRowHeights(wb, sheet = df_name, rows = 1, heights = 35)
    
    addStyle(wb, sheet = df_name, header_style, rows = 1, cols = 1:ncol(df), gridExpand = TRUE)
    
    if (nrow(df) > 0) {
      addStyle(wb, sheet = df_name, table_body_style, rows = 2:(nrow(df) + 1), cols = 1:ncol(df), gridExpand = TRUE)
    }
    
    freezePane(wb, sheet = df_name, firstRow = TRUE)
    addFilter(wb, sheet = df_name, rows = 1, cols = 1:ncol(df))
    
    # Highlight key count/total columns so supervisors can read quickly.
    total_cols <- which(str_detect(names(df), regex("total|forms|host|refugee|male|female|quality|members", ignore_case = TRUE)))
    if (length(total_cols) > 0 && nrow(df) > 0) {
      addStyle(wb, sheet = df_name, total_style, rows = 2:(nrow(df) + 1), cols = total_cols, gridExpand = TRUE, stack = TRUE)
    }
  }
  
  saveWorkbook(wb, file.path(export_directory, export_file_name), overwrite = TRUE)
}

if (!exists("path_cleaning_folder_today") || is.na(path_cleaning_folder_today) || path_cleaning_folder_today == "") {
  path_cleaning_folder_today <- "outputs"
}
if (!dir.exists(path_cleaning_folder_today)) dir.create(path_cleaning_folder_today, recursive = TRUE)

export_coverage(
  tracker_workbook,
  export_file_name = paste0("MSNA2026_Field_Data_Collection_Tracker_FINAL_tool_variables_", format(Sys.time(), "%Y-%m-%d_at_%Hh%M"), ".xlsx"),
  export_directory = path_cleaning_folder_today
)

cat("\n")
cat("\n")
cat(green$bold("District-level coverage update file exported"))
cat("\n")



# -------------------------------------------------------------------------------------------------
# 8. Save a Shiny-ready cache so the app can run without uploading files
# -------------------------------------------------------------------------------------------------

# By default this matches the Shiny app folder supplied with this script. If your Shiny app folder
# has another name/location, set path_shiny_tracker_folder before running this script, for example:
# path_shiny_tracker_folder <- "path/to/MSNA2026_Mozambique_Field_Tracker_Shiny_cached"
if (!exists("path_shiny_tracker_folder") || is.na(path_shiny_tracker_folder) || path_shiny_tracker_folder == "") {
  path_shiny_tracker_folder <- "MSNA2026_Mozambique_Field_Tracker_Shiny_cached"
}

path_shiny_tracker_cache <- file.path(path_shiny_tracker_folder, "data_cache")
if (!dir.exists(path_shiny_tracker_cache)) dir.create(path_shiny_tracker_cache, recursive = TRUE)

safe_get_tracker_object <- function(object_name) {
  if (exists(object_name, inherits = TRUE)) get(object_name, inherits = TRUE) else NULL
}

msna2026_tracker_cache <- list(
  cache_created_at = Sys.time(),
  generated_at = Sys.time(),
  data_collected = data_collected,
  data_hh_main = if (exists("data_hh_main", inherits = TRUE) && is.data.frame(data_hh_main)) data_hh_main else data_collected,
  raw_data_list = raw_data_list,
  main_tracker = main_tracker,
  roster_tracker = roster_tracker,
  quality_log = quality_log,
  quality_tracker = quality_tracker,
  cleaning_log_template = safe_get_tracker_object("cleaning_log_template"),
  cleaning_log = safe_get_tracker_object("cleaning_log"),
  combined_log = safe_get_tracker_object("combined_log"),
  deletion_log = safe_get_tracker_object("deletion_log"),
  cleaning_dashboard = safe_get_tracker_object("cleaning_dashboard"),
  repeat_count_summary = safe_get_tracker_object("repeat_count_summary"),
  sheet_record_counts = safe_get_tracker_object("sheet_record_counts"),
  df_survey = safe_get_tracker_object("df_survey"),
  df_choices = safe_get_tracker_object("df_choices"),
  tracker_workbook = tracker_workbook,
  coverage_update = coverage_update,
  path_cleaning_folder_today = if (exists("path_cleaning_folder_today", inherits = TRUE)) path_cleaning_folder_today else NA_character_
)

latest_cache_path <- file.path(path_shiny_tracker_cache, "latest_msna2026_tracker_cache.rds")
timestamped_cache_path <- file.path(
  path_shiny_tracker_cache,
  paste0("msna2026_tracker_cache_", format(Sys.time(), "%Y-%m-%d_at_%Hh%M"), ".rds")
)

saveRDS(msna2026_tracker_cache, latest_cache_path)
saveRDS(msna2026_tracker_cache, timestamped_cache_path)

cat("\n")
cat(green$bold("Shiny tracker cache saved"))
cat("\n")
cat("Latest cache: ", latest_cache_path, "\n", sep = "")
cat("Timestamped cache: ", timestamped_cache_path, "\n", sep = "")

# -------------------------------------------------------------------------------------------------
# 9. Object tracking output, same style as the cleaning scripts
# -------------------------------------------------------------------------------------------------

all_dfs_names_after <- Filter(function(x) is(x, "data.frame"), mget(ls())) |> names()
all_lists_names_after <- Filter(function(x) is(x, "list"), mget(ls())) |> names()

if(length(setdiff(all_dfs_names_after, all_dfs_names_before)) > 0){
  cat(blue("The new dataframes created are:"))
  cat("\n")
  for (df in setdiff(all_dfs_names_after, all_dfs_names_before)) {
    cat(df)
    cat("\n")
  }
}

if(length(setdiff(all_lists_names_after, all_lists_names_before)) > 0){
  cat(blue("The new lists created are:"))
  cat("\n")
  for (df in setdiff(all_lists_names_after, all_lists_names_before)) {
    cat(df)
    cat("\n")
  }
}
