cat("\014")

# ==================================================================================================
# REACH MSNA 2026 Uganda - 7. Target progress and daily-rate tracker
# Version: vector-safe rates + README guide sheet
# --------------------------------------------------------------------------------------------------
# Purpose:
#   - Create an Excel tracker showing target achieved, pending interviews, actual daily rate,
#     expected daily rate, and days needed to complete.
#   - Summaries are produced separately for host communities and refugee settlements.
#   - Assumption: each enumerator is expected to complete 3 interviews per day.
#   - Assumption: each host community target = 150 surveys; each refugee settlement target = 106 surveys.
#
# Run after Script 1, or after Script 4 if you want to use the processed checked dataset.
# ==================================================================================================

all_dfs_names_before <- Filter(function(x) is(x, "data.frame"), mget(ls())) |> names()
all_lists_names_before <- Filter(function(x) is(x, "list"), mget(ls())) |> names()

if (exists("print_step", mode = "function")) {
  print_step("7. Creating MSNA target progress and daily-rate tracker")
} else {
  cat("\n7. Creating MSNA target progress and daily-rate tracker\n")
}

# --------------------------------------------------------------------------------------------------
# 0. Packages and output folder
# --------------------------------------------------------------------------------------------------

required_packages <- c("dplyr", "tidyr", "stringr", "lubridate", "purrr", "tibble", "openxlsx", "ggplot2", "scales")

for (pkg in required_packages) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    stop("Package '", pkg, "' is required for Script 7.", call. = FALSE)
  }
}

if (!exists("path_cleaning_folder_today")) {
  path_cleaning_folder_today <- file.path("outputs", paste0("MSNA data cleaning ", format(Sys.Date(), "%Y-%b-%d")))
}

if (!dir.exists(path_cleaning_folder_today)) {
  dir.create(path_cleaning_folder_today, recursive = TRUE)
}

path_target_tracker_folder <- file.path(path_cleaning_folder_today, "MSNA target progress tracker")
if (!dir.exists(path_target_tracker_folder)) {
  dir.create(path_target_tracker_folder, recursive = TRUE)
}

# --------------------------------------------------------------------------------------------------
# 1. User-editable assumptions
# --------------------------------------------------------------------------------------------------

host_target_per_community <- 150
refugee_target_per_settlement <- 106
expected_interviews_per_enum_per_day <- 3

# Optional: set TRUE if point_number should be counted once only within each location.
# Default FALSE counts one row/submission as one completed survey, using UUID where available.
count_unique_point_number <- FALSE

# Optional manual target override.
# Leave empty unless a specific host community or settlement has a different target.
# Required columns if used: location_type, location_name, target
# location_type values: "host_community", "refugee_settlement"
manual_location_targets <- tibble::tibble(
  location_type = character(),
  location_name = character(),
  target = numeric()
)

# --------------------------------------------------------------------------------------------------
# 2. Helper functions
# --------------------------------------------------------------------------------------------------

`%||%` <- function(x, y) {
  if (is.null(x)) y else x
}

standardise_chr <- function(x) {
  x <- as.character(x)
  x <- stringr::str_squish(x)
  x[x == ""] <- NA_character_
  x
}

clean_key <- function(x) {
  x |>
    as.character() |>
    stringr::str_squish() |>
    stringr::str_to_lower() |>
    stringr::str_replace_all("[^a-z0-9]+", "_") |>
    stringr::str_replace_all("_+", "_") |>
    stringr::str_replace_all("^_|_$", "")
}

standardise_refugee_settlement_location <- function(x) {
  # User-specified grouping: these Adjumani settlement/site values should be
  # treated as one refugee settlement location called "adjumani" in the tracker.
  x_chr <- standardise_chr(x)
  x_key <- clean_key(x_chr)
  
  adjumani_settlement_keys <- c(
    "adjumani_pagirinya",
    "adjumani_ayilo_i",
    "adjumani_ayilo_ii",
    "adjumani_nyumanzi",
    "adjumani_olua_i",
    "adjumani_agojo",
    "adjumani_baratuku",
    "adjumani_olua_ii",
    "adjumani_mirieyi",
    "adjumani_maaji",
    "adjumani_mungula_i"
  )
  
  dplyr::case_when(
    x_key %in% adjumani_settlement_keys ~ "adjumani",
    TRUE ~ x_chr
  )
}


standardise_host_community_location <- function(admin2_value, admin3_value = NA_character_) {
  # Host-community tracking is by host district/community. Default is admin2.
  dplyr::coalesce(standardise_chr(admin2_value), standardise_chr(admin3_value), "missing_host_community")
}

first_existing_col <- function(df, candidates) {
  hit <- candidates[candidates %in% names(df)]
  if (length(hit) == 0) NA_character_ else hit[[1]]
}

pull_first_existing <- function(df, candidates, default = NA_character_) {
  hit <- first_existing_col(df, candidates)
  if (is.na(hit)) {
    rep(default, nrow(df))
  } else {
    df[[hit]]
  }
}

parse_datetime_tracker <- function(x) {
  if (inherits(x, "POSIXct") || inherits(x, "POSIXt")) return(x)
  if (inherits(x, "Date")) return(as.POSIXct(x))
  suppressWarnings(
    lubridate::parse_date_time(
      as.character(x),
      orders = c(
        "ymd HMS", "ymd HM", "ymd IMS p", "ymd IM p", "ymd",
        "dmy HMS", "dmy HM", "dmy IMS p", "dmy IM p", "dmy",
        "mdy HMS", "mdy HM", "mdy IMS p", "mdy IM p", "mdy",
        "Ymd HMS", "Ymd HM"
      ),
      tz = "Africa/Kampala"
    )
  )
}

safe_date_tracker <- function(x) {
  out <- suppressWarnings(lubridate::as_date(parse_datetime_tracker(x)))
  if (all(is.na(out))) {
    out <- suppressWarnings(lubridate::as_date(x))
  }
  out
}

safe_divide <- function(num, den) {
  # Vector-safe division. Works when numerator is a vector and denominator is either
  # a scalar, for example 3 interviews/day, or a vector.
  num <- suppressWarnings(as.numeric(num))
  den <- suppressWarnings(as.numeric(den))
  out_len <- max(length(num), length(den), 1)
  num <- rep(num, length.out = out_len)
  den <- rep(den, length.out = out_len)
  out <- rep(NA_real_, out_len)
  ok <- !is.na(num) & !is.na(den) & is.finite(num) & is.finite(den) & den > 0
  out[ok] <- num[ok] / den[ok]
  out
}

safe_ceiling <- function(x) {
  # Vector-safe ceiling that returns NA where values are missing or infinite.
  x <- suppressWarnings(as.numeric(x))
  out <- rep(NA_real_, length(x))
  ok <- !is.na(x) & is.finite(x)
  out[ok] <- ceiling(x[ok])
  out
}

format_percent_num <- function(x) {
  round(x * 100, 1)
}

make_valid_sheet_name <- function(x) {
  x <- as.character(x)
  x <- stringr::str_replace_all(x, "[\\[\\]\\*\\?/\\\\:]", "_")
  x <- stringr::str_squish(x)
  x <- ifelse(is.na(x) | x == "", "sheet", x)
  substr(x, 1, 31)
}

write_table_sheet <- function(wb, sheet_name, df, freeze_first_row = TRUE) {
  sheet_name <- make_valid_sheet_name(sheet_name)
  if (sheet_name %in% names(wb)) openxlsx::removeWorksheet(wb, sheet_name)
  openxlsx::addWorksheet(wb, sheet_name)
  df <- tibble::as_tibble(df)
  if (nrow(df) == 0 && ncol(df) == 0) {
    df <- tibble::tibble(message = "No records")
  }
  openxlsx::writeDataTable(wb, sheet = sheet_name, x = df, tableStyle = "TableStyleMedium2")
  if (freeze_first_row) openxlsx::freezePane(wb, sheet = sheet_name, firstRow = TRUE)
  if (ncol(df) > 0) openxlsx::setColWidths(wb, sheet = sheet_name, cols = seq_len(ncol(df)), widths = "auto")
  invisible(wb)
}

save_plot_png <- function(plot_obj, file_name, width = 12, height = 7, dpi = 160) {
  out <- file.path(path_target_tracker_folder, file_name)
  ggplot2::ggsave(filename = out, plot = plot_obj, width = width, height = height, dpi = dpi)
  out
}

# --------------------------------------------------------------------------------------------------
# 3. Get the main household dataset from the current environment
# --------------------------------------------------------------------------------------------------

if (exists("combined_log") && is.list(combined_log) && "checked_dataset" %in% names(combined_log) && is.data.frame(combined_log$checked_dataset)) {
  tracker_data_raw <- combined_log$checked_dataset
  tracker_data_source <- "combined_log$checked_dataset"
} else if (exists("combined_log_cleaningtools_style") && is.list(combined_log_cleaningtools_style) && "checked_dataset" %in% names(combined_log_cleaningtools_style) && is.data.frame(combined_log_cleaningtools_style$checked_dataset)) {
  tracker_data_raw <- combined_log_cleaningtools_style$checked_dataset
  tracker_data_source <- "combined_log_cleaningtools_style$checked_dataset"
} else if (exists("data_hh_main") && is.data.frame(data_hh_main)) {
  tracker_data_raw <- data_hh_main
  tracker_data_source <- "data_hh_main"
} else if (exists("raw_data_list") && is.list(raw_data_list) && "Submissions" %in% names(raw_data_list) && is.data.frame(raw_data_list$Submissions)) {
  tracker_data_raw <- raw_data_list$Submissions
  tracker_data_source <- "raw_data_list$Submissions"
} else if (exists("raw_data") && is.data.frame(raw_data)) {
  tracker_data_raw <- raw_data
  tracker_data_source <- "raw_data"
} else {
  stop(
    "No main household dataset found. Run Script 1 first. Expected one of: combined_log$checked_dataset, data_hh_main, raw_data_list$Submissions, or raw_data.",
    call. = FALSE
  )
}

if (!is.data.frame(tracker_data_raw) || nrow(tracker_data_raw) == 0) {
  stop("The main household dataset found for tracker creation has zero rows.", call. = FALSE)
}

tracker_data_raw <- tibble::as_tibble(tracker_data_raw)

# --------------------------------------------------------------------------------------------------
# 4. Standardise final-tool variables needed for tracking
# --------------------------------------------------------------------------------------------------

uuid_col <- first_existing_col(
  tracker_data_raw,
  c("uuid", "_uuid", "_id", "instance_name", "_submission__id", "_submission_id", "submission_uuid", "submission_id")
)

start_col <- first_existing_col(
  tracker_data_raw,
  c("start", "start_time", "begin", "begin_time", "today", "_submission_time", "submission_time")
)

end_col <- first_existing_col(
  tracker_data_raw,
  c("end", "end_time", "stop", "stop_time", "_submission_time", "submission_time")
)

date_col <- first_existing_col(
  tracker_data_raw,
  c("today", "start_date", "survey_date", "submission_date", "_submission_time", "submission_time", "start", "start_time")
)

tracker_data <- tracker_data_raw |>
  dplyr::mutate(
    survey_id = if (!is.na(uuid_col)) as.character(.data[[uuid_col]]) else paste0("row_", dplyr::row_number()),
    point_number = as.character(pull_first_existing(dplyr::cur_data_all(), c("point_number", "pt_num", "sample_point", "cluster_id"))),
    admin1 = as.character(pull_first_existing(dplyr::cur_data_all(), c("admin1", "region"))),
    admin2 = as.character(pull_first_existing(dplyr::cur_data_all(), c("admin2", "district", "district_name"))),
    admin3 = as.character(pull_first_existing(dplyr::cur_data_all(), c("admin3", "sub_county_division", "sub_county", "division"))),
    admin3_setlement = as.character(pull_first_existing(dplyr::cur_data_all(), c("admin3_setlement", "admin3_settlement", "settlement", "refugee_settlement"))),
    admin4 = as.character(pull_first_existing(dplyr::cur_data_all(), c("admin4", "zone", "refugee_settlement_zone", "settlement_zone"))),
    hoh_type = as.character(pull_first_existing(dplyr::cur_data_all(), c("hoh_type", "population_group", "status"))),
    enum_id = as.character(pull_first_existing(dplyr::cur_data_all(), c("enum_id", "enumerator_id", "enumerator", "username", "_submitted_by", "submitted_by"))),
    enum_gender = as.character(pull_first_existing(dplyr::cur_data_all(), c("enum_gender", "enumerator_gender"))),
    resp_gender = as.character(pull_first_existing(dplyr::cur_data_all(), c("resp_gender", "respondent_gender", "gender"))),
    start_time = if (!is.na(start_col)) parse_datetime_tracker(.data[[start_col]]) else as.POSIXct(NA),
    end_time = if (!is.na(end_col)) parse_datetime_tracker(.data[[end_col]]) else as.POSIXct(NA),
    start_date = if (!is.na(date_col)) safe_date_tracker(.data[[date_col]]) else safe_date_tracker(start_time)
  ) |>
  dplyr::mutate(
    start_date = dplyr::if_else(is.na(start_date), lubridate::as_date(start_time), start_date),
    admin1 = standardise_chr(admin1),
    admin2 = standardise_chr(admin2),
    admin3 = standardise_chr(admin3),
    admin3_setlement = standardise_chr(admin3_setlement),
    admin4 = standardise_chr(admin4),
    hoh_type = standardise_chr(hoh_type),
    enum_id = standardise_chr(enum_id),
    point_number = standardise_chr(point_number),
    population_group = dplyr::case_when(
      stringr::str_detect(stringr::str_to_lower(hoh_type), "refugee") ~ "refugee",
      stringr::str_detect(stringr::str_to_lower(hoh_type), "host") ~ "host_community",
      stringr::str_detect(stringr::str_to_lower(hoh_type), "national") ~ "host_community",
      TRUE ~ NA_character_
    ),
    location_type = dplyr::case_when(
      population_group == "refugee" ~ "refugee_settlement",
      population_group == "host_community" ~ "host_community",
      TRUE ~ "unclassified"
    ),
    raw_refugee_settlement = dplyr::coalesce(admin3_setlement, admin3, admin2, "missing_refugee_settlement"),
    location_name = dplyr::case_when(
      location_type == "refugee_settlement" ~ standardise_refugee_settlement_location(raw_refugee_settlement),
      location_type == "host_community" ~ standardise_host_community_location(admin2, admin3),
      TRUE ~ dplyr::coalesce(admin2, admin3_setlement, admin3, "unclassified")
    ),
    location_key = paste(location_type, clean_key(location_name), sep = "__"),
    expected_target = dplyr::case_when(
      location_type == "host_community" ~ as.numeric(host_target_per_community),
      location_type == "refugee_settlement" ~ as.numeric(refugee_target_per_settlement),
      TRUE ~ NA_real_
    )
  )

# Remove blank/test rows where no survey identifier and no meaningful location exist.
tracker_data <- tracker_data |>
  dplyr::filter(
    !(is.na(survey_id) | trimws(as.character(survey_id)) == "") |
      !(is.na(point_number) | trimws(as.character(point_number)) == "")
  )

# De-duplicate survey rows for progress counts.
# For normal Kobo household submissions, survey_id is the safest unit of counting.
if (isTRUE(count_unique_point_number) && "point_number" %in% names(tracker_data)) {
  tracker_count_data <- tracker_data |>
    dplyr::filter(!is.na(point_number), trimws(point_number) != "") |>
    dplyr::distinct(location_key, point_number, .keep_all = TRUE)
} else {
  tracker_count_data <- tracker_data |>
    dplyr::distinct(survey_id, .keep_all = TRUE)
}

# Apply manual target overrides if supplied.
if (is.data.frame(manual_location_targets) && nrow(manual_location_targets) > 0) {
  manual_location_targets_clean <- manual_location_targets |>
    dplyr::mutate(
      location_type = as.character(location_type),
      location_name = as.character(location_name),
      location_key = paste(location_type, clean_key(location_name), sep = "__"),
      target_override = suppressWarnings(as.numeric(target))
    ) |>
    dplyr::filter(!is.na(location_key), !is.na(target_override)) |>
    dplyr::select(location_key, target_override) |>
    dplyr::distinct(location_key, .keep_all = TRUE)
} else {
  manual_location_targets_clean <- tibble::tibble(location_key = character(), target_override = numeric())
}

# --------------------------------------------------------------------------------------------------
# 5. Daily and enumerator summaries
# --------------------------------------------------------------------------------------------------

location_targets <- tracker_count_data |>
  dplyr::filter(location_type %in% c("host_community", "refugee_settlement")) |>
  dplyr::group_by(location_type, location_key, location_name) |>
  dplyr::summarise(
    admin2 = paste(sort(unique(stats::na.omit(as.character(admin2)))), collapse = ", "),
    admin3_setlement = paste(sort(unique(stats::na.omit(as.character(admin3_setlement)))), collapse = ", "),
    raw_refugee_settlement_values = paste(sort(unique(stats::na.omit(as.character(raw_refugee_settlement)))), collapse = ", "),
    default_target = dplyr::first(expected_target),
    .groups = "drop"
  ) |>
  dplyr::left_join(manual_location_targets_clean, by = "location_key") |>
  dplyr::mutate(
    target = dplyr::coalesce(target_override, default_target),
    target_source = dplyr::if_else(!is.na(target_override), "manual override", "default assumption")
  ) |>
  dplyr::select(location_type, location_name, location_key, admin2, admin3_setlement, raw_refugee_settlement_values, target, target_source)

daily_by_location <- tracker_count_data |>
  dplyr::filter(location_type %in% c("host_community", "refugee_settlement")) |>
  dplyr::filter(!is.na(start_date)) |>
  dplyr::group_by(location_type, location_key, location_name, start_date) |>
  dplyr::summarise(
    completed_surveys = dplyr::n_distinct(survey_id, na.rm = TRUE),
    enumerators_used = dplyr::n_distinct(enum_id[!is.na(enum_id) & trimws(enum_id) != ""]),
    expected_surveys_at_3_per_enum = enumerators_used * expected_interviews_per_enum_per_day,
    .groups = "drop"
  ) |>
  dplyr::mutate(
    daily_rate_vs_expected = safe_divide(completed_surveys, expected_surveys_at_3_per_enum),
    daily_gap_against_expected = completed_surveys - expected_surveys_at_3_per_enum
  ) |>
  dplyr::arrange(location_type, location_name, start_date)

enumerator_daily <- tracker_count_data |>
  dplyr::filter(location_type %in% c("host_community", "refugee_settlement")) |>
  dplyr::filter(!is.na(start_date), !is.na(enum_id), trimws(enum_id) != "") |>
  dplyr::group_by(location_type, location_key, location_name, start_date, enum_id) |>
  dplyr::summarise(
    interviews_done = dplyr::n_distinct(survey_id, na.rm = TRUE),
    expected_interviews = expected_interviews_per_enum_per_day,
    difference_from_expected = interviews_done - expected_interviews,
    percent_of_expected = safe_divide(interviews_done, expected_interviews),
    .groups = "drop"
  ) |>
  dplyr::arrange(location_type, location_name, start_date, enum_id)

enumerator_summary <- enumerator_daily |>
  dplyr::group_by(location_type, location_key, location_name, enum_id) |>
  dplyr::summarise(
    days_worked = dplyr::n_distinct(start_date),
    total_interviews = sum(interviews_done, na.rm = TRUE),
    expected_interviews = sum(expected_interviews, na.rm = TRUE),
    average_interviews_per_day = safe_divide(total_interviews, days_worked),
    percent_of_expected = safe_divide(total_interviews, expected_interviews),
    .groups = "drop"
  ) |>
  dplyr::arrange(location_type, location_name, enum_id)

# --------------------------------------------------------------------------------------------------
# 6. Summary by host community / refugee settlement
# --------------------------------------------------------------------------------------------------

completed_by_location <- tracker_count_data |>
  dplyr::filter(location_type %in% c("host_community", "refugee_settlement")) |>
  dplyr::group_by(location_type, location_key, location_name) |>
  dplyr::summarise(
    completed_surveys = dplyr::n_distinct(survey_id, na.rm = TRUE),
    completed_point_numbers = dplyr::n_distinct(point_number[!is.na(point_number) & trimws(point_number) != ""]),
    enumerators_used = dplyr::n_distinct(enum_id[!is.na(enum_id) & trimws(enum_id) != ""]),
    first_survey_date = suppressWarnings(min(start_date, na.rm = TRUE)),
    latest_survey_date = suppressWarnings(max(start_date, na.rm = TRUE)),
    active_data_collection_days = dplyr::n_distinct(start_date[!is.na(start_date)]),
    enumerator_days_used = dplyr::n_distinct(paste(enum_id, start_date)[!is.na(enum_id) & trimws(enum_id) != "" & !is.na(start_date)]),
    .groups = "drop"
  ) |>
  dplyr::mutate(
    first_survey_date = dplyr::if_else(is.infinite(as.numeric(first_survey_date)), as.Date(NA), first_survey_date),
    latest_survey_date = dplyr::if_else(is.infinite(as.numeric(latest_survey_date)), as.Date(NA), latest_survey_date)
  )

summary_by_location <- location_targets |>
  dplyr::select(location_type, location_key, location_name, target, target_source) |>
  dplyr::full_join(completed_by_location, by = c("location_type", "location_key", "location_name")) |>
  dplyr::mutate(
    target = dplyr::coalesce(target, dplyr::case_when(
      location_type == "host_community" ~ as.numeric(host_target_per_community),
      location_type == "refugee_settlement" ~ as.numeric(refugee_target_per_settlement),
      TRUE ~ NA_real_
    )),
    completed_surveys = dplyr::coalesce(completed_surveys, 0L),
    pending_surveys = pmax(target - completed_surveys, 0),
    excess_surveys = pmax(completed_surveys - target, 0),
    percent_achieved = safe_divide(completed_surveys, target),
    percent_pending = safe_divide(pending_surveys, target),
    actual_daily_rate = safe_divide(completed_surveys, active_data_collection_days),
    actual_enum_day_rate = safe_divide(completed_surveys, enumerator_days_used),
    expected_daily_rate_from_enumerators = enumerators_used * expected_interviews_per_enum_per_day,
    expected_total_output_from_enum_days = enumerator_days_used * expected_interviews_per_enum_per_day,
    productivity_vs_expected_enum_days = safe_divide(completed_surveys, expected_total_output_from_enum_days),
    days_needed_at_actual_daily_rate = safe_ceiling(safe_divide(pending_surveys, actual_daily_rate)),
    days_needed_at_expected_enum_rate = safe_ceiling(safe_divide(pending_surveys, expected_daily_rate_from_enumerators)),
    enumerator_days_needed_at_3_per_day = safe_ceiling(safe_divide(pending_surveys, expected_interviews_per_enum_per_day)),
    status = dplyr::case_when(
      is.na(target) ~ "No target defined",
      completed_surveys >= target ~ "Target achieved",
      completed_surveys == 0 ~ "Not started",
      TRUE ~ "Pending"
    )
  ) |>
  dplyr::arrange(location_type, dplyr::desc(percent_achieved), location_name)

summary_by_population_group <- summary_by_location |>
  dplyr::group_by(location_type) |>
  dplyr::summarise(
    locations = dplyr::n(),
    target = sum(target, na.rm = TRUE),
    completed_surveys = sum(completed_surveys, na.rm = TRUE),
    pending_surveys = sum(pending_surveys, na.rm = TRUE),
    percent_achieved = safe_divide(completed_surveys, target),
    enumerators_used = dplyr::n_distinct(tracker_count_data$enum_id[tracker_count_data$location_type %in% unique(location_type) & !is.na(tracker_count_data$enum_id) & trimws(tracker_count_data$enum_id) != ""]),
    active_data_collection_days = dplyr::n_distinct(tracker_count_data$start_date[tracker_count_data$location_type %in% unique(location_type) & !is.na(tracker_count_data$start_date)]),
    actual_daily_rate = safe_divide(completed_surveys, active_data_collection_days),
    expected_daily_rate_from_enumerators = enumerators_used * expected_interviews_per_enum_per_day,
    days_needed_at_actual_daily_rate = safe_ceiling(safe_divide(pending_surveys, actual_daily_rate)),
    days_needed_at_expected_enum_rate = safe_ceiling(safe_divide(pending_surveys, expected_daily_rate_from_enumerators)),
    .groups = "drop"
  )

overall_summary <- tibble::tibble(
  data_source = tracker_data_source,
  date_created = as.character(Sys.time()),
  total_target = sum(summary_by_location$target, na.rm = TRUE),
  completed_surveys = sum(summary_by_location$completed_surveys, na.rm = TRUE),
  pending_surveys = sum(summary_by_location$pending_surveys, na.rm = TRUE),
  percent_achieved = safe_divide(completed_surveys, total_target),
  host_target_per_community = host_target_per_community,
  refugee_target_per_settlement = refugee_target_per_settlement,
  expected_interviews_per_enum_per_day = expected_interviews_per_enum_per_day,
  locations_tracked = nrow(summary_by_location),
  host_communities_tracked = sum(summary_by_location$location_type == "host_community", na.rm = TRUE),
  refugee_settlements_tracked = sum(summary_by_location$location_type == "refugee_settlement", na.rm = TRUE),
  unique_enumerators = dplyr::n_distinct(tracker_count_data$enum_id[!is.na(tracker_count_data$enum_id) & trimws(tracker_count_data$enum_id) != ""]),
  active_data_collection_days = dplyr::n_distinct(tracker_count_data$start_date[!is.na(tracker_count_data$start_date)]),
  actual_overall_daily_rate = safe_divide(completed_surveys, active_data_collection_days),
  expected_overall_daily_rate_from_enumerators = unique_enumerators * expected_interviews_per_enum_per_day,
  days_needed_at_actual_overall_daily_rate = safe_ceiling(safe_divide(pending_surveys, actual_overall_daily_rate)),
  days_needed_at_expected_overall_enum_rate = safe_ceiling(safe_divide(pending_surveys, expected_overall_daily_rate_from_enumerators))
)

# Friendlier percentage columns for Excel tables.
summary_by_location_export <- summary_by_location |>
  dplyr::mutate(
    percent_achieved_pct = format_percent_num(percent_achieved),
    percent_pending_pct = format_percent_num(percent_pending),
    productivity_vs_expected_enum_days_pct = format_percent_num(productivity_vs_expected_enum_days)
  ) |>
  dplyr::select(
    location_type,
    location_name,
    target,
    completed_surveys,
    pending_surveys,
    excess_surveys,
    percent_achieved_pct,
    percent_pending_pct,
    status,
    enumerators_used,
    active_data_collection_days,
    enumerator_days_used,
    actual_daily_rate,
    actual_enum_day_rate,
    expected_daily_rate_from_enumerators,
    expected_total_output_from_enum_days,
    productivity_vs_expected_enum_days_pct,
    days_needed_at_actual_daily_rate,
    days_needed_at_expected_enum_rate,
    enumerator_days_needed_at_3_per_day,
    first_survey_date,
    latest_survey_date,
    target_source,
    location_key
  )

daily_by_location_export <- daily_by_location |>
  dplyr::mutate(daily_rate_vs_expected_pct = format_percent_num(daily_rate_vs_expected))

enumerator_daily_export <- enumerator_daily |>
  dplyr::mutate(percent_of_expected_pct = format_percent_num(percent_of_expected))

enumerator_summary_export <- enumerator_summary |>
  dplyr::mutate(percent_of_expected_pct = format_percent_num(percent_of_expected))

# --------------------------------------------------------------------------------------------------
# 6a. Daily survey matrix (wide format: locations × dates)
# --------------------------------------------------------------------------------------------------
# Columns:  location_type | location_name | <date1> | <date2> | ... | Total
# Rows:     one per location; last row = Grand Total across all locations.
# Each cell shows the number of completed surveys for that location on that date.
# Running the script on a new day adds a new date column automatically.

all_survey_dates_sorted <- daily_by_location |>
  dplyr::filter(!is.na(start_date)) |>
  dplyr::pull(start_date) |>
  unique() |>
  sort()

date_col_names <- as.character(all_survey_dates_sorted)

daily_survey_matrix_wide <- daily_by_location |>
  dplyr::filter(!is.na(start_date)) |>
  dplyr::select(location_type, location_name, start_date, completed_surveys) |>
  dplyr::mutate(start_date = as.character(start_date)) |>
  tidyr::pivot_wider(
    names_from  = start_date,
    values_from = completed_surveys,
    values_fill = 0L,
    names_sort  = TRUE
  ) |>
  dplyr::arrange(location_type, location_name)

# Use only the date columns that actually exist in the pivot output
date_cols_present <- intersect(date_col_names, names(daily_survey_matrix_wide))

daily_survey_matrix_wide <- daily_survey_matrix_wide |>
  dplyr::select(location_type, location_name, dplyr::all_of(date_cols_present)) |>
  dplyr::mutate(Total = rowSums(dplyr::pick(dplyr::all_of(date_cols_present)), na.rm = TRUE))

# Embed daily date columns into summary_by_location_export (right after target, before completed_surveys).
# This allows the reader to see daily progress in the same row as location summary statistics.
summary_by_location_export <- summary_by_location_export |>
  dplyr::left_join(
    daily_survey_matrix_wide |>
      dplyr::select(location_type, location_name, dplyr::all_of(date_cols_present)),
    by = c("location_type", "location_name")
  ) |>
  dplyr::relocate(dplyr::all_of(date_cols_present), .after = target)

# Grand Total row (sum of every column)
grand_total_row <- dplyr::bind_cols(
  tibble::tibble(location_type = "GRAND TOTAL", location_name = "All locations"),
  daily_survey_matrix_wide |>
    dplyr::summarise(dplyr::across(dplyr::all_of(c(date_cols_present, "Total")), ~ sum(.x, na.rm = TRUE)))
)

daily_survey_matrix <- dplyr::bind_rows(daily_survey_matrix_wide, grand_total_row)

# Compact overall-daily totals for the Dashboard (all locations combined per day)
overall_daily_totals <- tracker_count_data |>
  dplyr::filter(!is.na(start_date)) |>
  dplyr::group_by(start_date) |>
  dplyr::summarise(
    total_surveys        = dplyr::n_distinct(survey_id, na.rm = TRUE),
    host_community       = dplyr::n_distinct(survey_id[location_type == "host_community"], na.rm = TRUE),
    refugee_settlement   = dplyr::n_distinct(survey_id[location_type == "refugee_settlement"], na.rm = TRUE),
    enumerators_active   = dplyr::n_distinct(enum_id[!is.na(enum_id) & trimws(enum_id) != ""]),
    .groups = "drop"
  ) |>
  dplyr::arrange(start_date) |>
  dplyr::mutate(cumulative_total = cumsum(total_surveys))

# --------------------------------------------------------------------------------------------------
# 7. Charts
# --------------------------------------------------------------------------------------------------

chart_data_achieved_pending <- summary_by_location_export |>
  dplyr::filter(location_type %in% c("host_community", "refugee_settlement")) |>
  dplyr::select(location_type, location_name, completed_surveys, pending_surveys) |>
  tidyr::pivot_longer(
    cols = c(completed_surveys, pending_surveys),
    names_to = "measure",
    values_to = "surveys"
  ) |>
  dplyr::mutate(
    location_label = paste0(location_name, " (", dplyr::if_else(location_type == "host_community", "Host", "Refugee"), ")"),
    measure = dplyr::case_when(
      measure == "completed_surveys" ~ "Achieved",
      measure == "pending_surveys" ~ "Pending",
      TRUE ~ measure
    )
  )

plot_achieved_pending <- ggplot2::ggplot(
  chart_data_achieved_pending,
  ggplot2::aes(x = stats::reorder(location_label, surveys, sum), y = surveys, fill = measure)
) +
  ggplot2::geom_col() +
  ggplot2::coord_flip() +
  ggplot2::labs(
    title = "MSNA target achieved and pending by location",
    x = NULL,
    y = "Number of surveys",
    fill = NULL
  ) +
  ggplot2::theme_minimal(base_size = 11)

plot_days_needed <- ggplot2::ggplot(
  summary_by_location_export,
  ggplot2::aes(x = stats::reorder(location_name, days_needed_at_actual_daily_rate), y = days_needed_at_actual_daily_rate)
) +
  ggplot2::geom_col() +
  ggplot2::coord_flip() +
  ggplot2::labs(
    title = "Estimated days needed to complete at current actual daily rate",
    x = NULL,
    y = "Days needed"
  ) +
  ggplot2::theme_minimal(base_size = 11)

rate_chart_data <- summary_by_location_export |>
  dplyr::select(location_type, location_name, actual_daily_rate, expected_daily_rate_from_enumerators) |>
  tidyr::pivot_longer(
    cols = c(actual_daily_rate, expected_daily_rate_from_enumerators),
    names_to = "rate_type",
    values_to = "rate"
  ) |>
  dplyr::mutate(
    rate_type = dplyr::case_when(
      rate_type == "actual_daily_rate" ~ "Actual daily rate",
      rate_type == "expected_daily_rate_from_enumerators" ~ "Expected daily rate (3 per enum/day)",
      TRUE ~ rate_type
    )
  )

plot_daily_rate <- ggplot2::ggplot(
  rate_chart_data,
  ggplot2::aes(x = stats::reorder(location_name, rate), y = rate, fill = rate_type)
) +
  ggplot2::geom_col(position = "dodge") +
  ggplot2::coord_flip() +
  ggplot2::labs(
    title = "Actual vs expected daily survey rate by location",
    x = NULL,
    y = "Surveys per day",
    fill = NULL
  ) +
  ggplot2::theme_minimal(base_size = 11)

plot_achieved_path <- save_plot_png(plot_achieved_pending, "chart_target_achieved_pending.png", width = 13, height = 8)
plot_days_path <- save_plot_png(plot_days_needed, "chart_days_needed_actual_rate.png", width = 13, height = 8)
plot_daily_rate_path <- save_plot_png(plot_daily_rate, "chart_actual_vs_expected_daily_rate.png", width = 13, height = 8)

# --------------------------------------------------------------------------------------------------
# 8. Export Excel workbook
# --------------------------------------------------------------------------------------------------

tracker_export_path <- file.path(
  path_target_tracker_folder,
  paste0("MSNA_target_progress_daily_rate_tracker_", format(Sys.time(), "%Y-%m-%d_at_%Hh%M"), ".xlsx")
)

wb <- openxlsx::createWorkbook()

# Dashboard sheet.
openxlsx::addWorksheet(wb, "Dashboard")

openxlsx::writeData(wb, "Dashboard", "MSNA 2026 Uganda - Target progress and daily-rate tracker", startRow = 1, startCol = 1)
openxlsx::writeData(wb, "Dashboard", paste0("Created: ", Sys.time()), startRow = 2, startCol = 1)
openxlsx::writeData(wb, "Dashboard", paste0("Data source: ", tracker_data_source), startRow = 3, startCol = 1)

header_style    <- openxlsx::createStyle(textDecoration = "bold", fontSize = 14)
subheader_style <- openxlsx::createStyle(textDecoration = "bold", fontSize = 11)
openxlsx::addStyle(wb, "Dashboard", header_style, rows = 1, cols = 1)

openxlsx::writeDataTable(wb, "Dashboard", overall_summary, startRow = 5, startCol = 1, tableStyle = "TableStyleMedium9")
openxlsx::writeDataTable(wb, "Dashboard", summary_by_population_group, startRow = 10, startCol = 1, tableStyle = "TableStyleMedium4")

# --- Daily surveys conducted (compact view on Dashboard) ---
dash_daily_row <- 16L
openxlsx::writeData(wb, "Dashboard", "Daily surveys conducted", startRow = dash_daily_row, startCol = 1)
openxlsx::addStyle(wb, "Dashboard", subheader_style, rows = dash_daily_row, cols = 1)
openxlsx::writeDataTable(
  wb, "Dashboard", overall_daily_totals,
  startRow = dash_daily_row + 1L, startCol = 1,
  tableStyle = "TableStyleMedium2"
)

charts_row <- dash_daily_row + nrow(overall_daily_totals) + 4L   # leave a gap above charts

openxlsx::writeData(wb, "Dashboard", "Charts", startRow = charts_row, startCol = 1)
openxlsx::addStyle(wb, "Dashboard", subheader_style, rows = charts_row, cols = 1)

if (file.exists(plot_achieved_path)) {
  openxlsx::insertImage(wb, "Dashboard", plot_achieved_path, startRow = charts_row + 2L, startCol = 1,  width = 12, height = 7)
}
if (file.exists(plot_days_path)) {
  openxlsx::insertImage(wb, "Dashboard", plot_days_path,     startRow = charts_row + 2L, startCol = 10, width = 12, height = 7)
}
if (file.exists(plot_daily_rate_path)) {
  openxlsx::insertImage(wb, "Dashboard", plot_daily_rate_path, startRow = charts_row + 40L, startCol = 1, width = 12, height = 7)
}

openxlsx::setColWidths(wb, "Dashboard", cols = 1:20, widths = "auto")

# ---- Daily survey tracker sheet (wide matrix: location × date) ----
{
  dsm_sheet <- "Daily_survey_tracker"
  if (dsm_sheet %in% names(wb)) openxlsx::removeWorksheet(wb, dsm_sheet)
  openxlsx::addWorksheet(wb, dsm_sheet)
  openxlsx::writeData(wb, dsm_sheet, daily_survey_matrix, startRow = 1, startCol = 1)

  n_dsm_rows <- nrow(daily_survey_matrix)
  n_dsm_cols <- ncol(daily_survey_matrix)

  # Freeze the first two columns (location_type, location_name) so they stay
  # visible as new date columns accumulate across the sheet.
  openxlsx::freezePane(wb, dsm_sheet, firstActiveCol = 3L)

  # Style helpers
  dsm_header_style <- openxlsx::createStyle(textDecoration = "bold", halign = "center", wrapText = TRUE)
  dsm_total_col_style <- openxlsx::createStyle(textDecoration = "bold", halign = "right",
                                                numFmt = "#,##0")
  dsm_grand_row_style <- openxlsx::createStyle(textDecoration = "bold", numFmt = "#,##0")
  dsm_data_style       <- openxlsx::createStyle(numFmt = "#,##0", halign = "right")

  # Header row
  openxlsx::addStyle(wb, dsm_sheet, dsm_header_style,
                     rows = 1L, cols = seq_len(n_dsm_cols), gridExpand = TRUE)

  # Data rows (all except the Grand Total row)
  if (n_dsm_rows > 1L) {
    openxlsx::addStyle(wb, dsm_sheet, dsm_data_style,
                       rows = 2L:(n_dsm_rows),            # excludes Grand Total
                       cols = seq(3L, n_dsm_cols),
                       gridExpand = TRUE, stack = TRUE)
  }

  # Total column (last column) – bold for all data + grand total rows
  openxlsx::addStyle(wb, dsm_sheet, dsm_total_col_style,
                     rows = 2L:(n_dsm_rows + 1L),
                     cols = n_dsm_cols,
                     gridExpand = TRUE, stack = TRUE)

  # Grand Total row (last row) – bold across all columns
  openxlsx::addStyle(wb, dsm_sheet, dsm_grand_row_style,
                     rows = n_dsm_rows + 1L,
                     cols = seq_len(n_dsm_cols),
                     gridExpand = TRUE, stack = TRUE)

  openxlsx::setColWidths(wb, dsm_sheet, cols = 1L, widths = 18)
  openxlsx::setColWidths(wb, dsm_sheet, cols = 2L, widths = 22)
  if (n_dsm_cols > 2L) {
    openxlsx::setColWidths(wb, dsm_sheet, cols = seq(3L, n_dsm_cols), widths = 12)
  }
}

# Write Summary_by_location with traffic-light row coloring.
# Daily survey date columns are embedded between target and completed_surveys.
{
  sbl_sheet <- "Summary_by_location"
  if (sbl_sheet %in% names(wb)) openxlsx::removeWorksheet(wb, sbl_sheet)
  openxlsx::addWorksheet(wb, sbl_sheet)

  df_sbl <- tibble::as_tibble(summary_by_location_export)
  n_sbl_rows <- nrow(df_sbl)
  n_sbl_cols <- ncol(df_sbl)

  openxlsx::writeDataTable(wb, sheet = sbl_sheet, x = df_sbl, tableStyle = "TableStyleMedium2")

  # Freeze header row + first 3 columns (location_type, location_name, target) so the user
  # can scroll right through the date columns while keeping location context visible.
  openxlsx::freezePane(wb, sheet = sbl_sheet, firstActiveRow = 2L, firstActiveCol = 4L)

  # Set column widths: auto for all, then override embedded date columns to be narrower.
  openxlsx::setColWidths(wb, sheet = sbl_sheet, cols = seq_len(n_sbl_cols), widths = "auto")
  date_col_positions <- which(names(df_sbl) %in% date_cols_present)
  if (length(date_col_positions) > 0) {
    openxlsx::setColWidths(wb, sheet = sbl_sheet, cols = date_col_positions, widths = 12)
  }

  # Traffic-light row coloring.
  # NOTE: addStyle() requires fgFill for solid cell background; bgFill is only for conditionalFormatting().
  sbl_style_green  <- openxlsx::createStyle(fgFill = "#C6EFCE")  # >= 100 %
  sbl_style_yellow <- openxlsx::createStyle(fgFill = "#FFEB9C")  # >= 50 % and < 100 %
  sbl_style_red    <- openxlsx::createStyle(fgFill = "#FFC7CE")  # < 50 %

  for (i in seq_len(n_sbl_rows)) {
    pct_val <- df_sbl$percent_achieved_pct[i]
    row_style <- if (!is.na(pct_val) && pct_val >= 100) {
      sbl_style_green
    } else if (!is.na(pct_val) && pct_val >= 50) {
      sbl_style_yellow
    } else if (!is.na(pct_val)) {
      sbl_style_red
    } else {
      NULL
    }
    if (!is.null(row_style)) {
      openxlsx::addStyle(
        wb, sbl_sheet, row_style,
        rows       = i + 1L,            # +1 to account for the header row
        cols       = seq_len(n_sbl_cols),
        gridExpand = TRUE, stack = TRUE
      )
    }
  }
}
write_table_sheet(wb, "Daily_by_location", daily_by_location_export)
write_table_sheet(wb, "Enumerator_daily", enumerator_daily_export)
write_table_sheet(wb, "Enumerator_summary", enumerator_summary_export)
write_table_sheet(wb, "Population_summary", summary_by_population_group)
write_table_sheet(wb, "Location_targets", location_targets)

assumptions <- tibble::tibble(
  assumption = c(
    "Host community target per location",
    "Refugee settlement target per location",
    "Expected interviews per enumerator per day",
    "Counting unit",
    "Host location field",
    "Refugee settlement location field",
    "Enumerator field candidates",
    "Date field candidates",
    "Data source used",
    "Adjumani refugee settlement grouping"
  ),
  value = c(
    as.character(host_target_per_community),
    as.character(refugee_target_per_settlement),
    as.character(expected_interviews_per_enum_per_day),
    ifelse(isTRUE(count_unique_point_number), "Unique point_number within location", "Unique survey UUID / submission row"),
    "admin2",
    "admin3_setlement; fallback to admin3/admin2 where needed",
    "enum_id, enumerator_id, enumerator, username, _submitted_by, submitted_by",
    "today, start_date, survey_date, _submission_time, submission_time, start/start_time",
    tracker_data_source,
    "adjumani_pagirinya, adjumani_ayilo_i, adjumani_ayilo_ii, adjumani_nyumanzi, adjumani_olua_i, adjumani_agojo, adjumani_baratuku, adjumani_olua_ii, adjumani_mirieyi are grouped as adjumani"
  )
)
write_table_sheet(wb, "Assumptions", assumptions)


summary_by_location_readme <- tibble::tribble(
  ~column, ~how_calculated, ~interpretation,
  "location_type", "Classified from hoh_type/population_group. Records containing refugee are classified as refugee_settlement; host/national are classified as host_community.", "Shows whether the row is a refugee settlement target or a host community target.",
  "location_name", "For host communities, this is usually admin2. For refugee settlements, this is admin3_setlement, with fallback to admin3/admin2 if needed. The script also groups selected Adjumani settlement/site values under adjumani.", "The community/settlement being tracked.",
  "target", "Default target is 150 for each host community and 106 for each refugee settlement, unless manual_location_targets supplies an override.", "Total surveys expected for the location.",
  "completed_surveys", "Number of unique survey_id values counted in the location after de-duplication. By default survey_id comes from uuid/_uuid or equivalent Kobo submission identifier.", "How many household surveys have been completed so far.",
  "pending_surveys", "pmax(target - completed_surveys, 0).", "How many surveys remain to reach the target. It never goes below zero.",
  "excess_surveys", "pmax(completed_surveys - target, 0).", "How many surveys are above the target, if the location has overshot.",
  "percent_achieved_pct", "100 * completed_surveys / target, rounded to one decimal place. If target is missing or zero, this is NA.", "Progress achieved as a percentage of the target.",
  "percent_pending_pct", "100 * pending_surveys / target, rounded to one decimal place. If target is missing or zero, this is NA.", "Remaining target as a percentage of the target.",
  "status", "Target achieved if completed_surveys >= target; Not started if completed_surveys == 0; Pending if target exists and completed_surveys is below target; No target defined if target is missing.", "Simple progress classification.",
  "enumerators_used", "Number of unique non-missing enum_id values used in the location across all data-collection days.", "How many enumerators contributed surveys in that location.",
  "active_data_collection_days", "Number of unique non-missing start_date values in the location.", "Number of days on which at least one survey was completed in that location.",
  "enumerator_days_used", "Number of unique enum_id + start_date combinations in the location.", "Total person-days worked by enumerators. Example: 4 enumerators working for 2 days = 8 enumerator-days.",
  "actual_daily_rate", "completed_surveys / active_data_collection_days.", "Average number of surveys completed per active data-collection day in the location.",
  "actual_enum_day_rate", "completed_surveys / enumerator_days_used.", "Average number of surveys completed per enumerator-day. This should be compared with the expected 3 interviews per enumerator per day.",
  "expected_daily_rate_from_enumerators", "enumerators_used * expected_interviews_per_enum_per_day. The default expected_interviews_per_enum_per_day is 3.", "Expected surveys per day if every enumerator who has worked in that location completes 3 interviews per day.",
  "expected_total_output_from_enum_days", "enumerator_days_used * expected_interviews_per_enum_per_day. The default expected_interviews_per_enum_per_day is 3.", "Expected total output from the actual number of enumerator-days already used.",
  "productivity_vs_expected_enum_days_pct", "100 * completed_surveys / expected_total_output_from_enum_days, rounded to one decimal place.", "Productivity against the 3-interviews-per-enumerator-day expectation. 100% means exactly 3 interviews per enumerator-day on average.",
  "days_needed_at_actual_daily_rate", "ceiling(pending_surveys / actual_daily_rate). Returns NA if actual_daily_rate is missing or zero.", "Estimated additional active data-collection days needed if the location continues at its current observed daily rate.",
  "days_needed_at_expected_enum_rate", "ceiling(pending_surveys / expected_daily_rate_from_enumerators). Returns NA if expected_daily_rate_from_enumerators is missing or zero.", "Estimated additional days needed if the enumerators used in that location each complete 3 interviews per day.",
  "enumerator_days_needed_at_3_per_day", "ceiling(pending_surveys / expected_interviews_per_enum_per_day). Default: ceiling(pending_surveys / 3).", "Total additional enumerator-days needed. Example: 30 pending surveys = 10 enumerator-days at 3 surveys per enumerator-day.",
  "first_survey_date", "Earliest non-missing start_date in the location.", "First day with a completed survey in the location.",
  "latest_survey_date", "Latest non-missing start_date in the location.", "Most recent day with a completed survey in the location.",
  "target_source", "default assumption unless a manual target override was supplied in manual_location_targets.", "Shows whether the target came from the default 150/106 assumption or a manual override.",
  "location_key", "location_type combined with a cleaned version of location_name.", "Technical matching key used internally for joins and charts."
)
write_table_sheet(wb, "README_summary", summary_by_location_readme)

# Keep a compact version of data used; full raw data can be large, but this is useful for QA.
data_used <- tracker_count_data |>
  dplyr::select(
    survey_id,
    point_number,
    start_date,
    enum_id,
    enum_gender,
    population_group,
    location_type,
    location_name,
    admin1,
    admin2,
    admin3,
    admin3_setlement,
    raw_refugee_settlement,
    admin4,
    hoh_type,
    resp_gender
  ) |>
  dplyr::arrange(location_type, location_name, start_date, enum_id)
write_table_sheet(wb, "Data_used", data_used)

openxlsx::saveWorkbook(wb, tracker_export_path, overwrite = TRUE)

# --------------------------------------------------------------------------------------------------
# 9. Console summary and objects for later use
# --------------------------------------------------------------------------------------------------

msna_target_tracker <- list(
  overall_summary = overall_summary,
  summary_by_population_group = summary_by_population_group,
  summary_by_location = summary_by_location_export,
  daily_by_location = daily_by_location_export,
  daily_survey_matrix = daily_survey_matrix,
  overall_daily_totals = overall_daily_totals,
  enumerator_daily = enumerator_daily_export,
  enumerator_summary = enumerator_summary_export,
  location_targets = location_targets,
  data_used = data_used,
  assumptions = assumptions,
  summary_by_location_readme = summary_by_location_readme,
  export_path = tracker_export_path,
  chart_paths = c(plot_achieved_path, plot_days_path, plot_daily_rate_path)
)

cat("\n")
cat("MSNA target progress tracker exported to:\n")
cat(tracker_export_path, "\n")
cat("\n")
cat("Overall completion:\n")
print(overall_summary)
cat("\n")
cat("Completion by population group:\n")
print(summary_by_population_group)
cat("\n")

if (exists("crayon", mode = "function")) {
  cat(crayon::green$bold("MSNA target progress and daily-rate tracker completed"))
} else if (requireNamespace("crayon", quietly = TRUE)) {
  cat(crayon::green$bold("MSNA target progress and daily-rate tracker completed"))
} else {
  cat("MSNA target progress and daily-rate tracker completed")
}
cat("\n\n")

all_dfs_names_after <- Filter(function(x) is(x, "data.frame"), mget(ls())) |> names()
all_lists_names_after <- Filter(function(x) is(x, "list"), mget(ls())) |> names()

if (length(setdiff(all_dfs_names_after, all_dfs_names_before)) > 0) {
  cat("New dataframes created:\n")
  cat(paste(setdiff(all_dfs_names_after, all_dfs_names_before), collapse = "\n"), "\n")
}

if (length(setdiff(all_lists_names_after, all_lists_names_before)) > 0) {
  cat("New lists created:\n")
  cat(paste(setdiff(all_lists_names_after, all_lists_names_before), collapse = "\n"), "\n")
}