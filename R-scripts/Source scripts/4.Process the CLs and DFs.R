cat("\014")

all_dfs_names_before <- Filter(function(x) is(x, "data.frame"), mget(ls())) |> names()
all_lists_names_before <- Filter(function(x) is(x, "list"), mget(ls())) |> names()

# =================================================================================================
# 4. Process the cleaning logs and dataframes - MSNA 2026 Uganda
# -------------------------------------------------------------------------------------------------
# Purpose:
#   - Take the combined cleaning log created in "3.Do the checks.R"
#   - Convert it into a cleaningtools-style object:
#       combined_log$checked_dataset
#       combined_log$cleaning_log
#   - Remove PII from checked datasets and cleaning logs
#   - Replace converted values with original values where relevant
#   - Remove duplicate cleaning-log flags
#   - Remove flags already detected in previous cleaning logs
#   - Split cleaning log and deletion log
#   - Create export-ready logbook objects for the next script
#
# Important:
#   - This script DOES NOT export any Excel file.
#   - The export step should be handled in "5. Export the logbooks.R".
# =================================================================================================

print_step("4. Processing cleaning logs and checked dataframes")

# --------------------------------------------------------------------------------------------------
# 0. Safety checks
# --------------------------------------------------------------------------------------------------

if (!exists("combined_log") && !exists("combined_log_cleaningtools_style")) {
  stop(
    "Neither 'combined_log' nor 'combined_log_cleaningtools_style' was found. Please run '3.Do the checks.R' before this script.",
    call. = FALSE
  )
}

required_packages <- c("dplyr", "tibble", "purrr", "tidyr", "stringr", "readxl", "janitor")

for (pkg in required_packages) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    stop("Package '", pkg, "' is required.", call. = FALSE)
  }
}

# --------------------------------------------------------------------------------------------------
# 1. Core columns and helper functions
# --------------------------------------------------------------------------------------------------

core_cleaning_log_cols <- c(
  "dataset",
  "uuid",
  "parent_uuid",
  "repeat_index",
  "question",
  "old_value",
  "new_value",
  "change_type",
  "issue",
  "check_id",
  "check_to_perform",
  "columns_to_clean",
  "notes"
)

# Final-tool-aligned context columns.
context_cols <- c(
  "point_number",
  "admin1",
  "admin2",
  "admin3",
  "admin3_setlement",
  "admin4",
  "hoh_type",
  "resp_gender",
  "resp_age",
  "hh_size",
  "enum_id",
  "enum_gender",
  "username",
  "_submitted_by",
  "_submission_time",
  "start",
  "end",
  "today"
)

standardise_dataset_name <- function(x) {
  x <- as.character(x)
  dplyr::case_when(
    x %in% c("Submissions", "household", "household_level", "data_hh_main") ~ "Submissions",
    x %in% c("roster", "data_hh_roster", "data_roster") ~ "roster",
    x %in% c("edu_ind", "education_ind", "data_edu_ind") ~ "edu_ind",
    x %in% c("health_ind", "data_health_ind") ~ "health_ind",
    x %in% c("nutrition_ind", "nut_ind", "data_nutrition_ind", "data_nut_ind") ~ "nutrition_ind",
    x %in% c("child_protection_ind", "prot_ind", "protection_ind", "data_child_protection_ind", "data_prot_ind") ~ "child_protection_ind",
    TRUE ~ x
  )
}

# PII / sensitive columns to remove from checked data and cleaning logs.
# Keep this list explicit and conservative. Only variables that directly identify people,
# coordinates, phone numbers, names, device identifiers, or exact GPS should be included.
pii_cols_manual <- c(
  "coordinates",
  "coordinates_latitude",
  "coordinates_longitude",
  "coordinates_altitude",
  "coordinates_accuracy",
  "geopoint",
  "geopoint_latitude",
  "geopoint_longitude",
  "geopoint_altitude",
  "geopoint_accuracy",
  "_geolocation",
  "_gps",
  "gps",
  "gps_latitude",
  "gps_longitude",
  "gps_altitude",
  "gps_accuracy",
  "latitude",
  "longitude",
  "location_latitude",
  "location_longitude",
  "location_altitude",
  "location_accuracy",
  "respondent_name",
  "resp_name",
  "hh_head_name",
  "head_name",
  "name",
  "phone",
  "phone_number",
  "telephone",
  "contact",
  "contact_phone",
  "respondent_phone",
  "deviceid",
  "device_id",
  "_device_id",
  "simserial",
  "subscriberid",
  "imei"
)

empty_msna_cleaning_log <- function() {
  tibble::tibble(
    dataset = character(),
    uuid = character(),
    parent_uuid = character(),
    repeat_index = character(),
    question = character(),
    old_value = character(),
    new_value = character(),
    change_type = character(),
    issue = character(),
    check_id = character(),
    check_to_perform = character(),
    columns_to_clean = character(),
    notes = character()
  )
}

safe_as_character <- function(x) {
  if (inherits(x, "POSIXct") || inherits(x, "POSIXt") || inherits(x, "Date")) {
    return(as.character(x))
  }
  as.character(x)
}

move_point_number_first <- function(df) {
  
  if (is.null(df) || !is.data.frame(df) || ncol(df) == 0) {
    return(df)
  }
  
  df <- tibble::as_tibble(df)
  
  if ("point_number" %in% names(df)) {
    df <- df |>
      dplyr::select(point_number, dplyr::everything())
  }
  
  df
}

standardise_msna_cleaning_log <- function(x) {
  
  # Step 3 may return either:
  #   1. a plain data frame called combined_log, or
  #   2. a cleaningtools-style list with element $cleaning_log.
  if (is.list(x) && !is.data.frame(x) && "cleaning_log" %in% names(x)) {
    x <- x$cleaning_log
  }
  
  if (is.null(x) || !is.data.frame(x) || nrow(x) == 0) {
    return(empty_msna_cleaning_log())
  }
  
  x <- tibble::as_tibble(x) |>
    janitor::clean_names()
  
  # Harmonise common alternative column names from cleaningtools or older scripts.
  if (!"uuid" %in% names(x)) {
    uuid_candidates <- c("_uuid", "id", "submission_id", "submission_uuid", "instance_name")
    uuid_hit <- uuid_candidates[uuid_candidates %in% names(x)][1]
    x$uuid <- if (!is.na(uuid_hit)) as.character(x[[uuid_hit]]) else NA_character_
  }
  
  if (!"parent_uuid" %in% names(x)) {
    parent_candidates <- c(
      "_parent_uuid",
      "_submission_uuid",
      "_submission__uuid",
      "_submission__id",
      "_submission_id",
      "parent_id",
      "parent_instance_name"
    )
    parent_hit <- parent_candidates[parent_candidates %in% names(x)][1]
    x$parent_uuid <- if (!is.na(parent_hit)) as.character(x[[parent_hit]]) else NA_character_
  }
  
  if (!"repeat_index" %in% names(x)) {
    index_candidates <- c("_index", "index", "repeat_position", "_parent_index")
    index_hit <- index_candidates[index_candidates %in% names(x)][1]
    x$repeat_index <- if (!is.na(index_hit)) as.character(x[[index_hit]]) else NA_character_
  }
  
  if (!"question" %in% names(x)) {
    question_candidates <- c(
      "name",
      "variable",
      "variable_name",
      "question_name",
      "variables_to_clean",
      "columns_to_clean"
    )
    question_hit <- question_candidates[question_candidates %in% names(x)][1]
    x$question <- if (!is.na(question_hit)) as.character(x[[question_hit]]) else NA_character_
  }
  
  if (!"old_value" %in% names(x)) {
    value_candidates <- c("value", "old", "response", "answer")
    value_hit <- value_candidates[value_candidates %in% names(x)][1]
    x$old_value <- if (!is.na(value_hit)) as.character(x[[value_hit]]) else NA_character_
  }
  
  if (!"new_value" %in% names(x)) {
    x$new_value <- NA_character_
  }
  
  if (!"change_type" %in% names(x)) {
    x$change_type <- NA_character_
  }
  
  if (!"issue" %in% names(x)) {
    issue_candidates <- c("description", "problem", "comment", "message")
    issue_hit <- issue_candidates[issue_candidates %in% names(x)][1]
    x$issue <- if (!is.na(issue_hit)) as.character(x[[issue_hit]]) else "Automated cleaning flag generated."
  }
  
  if (!"check_id" %in% names(x)) {
    x$check_id <- NA_character_
  }
  
  if (!"check_to_perform" %in% names(x)) {
    x$check_to_perform <- NA_character_
  }
  
  if (!"columns_to_clean" %in% names(x)) {
    x$columns_to_clean <- as.character(x$question)
  }
  
  if (!"dataset" %in% names(x)) {
    x$dataset <- "Submissions"
  }
  
  if (!"notes" %in% names(x)) {
    x$notes <- NA_character_
  }
  
  missing_core <- setdiff(core_cleaning_log_cols, names(x))
  
  for (cc in missing_core) {
    x[[cc]] <- NA_character_
  }
  
  x |>
    dplyr::mutate(
      dplyr::across(dplyr::all_of(core_cleaning_log_cols), safe_as_character),
      dataset = dplyr::if_else(is.na(dataset) | dataset == "", "Submissions", dataset),
      question = stringr::str_remove(as.character(question), "_converted$"),
      columns_to_clean = stringr::str_remove(as.character(columns_to_clean), "_converted$")
    )
}

standardise_checked_dataset <- function(x) {
  
  if (is.null(x) || !is.data.frame(x)) {
    return(tibble::tibble())
  }
  
  x <- tibble::as_tibble(x)
  
  if (!"uuid" %in% names(x)) {
    uuid_candidates <- c("_submission__uuid", "_submission_uuid", "submission_uuid", "_uuid", "_id", "instance_name", "_submission__id", "_submission_id")
    uuid_hit <- uuid_candidates[uuid_candidates %in% names(x)][1]
    x$uuid <- if (!is.na(uuid_hit)) as.character(x[[uuid_hit]]) else NA_character_
  } else {
    x$uuid <- as.character(x$uuid)
  }
  
  move_point_number_first(x)
}

# --------------------------------------------------------------------------------------------------
# Kobo server repeat linkage helper for export objects
# --------------------------------------------------------------------------------------------------
repeat_dataset_names <- c("roster", "edu_ind", "health_ind", "nutrition_ind", "child_protection_ind")

make_main_submission_context_for_repeats <- function(main_df) {
  
  if (!is.data.frame(main_df) || nrow(main_df) == 0) {
    return(tibble::tibble())
  }
  
  main_id_cols <- intersect(
    c("uuid", "_uuid", "submission_uuid", "_submission_uuid", "_id", "instance_name", "_submission__id", "_submission_id", "submission_id"),
    names(main_df)
  )
  
  context_cols_existing <- intersect(context_cols, names(main_df))
  context_cols_existing <- unique(c("point_number", context_cols_existing))
  context_cols_existing <- intersect(context_cols_existing, names(main_df))
  context_cols_existing <- setdiff(context_cols_existing, main_id_cols)
  
  if (length(main_id_cols) == 0) {
    return(tibble::tibble())
  }
  
  tmp <- main_df |>
    dplyr::select(dplyr::any_of(c(main_id_cols, context_cols_existing))) |>
    dplyr::mutate(dplyr::across(dplyr::all_of(main_id_cols), as.character))
  
  purrr::map_dfr(main_id_cols, function(id_col) {
    tmp |>
      dplyr::filter(!is.na(.data[[id_col]]), trimws(.data[[id_col]]) != "") |>
      dplyr::transmute(
        .submission_uuid = as.character(.data[[id_col]]),
        dplyr::across(dplyr::all_of(context_cols_existing))
      )
  }) |>
    dplyr::distinct(.submission_uuid, .keep_all = TRUE)
}

align_repeat_checked_dataset_uuid_point_number <- function(df, dataset_name, main_context) {
  
  if (!is.data.frame(df) || nrow(df) == 0) {
    return(df)
  }
  
  df <- tibble::as_tibble(df)
  
  if (!dataset_name %in% repeat_dataset_names) {
    return(df)
  }
  
  if (!"repeat_row_uuid" %in% names(df)) {
    repeat_row_id_col <- intersect(c("uuid", "_id", "instance_name", "KEY", "_submission__id", "_submission_id"), names(df))
    df$repeat_row_uuid <- if (length(repeat_row_id_col) > 0) {
      as.character(df[[repeat_row_id_col[[1]]]])
    } else {
      paste0(dataset_name, "_row_", seq_len(nrow(df)))
    }
  }
  
  submission_uuid_col <- intersect(
    c("_submission__uuid", "_submission_uuid", "submission_uuid", "parent_uuid", "_parent_uuid", "submission__uuid", "_submission__id", "_submission_id", "submission_id", "parent_instance_name"),
    names(df)
  )
  
  if (length(submission_uuid_col) > 0) {
    parent_id <- as.character(df[[submission_uuid_col[[1]]]])
    df$`_submission__uuid` <- parent_id
    df$submission_uuid <- parent_id
    df$parent_uuid <- parent_id
    df$uuid <- parent_id
  }
  
  if (is.data.frame(main_context) && nrow(main_context) > 0 && "_submission__uuid" %in% names(df)) {
    df <- df |>
      dplyr::mutate(.submission_uuid = as.character(.data[["_submission__uuid"]])) |>
      dplyr::left_join(main_context, by = ".submission_uuid", suffix = c("", "_main"))
    
    context_from_main <- setdiff(names(main_context), ".submission_uuid")
    for (cc in context_from_main) {
      cc_main <- paste0(cc, "_main")
      if (cc_main %in% names(df)) {
        if (cc %in% names(df)) {
          df[[cc]] <- dplyr::coalesce(as.character(df[[cc]]), as.character(df[[cc_main]]))
        } else {
          df[[cc]] <- as.character(df[[cc_main]])
        }
      }
    }
    
    df <- df |>
      dplyr::select(-dplyr::any_of(c(".submission_uuid", paste0(context_from_main, "_main"))))
  }
  
  move_point_number_first(df)
}

align_repeat_cleaning_log_uuid_to_submission_uuid <- function(log_df) {
  
  if (!is.data.frame(log_df) || nrow(log_df) == 0) {
    return(log_df)
  }
  
  log_df <- tibble::as_tibble(log_df)
  
  for (cc in c("dataset", "uuid", "parent_uuid")) {
    if (!cc %in% names(log_df)) log_df[[cc]] <- NA_character_
  }
  
  repeat_rows <- standardise_dataset_name(log_df$dataset) %in% repeat_dataset_names &
    !is.na(log_df$parent_uuid) & trimws(as.character(log_df$parent_uuid)) != ""
  
  log_df$uuid[repeat_rows] <- as.character(log_df$parent_uuid[repeat_rows])
  log_df
}


get_checked_dataset_from_previous_step <- function() {
  
  if (
    exists("combined_log_cleaningtools_style") &&
    is.list(combined_log_cleaningtools_style) &&
    "checked_dataset" %in% names(combined_log_cleaningtools_style)
  ) {
    return(standardise_checked_dataset(combined_log_cleaningtools_style$checked_dataset))
  }
  
  if (exists("data_hh_main") && is.data.frame(data_hh_main)) {
    return(standardise_checked_dataset(data_hh_main))
  }
  
  if (exists("raw_data") && is.data.frame(raw_data)) {
    return(standardise_checked_dataset(raw_data))
  }
  
  tibble::tibble()
}

get_cleaning_log_from_previous_step <- function() {
  
  if (
    exists("combined_log_cleaningtools_style") &&
    is.list(combined_log_cleaningtools_style) &&
    "cleaning_log" %in% names(combined_log_cleaningtools_style)
  ) {
    return(standardise_msna_cleaning_log(combined_log_cleaningtools_style$cleaning_log))
  }
  
  if (exists("combined_log")) {
    return(standardise_msna_cleaning_log(combined_log))
  }
  
  empty_msna_cleaning_log()
}

# Build checked datasets for all export sheets/repeats, not only Submissions.
get_checked_datasets_by_dataset <- function() {
  
  if (exists("raw_data_list") && is.list(raw_data_list)) {
    return(
      purrr::imap(
        raw_data_list,
        ~ standardise_checked_dataset(.x)
      )
    )
  }
  
  list(
    Submissions = get_checked_dataset_from_previous_step()
  )
}

find_original_value <- function(dataset_name, uuid_value, question_name) {
  
  if (
    !exists("checked_datasets_by_dataset") ||
    !is.list(checked_datasets_by_dataset) ||
    !dataset_name %in% names(checked_datasets_by_dataset)
  ) {
    return(NA_character_)
  }
  
  df <- checked_datasets_by_dataset[[dataset_name]]
  
  if (!is.data.frame(df) || nrow(df) == 0 || !"uuid" %in% names(df)) {
    return(NA_character_)
  }
  
  if (!question_name %in% names(df)) {
    return(NA_character_)
  }
  
  hit <- df |>
    dplyr::filter(as.character(uuid) == as.character(uuid_value)) |>
    dplyr::slice(1)
  
  if (nrow(hit) == 0) {
    return(NA_character_)
  }
  
  as.character(hit[[question_name]][[1]])
}

# --------------------------------------------------------------------------------------------------
# 2. Create cleaningtools-style combined_log object
# --------------------------------------------------------------------------------------------------

checked_dataset <- get_checked_dataset_from_previous_step()
checked_datasets_by_dataset <- get_checked_datasets_by_dataset()

main_context_for_repeat_exports <- if (exists("data_hh_main") && is.data.frame(data_hh_main)) {
  make_main_submission_context_for_repeats(data_hh_main)
} else {
  make_main_submission_context_for_repeats(checked_dataset)
}

checked_datasets_by_dataset <- purrr::imap(
  checked_datasets_by_dataset,
  ~ align_repeat_checked_dataset_uuid_point_number(.x, .y, main_context_for_repeat_exports)
)
checked_datasets_by_dataset <- purrr::map(checked_datasets_by_dataset, move_point_number_first)

cleaning_log_template <- get_cleaning_log_from_previous_step()
cleaning_log_template <- align_repeat_cleaning_log_uuid_to_submission_uuid(cleaning_log_template)

combined_log <- list(
  checked_dataset = checked_dataset,
  cleaning_log = cleaning_log_template
)

# Ensure repeat-level cleaning-log rows use the parent household _submission__uuid under uuid.
combined_log$cleaning_log <- align_repeat_cleaning_log_uuid_to_submission_uuid(combined_log$cleaning_log)

# --------------------------------------------------------------------------------------------------
# 3. Remove PII
# --------------------------------------------------------------------------------------------------

pii_list <- tibble::tibble()

if (
  exists("check_pii", mode = "function") &&
  is.data.frame(combined_log$checked_dataset) &&
  nrow(combined_log$checked_dataset) > 0
) {
  pii_list <- tryCatch(
    check_pii(combined_log$checked_dataset, uuid_column = "uuid"),
    error = function(e) tibble::tibble()
  )
}

pii_cols_from_function <- character()

if (is.data.frame(pii_list) && nrow(pii_list) > 0) {
  
  pii_list_clean <- pii_list |>
    janitor::clean_names()
  
  pii_col_candidates <- c("question", "variable", "name", "column", "columns_to_clean")
  pii_col_hit <- pii_col_candidates[pii_col_candidates %in% names(pii_list_clean)][1]
  
  if (!is.na(pii_col_hit)) {
    pii_cols_from_function <- pii_list_clean |>
      dplyr::pull(dplyr::all_of(pii_col_hit)) |>
      as.character() |>
      unique()
  }
}

coordinates_cols <- grep(
  "coord|gps|geo|latitude|longitude|altitude|accuracy",
  names(combined_log$checked_dataset),
  value = TRUE,
  ignore.case = TRUE
)

name_contact_cols <- grep(
  "name|phone|contact|telephone|device|imei|sim",
  names(combined_log$checked_dataset),
  value = TRUE,
  ignore.case = TRUE
)

pii_cols <- unique(c(
  coordinates_cols,
  name_contact_cols,
  pii_cols_manual,
  pii_cols_from_function
))

pii_cols <- pii_cols[!is.na(pii_cols) & pii_cols != ""]

# Do not drop safe operational variables that are useful in the cleaning log.
safe_to_keep_even_if_matched <- c(
  "username",
  "_submitted_by",
  "admin1",
  "admin2",
  "admin3",
  "admin3_setlement",
  "admin4",
  "enum_id",
  "enum_gender",
  "point_number"
)

pii_cols <- setdiff(pii_cols, safe_to_keep_even_if_matched)

# Remove PII columns from main checked dataset.
pii_cols_in_checked_dataset <- intersect(pii_cols, names(combined_log$checked_dataset))

if (length(pii_cols_in_checked_dataset) > 0) {
  combined_log$checked_dataset <- combined_log$checked_dataset |>
    dplyr::select(-dplyr::all_of(pii_cols_in_checked_dataset))
}

# Remove PII from each checked repeat dataset too.
if (exists("checked_datasets_by_dataset") && is.list(checked_datasets_by_dataset)) {
  
  checked_datasets_by_dataset <- purrr::imap(
    checked_datasets_by_dataset,
    function(df, nm) {
      
      if (!is.data.frame(df) || nrow(df) == 0) {
        return(df)
      }
      
      pii_in_df <- intersect(pii_cols, names(df))
      
      if (length(pii_in_df) > 0) {
        df <- df |>
          dplyr::select(-dplyr::all_of(pii_in_df))
      }
      
      df
    }
  )
}

# Remove PII-related flags from the cleaning log.
if (nrow(combined_log$cleaning_log) > 0) {
  combined_log$cleaning_log <- combined_log$cleaning_log |>
    dplyr::filter(!question %in% pii_cols) |>
    dplyr::filter(!columns_to_clean %in% pii_cols)
}

# --------------------------------------------------------------------------------------------------
# 4. Replace converted values with original values where needed
# --------------------------------------------------------------------------------------------------

# This mirrors the sample script logic but makes it safe for MSNA:
#   - If a flag came from variable_converted, store old_value as old_value_converted.
#   - Then pull the original variable value from the relevant checked dataset where available.
#   - If no original value is found, keep the converted value in old_value.

if (nrow(combined_log$cleaning_log) > 0) {
  
  combined_log$cleaning_log <- combined_log$cleaning_log |>
    dplyr::mutate(
      old_value_converted = dplyr::if_else(
        stringr::str_detect(question, "_converted$"),
        as.character(old_value),
        NA_character_
      ),
      question = stringr::str_remove(as.character(question), "_converted$"),
      columns_to_clean = stringr::str_remove(as.character(columns_to_clean), "_converted$")
    )
  
  converted_rows <- which(!is.na(combined_log$cleaning_log$old_value_converted))
  
  if (length(converted_rows) > 0) {
    
    original_values <- purrr::pmap_chr(
      combined_log$cleaning_log[converted_rows, c("dataset", "uuid", "question")],
      ~ find_original_value(
        dataset_name = ..1,
        uuid_value = ..2,
        question_name = ..3
      )
    )
    
    combined_log$cleaning_log$old_value[converted_rows] <- dplyr::if_else(
      is.na(original_values) | original_values == "",
      combined_log$cleaning_log$old_value_converted[converted_rows],
      original_values
    )
    
    combined_log$cleaning_log$old_value_converted[converted_rows] <- dplyr::if_else(
      is.na(original_values) | original_values == "",
      NA_character_,
      combined_log$cleaning_log$old_value_converted[converted_rows]
    )
  }
}

# --------------------------------------------------------------------------------------------------
# 5. Add main submission context to repeat logs
# --------------------------------------------------------------------------------------------------

if (
  exists("data_hh_main") &&
  is.data.frame(data_hh_main) &&
  nrow(data_hh_main) > 0
) {
  
  main_id_cols <- intersect(
    c("uuid", "_uuid", "_id", "instance_name", "_submission__id", "_submission_id"),
    names(data_hh_main)
  )
  
  context_cols_existing <- intersect(context_cols, names(data_hh_main))
  context_cols_existing <- setdiff(context_cols_existing, main_id_cols)
  
  if (length(main_id_cols) > 0) {
    
    main_context <- purrr::map_dfr(main_id_cols, function(id_col) {
      
      data_hh_main |>
        dplyr::mutate(.parent_key = as.character(.data[[id_col]])) |>
        dplyr::filter(!is.na(.parent_key), trimws(.parent_key) != "") |>
        dplyr::select(.parent_key, dplyr::all_of(context_cols_existing)) |>
        dplyr::rename(parent_uuid = .parent_key)
    }) |>
      dplyr::distinct(parent_uuid, .keep_all = TRUE)
    
    if (
      nrow(main_context) > 0 &&
      "parent_uuid" %in% names(combined_log$cleaning_log)
    ) {
      
      combined_log$cleaning_log <- combined_log$cleaning_log |>
        dplyr::mutate(parent_uuid = as.character(parent_uuid)) |>
        dplyr::left_join(
          main_context,
          by = "parent_uuid",
          suffix = c("", "_parent")
        )
      
      for (cc in context_cols_existing) {
        
        parent_cc <- paste0(cc, "_parent")
        
        if (parent_cc %in% names(combined_log$cleaning_log)) {
          
          if (cc %in% names(combined_log$cleaning_log)) {
            combined_log$cleaning_log[[cc]] <- dplyr::coalesce(
              as.character(combined_log$cleaning_log[[cc]]),
              as.character(combined_log$cleaning_log[[parent_cc]])
            )
          } else {
            combined_log$cleaning_log[[cc]] <- as.character(combined_log$cleaning_log[[parent_cc]])
          }
        }
      }
      
      combined_log$cleaning_log <- combined_log$cleaning_log |>
        dplyr::select(-dplyr::ends_with("_parent"))
    }
    
    # Add direct context for flags from Submissions using uuid.
    if ("uuid" %in% names(data_hh_main) && "uuid" %in% names(combined_log$cleaning_log)) {
      
      submission_context <- data_hh_main |>
        dplyr::mutate(uuid = as.character(uuid)) |>
        dplyr::select(dplyr::any_of(c("uuid", context_cols_existing))) |>
        dplyr::distinct(uuid, .keep_all = TRUE)
      
      combined_log$cleaning_log <- combined_log$cleaning_log |>
        dplyr::mutate(uuid = as.character(uuid)) |>
        dplyr::left_join(
          submission_context,
          by = "uuid",
          suffix = c("", "_main")
        )
      
      for (cc in context_cols_existing) {
        
        main_cc <- paste0(cc, "_main")
        
        if (main_cc %in% names(combined_log$cleaning_log)) {
          
          if (cc %in% names(combined_log$cleaning_log)) {
            combined_log$cleaning_log[[cc]] <- dplyr::coalesce(
              as.character(combined_log$cleaning_log[[cc]]),
              as.character(combined_log$cleaning_log[[main_cc]])
            )
          } else {
            combined_log$cleaning_log[[cc]] <- as.character(combined_log$cleaning_log[[main_cc]])
          }
        }
      }
      
      combined_log$cleaning_log <- combined_log$cleaning_log |>
        dplyr::select(-dplyr::ends_with("_main"))
    }
  }
}

# --------------------------------------------------------------------------------------------------
# 6. Remove duplicates final
# --------------------------------------------------------------------------------------------------

if (nrow(combined_log$cleaning_log) > 0) {
  
  combined_log$cleaning_log <- combined_log$cleaning_log |>
    dplyr::mutate(
      uuid = as.character(uuid),
      parent_uuid = as.character(parent_uuid),
      repeat_index = as.character(repeat_index),
      question = as.character(question),
      old_value = as.character(old_value),
      issue = as.character(issue),
      check_id = as.character(check_id)
    ) |>
    dplyr::distinct(
      dataset,
      uuid,
      parent_uuid,
      repeat_index,
      question,
      old_value,
      issue,
      check_id,
      .keep_all = TRUE
    )
}

# --------------------------------------------------------------------------------------------------
# 7. Remove flags that were already detected in previous cleaning logs
# --------------------------------------------------------------------------------------------------

# This follows the template concept:
#   old_cls_paths <- list.files("outputs/", pattern = "^Cleaning_log_20", recursive = TRUE)
#   old_cls       <- read old cleaning_log sheets
#   current log   <- anti_join(old_cls, by = c("uuid", "question"))
#
# MSNA adaptation:
#   - Search all existing cleaning-log workbooks already saved under outputs/.
#   - Read cleaning_log / cleaning_log_no_deletion / cleaning_log_all sheets where available.
#   - Standardise old logs into the same structure as the current log.
#   - Remove only flags already seen before, while remaining repeat-aware.
#   - For repeat sheets, use repeat_index when available so that a new child/person flag is not
#     wrongly removed just because another repeat record in the same household had the same question.

remove_flags_found_in_previous_cleaning_logs <- TRUE
previous_cleaning_log_search_folder <- "outputs/"
previous_cleaning_log_file_pattern <- "Cleaning_log|Cleaning log|cleaning_log|MSNA.*cleaning"
previous_cleaning_log_sheets_priority <- c(
  "cleaning_log",
  "cleaning_log_no_deletion",
  "cleaning_log_all"
)

old_cls_paths <- character()
old_cls <- tibble::tibble()
previously_flagged_flags_removed <- tibble::tibble()
old_cleaning_log_filter_summary <- tibble::tibble(
  previous_cleaning_log_files_found = 0L,
  previous_flags_read = 0L,
  current_flags_before_previous_log_filter = nrow(combined_log$cleaning_log),
  previously_flagged_rows_removed = 0L,
  current_flags_after_previous_log_filter = nrow(combined_log$cleaning_log)
)

normalise_previous_flag_key_value <- function(x) {
  x <- as.character(x)
  x <- stringr::str_squish(x)
  x[is.na(x) | x == ""] <- "<blank>"
  x
}

make_previous_flag_keys <- function(df) {
  if (!is.data.frame(df) || nrow(df) == 0) {
    return(tibble::tibble())
  }
  df <- tibble::as_tibble(df)
  for (cc in c("dataset", "uuid", "parent_uuid", "repeat_index", "question")) {
    if (!cc %in% names(df)) df[[cc]] <- NA_character_
  }
  df |>
    dplyr::mutate(
      dataset = standardise_dataset_name(dataset),
      uuid = as.character(uuid),
      parent_uuid = as.character(parent_uuid),
      repeat_index = as.character(repeat_index),
      question = as.character(question),
      .has_repeat_index = !is.na(repeat_index) & stringr::str_squish(repeat_index) != "",
      .is_repeat_dataset = standardise_dataset_name(dataset) %in% repeat_dataset_names,
      .flag_key_full = paste(
        normalise_previous_flag_key_value(dataset),
        normalise_previous_flag_key_value(uuid),
        normalise_previous_flag_key_value(parent_uuid),
        normalise_previous_flag_key_value(repeat_index),
        normalise_previous_flag_key_value(question),
        sep = "||"
      ),
      .flag_key_dataset_uuid_question = paste(
        normalise_previous_flag_key_value(dataset),
        normalise_previous_flag_key_value(uuid),
        normalise_previous_flag_key_value(question),
        sep = "||"
      ),
      .flag_key_uuid_question = paste(
        normalise_previous_flag_key_value(uuid),
        normalise_previous_flag_key_value(question),
        sep = "||"
      )
    )
}

read_one_previous_cleaning_log <- function(path_x) {
  sheets_x <- tryCatch(readxl::excel_sheets(path_x), error = function(e) character())
  sheet_to_read <- previous_cleaning_log_sheets_priority[previous_cleaning_log_sheets_priority %in% sheets_x][1]
  if (is.na(sheet_to_read) || length(sheet_to_read) == 0) {
    return(tibble::tibble())
  }
  tryCatch({
    readxl::read_xlsx(path_x, sheet = sheet_to_read, col_types = "text") |>
      janitor::clean_names() |>
      dplyr::mutate(
        source_old_cleaning_log = basename(path_x),
        source_old_cleaning_log_path = path_x,
        source_old_cleaning_log_sheet = sheet_to_read
      )
  }, error = function(e) {
    tibble::tibble()
  })
}

if (isTRUE(remove_flags_found_in_previous_cleaning_logs)) {
  
  if (dir.exists(previous_cleaning_log_search_folder)) {
    old_cls_paths <- list.files(
      previous_cleaning_log_search_folder,
      pattern = "\\.xlsx$",
      full.names = TRUE,
      recursive = TRUE,
      ignore.case = TRUE
    )
  }
  
  old_cls_paths <- old_cls_paths[
    stringr::str_detect(
      string = basename(old_cls_paths),
      pattern = stringr::regex(previous_cleaning_log_file_pattern, ignore_case = TRUE)
    ) |
      stringr::str_detect(
        string = old_cls_paths,
        pattern = stringr::regex(previous_cleaning_log_file_pattern, ignore_case = TRUE)
      )
  ]
  
  old_cls_paths <- old_cls_paths[!grepl("~\\$", basename(old_cls_paths))]
  old_cls_paths <- unique(normalizePath(old_cls_paths, winslash = "/", mustWork = FALSE))
  
  cat("\nPrevious cleaning-log files found: ", length(old_cls_paths), "\n", sep = "")
  
  if (length(old_cls_paths) > 0) {
    old_cls <- purrr::map_dfr(old_cls_paths, read_one_previous_cleaning_log)
  }
  
  if (is.data.frame(old_cls) && nrow(old_cls) > 0 && is.data.frame(combined_log$cleaning_log) && nrow(combined_log$cleaning_log) > 0) {
    
    old_cls_standardised <- old_cls |>
      standardise_msna_cleaning_log() |>
      align_repeat_cleaning_log_uuid_to_submission_uuid() |>
      make_previous_flag_keys()
    
    current_log_keyed <- combined_log$cleaning_log |>
      standardise_msna_cleaning_log() |>
      align_repeat_cleaning_log_uuid_to_submission_uuid() |>
      make_previous_flag_keys()
    
    old_full_keys <- old_cls_standardised |>
      dplyr::filter(.has_repeat_index) |>
      dplyr::pull(.flag_key_full) |>
      unique()
    
    old_dataset_uuid_question_keys <- old_cls_standardised |>
      dplyr::filter(!.has_repeat_index) |>
      dplyr::pull(.flag_key_dataset_uuid_question) |>
      unique()
    
    old_uuid_question_keys <- old_cls_standardised |>
      dplyr::filter(!.has_repeat_index) |>
      dplyr::pull(.flag_key_uuid_question) |>
      unique()
    
    current_log_keyed <- current_log_keyed |>
      dplyr::mutate(
        .previously_flagged = dplyr::case_when(
          .flag_key_full %in% old_full_keys ~ TRUE,
          !.is_repeat_dataset & .flag_key_dataset_uuid_question %in% old_dataset_uuid_question_keys ~ TRUE,
          !.is_repeat_dataset & .flag_key_uuid_question %in% old_uuid_question_keys ~ TRUE,
          .is_repeat_dataset & !.has_repeat_index & .flag_key_dataset_uuid_question %in% old_dataset_uuid_question_keys ~ TRUE,
          .is_repeat_dataset & !.has_repeat_index & .flag_key_uuid_question %in% old_uuid_question_keys ~ TRUE,
          TRUE ~ FALSE
        )
      )
    
    previously_flagged_flags_removed <- current_log_keyed |>
      dplyr::filter(.previously_flagged) |>
      dplyr::select(-dplyr::starts_with(".flag_key"), -dplyr::any_of(c(".previously_flagged", ".has_repeat_index", ".is_repeat_dataset")))
    
    combined_log$cleaning_log <- current_log_keyed |>
      dplyr::filter(!.previously_flagged) |>
      dplyr::select(-dplyr::starts_with(".flag_key"), -dplyr::any_of(c(".previously_flagged", ".has_repeat_index", ".is_repeat_dataset")))
    
    old_cleaning_log_filter_summary <- tibble::tibble(
      previous_cleaning_log_files_found = length(old_cls_paths),
      previous_flags_read = nrow(old_cls_standardised),
      current_flags_before_previous_log_filter = nrow(current_log_keyed),
      previously_flagged_rows_removed = nrow(previously_flagged_flags_removed),
      current_flags_after_previous_log_filter = nrow(combined_log$cleaning_log)
    )
    
  } else {
    old_cleaning_log_filter_summary <- tibble::tibble(
      previous_cleaning_log_files_found = length(old_cls_paths),
      previous_flags_read = if (is.data.frame(old_cls)) nrow(old_cls) else 0L,
      current_flags_before_previous_log_filter = nrow(combined_log$cleaning_log),
      previously_flagged_rows_removed = 0L,
      current_flags_after_previous_log_filter = nrow(combined_log$cleaning_log)
    )
  }
  
  print(old_cleaning_log_filter_summary)
  
} else {
  cat("\nPrevious cleaning-log exclusion skipped: keeping all current flags for district export.\n")
}

# --------------------------------------------------------------------------------------------------
# 7b. Remove flags already captured in previous specify-other cleaning logs
# -------------------------------------------------------------------------------------------------
# Script 6 generates a dedicated specify-other cleaning log every day (separate from the main log).
# Because both logs are produced daily, the main cleaning log can contain duplicate flags for
# the same specify-other text field (e.g. from check_others() in Script 3 or SPECIFY_OTHER_
# check IDs). This section removes those duplicates from the main log so reviewers see each
# specify-other issue only once — in the dedicated specify-other logbook.
#
# Deduplication logic:
#   1. Scan outputs/ (excluding today's folder) for Excel files whose name contains
#      "specify_other" or "specify other" in any case combination.
#   2. Read the cleaning_log sheet from each found file.
#   3. Build match keys from (dataset, uuid, parent_uuid, repeat_index, question) and
#      (dataset, uuid, parent_uuid, repeat_index, specify_other_variable) for rows
#      that carry change_type = "change_response" or row_role = "specify_other".
#   4. Remove matching rows from the main combined_log$cleaning_log ONLY when the question
#      looks like a specify-other field (name ends in _other / _specify, check_id starts with
#      SPECIFY_OTHER_, or the issue text mentions "other").
#   5. Summarise and print what was removed.
# --------------------------------------------------------------------------------------------------

specify_other_dedup_search_folder   <- "outputs/"
specify_other_dedup_file_pattern    <- "specify.*other|other.*specify|specify_other|Cleaning_log_specify"
specify_other_dedup_sheet_priority  <- c("cleaning_log", "cleaning_log_no_deletion", "cleaning_log_all")

old_specify_other_log_paths  <- character()
old_specify_other_log_raw    <- tibble::tibble()
flags_removed_by_specify_other_dedup <- tibble::tibble()
specify_other_dedup_summary <- tibble::tibble(
  previous_specify_other_files_found        = 0L,
  previous_specify_other_rows_read          = 0L,
  main_log_rows_before_specify_other_dedup  = nrow(combined_log$cleaning_log),
  main_log_rows_removed_specify_other_dedup = 0L,
  main_log_rows_after_specify_other_dedup   = nrow(combined_log$cleaning_log)
)

if (dir.exists(specify_other_dedup_search_folder)) {
  old_specify_other_log_paths <- list.files(
    specify_other_dedup_search_folder,
    pattern    = "\\.xlsx$",
    full.names = TRUE,
    recursive  = TRUE,
    ignore.case = TRUE
  )
}

# Keep only files whose name matches the specify-other pattern
old_specify_other_log_paths <- old_specify_other_log_paths[
  stringr::str_detect(
    basename(old_specify_other_log_paths),
    stringr::regex(specify_other_dedup_file_pattern, ignore_case = TRUE)
  )
]

# Exclude temp/lock files
old_specify_other_log_paths <- old_specify_other_log_paths[
  !grepl("~\\$", basename(old_specify_other_log_paths))
]

# Exclude today's output folder so we do not self-reference files created in this run
if (exists("path_cleaning_folder_today")) {
  today_norm <- normalizePath(path_cleaning_folder_today, winslash = "/", mustWork = FALSE)
  paths_norm <- normalizePath(old_specify_other_log_paths, winslash = "/", mustWork = FALSE)
  old_specify_other_log_paths <- old_specify_other_log_paths[
    !grepl(today_norm, paths_norm, fixed = TRUE)
  ]
}

old_specify_other_log_paths <- unique(
  normalizePath(old_specify_other_log_paths, winslash = "/", mustWork = FALSE)
)

cat("\nPrevious specify-other cleaning-log files found: ", length(old_specify_other_log_paths), "\n", sep = "")

if (length(old_specify_other_log_paths) > 0) {
  old_specify_other_log_raw <- purrr::map_dfr(old_specify_other_log_paths, function(path_x) {
    sheets_x <- tryCatch(readxl::excel_sheets(path_x), error = function(e) character())
    sheet_to_read <- specify_other_dedup_sheet_priority[
      specify_other_dedup_sheet_priority %in% sheets_x
    ][1]
    if (is.na(sheet_to_read) || length(sheet_to_read) == 0) return(tibble::tibble())
    tryCatch(
      readxl::read_xlsx(path_x, sheet = sheet_to_read, col_types = "text") |>
        janitor::clean_names() |>
        dplyr::mutate(source_specify_other_log_file = basename(path_x)),
      error = function(e) tibble::tibble()
    )
  })
}

if (is.data.frame(old_specify_other_log_raw) && nrow(old_specify_other_log_raw) > 0 &&
    is.data.frame(combined_log$cleaning_log) && nrow(combined_log$cleaning_log) > 0) {

  # Ensure required columns exist in the old specify-other log
  for (cc in c("dataset", "uuid", "parent_uuid", "repeat_index",
               "question", "specify_other_variable", "change_type", "row_role")) {
    if (!cc %in% names(old_specify_other_log_raw)) {
      old_specify_other_log_raw[[cc]] <- NA_character_
    }
  }

  # Keep only actionable rows from the old specify-other log.
  # Context-only rows (main_variable_context, select_multiple_choice_context) are not
  # issues to track; the specify_other and select_multiple_dummy rows are.
  old_specify_other_actionable <- old_specify_other_log_raw |>
    dplyr::filter(
      is.na(change_type) |
        change_type == "change_response" |
        (!is.na(row_role) & row_role %in% c("specify_other", "select_multiple_dummy", "linked_variable"))
    ) |>
    dplyr::mutate(
      dataset               = standardise_dataset_name(as.character(dataset)),
      uuid                  = as.character(uuid),
      parent_uuid           = as.character(parent_uuid),
      repeat_index          = as.character(repeat_index),
      question              = as.character(question),
      specify_other_variable = as.character(specify_other_variable)
    )

  # Build match keys.
  # Key type A: (dataset, uuid, parent_uuid, repeat_index, question) – direct column match.
  # Key type B: (dataset, uuid, parent_uuid, repeat_index, specify_other_variable) –
  #             covers cases where the main log uses the text variable name as question
  #             but the specify-other log records it in the specify_other_variable column.
  build_so_keys <- function(df, question_col) {
    df |>
      dplyr::filter(!is.na(.data[[question_col]]), trimws(.data[[question_col]]) != "") |>
      dplyr::transmute(
        .so_key = paste(
          normalise_previous_flag_key_value(dataset),
          normalise_previous_flag_key_value(uuid),
          normalise_previous_flag_key_value(parent_uuid),
          normalise_previous_flag_key_value(repeat_index),
          normalise_previous_flag_key_value(.data[[question_col]]),
          sep = "||"
        )
      ) |>
      dplyr::pull(.so_key) |>
      unique()
  }

  so_keys_from_question  <- build_so_keys(old_specify_other_actionable, "question")
  so_keys_from_so_var    <- build_so_keys(old_specify_other_actionable, "specify_other_variable")
  all_so_keys            <- unique(c(so_keys_from_question, so_keys_from_so_var))

  if (length(all_so_keys) > 0 && nrow(combined_log$cleaning_log) > 0) {

    n_main_log_before <- nrow(combined_log$cleaning_log)

    current_log_with_so_meta <- combined_log$cleaning_log |>
      dplyr::mutate(
        # Match key for this row
        .so_match_key = paste(
          normalise_previous_flag_key_value(standardise_dataset_name(as.character(dataset))),
          normalise_previous_flag_key_value(as.character(uuid)),
          normalise_previous_flag_key_value(as.character(parent_uuid)),
          normalise_previous_flag_key_value(as.character(repeat_index)),
          normalise_previous_flag_key_value(as.character(question)),
          sep = "||"
        ),
        # Only suppress flags that are genuinely specify-other in nature.
        # This avoids accidentally hiding unrelated logical-check flags for the same uuid.
        .is_specify_other_flag = (
          stringr::str_detect(
            as.character(check_id),
            stringr::regex("^SPECIFY_OTHER_", ignore_case = FALSE)
          ) |
          stringr::str_detect(
            as.character(issue),
            stringr::regex("specify.other|other response|specify-other", ignore_case = TRUE)
          ) |
          stringr::str_detect(
            as.character(question),
            stringr::regex("_other$|_other_specify$|_specify_other$|_specify$|^other_|_others$", ignore_case = TRUE)
          )
        )
      )

    flags_removed_by_specify_other_dedup <- current_log_with_so_meta |>
      dplyr::filter(.is_specify_other_flag & .so_match_key %in% all_so_keys) |>
      dplyr::select(-dplyr::any_of(c(".so_match_key", ".is_specify_other_flag")))

    combined_log$cleaning_log <- current_log_with_so_meta |>
      dplyr::filter(!(.is_specify_other_flag & .so_match_key %in% all_so_keys)) |>
      dplyr::select(-dplyr::any_of(c(".so_match_key", ".is_specify_other_flag")))

    n_main_log_after <- nrow(combined_log$cleaning_log)

    specify_other_dedup_summary <- tibble::tibble(
      previous_specify_other_files_found        = length(old_specify_other_log_paths),
      previous_specify_other_rows_read          = nrow(old_specify_other_log_raw),
      main_log_rows_before_specify_other_dedup  = n_main_log_before,
      main_log_rows_removed_specify_other_dedup = n_main_log_before - n_main_log_after,
      main_log_rows_after_specify_other_dedup   = n_main_log_after
    )

    cat(
      "Main-log rows removed (already in specify-other log): ",
      n_main_log_before - n_main_log_after, "\n",
      sep = ""
    )

  }
} else {
  cat("No previous specify-other flags found to deduplicate against.\n")
}

print(specify_other_dedup_summary)

# --------------------------------------------------------------------------------------------------
# 8. Split cleaning log and deletion log
# --------------------------------------------------------------------------------------------------

cleaning_log_template <- combined_log$cleaning_log |>
  standardise_msna_cleaning_log() |>
  align_repeat_cleaning_log_uuid_to_submission_uuid() |>
  dplyr::select(
    dplyr::all_of(core_cleaning_log_cols),
    dplyr::any_of(context_cols),
    dplyr::any_of("old_value_converted"),
    dplyr::everything()
  ) |>
  dplyr::distinct(
    dataset,
    uuid,
    parent_uuid,
    repeat_index,
    question,
    issue,
    check_id,
    .keep_all = TRUE
  ) |>
  dplyr::arrange(dataset, check_id, uuid, question)

deletion_log <- cleaning_log_template |>
  dplyr::filter(change_type == "remove_survey")

cleaning_log_no_dl <- cleaning_log_template |>
  dplyr::filter(is.na(change_type) | change_type != "remove_survey")

final_cleaning_log_no_dl <- cleaning_log_no_dl

# Main object names for compatibility with later scripts.
combined_log$cleaning_log <- cleaning_log_template
cleaning_log <- cleaning_log_template
combined_log_df <- cleaning_log_template

# Keep checked datasets for all export/repeat sheets.
combined_checked_datasets_by_dataset <- checked_datasets_by_dataset

combined_log_no_deletion <- list(
  checked_dataset = combined_log$checked_dataset,
  cleaning_log = cleaning_log_no_dl
)

combined_log_no_deletion_cleaningtools_style <- combined_log_no_deletion

# --------------------------------------------------------------------------------------------------
# 9. Create per-dataset cleaning logs
# --------------------------------------------------------------------------------------------------

if (nrow(cleaning_log_template) > 0) {
  
  combined_log_by_dataset <- cleaning_log_template |>
    dplyr::group_split(dataset)
  
  names(combined_log_by_dataset) <- cleaning_log_template |>
    dplyr::distinct(dataset) |>
    dplyr::pull(dataset)
  
} else {
  
  combined_log_by_dataset <- list()
}

cleaning_logs_by_dataset <- combined_log_by_dataset

if (nrow(deletion_log) > 0) {
  
  deletion_log_by_dataset <- deletion_log |>
    dplyr::group_split(dataset)
  
  names(deletion_log_by_dataset) <- deletion_log |>
    dplyr::distinct(dataset) |>
    dplyr::pull(dataset)
  
} else {
  
  deletion_log_by_dataset <- list()
}

# --------------------------------------------------------------------------------------------------
# 10. Create check and dashboard summaries
# --------------------------------------------------------------------------------------------------

check_summary <- cleaning_log_template |>
  dplyr::count(dataset, check_id, issue, name = "n_flags") |>
  dplyr::arrange(dataset, dplyr::desc(n_flags))

cleaning_dashboard <- cleaning_log_template |>
  dplyr::count(dataset, issue, name = "n_flags") |>
  dplyr::arrange(dataset, dplyr::desc(n_flags))

# --------------------------------------------------------------------------------------------------
# 11. Missingness summary - household level only
# --------------------------------------------------------------------------------------------------
# Missingness flagging is restricted to household-level Submissions in Script 3.
# Keep this summary aligned with that rule so repeat sheets are not interpreted as missingness issues.

if (exists("raw_data_list") && is.list(raw_data_list) && "Submissions" %in% names(raw_data_list)) {
  
  df <- raw_data_list$Submissions
  
  if (!is.data.frame(df) || nrow(df) == 0) {
    
    missingness_summary <- tibble::tibble(
      dataset = "Submissions",
      variable = NA_character_,
      percent_missing = NA_real_
    )
    
  } else {
    
    missingness_summary <- df |>
      dplyr::summarise(
        dplyr::across(
          dplyr::everything(),
          ~ mean(is.na(.x) | trimws(as.character(.x)) == "") * 100
        )
      ) |>
      tidyr::pivot_longer(
        cols = dplyr::everything(),
        names_to = "variable",
        values_to = "percent_missing"
      ) |>
      dplyr::mutate(dataset = "Submissions", .before = 1) |>
      dplyr::arrange(dataset, dplyr::desc(percent_missing))
  }
  
} else {
  
  missingness_summary <- tibble::tibble(
    dataset = character(),
    variable = character(),
    percent_missing = numeric()
  )
}

# --------------------------------------------------------------------------------------------------
# 12. Repeat count summary
# --------------------------------------------------------------------------------------------------

if (exists("make_repeat_count_summary", mode = "function") && exists("raw_data_list") && is.list(raw_data_list)) {
  
  repeat_count_summary <- make_repeat_count_summary(raw_data_list)
  
} else if (exists("raw_data_list") && is.list(raw_data_list)) {
  
  repeat_count_summary <- purrr::imap_dfr(raw_data_list, function(df, nm) {
    
    tibble::tibble(
      dataset = nm,
      records = if (is.data.frame(df)) nrow(df) else NA_integer_,
      unique_uuid = if (is.data.frame(df) && "uuid" %in% names(df)) {
        dplyr::n_distinct(df$uuid, na.rm = TRUE)
      } else {
        NA_integer_
      },
      unique_parent_uuid = if (is.data.frame(df) && "parent_uuid" %in% names(df)) {
        dplyr::n_distinct(df$parent_uuid, na.rm = TRUE)
      } else {
        NA_integer_
      }
    )
  })
  
} else {
  
  repeat_count_summary <- tibble::tibble(
    dataset = character(),
    records = integer(),
    unique_uuid = integer(),
    unique_parent_uuid = integer()
  )
}

# --------------------------------------------------------------------------------------------------
# 13. Special value summary
# --------------------------------------------------------------------------------------------------

special_value_summary <- cleaning_log_template |>
  dplyr::filter(
    grepl("Special missing/unknown code|special missing|999|-999|_999|-990", issue, ignore.case = TRUE) |
      old_value %in% c("999", "999.0", "_999", "-999", "-990", " _999")
  ) |>
  dplyr::count(dataset, question, old_value, name = "n_flags") |>
  dplyr::arrange(dataset, dplyr::desc(n_flags))

# Keep logical-check diagnostics from Script 3 if available.
if (!exists("logical_check_variable_availability")) {
  logical_check_variable_availability <- tibble::tibble()
}

if (!exists("logical_check_summary")) {
  
  logical_check_summary <- if (nrow(logical_check_variable_availability) > 0) {
    logical_check_variable_availability |>
      dplyr::count(
        dataset,
        all_columns_to_clean_found,
        any_columns_to_clean_found,
        name = "n_checks"
      )
  } else {
    tibble::tibble()
  }
}


# --------------------------------------------------------------------------------------------------
# 13B. Split processed cleaning logs by district/admin2 for district-level export
# --------------------------------------------------------------------------------------------------
# This section prepares one cleaningtools-style object per district AFTER all Script 4 processing.
# Script 5 should then only loop through district_combined_logs_by_admin2 and export each object.

make_safe_file_name_script4 <- function(x) {
  x <- as.character(x)
  x <- ifelse(is.na(x) | trimws(x) == "", "missing_admin2", x)
  x <- stringr::str_replace_all(x, "[^A-Za-z0-9_ -]", "_")
  x <- stringr::str_replace_all(x, "\\s+", "_")
  x <- stringr::str_replace_all(x, "_+", "_")
  x <- stringr::str_replace_all(x, "^_|_$", "")
  ifelse(x == "", "missing_admin2", x)
}

standardise_admin2_for_district_export <- function(x) {
  x <- as.character(x)
  x <- stringr::str_squish(x)
  x <- dplyr::na_if(x, "")
  stringr::str_to_lower(x)
}

ensure_admin2_standardised_for_export <- function(df) {
  if (!is.data.frame(df) || nrow(df) == 0) {
    return(df)
  }
  if ("admin2" %in% names(df)) {
    df <- df |>
      dplyr::mutate(admin2 = standardise_admin2_for_district_export(admin2))
  }
  df
}

filter_checked_dataset_for_district_after_script4 <- function(df, dataset_name, district_name, district_keys) {
  if (!is.data.frame(df) || nrow(df) == 0) {
    return(tibble::tibble())
  }
  
  df <- tibble::as_tibble(df)
  df <- ensure_admin2_standardised_for_export(df)
  
  if (dataset_name == "Submissions") {
    if ("admin2" %in% names(df)) {
      return(
        df |>
          dplyr::filter(as.character(admin2) == as.character(district_name))
      )
    }
    return(df)
  }
  
  parent_cols <- intersect(
    c(
      "parent_uuid",
      "_parent_uuid",
      "_submission_uuid",
      "_submission__uuid",
      "_submission__id",
      "_submission_id",
      "submission_uuid",
      "submission_id",
      "parent_instance_name"
    ),
    names(df)
  )
  
  if (length(parent_cols) > 0) {
    keep <- rep(FALSE, nrow(df))
    for (pc in parent_cols) {
      keep <- keep | as.character(df[[pc]]) %in% district_keys
    }
    return(df[keep, , drop = FALSE])
  }
  
  if ("admin2" %in% names(df)) {
    return(
      df |>
        dplyr::filter(as.character(admin2) == as.character(district_name))
    )
  }
  
  tibble::tibble()
}

make_district_check_summary <- function(log_df) {
  if (!is.data.frame(log_df) || nrow(log_df) == 0) {
    return(tibble::tibble(message = "No records"))
  }
  
  summary_cols <- intersect(c("dataset", "check_id", "issue"), names(log_df))
  
  if (length(summary_cols) == 0) {
    return(tibble::tibble(n_flags = nrow(log_df)))
  }
  
  log_df |>
    dplyr::group_by(dplyr::across(dplyr::all_of(summary_cols))) |>
    dplyr::summarise(n_flags = dplyr::n(), .groups = "drop") |>
    dplyr::arrange(dplyr::desc(n_flags))
}

checked_sheet_name_map <- c(
  Submissions = "checked_household_level",
  roster = "checked_roster",
  edu_ind = "checked_edu_ind",
  health_ind = "checked_health_ind",
  nutrition_ind = "checked_nutrition_ind",
  child_protection_ind = "checked_child_protection"
)

# Standardise admin2 in the processed household dataset and cleaning log before splitting.
combined_log$checked_dataset <- ensure_admin2_standardised_for_export(combined_log$checked_dataset)
combined_log$checked_dataset <- move_point_number_first(combined_log$checked_dataset)
cleaning_log_template <- ensure_admin2_standardised_for_export(cleaning_log_template)
cleaning_log <- cleaning_log_template
combined_log$cleaning_log <- cleaning_log_template

if (exists("checked_datasets_by_dataset") && is.list(checked_datasets_by_dataset)) {
  checked_datasets_by_dataset <- purrr::map(checked_datasets_by_dataset, ensure_admin2_standardised_for_export)
  checked_datasets_by_dataset <- purrr::imap(
    checked_datasets_by_dataset,
    ~ align_repeat_checked_dataset_uuid_point_number(.x, .y, main_context_for_repeat_exports)
  )
  checked_datasets_by_dataset <- purrr::map(checked_datasets_by_dataset, move_point_number_first)
  combined_checked_datasets_by_dataset <- checked_datasets_by_dataset
}

main_data_for_district_export <- combined_log$checked_dataset

if (!is.data.frame(main_data_for_district_export) || nrow(main_data_for_district_export) == 0) {
  district_combined_logs_by_admin2 <- list()
  district_extra_sheets_by_admin2 <- list()
  district_workbook_sheets_by_admin2 <- list()
  district_export_manifest <- tibble::tibble()
} else if (!"admin2" %in% names(main_data_for_district_export)) {
  warning("admin2 was not found in combined_log$checked_dataset. District-level split objects were not created.")
  district_combined_logs_by_admin2 <- list()
  district_extra_sheets_by_admin2 <- list()
  district_workbook_sheets_by_admin2 <- list()
  district_export_manifest <- tibble::tibble()
} else {
  
  main_id_cols_for_district_export <- intersect(
    c(
      "uuid",
      "_uuid",
      "_id",
      "instance_name",
      "_submission__id",
      "_submission_id",
      "submission_uuid",
      "submission_id"
    ),
    names(main_data_for_district_export)
  )
  
  district_values <- main_data_for_district_export |>
    dplyr::filter(!is.na(admin2), trimws(as.character(admin2)) != "") |>
    dplyr::distinct(admin2) |>
    dplyr::arrange(admin2) |>
    dplyr::pull(admin2) |>
    as.character()
  
  district_combined_logs_by_admin2 <- list()
  district_extra_sheets_by_admin2 <- list()
  district_workbook_sheets_by_admin2 <- list()
  
  for (current_district in district_values) {
    
    district_main_data <- main_data_for_district_export |>
      dplyr::filter(as.character(admin2) == as.character(current_district)) |>
      move_point_number_first()
    
    district_keys <- unique(unlist(
      purrr::map(
        main_id_cols_for_district_export,
        ~ as.character(district_main_data[[.x]])
      )
    ))
    
    district_keys <- district_keys[!is.na(district_keys) & district_keys != ""]
    
    # Keep all flags linked to the district, including repeat-level flags.
    district_cleaning_log <- cleaning_log_template |>
      dplyr::filter(
        as.character(admin2) == as.character(current_district) |
          uuid %in% district_keys |
          parent_uuid %in% district_keys
      )
    
    # Keep change_type blank for review/export.
    if ("change_type" %in% names(district_cleaning_log)) {
      district_cleaning_log$change_type <- NA_character_
    }
    
    district_combined_log <- list(
      checked_dataset = district_main_data,
      cleaning_log = district_cleaning_log
    )
    
    district_checked_sheets <- list()
    
    if (exists("checked_datasets_by_dataset") && is.list(checked_datasets_by_dataset)) {
      district_checked_datasets <- purrr::imap(
        checked_datasets_by_dataset[names(checked_datasets_by_dataset) %in% names(checked_sheet_name_map)],
        ~ filter_checked_dataset_for_district_after_script4(
          df = .x,
          dataset_name = .y,
          district_name = current_district,
          district_keys = district_keys
        )
      )
      
      district_checked_sheets <- purrr::map(district_checked_datasets, move_point_number_first)
      names(district_checked_sheets) <- unname(
        checked_sheet_name_map[names(district_checked_sheets)]
      )
    }
    
    # Additional sheets for this district. Summary objects remain national summaries unless already district-filtered.
    district_extra_sheets <- c(
      district_checked_sheets[names(district_checked_sheets) != "checked_household_level"],
      list(
        check_summary = make_district_check_summary(district_cleaning_log),
        repeat_count_summary = repeat_count_summary,
        missingness_summary = missingness_summary,
        special_value_summary = special_value_summary,
        logical_check_summary = logical_check_summary,
        logical_check_variable_availability = logical_check_variable_availability
      )
    )
    
    district_combined_logs_by_admin2[[current_district]] <- district_combined_log
    district_extra_sheets_by_admin2[[current_district]] <- district_extra_sheets
    district_workbook_sheets_by_admin2[[current_district]] <- c(
      district_combined_log,
      district_extra_sheets
    )
  }
  
  district_export_manifest <- purrr::imap_dfr(
    district_combined_logs_by_admin2,
    function(x, nm) {
      tibble::tibble(
        admin2 = nm,
        admin2_safe = make_safe_file_name_script4(nm),
        n_households = if (is.data.frame(x$checked_dataset)) nrow(x$checked_dataset) else NA_integer_,
        n_cleaning_flags = if (is.data.frame(x$cleaning_log)) nrow(x$cleaning_log) else NA_integer_
      )
    }
  )
}

# --------------------------------------------------------------------------------------------------
# 14. Create export-ready named lists, but DO NOT export here
# --------------------------------------------------------------------------------------------------

cleaning_log_workbook_sheets <- c(
  list(
    cleaning_log = cleaning_log_template,
    cleaning_log_no_deletion = cleaning_log_no_dl,
    deletion_log = deletion_log,
    check_summary = check_summary,
    dashboard = cleaning_dashboard,
    repeat_count_summary = repeat_count_summary,
    missingness_summary = missingness_summary,
    special_value_summary = special_value_summary,
    logical_check_summary = logical_check_summary,
    logical_check_variable_availability = logical_check_variable_availability,
    specify_other_dedup_summary = specify_other_dedup_summary
  ),
  stats::setNames(
    cleaning_logs_by_dataset,
    paste0("log_", names(cleaning_logs_by_dataset))
  )
)

logbook_to_export <- cleaning_log_workbook_sheets
combined_log_to_export <- cleaning_log_template
combined_log_cleaningtools_style <- combined_log

# --------------------------------------------------------------------------------------------------
# 15. Compact console summary
# --------------------------------------------------------------------------------------------------

cat("\n")
cat("Cleaning-log records: ", nrow(cleaning_log_template), "\n", sep = "")
cat("Cleaning-log records excluding deletion log: ", nrow(cleaning_log_no_dl), "\n", sep = "")
cat("Deletion-log records: ", nrow(deletion_log), "\n", sep = "")
cat("PII columns removed from checked dataset: ", length(pii_cols_in_checked_dataset), "\n", sep = "")
cat("Previous cleaning-log files found: ", length(old_cls_paths), "\n", sep = "")
cat("Main-log rows removed (specify-other dedup): ", specify_other_dedup_summary$main_log_rows_removed_specify_other_dedup[[1]], "\n", sep = "")
cat("Datasets with flags: ", paste(names(cleaning_logs_by_dataset), collapse = ", "), "\n", sep = "")
cat("\n")

if (nrow(check_summary) > 0) {
  print(
    check_summary |>
      dplyr::group_by(dataset) |>
      dplyr::summarise(flags = sum(n_flags), .groups = "drop")
  )
}

cat("\n\n\n\n\n\n\n")
cat(crayon::green$bold("Cleaning logs and deletion logs processed"))
cat("\n\n\n")

# --------------------------------------------------------------------------------------------------
# 16. Report new objects created
# --------------------------------------------------------------------------------------------------

all_dfs_names_after <- Filter(function(x) is(x, "data.frame"), mget(ls())) |> names()
all_lists_names_after <- Filter(function(x) is(x, "list"), mget(ls())) |> names()

if (length(setdiff(all_dfs_names_after, all_dfs_names_before)) > 0) {
  
  cat(crayon::blue("The new dataframes created are:"))
  cat("\n")
  
  for (df in setdiff(all_dfs_names_after, all_dfs_names_before)) {
    cat(df)
    cat("\n")
  }
}

if (length(setdiff(all_lists_names_after, all_lists_names_before)) > 0) {
  
  cat(crayon::blue("The new lists created are:"))
  cat("\n")
  
  for (df in setdiff(all_lists_names_after, all_lists_names_before)) {
    cat(df)
    cat("\n")
  }
}