cat("\014")

# =================================================================================================
# REACH MSNA 2026 Mozambique - 6. Specify-other checks and conversion log
# -------------------------------------------------------------------------------------------------
# Purpose:
#   - Create a separate cleaning log for all filled "specify other" variables in the final Kobo tool.
#   - Follow the same broad structure as the unit-other sample script:
#       1) prepare data;
#       2) create a checklist;
#       3) run the checks;
#       4) create a combined cleaning log;
#       5) assign change_type/new_value rules;
#       6) remove data points already flagged previously for specify-other review;
#       7) export a cleaning log.
#
# Key MSNA-specific rules:
#   - Main household sheet and repeat sheets are all checked.
#   - For select_one linked questions:
#       * main variable row is included with change_type = "no_action";
#       * specify-other text variable row is included with change_type = "change_response".
#   - For select_multiple linked questions:
#       * main variable row is included with change_type = "no_action";
#       * corresponding split dummy variable(s), e.g. variable_choice_other, are included with
#         change_type = "change_response";
#       * specify-other text variable row is included with change_type = "change_response".
#   - Previously exported specify-other flags are removed before exporting the new log.
#
# Recommended workflow position:
#   Run this AFTER scripts 1 to 4 have run successfully.
#
# Required objects from earlier scripts:
#   - path_tool
#   - path_cleaning_folder_today
#   - checked_datasets_by_dataset and/or raw_data_list
#   - data_hh_main, when available, for context linkage
# =================================================================================================

all_dfs_names_before <- Filter(function(x) is(x, "data.frame"), mget(ls())) |> names()
all_lists_names_before <- Filter(function(x) is(x, "list"), mget(ls())) |> names()

if (exists("print_step", mode = "function")) {
  print_step("6. Creating specify-other cleaning log")
} else {
  cat("\n6. Creating specify-other cleaning log\n")
}

# --------------------------------------------------------------------------------------------------
# 0. Load packages and define switches
# --------------------------------------------------------------------------------------------------

required_packages <- c(
  "dplyr",
  "tibble",
  "tidyr",
  "purrr",
  "stringr",
  "readxl",
  "janitor",
  "openxlsx",
  "fs"
)

for (pkg in required_packages) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    stop("Package '", pkg, "' is required for Script 6 specify-other checks.", call. = FALSE)
  }
}



# Keep these TRUE for routine cleaning-log export.
export_specify_other_logbook <- TRUE
export_specify_other_district_logbooks <- TRUE
remove_previous_specify_other_flags <- TRUE

# Set TRUE only if you specifically want to use cleaningtools::check_logical_with_list().
# The default manual engine is safer for repeated sheets because it keeps repeat_index and context.
use_cleaningtools_engine_if_available <- FALSE

# --------------------------------------------------------------------------------------------------
# 1. Helper functions
# --------------------------------------------------------------------------------------------------

`%||%` <- function(x, y) {
  if (is.null(x)) y else x
}

safe_as_character <- function(x) {
  if (inherits(x, "POSIXct") || inherits(x, "POSIXt") || inherits(x, "Date")) {
    return(as.character(x))
  }
  as.character(x)
}


# Returns TRUE for values that look purely numeric / coded and carry no
# meaningful text information — e.g. "999", "_999", "5000", "-1", "1.5".
# Pattern: optional leading underscores / spaces / minus, then at least one
# digit, then only digits, separators (. , space) and trailing underscores.
is_numeric_text <- function(x) {
  x_clean <- trimws(as.character(x))
  !is.na(x_clean) & nchar(x_clean) > 0L &
    grepl("^[_\\s]*-?[0-9][0-9_.\\-,\\s]*$", x_clean, perl = TRUE)
}

as_one_chr <- function(x, default = NA_character_) {
  if (is.null(x) || length(x) == 0) {
    return(default)
  }
  out <- suppressWarnings(as.character(x[[1]]))
  if (length(out) == 0 || is.na(out)) default else out
}

as_one_nonmissing_chr <- function(x, default = "") {
  out <- as_one_chr(x, default = default)
  if (length(out) == 0 || is.na(out)) default else out
}

is_filled_value <- function(x) {
  y <- trimws(as.character(x))
  !is.na(x) & !is.na(y) & y != "" & !(tolower(y) %in% c("na", "nan", "null", "n/a"))
}

suggest_unselect_value <- function(x) {
  y <- tolower(trimws(as.character(x)))
  dplyr::case_when(
    is.na(y) | y == "" ~ NA_character_,
    y %in% c("yes", "y") ~ "no",
    y %in% c("1") ~ "0",
    y %in% c("true", "t") ~ "FALSE",
    TRUE ~ NA_character_
  )
}

first_existing_col <- function(df, candidates) {
  if (!is.data.frame(df) || ncol(df) == 0) {
    return(NA_character_)
  }
  hit <- candidates[candidates %in% names(df)]
  if (length(hit) == 0) NA_character_ else hit[[1]]
}

standardise_dataset_name <- function(x) {
  x <- as.character(x)
  dplyr::case_when(
    x %in% c("Submissions", "household", "household_level", "data_hh_main", "REACH_MOZ_MSNA2026_Kobo_form") ~ "Submissions",
    x %in% c("roster", "data_hh_roster", "data_roster") ~ "roster",
    x %in% c("edu_ind", "education_ind", "data_edu_ind") ~ "edu_ind",
    x %in% c("health_ind", "data_health_ind") ~ "health_ind",
    x %in% c("nutrition_ind", "nut_ind", "data_nutrition_ind", "data_nut_ind") ~ "nutrition_ind",
    x %in% c("child_protection_ind", "prot_ind", "protection_ind", "data_child_protection_ind", "data_prot_ind") ~ "child_protection_ind",
    TRUE ~ x
  )
}

source_dataset_label <- function(x) {
  x <- standardise_dataset_name(x)
  dplyr::case_when(
    x == "Submissions" ~ "household_level",
    TRUE ~ x
  )
}

make_safe_file_name <- function(x) {
  x <- as.character(x)
  x <- ifelse(is.na(x) | trimws(x) == "", "missing_admin2", x)
  x <- stringr::str_replace_all(x, "[^A-Za-z0-9_ -]", "_")
  x <- stringr::str_replace_all(x, "\\s+", "_")
  x <- stringr::str_replace_all(x, "_+", "_")
  x <- stringr::str_replace_all(x, "^_|_$", "")
  ifelse(x == "", "missing_admin2", x)
}

move_point_number_first <- function(df) {
  if (is.null(df) || !is.data.frame(df) || ncol(df) == 0) {
    return(df)
  }
  df <- tibble::as_tibble(df)
  if ("point_number" %in% names(df)) {
    df <- df |> dplyr::select(point_number, dplyr::everything())
  }
  df
}

empty_specify_other_log <- function() {
  tibble::tibble(
    source_dataset = character(),
    dataset = character(),
    uuid = character(),
    enum_id = character(),
    point_number = character(),
    question = character(),
    question_label = character(),
    issue = character(),
    old_value = character(),
    new_value = character(),
    notes = character(),
    change_type = character(),
    admin1 = character(),
    admin2 = character(),
    admin3 = character(),
    admin3_setlement = character(),
    admin4 = character(),
    hoh_type = character(),
    parent_uuid = character(),
    repeat_index = character(),
    person_id = character(),
    check_id = character(),
    check_to_perform = character(),
    columns_to_clean = character(),
    specify_other_variable = character(),
    main_variable = character(),
    main_question_type = character(),
    other_choice = character(),
    row_role = character(),
    color = character()
  )
}

specify_other_export_cols <- names(empty_specify_other_log())

# Final column order for the exported Excel file.
# question_label sits right after question so reviewers immediately see the
# human-readable label alongside the variable name.
# specify_other_text appears after old_value so the typed text is always visible.
specify_other_final_export_cols <- c(
  "source_dataset", "dataset", "uuid", "enum_id", "point_number",
  "question", "question_label", "issue", "old_value", "specify_other_text",
  "new_value", "notes", "change_type",
  "admin1", "admin2", "admin3", "admin3_setlement", "admin4",
  "hoh_type", "parent_uuid", "repeat_index", "person_id", "color"
)

# --------------------------------------------------------------------------------------------------
# 2. Define expected datasets and obtain data from the current MSNA workflow
# --------------------------------------------------------------------------------------------------

expected_datasets <- c(
  "Submissions",
  "roster",
  "edu_ind",
  "health_ind",
  "nutrition_ind",
  "child_protection_ind"
)

repeat_dataset_names <- setdiff(expected_datasets, "Submissions")

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

get_source_data_list <- function() {
  if (exists("checked_datasets_by_dataset") && is.list(checked_datasets_by_dataset)) {
    return(checked_datasets_by_dataset)
  }
  if (exists("combined_checked_datasets_by_dataset") && is.list(combined_checked_datasets_by_dataset)) {
    return(combined_checked_datasets_by_dataset)
  }
  if (exists("raw_data_list") && is.list(raw_data_list)) {
    return(raw_data_list)
  }
  out <- list()
  if (exists("data_hh_main") && is.data.frame(data_hh_main)) out$Submissions <- data_hh_main
  if (exists("data_hh_roster") && is.data.frame(data_hh_roster)) out$roster <- data_hh_roster
  if (exists("data_roster") && is.data.frame(data_roster)) out$roster <- data_roster
  if (exists("data_edu_ind") && is.data.frame(data_edu_ind)) out$edu_ind <- data_edu_ind
  if (exists("data_health_ind") && is.data.frame(data_health_ind)) out$health_ind <- data_health_ind
  if (exists("data_nutrition_ind") && is.data.frame(data_nutrition_ind)) out$nutrition_ind <- data_nutrition_ind
  if (exists("data_nut_ind") && is.data.frame(data_nut_ind)) out$nutrition_ind <- data_nut_ind
  if (exists("data_child_protection_ind") && is.data.frame(data_child_protection_ind)) out$child_protection_ind <- data_child_protection_ind
  if (exists("data_prot_ind") && is.data.frame(data_prot_ind)) out$child_protection_ind <- data_prot_ind
  out
}

standardise_uuid_for_dataset <- function(df, dataset_name) {
  if (!is.data.frame(df) || nrow(df) == 0) {
    return(tibble::tibble())
  }
  df <- tibble::as_tibble(df)
  dataset_name <- standardise_dataset_name(dataset_name)
  
  if (dataset_name == "Submissions") {
    uuid_col <- first_existing_col(df, c("uuid", "_uuid", "_id", "instance_name", "_submission__id", "_submission_id", "submission_uuid", "submission_id"))
    if (!is.na(uuid_col)) {
      df$uuid <- as.character(df[[uuid_col]])
    } else if (!"uuid" %in% names(df)) {
      df$uuid <- paste0("Submissions_row_", seq_len(nrow(df)))
    }
    df$parent_uuid <- if ("parent_uuid" %in% names(df)) as.character(df$parent_uuid) else NA_character_
    if (!"submission_uuid" %in% names(df)) df$submission_uuid <- as.character(df$uuid)
  } else {
    if (!"repeat_row_uuid" %in% names(df)) {
      repeat_row_id_col <- first_existing_col(df, c("repeat_row_uuid", "_id", "instance_name", "KEY", "_submission__id", "_submission_id", "uuid", "_uuid"))
      df$repeat_row_uuid <- if (!is.na(repeat_row_id_col)) as.character(df[[repeat_row_id_col]]) else paste0(dataset_name, "_row_", seq_len(nrow(df)))
    }
    parent_col <- first_existing_col(df, c("_submission__uuid", "_submission_uuid", "submission_uuid", "parent_uuid", "_parent_uuid", "submission__uuid", "_submission__id", "_submission_id", "submission_id", "parent_instance_name", "uuid"))
    if (!is.na(parent_col)) {
      parent_id <- as.character(df[[parent_col]])
      df$`_submission__uuid` <- parent_id
      df$submission_uuid <- parent_id
      df$parent_uuid <- parent_id
      df$uuid <- parent_id
    } else if (!"uuid" %in% names(df)) {
      df$uuid <- paste0(dataset_name, "_row_", seq_len(nrow(df)))
      df$parent_uuid <- NA_character_
    }
  }
  
  if (!"repeat_index" %in% names(df)) {
    repeat_col <- first_existing_col(df, c("repeat_index", "_index", "index", "_parent_index", "repeat_position", "KEY"))
    df$repeat_index <- if (!is.na(repeat_col)) {
      as.character(df[[repeat_col]])
    } else if (dataset_name == "Submissions") {
      NA_character_
    } else {
      as.character(seq_len(nrow(df)))
    }
  } else {
    df$repeat_index <- as.character(df$repeat_index)
  }
  
  move_point_number_first(df)
}

specify_other_data_list <- get_source_data_list()
specify_other_data_list <- specify_other_data_list[names(specify_other_data_list) %in% expected_datasets]
specify_other_data_list <- purrr::imap(specify_other_data_list, ~ standardise_uuid_for_dataset(.x, .y))

for (ds in expected_datasets) {
  if (!ds %in% names(specify_other_data_list)) {
    specify_other_data_list[[ds]] <- tibble::tibble()
  }
}

# --------------------------------------------------------------------------------------------------
# 3. Read final Kobo XLSForm and assign every survey row to the correct export dataset
# --------------------------------------------------------------------------------------------------

if (!exists("path_tool")) {
  stop("Object 'path_tool' is missing. Please run Script 1 before Script 6.", call. = FALSE)
}

if (!file.exists(path_tool)) {
  stop("Final Kobo XLSForm was not found at path_tool: ", path_tool, call. = FALSE)
}

survey_raw <- readxl::read_xlsx(path_tool, sheet = "survey", .name_repair = "unique_quiet") |>
  janitor::clean_names()

if (!all(c("type", "name") %in% names(survey_raw))) {
  stop("The final Kobo XLSForm survey sheet must contain at least 'type' and 'name' columns.", call. = FALSE)
}

label_col <- first_existing_col(
  survey_raw,
  c("label", "label_english_en", "label_english", "label_en", "label_english_", "question", "hint")
)

normalise_question_type <- function(type_i) {
  type_i <- tolower(trimws(as_one_nonmissing_chr(type_i, default = "")))
  dplyr::case_when(
    stringr::str_detect(type_i, "^select_one") ~ "select_one",
    stringr::str_detect(type_i, "^select_multiple") ~ "select_multiple",
    stringr::str_detect(type_i, "^text") ~ "text",
    stringr::str_detect(type_i, "^integer") ~ "integer",
    stringr::str_detect(type_i, "^decimal") ~ "decimal",
    stringr::str_detect(type_i, "^calculate") ~ "calculate",
    stringr::str_detect(type_i, "^note") ~ "note",
    stringr::str_detect(type_i, "^begin[ _]repeat") ~ "begin_repeat",
    stringr::str_detect(type_i, "^end[ _]repeat") ~ "end_repeat",
    TRUE ~ type_i
  )
}

extract_choice_list <- function(type_i) {
  type_i <- trimws(as_one_nonmissing_chr(type_i, default = ""))
  if (!stringr::str_detect(tolower(type_i), "^select_one|^select_multiple")) {
    return(NA_character_)
  }
  parts <- unlist(strsplit(type_i, "\\s+"))
  if (length(parts) < 2) NA_character_ else parts[[2]]
}

assign_survey_datasets <- function(survey_df) {
  survey_df <- tibble::as_tibble(survey_df)
  if (!"type" %in% names(survey_df)) survey_df$type <- NA_character_
  if (!"name" %in% names(survey_df)) survey_df$name <- NA_character_
  
  current_dataset <- "Submissions"
  dataset_out <- rep("Submissions", nrow(survey_df))
  dataset_stack <- character()
  
  for (i in seq_len(nrow(survey_df))) {
    type_i <- tolower(trimws(as_one_nonmissing_chr(survey_df$type[[i]], default = "")))
    name_i <- as_one_nonmissing_chr(survey_df$name[[i]], default = "")
    
    is_begin_repeat <- isTRUE(stringr::str_detect(type_i, "^begin[ _]repeat"))
    is_end_repeat <- isTRUE(stringr::str_detect(type_i, "^end[ _]repeat"))
    
    if (is_begin_repeat) {
      dataset_stack <- c(dataset_stack, current_dataset)
      current_dataset <- standardise_dataset_name(name_i)
      if (!current_dataset %in% expected_datasets) {
        current_dataset <- name_i
      }
      dataset_out[[i]] <- current_dataset
      next
    }
    
    dataset_out[[i]] <- current_dataset
    
    if (is_end_repeat) {
      if (length(dataset_stack) > 0) {
        current_dataset <- dataset_stack[[length(dataset_stack)]]
        dataset_stack <- dataset_stack[-length(dataset_stack)]
      } else {
        current_dataset <- "Submissions"
      }
    }
  }
  
  survey_df |>
    dplyr::mutate(dataset = standardise_dataset_name(dataset_out), .before = 1)
}

survey_with_dataset <- assign_survey_datasets(survey_raw)

# Keep this outside mutate() because the native pipe (|>) does not expose the dot placeholder.
# Some XLSForms may also lack a relevant or label column, so create safe defaults first.
if (!"relevant" %in% names(survey_with_dataset)) {
  survey_with_dataset$relevant <- NA_character_
}

if (!is.na(label_col) && label_col %in% names(survey_with_dataset)) {
  survey_with_dataset$label <- as.character(survey_with_dataset[[label_col]])
} else {
  survey_with_dataset$label <- NA_character_
}

survey_with_dataset <- survey_with_dataset |>
  dplyr::mutate(
    type = as.character(type),
    name = as.character(name),
    relevant = as.character(relevant),
    label = as.character(label),
    question_type = purrr::map_chr(type, normalise_question_type),
    choice_list_name = purrr::map_chr(type, extract_choice_list)
  )

# --------------------------------------------------------------------------------------------------
# 4. Create the specify-other checklist from the final Kobo tool
# --------------------------------------------------------------------------------------------------

extract_other_links_from_relevant <- function(relevant_i) {
  relevant_i <- as_one_nonmissing_chr(relevant_i, default = "")
  
  selected_matches <- stringr::str_match_all(
    relevant_i,
    "selected\\s*\\(\\s*\\$\\{([^}]+)\\}\\s*,\\s*['\"]([^'\"]+)['\"]\\s*\\)"
  )[[1]]
  
  equals_matches <- stringr::str_match_all(
    relevant_i,
    "\\$\\{([^}]+)\\}\\s*=\\s*['\"]([^'\"]+)['\"]"
  )[[1]]
  
  out <- tibble::tibble(main_variable = character(), other_choice = character(), link_style = character())
  
  if (is.matrix(selected_matches) && nrow(selected_matches) > 0) {
    out <- dplyr::bind_rows(
      out,
      tibble::tibble(
        main_variable = selected_matches[, 2],
        other_choice = selected_matches[, 3],
        link_style = "selected"
      )
    )
  }
  
  if (is.matrix(equals_matches) && nrow(equals_matches) > 0) {
    out <- dplyr::bind_rows(
      out,
      tibble::tibble(
        main_variable = equals_matches[, 2],
        other_choice = equals_matches[, 3],
        link_style = "equals"
      )
    )
  }
  
  out |>
    dplyr::filter(
      stringr::str_detect(
        stringr::str_to_lower(other_choice),
        "(^|_)other($|_)|others|specify"
      )
    ) |>
    dplyr::distinct(main_variable, other_choice, link_style, .keep_all = TRUE)
}

infer_main_variable_from_other_name <- function(other_name, possible_main_names = character()) {
  other_name <- as_one_nonmissing_chr(other_name, default = "")
  if (other_name == "") return(NA_character_)
  
  candidates <- c(
    stringr::str_remove(other_name, "^other_"),
    stringr::str_remove(other_name, "_other$"),
    stringr::str_remove(other_name, "_other_specify$"),
    stringr::str_remove(other_name, "_specify_other$"),
    stringr::str_remove(other_name, "_specify$"),
    stringr::str_remove(other_name, "_other_text$"),
    stringr::str_remove(other_name, "_other_response$")
  )
  
  candidates <- unique(candidates[!is.na(candidates) & candidates != "" & candidates != other_name])
  hits <- candidates[candidates %in% possible_main_names]
  
  if (length(hits) > 0) hits[[1]] else if (length(candidates) > 0) candidates[[1]] else NA_character_
}

main_question_metadata <- survey_with_dataset |>
  dplyr::filter(question_type %in% c("select_one", "select_multiple")) |>
  dplyr::transmute(
    main_variable = name,
    main_question_type = question_type,
    main_dataset = dataset,
    main_choice_list_name = choice_list_name,
    main_label = label
  ) |>
  dplyr::filter(!is.na(main_variable), trimws(main_variable) != "") |>
  dplyr::distinct(main_variable, .keep_all = TRUE)

possible_main_names <- main_question_metadata$main_variable

other_text_candidates <- survey_with_dataset |>
  dplyr::filter(
    !is.na(name),
    trimws(name) != "",
    question_type %in% c("text", "integer", "decimal"),
    stringr::str_detect(
      stringr::str_to_lower(paste(name, label, relevant, sep = " ")),
      "(^|_)other($|_)|other_|_other|specify other|other specify|specify_other"
    )
  ) |>
  dplyr::mutate(
    inferred_main_variable = purrr::map_chr(name, infer_main_variable_from_other_name, possible_main_names = possible_main_names),
    relevant_links = purrr::map(relevant, extract_other_links_from_relevant)
  )

specify_other_mapping_base <- if (nrow(other_text_candidates) > 0) {
  purrr::map_dfr(seq_len(nrow(other_text_candidates)), function(i) {
    row_i <- other_text_candidates[i, , drop = FALSE]
    links_i <- row_i$relevant_links[[1]]
    
    if (!is.data.frame(links_i) || nrow(links_i) == 0) {
      links_i <- tibble::tibble(
        main_variable = as.character(row_i$inferred_main_variable[[1]]),
        other_choice = "other",
        link_style = "inferred_from_name"
      )
    }
    
    links_i |>
      dplyr::mutate(
        dataset = standardise_dataset_name(as.character(row_i$dataset[[1]])),
        specify_other_variable = as.character(row_i$name[[1]]),
        specify_other_label = as.character(row_i$label[[1]]),
        specify_other_relevant = as.character(row_i$relevant[[1]])
      )
  })
} else {
  tibble::tibble(
    main_variable = character(),
    other_choice = character(),
    link_style = character(),
    dataset = character(),
    specify_other_variable = character(),
    specify_other_label = character(),
    specify_other_relevant = character()
  )
}

specify_other_mapping <- specify_other_mapping_base |>
  dplyr::left_join(main_question_metadata, by = "main_variable") |>
  dplyr::mutate(
    # If the linked main question is on the same repeat sheet, keep that dataset.
    # If main_dataset is missing, keep the dataset of the specify-other variable.
    dataset = standardise_dataset_name(dplyr::coalesce(dataset, main_dataset)),
    main_question_type = dplyr::coalesce(main_question_type, "unknown"),
    other_choice = dplyr::if_else(is.na(other_choice) | trimws(other_choice) == "", "other", other_choice)
  ) |>
  dplyr::filter(dataset %in% expected_datasets) |>
  dplyr::distinct(dataset, specify_other_variable, main_variable, other_choice, .keep_all = TRUE)

if (nrow(specify_other_mapping) == 0) {
  warning("No specify-other variables were detected from the final Kobo tool.", call. = FALSE)
}

make_dummy_candidates <- function(main_var, choice_name) {
  main_var <- as_one_nonmissing_chr(main_var, default = "")
  choice_name <- as_one_nonmissing_chr(choice_name, default = "other")
  choice_clean <- janitor::make_clean_names(choice_name)
  
  unique(c(
    # Original Kobo names with "/" (Script 1 now preserves these)
    paste0(main_var, "/", choice_name),
    paste0(main_var, "/other"),
    # Alternative cleaned patterns (for backward compatibility)
    paste0(main_var, "_choice_", choice_clean),
    paste0(main_var, "_", choice_clean),
    paste0(main_var, "_choice_other"),
    paste0(main_var, "_other"),
    paste0(main_var, ".", choice_clean)
  ))
}

# Return ALL select_multiple dummy columns for main_var that exist in data_df.
# Script 1 now preserves original Kobo variable names with "/" (e.g., variable/choice).
# This function searches for both "/" patterns (primary) and "_choice_" patterns (backward compatibility).
get_all_sm_dummy_cols_from_data <- function(main_var, data_df) {
  if (is.na(main_var) || trimws(main_var) == "" || !is.data.frame(data_df) || ncol(data_df) == 0) {
    return(character())
  }
  
  col_names <- names(data_df)
  choice_cols <- character()
  
  # Primary: Original Kobo format with "/" – {main_var}/{anything}
  prefix_slash <- paste0(main_var, "/")
  choice_cols <- col_names[startsWith(col_names, prefix_slash)]
  
  # Secondary: Repaired format with "_choice_" (for backward compatibility) – {main_var}_choice_{anything}
  if (length(choice_cols) == 0) {
    prefix_choice <- paste0(main_var, "_choice_")
    choice_cols <- col_names[startsWith(col_names, prefix_choice)]
  }
  
  # Fallback: columns that start with {main_var}_ AND are binary (0/1) numeric
  if (length(choice_cols) == 0) {
    prefix_plain <- paste0(main_var, "_")
    candidate_cols <- col_names[startsWith(col_names, prefix_plain)]
    choice_cols <- candidate_cols[vapply(candidate_cols, function(col) {
      x <- data_df[[col]]
      if (!is.numeric(x)) return(FALSE)
      observed <- unique(stats::na.omit(x))
      length(observed) > 0 && all(observed %in% c(0, 1))
    }, logical(1))]
  }
  
  choice_cols
}

make_specify_other_checklist_for_dataset <- function(mapping_df, data_df, dataset_name) {
  if (!is.data.frame(mapping_df) || nrow(mapping_df) == 0 || !is.data.frame(data_df) || nrow(data_df) == 0) {
    return(tibble::tibble())
  }
  
  dataset_name <- standardise_dataset_name(dataset_name)
  mapping_df <- mapping_df |>
    dplyr::filter(dataset == dataset_name, specify_other_variable %in% names(data_df))
  
  if (nrow(mapping_df) == 0) {
    return(tibble::tibble())
  }
  
  purrr::map_dfr(seq_len(nrow(mapping_df)), function(i) {
    row_i <- mapping_df[i, , drop = FALSE]
    specify_var  <- as.character(row_i$specify_other_variable[[1]])
    main_var     <- as.character(row_i$main_variable[[1]])
    main_type    <- as.character(row_i$main_question_type[[1]])
    other_choice <- as.character(row_i$other_choice[[1]])
    
    main_var_found <- !is.na(main_var) && main_var %in% names(data_df)
    
    # "other" dummy column only (used for change_response tagging)
    other_dummy_vars <- character()
    # ALL dummy columns for context (all choices)
    all_dummy_vars <- character()
    
    if (identical(main_type, "select_multiple") && main_var_found) {
      # "Other" dummy: the specific dummy for the "other" choice.
      # Filter strictly to binary numeric/logical columns so that a text variable
      # that shares the {main_var}_other naming pattern (e.g. prot_child_sep_reason_other)
      # is never mistaken for a 0/1 dummy column.
      other_dummy_vars <- make_dummy_candidates(main_var, other_choice)
      other_dummy_vars <- other_dummy_vars[other_dummy_vars %in% names(data_df)]
      other_dummy_vars <- other_dummy_vars[vapply(other_dummy_vars, function(col) {
        x <- data_df[[col]]
        is.numeric(x) || is.logical(x)
      }, logical(1))]
      
      # ALL dummies: every binary dummy column belonging to this SM variable
      all_dummy_vars <- get_all_sm_dummy_cols_from_data(main_var, data_df)
      
      # Ensure the other dummy is always included in all_dummy_vars
      all_dummy_vars <- unique(c(other_dummy_vars, all_dummy_vars))
    }
    
    # columns_to_clean covers: main variable, all dummies, and the specify-other text
    columns_to_clean <- unique(c(
      if (main_var_found) main_var else character(),
      all_dummy_vars,
      specify_var
    ))
    
    if (length(columns_to_clean) == 0) {
      return(tibble::tibble())
    }
    
    specify_bt <- paste0("`", stringr::str_replace_all(specify_var, "`", ""), "`")
    check_expr <- paste0(
      "!is.na(", specify_bt, ") & ",
      "trimws(as.character(", specify_bt, ")) != '' & ",
      "!(tolower(trimws(as.character(", specify_bt, "))) %in% c('na','nan','null','n/a'))"
    )
    
    tibble::tibble(
      dataset                = dataset_name,
      description            = "Specify-other response is filled. Review and recode the specify-other response and linked main response/dummy where needed.",
      check_id               = paste0("SPECIFY_OTHER_", stringr::str_to_upper(dataset_name), "_", specify_var),
      check_to_perform       = check_expr,
      columns_to_clean       = paste(columns_to_clean, collapse = ", "),
      specify_other_variable = specify_var,
      main_variable          = ifelse(is.na(main_var), NA_character_, main_var),
      main_question_type     = main_type,
      other_choice           = other_choice,
      # Store the "other" dummy separately from the full list of dummies
      dummy_variables        = paste(other_dummy_vars, collapse = ", "),
      all_sm_dummy_variables = paste(all_dummy_vars, collapse = ", "),
      main_variable_found    = main_var_found,
      dummy_variable_found   = length(other_dummy_vars) > 0
    )
  }) |>
    dplyr::distinct(check_id, columns_to_clean, .keep_all = TRUE)
}

specify_other_checklist_by_dataset <- purrr::imap(
  specify_other_data_list,
  ~ make_specify_other_checklist_for_dataset(specify_other_mapping, .x, .y)
)

specify_other_checklist <- dplyr::bind_rows(specify_other_checklist_by_dataset)

# --------------------------------------------------------------------------------------------------
# 5. Run specify-other checks across the main sheet and all repeat sheets
# --------------------------------------------------------------------------------------------------

split_vars <- function(x) {
  if (is.null(x) || length(x) == 0 || is.na(x) || trimws(as.character(x)) == "") return(character())
  out <- unlist(strsplit(as.character(x), ","))
  out <- trimws(out)
  out[out != ""]
}

make_actual_value <- function(df, var_name) {
  if (!is.data.frame(df) || nrow(df) == 0 || is.na(var_name) || !var_name %in% names(df)) {
    return(rep(NA_character_, if (is.data.frame(df)) nrow(df) else 0))
  }
  x <- df[[var_name]]
  if (inherits(x, "POSIXct") || inherits(x, "POSIXt") || inherits(x, "Date")) {
    return(as.character(x))
  }
  if (is.list(x)) {
    return(vapply(x, function(z) paste(as.character(z), collapse = " "), character(1)))
  }
  as.character(x)
}

# For select_multiple questions: reconstruct the combined space-separated choice string
# from binary dummy columns, falling back to the main variable column when it is non-empty.
# This ensures the reviewer always sees the full current selection in old_value even when
# the main combined column is absent or NA for some rows.
# Script 1 now preserves original Kobo names with "/" (e.g., variable/choice), so strip both "/" and "_" prefixes.
make_sm_combined_values <- function(df, main_var, all_dummy_cols) {
  all_dummy_cols <- all_dummy_cols[all_dummy_cols %in% names(df)]
  
  # Pre-compute bare choice labels by stripping the {main_var}/ (original) or {main_var}_choice_ / {main_var}_ (repaired) prefix
  main_esc <- stringr::str_replace_all(
    main_var %||% "", "([.^$|?*+()\\[\\]{}\\\\])", "\\\\\\1"
  )
  labels <- all_dummy_cols |>
    stringr::str_remove(paste0("^", main_esc, "/")) |>
    stringr::str_remove(paste0("^", main_esc, "_choice_")) |>
    stringr::str_remove(paste0("^", main_esc, "_"))
  
  # Build logical matrix for a subset of rows (vectorised over columns)
  # then paste active labels per row with apply() — much faster than nested vapply.
  reconstruct_rows <- function(sub_df) {
    if (ncol(sub_df) == 0L || nrow(sub_df) == 0L) return(rep(NA_character_, nrow(sub_df)))
    # Column-wise vectorised: each column is converted to logical in one call
    active_mat <- vapply(sub_df, function(col_vals) {
      tolower(trimws(as.character(col_vals))) %in% c("1", "true", "yes")
    }, logical(nrow(sub_df)))
    if (!is.matrix(active_mat)) active_mat <- matrix(active_mat, nrow = nrow(sub_df))
    apply(active_mat, 1L, function(row) {
      chosen <- labels[row]
      if (length(chosen) == 0L) NA_character_ else paste(chosen, collapse = " ")
    })
  }
  
  if (!is.na(main_var) && main_var %in% names(df)) {
    vals <- as.character(df[[main_var]])
    has_val <- !is.na(vals) & trimws(vals) != "" &
      !(tolower(trimws(vals)) %in% c("na", "nan", "null", "n/a"))
    needs_idx <- which(!has_val)
    if (length(needs_idx) > 0L && length(all_dummy_cols) > 0L) {
      vals[needs_idx] <- reconstruct_rows(df[needs_idx, all_dummy_cols, drop = FALSE])
    }
    return(vals)
  }
  
  # Main combined column entirely absent — reconstruct all rows from dummies
  reconstruct_rows(df[, all_dummy_cols, drop = FALSE])
}

run_checklist_manually <- function(data_df, checklist_df, dataset_name) {
  if (!is.data.frame(data_df) || nrow(data_df) == 0 || !is.data.frame(checklist_df) || nrow(checklist_df) == 0) {
    return(empty_specify_other_log())
  }
  
  data_df <- tibble::as_tibble(data_df)
  dataset_name <- standardise_dataset_name(dataset_name)
  
  uuid_col <- first_existing_col(data_df, c("uuid", "_uuid", "_submission__uuid", "submission_uuid", "_id", "instance_name"))
  if (is.na(uuid_col)) {
    data_df$uuid <- paste0(dataset_name, "_row_", seq_len(nrow(data_df)))
    uuid_col <- "uuid"
  }
  
  # Inline helper: build one tibble row per flagged respondent per variable
  make_log_rows <- function(flagged, question_i, chk) {
    tibble::tibble(
      source_dataset         = source_dataset_label(dataset_name),
      dataset                = dataset_name,
      uuid                   = as.character(flagged[[uuid_col]]),
      enum_id                = if ("enum_id" %in% names(flagged)) as.character(flagged$enum_id) else if ("username" %in% names(flagged)) as.character(flagged$username) else NA_character_,
      point_number           = if ("point_number" %in% names(flagged)) as.character(flagged$point_number) else NA_character_,
      question               = question_i,
      issue                  = as.character(chk$description[[1]]),
      old_value              = make_actual_value(flagged, question_i),
      new_value              = NA_character_,
      notes                  = NA_character_,
      change_type            = NA_character_,
      admin1                 = if ("admin1" %in% names(flagged)) as.character(flagged$admin1) else NA_character_,
      admin2                 = if ("admin2" %in% names(flagged)) as.character(flagged$admin2) else NA_character_,
      admin3                 = if ("admin3" %in% names(flagged)) as.character(flagged$admin3) else NA_character_,
      admin3_setlement       = if ("admin3_setlement" %in% names(flagged)) as.character(flagged$admin3_setlement) else NA_character_,
      admin4                 = if ("admin4" %in% names(flagged)) as.character(flagged$admin4) else NA_character_,
      hoh_type               = if ("hoh_type" %in% names(flagged)) as.character(flagged$hoh_type) else NA_character_,
      parent_uuid            = if ("parent_uuid" %in% names(flagged)) as.character(flagged$parent_uuid) else NA_character_,
      repeat_index           = if ("repeat_index" %in% names(flagged)) as.character(flagged$repeat_index) else NA_character_,
      person_id              = if ("person_id" %in% names(flagged)) as.character(flagged$person_id) else NA_character_,
      check_id               = as.character(chk$check_id[[1]]),
      check_to_perform       = as.character(chk$check_to_perform[[1]]),
      columns_to_clean       = question_i,
      specify_other_variable = as.character(chk$specify_other_variable[[1]]),
      main_variable          = as.character(chk$main_variable[[1]]),
      main_question_type     = as.character(chk$main_question_type[[1]]),
      other_choice           = as.character(chk$other_choice[[1]]),
      row_role               = NA_character_,
      color                  = NA_character_
    )
  }
  
  purrr::map_dfr(seq_len(nrow(checklist_df)), function(i) {
    chk <- checklist_df[i, , drop = FALSE]
    specify_var  <- as.character(chk$specify_other_variable[[1]])
    main_var     <- as.character(chk$main_variable[[1]])
    main_type    <- as.character(chk$main_question_type[[1]])
    other_choice <- if ("other_choice" %in% names(chk)) as.character(chk$other_choice[[1]]) else "other"
    
    if (!specify_var %in% names(data_df)) {
      return(empty_specify_other_log())
    }
    
    flagged <- data_df[is_filled_value(data_df[[specify_var]]), , drop = FALSE]
    if (nrow(flagged) == 0) {
      return(empty_specify_other_log())
    }
    
    # ---- 1. Specify-other text variable row (always present) ----
    rows_specify <- make_log_rows(flagged, specify_var, chk)
    
    # Vector of specify-other text for each flagged row — used to enrich issue text.
    specify_texts_per_row <- make_actual_value(flagged, specify_var)
    
    # ---- 2. Main question row (context) ----
    # For select_multiple, resolve the full dummy-column list first so we can
    # (a) reconstruct the combined value when the main column is NA/empty and
    # (b) reuse the same list for the dummy rows below.
    all_dummy_cols_main <- character()
    if (identical(main_type, "select_multiple") && !is.na(main_var) && main_var != "") {
      all_dummy_cols_str <- if ("all_sm_dummy_variables" %in% names(chk)) {
        as.character(chk$all_sm_dummy_variables[[1]])
      } else {
        NA_character_
      }
      all_dummy_cols_main <- if (!is.na(all_dummy_cols_str) && trimws(all_dummy_cols_str) != "") {
        split_vars(all_dummy_cols_str)
      } else {
        get_all_sm_dummy_cols_from_data(main_var, flagged)
      }
      all_dummy_cols_main <- all_dummy_cols_main[all_dummy_cols_main %in% names(flagged)]
      # Exclude the specify-other text variable itself and any non-binary columns.
      # A text variable can share the {main_var}_other naming pattern and slip through
      # make_dummy_candidates(); filtering here prevents pivot_longer type conflicts.
      all_dummy_cols_main <- all_dummy_cols_main[all_dummy_cols_main != specify_var]
      all_dummy_cols_main <- all_dummy_cols_main[vapply(all_dummy_cols_main, function(col) {
        x <- flagged[[col]]
        is.numeric(x) || is.logical(x)
      }, logical(1))]
    }
    
    rows_main <- if (!is.na(main_var) && main_var != "" &&
                     (main_var %in% names(flagged) || length(all_dummy_cols_main) > 0)) {
      tmp_main <- if (main_var %in% names(flagged)) {
        make_log_rows(flagged, main_var, chk)
      } else {
        # Main combined column absent — scaffold rows from the specify row structure
        # and set question to main_var so the reviewer sees the right question name.
        tmp <- make_log_rows(flagged, specify_var, chk)
        tmp$question <- main_var
        tmp$old_value <- NA_character_
        tmp
      }
      
      if (identical(main_type, "select_multiple") && nrow(tmp_main) > 0L && length(all_dummy_cols_main) > 0L) {
        # Replace old_value with the reconstructed combined selection string
        # (falls back to dummy columns when the main column is NA/empty for a row).
        reconstructed <- make_sm_combined_values(flagged, main_var, all_dummy_cols_main)
        tmp_main$old_value <- reconstructed
        # Pre-fill new_value with the same combined string so the reviewer can
        # directly edit it to add or remove the recoded choice.
        tmp_main$new_value <- reconstructed
      }
      
      # Enrich the issue text per row so the reviewer sees what was actually typed
      # in the specify-other field while looking at the main question row.
      tmp_main$issue <- dplyr::case_when(
        identical(main_type, "select_multiple") ~
          paste0(
            'Linked specify-other response: "', specify_texts_per_row, '". ',
            "Review the current selection shown in old_value. ",
            "If the typed text matches an existing choice, update new_value to include that choice ",
            "and remove 'other', then set change_type to 'change_response'. ",
            "Otherwise leave as no_action."
          ),
        identical(main_type, "select_one") ~
          paste0(
            'Linked specify-other response: "', specify_texts_per_row, '". ',
            "Review the current response shown in old_value. ",
            "If the typed text matches an existing choice, update new_value with that choice ",
            "and set change_type to 'change_response'. Otherwise leave as no_action."
          ),
        TRUE ~
          paste0(
            'Linked specify-other response: "', specify_texts_per_row, '". ',
            "Review and update new_value if the text can be recoded to a standard choice."
          )
      )
      tmp_main
    } else {
      empty_specify_other_log()
    }
    
    # ---- 3. ALL select_multiple dummy columns ----
    # Use pivot_longer to reshape all dummy columns at once, then join context.
    # The previous approach called make_log_rows() once per dummy column (e.g. 18
    # separate tibble() constructions per check item), which scaled poorly.
    rows_all_dummies <- empty_specify_other_log()
    
    if (identical(main_type, "select_multiple") && length(all_dummy_cols_main) > 0L) {
      # Context columns extracted from flagged once, then joined to all pivoted rows
      ctx_cols_wanted <- c(
        "enum_id", "point_number", "admin1", "admin2", "admin3",
        "admin3_setlement", "admin4", "hoh_type", "parent_uuid", "repeat_index", "person_id"
      )
      flagged_ctx <- flagged |>
        dplyr::mutate(.uuid_join = as.character(.data[[uuid_col]])) |>
        dplyr::select(.uuid_join, dplyr::any_of(ctx_cols_wanted))
      
      # Fallback: use username as enum_id when enum_id column is absent
      if (!"enum_id" %in% names(flagged_ctx) && "username" %in% names(flagged)) {
        flagged_ctx$enum_id <- as.character(flagged$username)
      }
      
      rows_all_dummies <- flagged |>
        dplyr::mutate(.uuid_join = as.character(.data[[uuid_col]])) |>
        dplyr::select(.uuid_join, dplyr::all_of(all_dummy_cols_main)) |>
        tidyr::pivot_longer(
          cols      = dplyr::all_of(all_dummy_cols_main),
          names_to  = "question",
          values_to = "old_value"
        ) |>
        dplyr::mutate(old_value = as.character(old_value)) |>
        dplyr::left_join(flagged_ctx, by = ".uuid_join") |>
        dplyr::rename(uuid = .uuid_join) |>
        dplyr::mutate(
          source_dataset         = source_dataset_label(dataset_name),
          dataset                = dataset_name,
          new_value              = NA_character_,
          notes                  = NA_character_,
          change_type            = NA_character_,
          issue                  = as.character(chk$description[[1]]),
          check_id               = as.character(chk$check_id[[1]]),
          check_to_perform       = as.character(chk$check_to_perform[[1]]),
          columns_to_clean       = question,
          specify_other_variable = as.character(chk$specify_other_variable[[1]]),
          main_variable          = as.character(chk$main_variable[[1]]),
          main_question_type     = as.character(chk$main_question_type[[1]]),
          other_choice           = if ("other_choice" %in% names(chk)) as.character(chk$other_choice[[1]]) else "other",
          row_role               = NA_character_,
          color                  = NA_character_,
          question_label         = NA_character_
        )
      
      # Ensure schema alignment with the rest of the log
      for (cc in setdiff(specify_other_export_cols, names(rows_all_dummies))) {
        rows_all_dummies[[cc]] <- NA_character_
      }
      rows_all_dummies <- dplyr::select(rows_all_dummies, dplyr::all_of(specify_other_export_cols))
    }
    
    dplyr::bind_rows(rows_specify, rows_main, rows_all_dummies)
  })
}

standardise_cleaningtools_log <- function(log_df, dataset_name, checklist_df, data_df) {
  if (!is.data.frame(log_df) || nrow(log_df) == 0) {
    return(empty_specify_other_log())
  }
  
  log_df <- tibble::as_tibble(log_df) |>
    janitor::clean_names()
  
  if (!"dataset" %in% names(log_df)) log_df$dataset <- dataset_name
  if (!"source_dataset" %in% names(log_df)) log_df$source_dataset <- source_dataset_label(dataset_name)
  if (!"question" %in% names(log_df)) {
    q_hit <- first_existing_col(log_df, c("variable", "name", "question_name", "columns_to_clean"))
    log_df$question <- if (!is.na(q_hit)) as.character(log_df[[q_hit]]) else NA_character_
  }
  if (!"old_value" %in% names(log_df)) {
    old_hit <- first_existing_col(log_df, c("value", "old", "response", "answer"))
    log_df$old_value <- if (!is.na(old_hit)) as.character(log_df[[old_hit]]) else NA_character_
  }
  if (!"uuid" %in% names(log_df)) {
    uuid_hit <- first_existing_col(log_df, c("_uuid", "id", "submission_uuid", "instance_name"))
    log_df$uuid <- if (!is.na(uuid_hit)) as.character(log_df[[uuid_hit]]) else NA_character_
  }
  if (!"check_id" %in% names(log_df)) log_df$check_id <- NA_character_
  
  log_df <- log_df |>
    dplyr::left_join(
      checklist_df |>
        dplyr::select(
          check_id,
          specify_other_variable,
          main_variable,
          main_question_type,
          other_choice,
          dummy_variables,
          dplyr::any_of("all_sm_dummy_variables"),
          check_to_perform
        ) |>
        dplyr::distinct(),
      by = "check_id"
    )
  
  for (cc in setdiff(specify_other_export_cols, names(log_df))) {
    log_df[[cc]] <- NA_character_
  }
  
  log_df |>
    dplyr::select(dplyr::all_of(specify_other_export_cols), dplyr::everything())
}

run_checklist_with_cleaningtools <- function(data_df, checklist_df, dataset_name) {
  if (!isTRUE(use_cleaningtools_engine_if_available)) {
    return(run_checklist_manually(data_df, checklist_df, dataset_name))
  }
  if (!requireNamespace("cleaningtools", quietly = TRUE) || !exists("check_logical_with_list", mode = "function") || !exists("create_combined_log", mode = "function")) {
    return(run_checklist_manually(data_df, checklist_df, dataset_name))
  }
  
  uuid_col <- first_existing_col(data_df, c("uuid", "_uuid", "_submission__uuid", "submission_uuid", "_id", "instance_name"))
  if (is.na(uuid_col)) {
    return(run_checklist_manually(data_df, checklist_df, dataset_name))
  }
  
  cl_obj <- tryCatch(
    {
      check_logical_with_list(
        dataset = data_df,
        uuid_column = uuid_col,
        information_to_add = NULL,
        list_of_check = checklist_df,
        check_id_column = "check_id",
        check_to_perform_column = "check_to_perform",
        columns_to_clean_column = "columns_to_clean",
        description_column = "description"
      )
    },
    error = function(e) e
  )
  
  if (inherits(cl_obj, "error")) {
    warning("cleaningtools::check_logical_with_list failed for dataset '", dataset_name, "'. Manual specify-other engine used. Error: ", conditionMessage(cl_obj), call. = FALSE)
    return(run_checklist_manually(data_df, checklist_df, dataset_name))
  }
  
  combined <- tryCatch(
    create_combined_log(cl_obj),
    error = function(e) e
  )
  
  if (inherits(combined, "error") || !is.list(combined) || !"cleaning_log" %in% names(combined)) {
    return(run_checklist_manually(data_df, checklist_df, dataset_name))
  }
  
  standardise_cleaningtools_log(combined$cleaning_log, dataset_name, checklist_df, data_df)
}

specify_other_log_raw <- purrr::imap_dfr(
  specify_other_data_list,
  function(data_df, dataset_name) {
    checklist_df <- specify_other_checklist_by_dataset[[dataset_name]] %||% tibble::tibble()
    run_checklist_with_cleaningtools(data_df, checklist_df, dataset_name)
  }
)

# --------------------------------------------------------------------------------------------------
# 6. Add change_type, new_value and row roles
# --------------------------------------------------------------------------------------------------

# classify_row_role_vec: fully vectorised version that operates on character
# vectors of the same length.  This replaces the old scalar function +
# purrr::pmap_chr(), eliminating one R-level function call per log row.
classify_row_role_vec <- function(question, main_variable, specify_other_variable,
                                  main_question_type, other_choice) {
  question               <- as.character(question)
  main_variable          <- as.character(main_variable)
  specify_other_variable <- as.character(specify_other_variable)
  main_question_type     <- as.character(main_question_type)
  other_choice           <- as.character(other_choice)
  
  other_choice_clean <- dplyr::if_else(
    !is.na(other_choice) & nchar(trimws(other_choice)) > 0L,
    janitor::make_clean_names(other_choice),
    "other"
  )
  
  # A dummy column belongs to the select_multiple question if it starts with
  # {main_var}_choice_ or {main_var}_ (excluding the main variable itself).
  # startsWith / endsWith are vectorised over both arguments in base R.
  is_sm_dummy <- (
    !is.na(question) & !is.na(main_variable) & nchar(trimws(main_variable)) > 0L &
      question != main_variable &
      (startsWith(question, paste0(main_variable, "_choice_")) |
         startsWith(question, paste0(main_variable, "_")))
  )
  is_sm_dummy[is.na(is_sm_dummy)] <- FALSE
  
  is_other_dummy <- is_sm_dummy & (
    endsWith(question, paste0("_choice_", other_choice_clean)) |
      endsWith(question, paste0("_",        other_choice_clean)) |
      endsWith(question, "_choice_other") |
      endsWith(question, "_other")
  )
  is_other_dummy[is.na(is_other_dummy)] <- FALSE
  
  dplyr::case_when(
    !is.na(question) & !is.na(specify_other_variable) & question == specify_other_variable
    ~ "specify_other",
    !is.na(question) & !is.na(main_variable) & question == main_variable
    ~ "main_variable_context",
    main_question_type == "select_multiple" & is_other_dummy
    ~ "select_multiple_dummy",
    main_question_type == "select_multiple" & is_sm_dummy
    ~ "select_multiple_choice_context",
    TRUE ~ "linked_variable"
  )
}

specify_other_log <- specify_other_log_raw |>
  dplyr::mutate(
    dplyr::across(dplyr::any_of(specify_other_export_cols), safe_as_character),
    # classify_row_role_vec operates on whole columns at once — no pmap overhead
    row_role = classify_row_role_vec(
      question, main_variable, specify_other_variable, main_question_type, other_choice
    ),
    change_type = dplyr::case_when(
      row_role %in% c("main_variable_context", "select_multiple_choice_context") ~ "no_action",
      row_role %in% c("specify_other", "select_multiple_dummy", "linked_variable") ~ "change_response",
      TRUE ~ "change_response"
    ),
    new_value = dplyr::case_when(
      row_role == "select_multiple_dummy"    ~ suggest_unselect_value(old_value),
      # Pre-fill the main variable row with its current value so a reviewer can
      # easily overwrite it with the correct standard choice when re-categorising
      # the specify-other text.  change_type stays "no_action" until the reviewer
      # decides a recoding is warranted, at which point they update new_value and
      # flip change_type to "change_response".
      row_role == "main_variable_context"    ~ old_value,
      TRUE ~ NA_character_
    ),
    issue = dplyr::case_when(
      # Preserve per-row issue text already set in run_checklist_manually
      # (identified by the "Linked specify-other response:" prefix).
      stringr::str_detect(issue, "^Linked specify-other response:") ~ issue,
      # Generic fallbacks used when the cleaningtools engine was used instead,
      # or when the issue column was not enriched per-row for any other reason.
      row_role == "main_variable_context"
      ~ "Specify-other response is filled. new_value is pre-filled with the current selection. If the specify-other text can be re-categorised to an existing choice, update new_value and set change_type to 'change_response'. Otherwise leave as no_action.",
      row_role == "select_multiple_choice_context"
      ~ "Specify-other response is filled. This select_multiple choice column is shown as context so you can see the full selection for this respondent. No action required unless the choice itself is wrong.",
      row_role == "select_multiple_dummy"
      ~ "Specify-other response is filled. Review this 'other' dummy column; set to 0 (change_response) if the response is recoded to a standard choice.",
      row_role == "specify_other"
      ~ "Specify-other response is filled. Review and recode to an existing standard choice if possible; if a match exists, blank this field and update the main question.",
      TRUE ~ "Specify-other response is filled. Review linked variable."
    ),
    notes = NA_character_,
    color = paste(dataset, uuid, repeat_index, specify_other_variable, sep = " - ")
  ) |>
  dplyr::select(dplyr::all_of(specify_other_export_cols)) |>
  dplyr::distinct(
    dataset,
    uuid,
    parent_uuid,
    repeat_index,
    question,
    old_value,
    check_id,
    row_role,
    .keep_all = TRUE
  ) |>
  # Sort so grouped rows appear together: context rows first within each color group,
  # then the specify-other text row last so reviewers read context before deciding.
  dplyr::arrange(
    dataset,
    color,
    dplyr::case_when(
      row_role == "main_variable_context"         ~ 1L,
      row_role == "select_multiple_choice_context" ~ 2L,
      row_role == "select_multiple_dummy"          ~ 3L,
      row_role == "specify_other"                  ~ 4L,
      TRUE                                         ~ 5L
    ),
    question
  )

# Build a question-name → label lookup from the survey sheet so every row in
# the cleaning log carries a human-readable label for the variable being flagged.
question_label_lkp <- survey_with_dataset |>
  dplyr::filter(!is.na(name), trimws(name) != "", !is.na(label), trimws(label) != "") |>
  dplyr::distinct(name, .keep_all = TRUE) |>
  dplyr::select(name, question_label = label)

specify_other_log <- specify_other_log |>
  dplyr::left_join(question_label_lkp, by = c("question" = "name")) |>
  # question_label may already exist as an empty column from empty_specify_other_log();
  # coalesce keeps any pre-filled value and fills NAs from the join.
  dplyr::mutate(
    question_label = dplyr::coalesce(question_label.y, question_label.x)
  ) |>
  dplyr::select(-dplyr::any_of(c("question_label.x", "question_label.y")))

# Carry the actual open-text response to every row in the same group so reviewers
# can read what was typed without having to find the specify_other row.
specify_other_text_lkp <- specify_other_log |>
  dplyr::filter(row_role == "specify_other") |>
  dplyr::select(color, specify_other_text = old_value) |>
  dplyr::distinct(color, .keep_all = TRUE)

specify_other_log <- specify_other_log |>
  dplyr::left_join(specify_other_text_lkp, by = "color") |>
  # Now that specify_other_text is available on every row, enrich the issue
  # for the specify_other row so the typed text is immediately visible there too,
  # and confirm the main_variable_context issue already set per-row in
  # run_checklist_manually (it is only overwritten here when it still contains
  # the generic fallback text, e.g. when the cleaningtools engine was used).
  dplyr::mutate(
    issue = dplyr::case_when(
      # specify_other text row: show what was typed + remind reviewer to recode
      row_role == "specify_other" ~
        paste0(
          'Specify-other response filled: "', old_value, '". ',
          "Recode to an existing standard choice if possible. ",
          "If a match exists, blank this field (blank_response) and update the main question."
        ),
      # select_multiple_dummy (the "other" dummy column): suggest unselecting
      row_role == "select_multiple_dummy" ~
        paste0(
          'The "other" dummy column is selected. ',
          'Specify-other response: "', specify_other_text, '". ',
          "If recoding to a standard choice, set this dummy to 0 (change_response) ",
          "and enable the matching standard-choice dummy instead."
        ),
      # For context rows that were not enriched per-row (e.g. cleaningtools path),
      # provide a clear fallback that still includes the specify-other text.
      row_role == "main_variable_context" &
        !is.na(specify_other_text) & trimws(specify_other_text) != "" &
        !stringr::str_detect(issue, "Linked specify-other") ~
        paste0(
          'Linked specify-other response: "', specify_other_text, '". ',
          "Review the current response shown in old_value. ",
          "If the typed text matches an existing choice, update new_value and ",
          "set change_type to 'change_response'. Otherwise leave as no_action."
        ),
      row_role == "select_multiple_choice_context" ~
        paste0(
          "Context column — current selection for this choice shown in old_value. ",
          'Specify-other response: "', specify_other_text, '". ',
          "No action required unless this choice itself is wrong."
        ),
      TRUE ~ issue
    )
  ) |>
  # Re-assign color using source_dataset + uuid as the grouping key so that
  # every row belonging to the same submission (same sheet + same uuid) shares
  # exactly one color in the Excel band, regardless of point_number format or
  # repeat-index differences.
  dplyr::mutate(
    color = paste(
      dplyr::coalesce(as.character(source_dataset), as.character(dataset), "unknown"),
      dplyr::coalesce(as.character(uuid), "unknown_uuid"),
      sep = " | "
    )
  )

# Drop entire groups where the specify-other text is purely numeric (e.g. "999",
# "_999", "5000").  These are almost always data-entry codes rather than genuine
# open-text responses and do not need manual review.
specify_other_log <- specify_other_log |>
  dplyr::filter(
    is.na(specify_other_text) | !is_numeric_text(specify_other_text)
  )

cat("Rows removed because specify_other_text is purely numeric: ",
    nrow(specify_other_log_raw) - nrow(specify_other_log), "\n", sep = "")

# Re-sort by the new color key (source_dataset | uuid) so that rows sharing the
# same color are always contiguous in the sheet.  Without this sort the banding
# code sees spurious group-changes and mis-assigns colors.
specify_other_log <- specify_other_log |>
  dplyr::arrange(
    source_dataset,
    uuid,
    dplyr::case_when(
      row_role == "main_variable_context"          ~ 1L,
      row_role == "select_multiple_choice_context" ~ 2L,
      row_role == "select_multiple_dummy"          ~ 3L,
      row_role == "specify_other"                  ~ 4L,
      TRUE                                         ~ 5L
    ),
    question
  )

# --------------------------------------------------------------------------------------------------
# 7. Remove data points already flagged before for this specific specify-other reason
# --------------------------------------------------------------------------------------------------

standardise_old_specify_other_log <- function(x, source_file = NA_character_) {
  if (!is.data.frame(x) || nrow(x) == 0) {
    return(empty_specify_other_log())
  }
  
  x <- tibble::as_tibble(x) |>
    janitor::clean_names()
  
  if (!"dataset" %in% names(x)) x$dataset <- "Submissions"
  if (!"uuid" %in% names(x)) {
    uuid_hit <- first_existing_col(x, c("_uuid", "id", "submission_uuid", "instance_name"))
    x$uuid <- if (!is.na(uuid_hit)) as.character(x[[uuid_hit]]) else NA_character_
  }
  if (!"parent_uuid" %in% names(x)) x$parent_uuid <- NA_character_
  if (!"repeat_index" %in% names(x)) x$repeat_index <- NA_character_
  if (!"question" %in% names(x)) {
    q_hit <- first_existing_col(x, c("variable", "name", "question_name", "columns_to_clean"))
    x$question <- if (!is.na(q_hit)) as.character(x[[q_hit]]) else NA_character_
  }
  if (!"check_id" %in% names(x)) x$check_id <- NA_character_
  if (!"issue" %in% names(x)) x$issue <- NA_character_
  if (!"change_type" %in% names(x)) x$change_type <- NA_character_
  if (!"specify_other_variable" %in% names(x)) x$specify_other_variable <- NA_character_
  if (!"row_role" %in% names(x)) x$row_role <- NA_character_
  
  for (cc in setdiff(specify_other_export_cols, names(x))) {
    x[[cc]] <- NA_character_
  }
  
  x |>
    dplyr::mutate(
      dplyr::across(dplyr::any_of(specify_other_export_cols), safe_as_character),
      source_old_cleaning_log = source_file
    )
}

old_specify_other_paths <- character()
old_specify_other_flags <- empty_specify_other_log()
previous_specify_other_flags_removed <- empty_specify_other_log()

if (isTRUE(remove_previous_specify_other_flags)) {
  # Look for "Specify other cleaning logs" folders inside daily data collection folders
  # in the outputs/ directory
  daily_folders <- list.dirs(
    "outputs/",
    full.names = TRUE,
    recursive = FALSE
  )
  
  # For each daily folder, look for the "Specify other cleaning logs" subdirectory
  specify_other_log_dirs <- character()
  for (daily_dir in daily_folders) {
    so_log_dir <- file.path(daily_dir, "Specify other cleaning logs")
    if (dir.exists(so_log_dir)) {
      specify_other_log_dirs <- c(specify_other_log_dirs, so_log_dir)
    }
  }
  
  # Find all Excel files in these directories
  if (length(specify_other_log_dirs) > 0) {
    old_specify_other_paths <- unlist(purrr::map(specify_other_log_dirs, function(dir_i) {
      list.files(
        dir_i,
        pattern = "\\.xlsx$",
        full.names = TRUE,
        ignore.case = TRUE
      )
    }))
    
    # Remove temporary/lock files
    old_specify_other_paths <- old_specify_other_paths[
      !grepl("~\\$", basename(old_specify_other_paths))
    ]
  }
  
  # Exclude files from today's folder (if it exists)
  if (exists("path_cleaning_folder_today") && length(old_specify_other_paths) > 0) {
    old_specify_other_paths <- old_specify_other_paths[
      !grepl(normalizePath(path_cleaning_folder_today, winslash = "/", mustWork = FALSE), normalizePath(old_specify_other_paths, winslash = "/", mustWork = FALSE), fixed = TRUE)
    ]
  }
  
  if (length(old_specify_other_paths) > 0) {
    old_specify_other_flags <- purrr::map_dfr(old_specify_other_paths, function(path_i) {
      sheets_i <- tryCatch(readxl::excel_sheets(path_i), error = function(e) character())
      # Primarily look for "cleaning_log" sheet
      sheet_to_read <- if ("cleaning_log" %in% sheets_i) {
        "cleaning_log"
      } else if ("cleaning_log_no_deletion" %in% sheets_i) {
        "cleaning_log_no_deletion"
      } else if ("cleaning_log_all" %in% sheets_i) {
        "cleaning_log_all"
      } else {
        NA_character_
      }
      if (is.na(sheet_to_read)) {
        return(tibble::tibble())
      }
      tryCatch(
        readxl::read_xlsx(path_i, sheet = sheet_to_read) |>
          standardise_old_specify_other_log(source_file = basename(path_i)),
        error = function(e) tibble::tibble()
      )
    })
  }
  
  if (is.data.frame(old_specify_other_flags) && nrow(old_specify_other_flags) > 0 && nrow(specify_other_log) > 0) {
    old_specify_other_flags <- old_specify_other_flags |>
      dplyr::filter(
        stringr::str_detect(as.character(check_id), "^SPECIFY_OTHER") |
          stringr::str_detect(stringr::str_to_lower(as.character(issue)), "specify.other|other response|specify-other") |
          stringr::str_detect(stringr::str_to_lower(as.character(question)), "other|specify")
      ) |>
      dplyr::mutate(
        dataset = standardise_dataset_name(dataset),
        uuid = as.character(uuid),
        parent_uuid = as.character(parent_uuid),
        repeat_index = as.character(repeat_index),
        question = as.character(question),
        check_id = as.character(check_id)
      )
    
    old_keys_with_check_id <- old_specify_other_flags |>
      dplyr::filter(!is.na(check_id), trimws(check_id) != "") |>
      dplyr::select(dataset, uuid, parent_uuid, repeat_index, question, check_id) |>
      dplyr::distinct()
    
    old_keys_without_check_id <- old_specify_other_flags |>
      dplyr::select(dataset, uuid, parent_uuid, repeat_index, question) |>
      dplyr::distinct()
    
    # NEW: Extract point_number from old flags to exclude entire households that were already flagged
    old_point_numbers <- old_specify_other_flags |>
      dplyr::filter(!is.na(point_number), trimws(as.character(point_number)) != "") |>
      dplyr::pull(point_number) |>
      unique()
    
    before_filter <- specify_other_log
    
    # NEW: First filter by point_number - if a household was flagged before, exclude all its flags
    if (length(old_point_numbers) > 0) {
      specify_other_log <- specify_other_log |>
        dplyr::filter(!(point_number %in% old_point_numbers))
    }
    
    if (nrow(old_keys_with_check_id) > 0) {
      specify_other_log <- specify_other_log |>
        dplyr::anti_join(old_keys_with_check_id, by = c("dataset", "uuid", "parent_uuid", "repeat_index", "question", "check_id"))
    }
    
    if (nrow(old_keys_without_check_id) > 0) {
      specify_other_log <- specify_other_log |>
        dplyr::anti_join(old_keys_without_check_id, by = c("dataset", "uuid", "parent_uuid", "repeat_index", "question"))
    }
    
    previous_specify_other_flags_removed <- before_filter |>
      dplyr::anti_join(
        specify_other_log |>
          dplyr::select(dataset, uuid, parent_uuid, repeat_index, question, check_id, row_role),
        by = c("dataset", "uuid", "parent_uuid", "repeat_index", "question", "check_id", "row_role")
      )
  }
}

specify_other_filter_summary <- tibble::tibble(
  previous_cleaning_log_files_found = length(old_specify_other_paths),
  previous_specify_other_rows_read = if (is.data.frame(old_specify_other_flags)) nrow(old_specify_other_flags) else 0L,
  current_specify_other_rows_after_filter = nrow(specify_other_log),
  previously_flagged_rows_removed = if (is.data.frame(previous_specify_other_flags_removed)) nrow(previous_specify_other_flags_removed) else 0L
)

# --------------------------------------------------------------------------------------------------
# 8. Create combined-log object and export national/district logbooks
# --------------------------------------------------------------------------------------------------

# Determine the most recent daily output folder
if (!exists("path_cleaning_folder_today")) {
  # Find all daily folders in outputs/
  daily_folders <- list.dirs(
    "outputs/",
    full.names = TRUE,
    recursive = FALSE
  )
  
  if (length(daily_folders) > 0) {
    # Sort by modification time and get the most recent one
    folder_info <- file.info(daily_folders)
    most_recent_idx <- which.max(folder_info$mtime)
    path_cleaning_folder_today <- daily_folders[most_recent_idx]
  } else {
    # Fallback: create a new folder with today's date
    path_cleaning_folder_today <- paste0("outputs/MSNA cleaning ", format(Sys.Date(), "%Y-%b-%d"))
  }
}

specify_other_output_dir <- file.path(path_cleaning_folder_today, "Specify other cleaning logs")
if (!dir.exists(specify_other_output_dir)) {
  dir.create(specify_other_output_dir, recursive = TRUE, showWarnings = FALSE)
}

# Checked dataset is included for compatibility with cleaningtools export.
specify_other_checked_dataset <- specify_other_data_list$Submissions %||% tibble::tibble()

combined_log_specify_other <- list(
  checked_dataset = specify_other_checked_dataset,
  cleaning_log = specify_other_log
)

specify_other_checklist_summary <- specify_other_checklist |>
  dplyr::count(dataset, main_question_type, name = "n_specify_other_checks") |>
  dplyr::arrange(dataset, main_question_type)

specify_other_flag_summary <- specify_other_log |>
  dplyr::count(dataset, row_role, change_type, question, name = "n_rows") |>
  dplyr::arrange(dataset, row_role, dplyr::desc(n_rows))

# Row-count breakdown by role for the console summary
specify_other_role_summary <- specify_other_log |>
  dplyr::count(dataset, row_role, change_type, name = "n_rows") |>
  dplyr::arrange(dataset, row_role)

export_logbook_openxlsx <- function(log_obj, output_path, extra_sheets = list(),
                                    final_cols = specify_other_final_export_cols) {
  wb <- openxlsx::createWorkbook()
  
  cl <- log_obj$cleaning_log
  if (is.null(cl) || !is.data.frame(cl)) cl <- tibble::tibble()
  
  # Apply final column selection (drops internal-only cols, orders for reviewer)
  if (!is.null(final_cols) && length(final_cols) > 0) {
    cl <- cl |> dplyr::select(dplyr::any_of(final_cols))
  }
  
  n_rows <- nrow(cl)
  n_cols <- ncol(cl)
  
  openxlsx::addWorksheet(wb, "cleaning_log")
  openxlsx::writeData(wb, "cleaning_log", cl, withFilter = TRUE)
  openxlsx::freezePane(wb, "cleaning_log", firstRow = TRUE)
  
  # Header style — bold + centered; fill applied separately via tryCatch so a
  # formatting issue never breaks the export.
  tryCatch({
    header_style <- openxlsx::createStyle(
      fontColour = "#FFFFFF", halign = "center", textDecoration = "bold"
    )
    openxlsx::addStyle(
      wb, "cleaning_log", style = header_style,
      rows = 1L, cols = seq_len(n_cols), gridExpand = TRUE
    )
    # Header background – fgFill is the correct openxlsx argument for solid cell fills.
    # Wrapped separately so the font style is always applied even if this fails.
    openxlsx::addStyle(
      wb, "cleaning_log",
      style = openxlsx::createStyle(fgFill = "#4472C4"),
      rows = 1L, cols = seq_len(n_cols), gridExpand = TRUE, stack = TRUE
    )
  }, error = function(e) {
    # Fall back to bold only if fgColor is not supported by this openxlsx build.
    tryCatch(
      openxlsx::addStyle(
        wb, "cleaning_log",
        style = openxlsx::createStyle(halign = "center", textDecoration = "bold"),
        rows = 1L, cols = seq_len(n_cols), gridExpand = TRUE
      ),
      error = function(e2) invisible(NULL)
    )
  })
  
  # ---- Highlight rows with the same source_dataset + uuid ----
  # Every row belonging to the same source_dataset + uuid combination receives
  # exactly the same background colour. Different groups cycle through a soft
  # pastel palette. The grouping is calculated directly from source_dataset and
  # uuid, so it does not depend on row order or on the internal helper `color`.
  # Red is deliberately excluded because it is reserved for the specify-other
  # font highlight applied below.
  tryCatch({
    if (n_rows > 0L && all(c("source_dataset", "uuid") %in% names(cl))) {
      band_palette <- c(
        "#D6E4F0",   # soft blue
        "#D5F5E3",   # soft green
        "#FEF9E7",   # soft yellow
        "#E8DAEF",   # soft lavender
        "#FDEBD0",   # soft peach / orange
        "#D1ECF1",   # soft cyan
        "#E2F0D9",   # light green
        "#FFF2CC"    # light gold
      )
      
      # Clean the two grouping variables.
      source_val <- as.character(cl[["source_dataset"]])
      uuid_val   <- as.character(cl[["uuid"]])
      
      source_val[is.na(source_val) | trimws(source_val) == ""] <- "unknown_source"
      
      # Do not collapse all missing UUIDs into one artificial respondent group.
      # Each row with a missing UUID receives a unique temporary key.
      missing_uuid <- is.na(uuid_val) | trimws(uuid_val) == ""
      if (any(missing_uuid)) {
        uuid_val[missing_uuid] <- paste0(
          "__missing_uuid_row_",
          which(missing_uuid)
        )
      }
      
      # Build the exact source_dataset + uuid grouping key.
      grp_key <- paste(source_val, uuid_val, sep = " | ")
      
      # match() guarantees that the same key always receives the same group ID,
      # even if rows from that group are not adjacent in the Excel data.
      grp_ids <- match(grp_key, unique(grp_key))
      
      # Map each group to one palette colour; cycle if there are more groups
      # than available colours.
      row_colors <- band_palette[(grp_ids - 1L) %% length(band_palette) + 1L]
      
      # Apply formatting in batches: one addStyle() call per palette colour.
      for (pal_col in unique(row_colors)) {
        data_rows <- which(row_colors == pal_col) + 1L   # +1 for header row
        
        openxlsx::addStyle(
          wb, "cleaning_log",
          style      = openxlsx::createStyle(fgFill = pal_col),
          rows       = data_rows,
          cols       = seq_len(n_cols),
          gridExpand = TRUE,
          stack      = FALSE
        )
      }
    }
  }, error = function(e) {
    warning(
      "Row colour highlighting could not be applied: ",
      conditionMessage(e),
      call. = FALSE
    )
  })
  
  # ---- Red font for "If other, please specify:" rows ----
  # These are the specify_other text rows — highlighting them in red makes it
  # easy to scan for the actual open-text response amid the context rows.
  tryCatch({
    if (n_rows > 0L && "question_label" %in% names(cl)) {
      lbl <- tolower(trimws(as.character(cl[["question_label"]])))
      specify_rows <- which(
        grepl("if other.*specify|please specify|other.*specify", lbl, perl = TRUE)
      ) + 1L   # +1 for header row
      if (length(specify_rows) > 0L) {
        openxlsx::addStyle(
          wb, "cleaning_log",
          style      = openxlsx::createStyle(fontColour = "#FF0000"),
          rows       = specify_rows,
          cols       = seq_len(n_cols),
          gridExpand = TRUE,
          stack      = TRUE   # stack = TRUE preserves the background band fill
        )
      }
    }
  }, error = function(e) invisible(NULL))
  
  # Dropdown data validation on change_type column.
  # Choices: change_response | blank_response | remove_survey | no_action
  if (n_rows > 0L && "change_type" %in% names(cl)) {
    ct_col <- which(names(cl) == "change_type")
    openxlsx::dataValidation(
      wb, "cleaning_log",
      col   = ct_col,
      rows  = 2L:(n_rows + 1L),
      type  = "list",
      value = '"change_response,blank_response,remove_survey,no_action"'
    )
  }
  
  # checked_dataset sheet (context for follow-up)
  if (is.data.frame(log_obj$checked_dataset) && nrow(log_obj$checked_dataset) > 0L) {
    openxlsx::addWorksheet(wb, "checked_dataset")
    openxlsx::writeData(
      wb, "checked_dataset",
      move_point_number_first(log_obj$checked_dataset)
    )
  }
  
  # Extra metadata sheets (summaries, checklists, etc.)
  if (length(extra_sheets) > 0L) {
    for (sheet_nm in names(extra_sheets)) {
      sheet_clean <- substr(gsub("[\\[\\]\\*\\?/\\\\:]", "_", sheet_nm), 1L, 31L)
      if (sheet_clean %in% names(wb)) next
      openxlsx::addWorksheet(wb, sheet_clean)
      openxlsx::writeData(wb, sheet_clean, extra_sheets[[sheet_nm]])
    }
  }
  
  openxlsx::saveWorkbook(wb, output_path, overwrite = TRUE)
  invisible(output_path)
}

specify_other_export_path <- file.path(
  specify_other_output_dir,
  paste0("Cleaning_log_specify_others_", format(Sys.time(), "%Y-%m-%d_at_%Hh%M"), ".xlsx")
)

if (isTRUE(export_specify_other_logbook)) {
  # Always use the openxlsx engine so the dropdown validation and row banding
  # are always present regardless of whether cleaningtools is installed.
  export_logbook_openxlsx(
    combined_log_specify_other,
    specify_other_export_path,
    extra_sheets = list(
      specify_other_role_summary   = specify_other_role_summary,
      specify_other_checklist      = specify_other_checklist,
      specify_other_mapping        = specify_other_mapping,
      specify_other_filter_summary = specify_other_filter_summary,
      specify_other_flag_summary   = specify_other_flag_summary
    )
  )
}

specify_other_district_export_paths <- tibble::tibble()

if (isTRUE(export_specify_other_district_logbooks) && is.data.frame(specify_other_log) && nrow(specify_other_log) > 0 && "admin2" %in% names(specify_other_log)) {
  district_values <- specify_other_log |>
    dplyr::filter(!is.na(admin2), trimws(as.character(admin2)) != "") |>
    dplyr::distinct(admin2) |>
    dplyr::arrange(admin2) |>
    dplyr::pull(admin2) |>
    as.character()
  
  specify_other_district_dir <- file.path(specify_other_output_dir, "District specify other logs")
  if (!dir.exists(specify_other_district_dir)) {
    dir.create(specify_other_district_dir, recursive = TRUE, showWarnings = FALSE)
  }
  
  specify_other_district_export_paths <- purrr::map_dfr(district_values, function(current_district) {
    district_log <- specify_other_log |>
      dplyr::filter(as.character(admin2) == as.character(current_district))
    
    district_checked_dataset <- specify_other_checked_dataset
    if (is.data.frame(district_checked_dataset) && nrow(district_checked_dataset) > 0 && "admin2" %in% names(district_checked_dataset)) {
      district_checked_dataset <- district_checked_dataset |>
        dplyr::filter(as.character(admin2) == as.character(current_district)) |>
        move_point_number_first()
    }
    
    district_obj <- list(
      checked_dataset = district_checked_dataset,
      cleaning_log = district_log
    )
    
    out_path <- file.path(
      specify_other_district_dir,
      paste0("Cleaning_log_specify_others_", make_safe_file_name(current_district), "_", format(Sys.time(), "%Y-%m-%d_at_%Hh%M"), ".xlsx")
    )
    
    export_logbook_openxlsx(
      district_obj,
      out_path,
      extra_sheets = list(
        specify_other_filter_summary = specify_other_filter_summary,
        specify_other_flag_summary   = district_log |>
          dplyr::count(dataset, change_type, question, name = "n_rows")
      )
    )
    
    tibble::tibble(
      admin2 = current_district,
      output_path = out_path,
      n_flags = nrow(district_log)
    )
  })
}

# --------------------------------------------------------------------------------------------------
# 9. Console summary and exported objects
# --------------------------------------------------------------------------------------------------

cat("\n")
cat("Specify-other variables detected in final Kobo tool: ", dplyr::n_distinct(specify_other_mapping$specify_other_variable), "\n", sep = "")
cat("Specify-other checks created: ", nrow(specify_other_checklist), "\n", sep = "")
cat("Specify-other cleaning-log rows after previous-flag filter: ", nrow(specify_other_log), "\n", sep = "")
cat("Previously flagged specify-other rows removed: ", specify_other_filter_summary$previously_flagged_rows_removed[[1]], "\n", sep = "")
cat("National specify-other logbook: ", specify_other_export_path, "\n", sep = "")

cat("\nRow role breakdown (change_type | n_rows):\n")
print(specify_other_role_summary)

if (nrow(specify_other_district_export_paths) > 0) {
  cat("District specify-other logbooks exported: ", nrow(specify_other_district_export_paths), "\n", sep = "")
}

print(specify_other_filter_summary)

cat("\n")
if (exists("green") && is.function(green)) {
  cat(green("Specify-other cleaning log processed"))
} else if (requireNamespace("crayon", quietly = TRUE)) {
  cat(crayon::green("Specify-other cleaning log processed"))
} else {
  cat("Specify-other cleaning log processed")
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

# Main objects created by this script:
#   specify_other_mapping
#   specify_other_checklist_by_dataset
#   specify_other_checklist
#   specify_other_log
#   combined_log_specify_other
#   specify_other_filter_summary
#   specify_other_flag_summary
#   specify_other_export_path
#   specify_other_district_export_paths
