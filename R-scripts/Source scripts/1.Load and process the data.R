cat("\014")

all_dfs_names_before <- Filter(function(x) is(x, "data.frame"), mget(ls())) |> names()
all_lists_names_before <- Filter(function(x) is(x, "list"), mget(ls())) |> names()

print_step("1. Loading Kobo export, XLSForm, and logical checks")

# ==================================================================================================
# REACH MSNA 2026 Mozambique - 1. Load and process the data
# Updated for the real Kobo server export structure.
#
# Real Kobo export sheets expected:
#   - REACH_MOZ_MSNA2026_Kobo_form  -> internal dataset: Submissions
#   - roster                           -> internal dataset: roster
#   - edu_ind                          -> internal dataset: edu_ind
#   - health_ind                       -> internal dataset: health_ind
#   - nut_ind / nutrition_ind          -> internal dataset: nutrition_ind
#   - prot_ind / child_protection_ind  -> internal dataset: child_protection_ind
#
# Important:
#   - The main household sheet is stored internally as raw_data_list$Submissions.
#   - The logical-check workbook sheet for the main household level remains household_level.
#   - Kobo split select_multiple columns such as variable/choice are repaired before cleaning names.
# ==================================================================================================

# --------------------------------------------------------------------------------------------------
# 0. Check input files
# --------------------------------------------------------------------------------------------------

if (!file.exists(path_data)) {
  stop("Kobo export not found at path_data: ", path_data, call. = FALSE)
}

if (!file.exists(path_tool)) {
  stop("Kobo XLSForm not found at path_tool: ", path_tool, call. = FALSE)
}

if (!file.exists(path_checks)) {
  stop("Logical checks workbook not found at path_checks: ", path_checks, call. = FALSE)
}

if (!exists("test_submission_user_pattern")) {
  test_submission_user_pattern <- NULL
}

# --------------------------------------------------------------------------------------------------
# 1. Local helper functions
# --------------------------------------------------------------------------------------------------

`%||%` <- function(x, y) {
  if (is.null(x)) y else x
}

read_xlsx_sheet_raw_names_safe <- function(path, sheet, guess_max = 1000000) {
  
  if (exists("read_xlsx_sheet_raw_names", mode = "function")) {
    return(
      tryCatch(
        read_xlsx_sheet_raw_names(
          path,
          sheet,
          guess_max = guess_max
        ),
        error = function(e) {
          read_xlsx_sheet_raw_names(path, sheet)
        }
      )
    )
  }
  
  readxl::read_xlsx(
    path,
    sheet = sheet,
    guess_max = guess_max,
    .name_repair = "minimal"
  )
}

safe_sheet_names_safe <- function(path) {
  
  if (exists("safe_sheet_names", mode = "function")) {
    return(safe_sheet_names(path))
  }
  
  readxl::excel_sheets(path)
}

repair_duplicate_column_names <- function(df) {
  
  if (!is.data.frame(df) || ncol(df) == 0) {
    return(df)
  }
  
  names(df) <- make.unique(names(df), sep = "_dup")
  
  df
}

repair_kobo_export_column_names <- function(df, dataset_name = NA_character_, sheet_name = NA_character_) {
  
  if (!is.data.frame(df) || ncol(df) == 0) {
    return(df)
  }
  
  original_names <- names(df)
  
  # Keep original Kobo variable names unchanged - do not replace "/" with "_choice_"
  repaired_names <- original_names |>
    as.character()
  
  names(df) <- repaired_names
  df <- repair_duplicate_column_names(df)
  
  repair_log <- tibble::tibble(
    dataset = dataset_name,
    sheet = sheet_name,
    original_name = original_names,
    repaired_name = names(df),
    changed = original_name != repaired_name
  )
  
  if (!exists("kobo_column_repair_log", inherits = TRUE)) {
    kobo_column_repair_log <<- repair_log
  } else {
    kobo_column_repair_log <<- dplyr::bind_rows(kobo_column_repair_log, repair_log)
  }
  
  df
}

standardise_column_names_safe <- function(df) {
  
  if (!is.data.frame(df) || ncol(df) == 0) {
    return(df)
  }
  
  # Keep original Kobo variable names unchanged
  df <- repair_duplicate_column_names(df)
  df
}

standardise_uuid_columns_safe <- function(df, dataset_name) {
  
  if (!is.data.frame(df) || nrow(df) == 0) {
    return(df)
  }
  
  df <- repair_duplicate_column_names(df)
  
  out <- tryCatch(
    {
      if (exists("standardise_uuid_columns", mode = "function")) {
        standardise_uuid_columns(df, dataset_name = dataset_name)
      } else {
        df
      }
    },
    error = function(e) {
      warning(
        "standardise_uuid_columns() failed for dataset '", dataset_name,
        "'. Local ID repair will be used. Error: ",
        conditionMessage(e),
        call. = FALSE
      )
      df
    }
  )
  
  repair_duplicate_column_names(out)
}

filter_test_submissions_safe <- function(df, test_submission_user_pattern = NULL) {
  
  if (!is.data.frame(df) || nrow(df) == 0) {
    return(df)
  }
  
  if (exists("filter_test_submissions", mode = "function")) {
    return(
      tryCatch(
        filter_test_submissions(df, test_submission_user_pattern),
        error = function(e) {
          warning(
            "filter_test_submissions() failed; returning unfiltered data. Error: ",
            conditionMessage(e),
            call. = FALSE
          )
          df
        }
      )
    )
  }
  
  if (is.null(test_submission_user_pattern) || is.na(test_submission_user_pattern) || test_submission_user_pattern == "") {
    return(df)
  }
  
  test_cols <- intersect(
    c("username", "_submitted_by", "submitted_by", "enum_id", "enumerator_id", "point_number"),
    names(df)
  )
  
  if (length(test_cols) == 0) {
    return(df)
  }
  
  df |>
    dplyr::filter(
      !dplyr::if_any(
        dplyr::all_of(test_cols),
        ~ stringr::str_detect(
          as.character(.x),
          stringr::regex(test_submission_user_pattern, ignore_case = TRUE)
        )
      )
    )
}

find_first_existing_col <- function(df, candidates) {
  
  if (!is.data.frame(df) || ncol(df) == 0) {
    return(NA_character_)
  }
  
  hit <- candidates[candidates %in% names(df)]
  
  if (length(hit) == 0) {
    NA_character_
  } else {
    hit[[1]]
  }
}

add_alias_if_missing <- function(df, new_col, candidate_cols) {
  
  if (!is.data.frame(df) || ncol(df) == 0) {
    return(df)
  }
  
  if (new_col %in% names(df)) {
    return(df)
  }
  
  hit <- find_first_existing_col(df, candidate_cols)
  
  if (!is.na(hit)) {
    df[[new_col]] <- df[[hit]]
  }
  
  df
}

extract_geopoint_parts <- function(df) {
  
  if (!is.data.frame(df) || nrow(df) == 0) {
    return(df)
  }
  
  geopoint_col <- find_first_existing_col(
    df,
    c("geopoint", "_geopoint", "location", "gps")
  )
  
  if (is.na(geopoint_col)) {
    return(df)
  }
  
  geopoint_chr <- as.character(df[[geopoint_col]])
  geopoint_split <- stringr::str_split_fixed(geopoint_chr, "\\s+", 4)
  
  suppressWarnings({
    
    if (!"latitude" %in% names(df)) {
      df$latitude <- as.numeric(geopoint_split[, 1])
    }
    
    if (!"longitude" %in% names(df)) {
      df$longitude <- as.numeric(geopoint_split[, 2])
    }
    
    if (!"altitude" %in% names(df)) {
      df$altitude <- as.numeric(geopoint_split[, 3])
    }
    
    if (!"accuracy" %in% names(df)) {
      df$accuracy <- as.numeric(geopoint_split[, 4])
    }
  })
  
  df
}

add_final_tool_aliases <- function(df, dataset_name = "Submissions") {
  
  if (!is.data.frame(df) || nrow(df) == 0) {
    return(df)
  }
  
  df <- repair_duplicate_column_names(df)
  
  df <- add_alias_if_missing(df, "district_name", c("admin2", "district", "district_name"))
  df <- add_alias_if_missing(df, "district", c("admin2", "district", "district_name"))
  
  df <- add_alias_if_missing(df, "sub_county_division", c("admin3", "sub_county_division", "sub_county", "division"))
  df <- add_alias_if_missing(df, "refugee_settlement", c("admin3_setlement", "admin3_settlement", "settlement", "refugee_settlement"))
  df <- add_alias_if_missing(df, "settlement", c("admin3_setlement", "admin3_settlement", "refugee_settlement", "settlement"))
  df <- add_alias_if_missing(df, "refugee_settlement_zone", c("admin4", "zone", "settlement_zone", "refugee_settlement_zone"))
  df <- add_alias_if_missing(df, "zone", c("admin4", "refugee_settlement_zone", "settlement_zone", "zone"))
  
  df <- add_alias_if_missing(df, "enumerator_id", c("enum_id", "enumerator_id", "username", "_submitted_by", "submitted_by"))
  df <- add_alias_if_missing(df, "enum_id", c("enum_id", "enumerator_id"))
  
  df <- add_alias_if_missing(df, "submission_time", c("_submission_time", "submission_time", "end", "end_time"))
  
  df <- add_alias_if_missing(df, "start_time", c("start", "start_time", "begin", "today"))
  df <- add_alias_if_missing(df, "end_time", c("end", "end_time", "submission_time", "_submission_time"))
  df <- add_alias_if_missing(df, "start_date", c("today", "start_date", "submission_date"))
  
  if ("start_date" %in% names(df)) {
    df$start_date <- suppressWarnings(lubridate::as_date(df$start_date))
  } else if ("start" %in% names(df)) {
    df$start_date <- suppressWarnings(lubridate::as_date(df$start))
  } else if ("_submission_time" %in% names(df)) {
    df$start_date <- suppressWarnings(lubridate::as_date(df$`_submission_time`))
  } else if ("submission_time" %in% names(df)) {
    df$start_date <- suppressWarnings(lubridate::as_date(df$submission_time))
  }
  
  df <- extract_geopoint_parts(df)
  
  df
}

add_msna_derived_variables_safe <- function(df, dataset_name) {
  
  if (!is.data.frame(df) || nrow(df) == 0) {
    return(df)
  }
  
  df <- add_final_tool_aliases(df, dataset_name)
  df <- repair_duplicate_column_names(df)
  
  out <- tryCatch(
    {
      if (exists("add_msna_derived_variables", mode = "function")) {
        add_msna_derived_variables(df, dataset_name = dataset_name)
      } else {
        df
      }
    },
    error = function(e) {
      warning(
        "add_msna_derived_variables() failed for dataset '", dataset_name,
        "'. Continuing with final-tool aliases only. Error: ",
        conditionMessage(e),
        call. = FALSE
      )
      df
    }
  )
  
  out <- repair_duplicate_column_names(out)
  out <- add_final_tool_aliases(out, dataset_name)
  out <- repair_duplicate_column_names(out)
  
  out
}

ensure_msna_export_ids <- function(df, dataset_name = "Submissions") {
  
  if (!is.data.frame(df) || nrow(df) == 0) {
    return(df)
  }
  
  df <- repair_duplicate_column_names(df)
  
  uuid_col <- find_first_existing_col(
    df,
    c(
      "uuid",
      "_uuid",
      "id",
      "_id",
      "instance_name",
      "instanceid",
      "submission_id",
      "_submission_id",
      "submission_uuid",
      "_submission_uuid"
    )
  )
  
  parent_uuid_col <- find_first_existing_col(
    df,
    c(
      "parent_uuid",
      "_parent_uuid",
      "submission_uuid",
      "_submission_uuid",
      "submission__uuid",
      "_submission__uuid",
      "parent_instance_name",
      "parent_instance_id",
      "parent_id",
      "_parent_id"
    )
  )
  
  submission_id_col <- find_first_existing_col(
    df,
    c(
      "submission_id",
      "_submission_id",
      "submission__id",
      "_submission__id",
      "id",
      "_id"
    )
  )
  
  repeat_index_col <- find_first_existing_col(
    df,
    c(
      "repeat_index",
      "_index",
      "index",
      "_parent_index",
      "parent_index",
      "repeat_position"
    )
  )
  
  if (dataset_name == "Submissions") {
    
    if (is.na(uuid_col)) {
      df$uuid <- paste0("Submissions_row_", seq_len(nrow(df)))
    } else {
      df$uuid <- as.character(df[[uuid_col]])
    }
    
    df$parent_uuid <- NA_character_
    df$repeat_index <- NA_character_
    df$submission_uuid <- as.character(df$uuid)
    
    if (!is.na(submission_id_col)) {
      df$submission_id <- as.character(df[[submission_id_col]])
    } else {
      df$submission_id <- as.character(df$uuid)
    }
    
  } else {
    
    if (!is.na(parent_uuid_col)) {
      df$parent_uuid <- as.character(df[[parent_uuid_col]])
    } else if ("parent_uuid" %in% names(df)) {
      df$parent_uuid <- as.character(df$parent_uuid)
    } else {
      df$parent_uuid <- NA_character_
    }
    
    if (!is.na(repeat_index_col)) {
      df$repeat_index <- as.character(df[[repeat_index_col]])
    } else if ("repeat_index" %in% names(df)) {
      df$repeat_index <- as.character(df$repeat_index)
    } else {
      df$repeat_index <- as.character(seq_len(nrow(df)))
    }
    
    if (!is.na(uuid_col) && uuid_col != "parent_uuid") {
      df$uuid <- as.character(df[[uuid_col]])
    } else {
      df$uuid <- paste0(
        dataset_name,
        "_",
        dplyr::if_else(
          is.na(df$parent_uuid) | df$parent_uuid == "",
          "missing_parent",
          df$parent_uuid
        ),
        "_",
        df$repeat_index
      )
    }
    
    df$submission_uuid <- as.character(df$parent_uuid)
    
    if (!is.na(submission_id_col)) {
      df$submission_id <- as.character(df[[submission_id_col]])
    } else {
      df$submission_id <- as.character(df$parent_uuid)
    }
  }
  
  df |> repair_duplicate_column_names()
}


# --------------------------------------------------------------------------------------------------
# Kobo server repeat linkage helper
# --------------------------------------------------------------------------------------------------
# Kobo server exports the household UUID in _uuid on the main sheet and in _submission__uuid on
# repeat sheets. For cleaning-log review, repeat-level flags should therefore use the parent
# submission UUID under uuid, while keeping a separate repeat_row_uuid where available.

repeat_dataset_names <- c("roster", "edu_ind", "health_ind", "nutrition_ind", "child_protection_ind")

main_submission_id_candidates <- c(
  "uuid",
  "_uuid",
  "submission_uuid",
  "_submission_uuid",
  "_id",
  "instance_name",
  "_submission__id",
  "_submission_id",
  "submission_id"
)

repeat_submission_uuid_candidates <- c(
  "_submission__uuid",
  "_submission_uuid",
  "submission_uuid",
  "parent_uuid",
  "_parent_uuid",
  "submission__uuid",
  "_submission__id",
  "_submission_id",
  "submission_id",
  "parent_instance_name"
)

build_main_submission_context_for_repeats <- function(main_df) {
  
  if (!is.data.frame(main_df) || nrow(main_df) == 0) {
    return(tibble::tibble())
  }
  
  main_id_cols <- intersect(main_submission_id_candidates, names(main_df))
  context_cols <- intersect(
    c(
      "point_number",
      "admin1",
      "admin2",
      "admin3",
      "admin3_setlement",
      "admin4",
      "hoh_type",
      "enum_id",
      "enum_gender",
      "username",
      "_submitted_by",
      "_submission_time",
      "start",
      "end",
      "today"
    ),
    names(main_df)
  )
  context_cols <- setdiff(context_cols, main_id_cols)
  
  if (length(main_id_cols) == 0) {
    return(tibble::tibble())
  }
  
  tmp <- main_df |>
    dplyr::select(dplyr::any_of(c(main_id_cols, context_cols))) |>
    dplyr::mutate(dplyr::across(dplyr::all_of(main_id_cols), as.character))
  
  purrr::map_dfr(main_id_cols, function(id_col) {
    tmp |>
      dplyr::filter(!is.na(.data[[id_col]]), trimws(.data[[id_col]]) != "") |>
      dplyr::transmute(
        .submission_uuid = as.character(.data[[id_col]]),
        dplyr::across(dplyr::all_of(context_cols))
      )
  }) |>
    dplyr::distinct(.submission_uuid, .keep_all = TRUE)
}

standardise_repeat_submission_uuid_and_point_number <- function(df, dataset_name, main_context = NULL) {
  
  if (!is.data.frame(df) || nrow(df) == 0) {
    return(df)
  }
  
  df <- tibble::as_tibble(df) |>
    repair_duplicate_column_names()
  
  if (dataset_name == "Submissions") {
    main_uuid_col <- find_first_existing_col(df, c("uuid", "_uuid", "instance_name", "_id", "_submission__id", "_submission_id"))
    if (!is.na(main_uuid_col)) {
      df$uuid <- as.character(df[[main_uuid_col]])
      df$submission_uuid <- as.character(df[[main_uuid_col]])
    }
    df$parent_uuid <- NA_character_
    return(df |> repair_duplicate_column_names())
  }
  
  # Preserve the repeat-row identifier separately before making uuid equal to the parent submission UUID.
  if (!"repeat_row_uuid" %in% names(df)) {
    repeat_row_id_col <- find_first_existing_col(
      df,
      c("uuid", "_id", "instance_name", "KEY", "_submission__id", "_submission_id")
    )
    if (!is.na(repeat_row_id_col)) {
      df$repeat_row_uuid <- as.character(df[[repeat_row_id_col]])
    } else {
      df$repeat_row_uuid <- paste0(dataset_name, "_row_", seq_len(nrow(df)))
    }
  }
  
  submission_uuid_col <- find_first_existing_col(df, repeat_submission_uuid_candidates)
  
  if (!is.na(submission_uuid_col)) {
    df$`_submission__uuid` <- as.character(df[[submission_uuid_col]])
    df$submission_uuid <- as.character(df[[submission_uuid_col]])
    df$parent_uuid <- as.character(df[[submission_uuid_col]])
    df$uuid <- as.character(df[[submission_uuid_col]])
  } else if ("parent_uuid" %in% names(df)) {
    df$`_submission__uuid` <- as.character(df$parent_uuid)
    df$submission_uuid <- as.character(df$parent_uuid)
    df$uuid <- as.character(df$parent_uuid)
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
  
  df |> repair_duplicate_column_names()
}

enrich_repeat_datasets_with_submission_uuid_and_point_number <- function(data_list) {
  
  if (!is.list(data_list) || !"Submissions" %in% names(data_list)) {
    return(data_list)
  }
  
  main_context <- build_main_submission_context_for_repeats(data_list$Submissions)
  
  purrr::imap(data_list, function(df, dataset_name) {
    standardise_repeat_submission_uuid_and_point_number(
      df = df,
      dataset_name = dataset_name,
      main_context = main_context
    )
  })
}

find_export_sheet <- function(candidates, available_sheets) {
  
  exact_hit <- candidates[candidates %in% available_sheets]
  
  if (length(exact_hit) > 0) {
    return(exact_hit[[1]])
  }
  
  lower_available <- tolower(available_sheets)
  lower_candidates <- tolower(candidates)
  
  lower_hit_index <- match(lower_candidates, lower_available)
  lower_hit_index <- lower_hit_index[!is.na(lower_hit_index)]
  
  if (length(lower_hit_index) > 0) {
    return(available_sheets[[lower_hit_index[[1]]]])
  }
  
  NA_character_
}

read_one_kobo_export_sheet <- function(path_data, sheet_name, dataset_name) {
  
  read_xlsx_sheet_raw_names_safe(
    path_data,
    sheet_name,
    guess_max = 1000000
  ) |>
    repair_kobo_export_column_names(
      dataset_name = dataset_name,
      sheet_name = sheet_name
    ) |>
    standardise_column_names_safe() |>
    repair_duplicate_column_names() |>
    standardise_uuid_columns_safe(dataset_name = dataset_name) |>
    repair_duplicate_column_names() |>
    filter_test_submissions_safe(test_submission_user_pattern = test_submission_user_pattern) |>
    repair_duplicate_column_names() |>
    ensure_msna_export_ids(dataset_name = dataset_name) |>
    add_final_tool_aliases(dataset_name = dataset_name) |>
    repair_duplicate_column_names()
}

# --------------------------------------------------------------------------------------------------
# 2. Load XLSForm metadata with original names
# --------------------------------------------------------------------------------------------------


df_survey <- read_xlsx_sheet_raw_names_safe(path_tool, "survey")
df_choices <- read_xlsx_sheet_raw_names_safe(path_tool, "choices")
df_settings <- read_xlsx_sheet_raw_names_safe(path_tool, "settings")

df_survey_clean <- df_survey |>
  janitor::clean_names() |>
  repair_duplicate_column_names()

df_choices_clean <- df_choices |>
  janitor::clean_names() |>
  repair_duplicate_column_names()

survey_repeat_summary <- df_survey_clean |>
  dplyr::filter(stringr::str_detect(type, "^begin[_ ]repeat$")) |>
  dplyr::select(dplyr::any_of(c(
    "type",
    "name",
    "label_english_en",
    "repeat_count",
    "relevant"
  )))

# --------------------------------------------------------------------------------------------------
# 3. Load Kobo export sheets
# --------------------------------------------------------------------------------------------------

export_sheets <- safe_sheet_names_safe(path_data)

expected_datasets <- c(
  "Submissions",
  "roster",
  "edu_ind",
  "health_ind",
  "nutrition_ind",
  "child_protection_ind"
)

main_sheet <- find_export_sheet(
  candidates = c(
    "REACH_MOZ_MSNA2026_Kobo_form",
    "Submissions",
    "submissions",
    "survey",
    "data"
  ),
  available_sheets = export_sheets
)

if (is.na(main_sheet)) {
  main_sheet <- export_sheets[[1]]
}

sheet_map <- c(
  Submissions = main_sheet,
  roster = find_export_sheet(
    c("roster", "hh_roster", "household_roster"),
    export_sheets
  ),
  edu_ind = find_export_sheet(
    c("edu_ind", "education_ind", "education"),
    export_sheets
  ),
  health_ind = find_export_sheet(
    c("health_ind", "health"),
    export_sheets
  ),
  nutrition_ind = find_export_sheet(
    c("nutrition_ind", "nut_ind", "nutrition", "nut"),
    export_sheets
  ),
  child_protection_ind = find_export_sheet(
    c("child_protection_ind", "prot_ind", "protection_ind", "cp_ind", "child_protection", "protection"),
    export_sheets
  )
)

sheet_map_diagnostics <- tibble::tibble(
  dataset = names(sheet_map),
  export_sheet = unname(sheet_map),
  sheet_found = !is.na(unname(sheet_map)) & unname(sheet_map) %in% export_sheets
)

raw_data_list <- list()

for (nm in names(sheet_map)) {
  
  sh <- sheet_map[[nm]]
  
  if (!is.na(sh) && sh %in% export_sheets) {
    
    raw_data_list[[nm]] <- read_one_kobo_export_sheet(
      path_data = path_data,
      sheet_name = sh,
      dataset_name = nm
    )
    
  } else {
    
    raw_data_list[[nm]] <- tibble::tibble()
  }
}

for (ds in expected_datasets) {
  if (!ds %in% names(raw_data_list)) {
    raw_data_list[[ds]] <- tibble::tibble()
  }
}

raw_data_list <- raw_data_list[expected_datasets]

# --------------------------------------------------------------------------------------------------
# 3.a Remove pilot submissions
# --------------------------------------------------------------------------------------------------
# Pilot submissions are defined as:
#   1. ALL surveys conducted on 2026-08-30; OR
#   2. Surveys conducted on 2026-09-09 for selected pilot point numbers.
#
# Pilot submissions are removed from the main Submissions dataset and from every
# repeat dataset using the submission UUID.

pilot_date_1 <- as.Date("2026-08-30")
pilot_date_2 <- as.Date("2026-09-09")

# Selected pilot point numbers for 2026-09-09
pilot_point_numbers <- unique(c(
  "Bid_507",
  "Bid_522",
  "Bid_001",
  "Bid_509",
  "Bid_506"
))

pilot_uuids <- character(0)

if (is.data.frame(raw_data_list$Submissions) &&
    nrow(raw_data_list$Submissions) > 0) {
  
  # Identify date column
  date_col <- find_first_existing_col(
    raw_data_list$Submissions,
    c(
      "today",
      "start_date",
      "start",
      "_submission_time",
      "submission_time"
    )
  )
  
  # Identify point-number column
  point_col <- find_first_existing_col(
    raw_data_list$Submissions,
    c(
      "point_number",
      "point_no",
      "point",
      "point_id",
      "bid",
      "Bid"
    )
  )
  
  if (!is.na(date_col)) {
    
    # --------------------------------------------------------------------------
    # Pilot 1:
    # Remove ALL submissions conducted on 2026-08-30
    # --------------------------------------------------------------------------
    
    pilot_uuids_date1 <- raw_data_list$Submissions |>
      dplyr::filter(
        suppressWarnings(
          lubridate::as_date(.data[[date_col]])
        ) == pilot_date_1
      ) |>
      dplyr::pull(uuid) |>
      as.character() |>
      unique()
    
    
    # --------------------------------------------------------------------------
    # Pilot 2:
    # Remove selected point numbers conducted on 2026-09-09
    # --------------------------------------------------------------------------
    
    if (!is.na(point_col)) {
      
      pilot_uuids_date2 <- raw_data_list$Submissions |>
        dplyr::filter(
          suppressWarnings(
            lubridate::as_date(.data[[date_col]])
          ) == pilot_date_2,
          
          as.character(.data[[point_col]]) %in% pilot_point_numbers
        ) |>
        dplyr::pull(uuid) |>
        as.character() |>
        unique()
      
    } else {
      
      pilot_uuids_date2 <- character(0)
      
      warning(
        "Point-number column could not be identified. ",
        "Pilot submissions from 2026-09-09 were therefore not removed."
      )
    }
    
    
    # --------------------------------------------------------------------------
    # Combine pilot UUIDs from both definitions
    # --------------------------------------------------------------------------
    
    pilot_uuids <- unique(c(
      pilot_uuids_date1,
      pilot_uuids_date2
    ))
  }
}


# Number of unique pilot submissions identified
n_pilot_main <- length(pilot_uuids)


# --------------------------------------------------------------------------------------------------
# Remove pilot submissions from main and repeat datasets
# --------------------------------------------------------------------------------------------------

if (n_pilot_main > 0) {
  
  # Remove pilot rows from the main sheet
  raw_data_list$Submissions <- raw_data_list$Submissions |>
    dplyr::filter(!as.character(uuid) %in% pilot_uuids)
  
  
  # Identify all repeat datasets
  repeat_dataset_nms <- setdiff(
    names(raw_data_list),
    "Submissions"
  )
  
  
  # Remove records belonging to pilot submissions from every repeat dataset
  for (rpt_nm in repeat_dataset_nms) {
    
    df_rpt <- raw_data_list[[rpt_nm]]
    
    if (!is.data.frame(df_rpt) || nrow(df_rpt) == 0) next
    
    
    # Find UUID linking repeat records to the parent submission
    link_col <- find_first_existing_col(
      df_rpt,
      c(
        "_submission_uuid",
        "submission_uuid",
        "parent_uuid",
        "_submission__uuid"
      )
    )
    
    
    if (!is.na(link_col)) {
      
      raw_data_list[[rpt_nm]] <- df_rpt |>
        dplyr::filter(
          !as.character(.data[[link_col]]) %in% pilot_uuids
        )
    }
  }
  
  
  cat(
    crayon::yellow(
      sprintf(
        paste0(
          "Pilot data removed: %d unique main submission(s) excluded ",
          "from all datasets.\n",
          "  - %d submission(s) from %s\n",
          "  - %d selected point submission(s) from %s\n"
        ),
        n_pilot_main,
        length(pilot_uuids_date1),
        format(pilot_date_1),
        length(pilot_uuids_date2),
        format(pilot_date_2)
      )
    )
  )
  
} else {
  
  cat(
    crayon::green(
      "No pilot submissions matching the specified criteria were found.\n"
    )
  )
}

# --------------------------------------------------------------------------------------------------
# 3.1 Standardise numeric fields required by HumInd/MSNA functions
# --------------------------------------------------------------------------------------------------
# Kobo's XLSX export can store genuinely numeric survey fields as text. This is especially common
# for:
#   1) XLSForm integer/decimal questions;
#   2) calculate fields copied from numeric roster values into repeat sheets; and
#   3) split select_multiple indicator columns, which Kobo exports as "0"/"1" text.
#
# The HumInd package validates numeric inputs strictly, so normalize only these known-safe classes.
# Unexpected non-numeric values in declared numeric fields are converted to NA and logged.

xlsform_numeric_vars <- df_survey_clean |>
  dplyr::filter(type %in% c("integer", "decimal")) |>
  dplyr::pull(name) |>
  as.character() |>
  unique()

# These are calculate fields in the final Mozambique XLSForm but are numerically defined.
# They are indexed-repeat copies of numeric roster fields and are exported by Kobo as text.
numeric_calculate_vars <- c(
  "edu_ind_age",
  "health_ind_age",
  "nut_ind_age",
  "prot_ind_age",
  "edu_ind_age_schooling"
)

numeric_conversion_issues <- tibble::tibble(
  dataset = character(),
  variable = character(),
  unexpected_value = character()
)

for (dataset_nm in names(raw_data_list)) {
  
  df_tmp <- raw_data_list[[dataset_nm]]
  
  if (!is.data.frame(df_tmp) || nrow(df_tmp) == 0) {
    next
  }
  
  # A. XLSForm integer/decimal fields + known numeric calculate fields
  vars_to_numeric <- intersect(
    unique(c(xlsform_numeric_vars, numeric_calculate_vars)),
    names(df_tmp)
  )
  
  for (var_nm in vars_to_numeric) {
    
    x_chr <- trimws(as.character(df_tmp[[var_nm]]))
    x_chr[x_chr == ""] <- NA_character_
    
    x_num <- suppressWarnings(as.numeric(x_chr))
    bad <- !is.na(x_chr) & is.na(x_num)
    
    if (any(bad)) {
      numeric_conversion_issues <- dplyr::bind_rows(
        numeric_conversion_issues,
        tibble::tibble(
          dataset = dataset_nm,
          variable = var_nm,
          unexpected_value = unique(x_chr[bad])
        )
      )
    }
    
    df_tmp[[var_nm]] <- x_num
  }
  
  # B. Kobo split select_multiple columns.
  # Kobo split columns use "/" separator. Convert a split column only when all
  # observed non-missing values are 0/1, so ordinary character variables are never coerced.
  split_choice_vars <- names(df_tmp)[
    stringr::str_detect(names(df_tmp), "/")
  ]
  
  for (var_nm in split_choice_vars) {
    
    x_chr <- trimws(as.character(df_tmp[[var_nm]]))
    x_chr[x_chr == ""] <- NA_character_
    
    observed <- unique(stats::na.omit(x_chr))
    
    if (length(observed) == 0 || all(observed %in% c("0", "1"))) {
      df_tmp[[var_nm]] <- suppressWarnings(as.numeric(x_chr))
    }
  }
  
  raw_data_list[[dataset_nm]] <- df_tmp
}

if (nrow(numeric_conversion_issues) > 0) {
  warning(
    "Some declared numeric XLSForm fields contained non-numeric values and were converted to NA. ",
    "Inspect numeric_conversion_issues for details.",
    call. = FALSE
  )
}

# --------------------------------------------------------------------------------------------------
# 4. Apply HumInd/MSNA derived-variable functions that are applicable to the final Mozambique XLSForm
# --------------------------------------------------------------------------------------------------
# NOTE:
# - Package functions are called directly; their source definitions are not repeated here.
# - Kobo select_multiple split columns use "/" separator (e.g., variable/choice); package functions 
#   that construct split-column names are called with sep = "/".
# - Repeat datasets link back to the household using parent_uuid; a temporary common hh_uuid key is used for package aggregations.
# - The final WASH composite and MSNI are intentionally deferred until the survey weight exists.

# Working objects
# --------------------------------------------------------------------------------------------------
# Filter for Kampala only
data_hh_main <- raw_data_list$Submissions |>
  filter(admin2 == "kampala")

# Filter repeat datasets for Kampala households
data_hh_roster <- raw_data_list$roster |>
  filter(parent_uuid %in% data_hh_main$'_uuid')

data_roster <- raw_data_list$roster |>
  filter(parent_uuid %in% data_hh_main$'_uuid')

data_edu_ind <- raw_data_list$edu_ind |>
  filter(parent_uuid %in% data_hh_main$'_uuid')

data_health_ind <- raw_data_list$health_ind |>
  filter(parent_uuid %in% data_hh_main$'_uuid')

data_nutrition_ind <- raw_data_list$nutrition_ind |>
  filter(parent_uuid %in% data_hh_main$'_uuid')

data_child_protection_ind <- raw_data_list$child_protection_ind |>
  filter(parent_uuid %in% data_hh_main$'_uuid')

# table(data_hh_main$admin2)


# Common household linkage key for package *_to_main() functions.
#
# Kobo repeat sheets contain the household UUID in parent_uuid, whereas the
# household sheet uses uuid. Several HumInd aggregation functions internally
# call impactR.utils::df_diff() and assume that the linking column has the same
# name in both data frames. We therefore create a temporary common key without
# overwriting the repeat-level uuid.
if ("uuid" %in% names(data_hh_main)) {
  data_hh_main <- data_hh_main |>
    dplyr::mutate(hh_uuid = as.character(uuid))
}

if ("parent_uuid" %in% names(data_roster)) {
  data_roster <- data_roster |>
    dplyr::mutate(hh_uuid = as.character(parent_uuid))
  data_hh_roster <- data_roster
}

if ("parent_uuid" %in% names(data_edu_ind)) {
  data_edu_ind <- data_edu_ind |>
    dplyr::mutate(hh_uuid = as.character(parent_uuid))
}

if ("parent_uuid" %in% names(data_health_ind)) {
  data_health_ind <- data_health_ind |>
    dplyr::mutate(hh_uuid = as.character(parent_uuid))
}

if ("parent_uuid" %in% names(data_nutrition_ind)) {
  data_nutrition_ind <- data_nutrition_ind |>
    dplyr::mutate(hh_uuid = as.character(parent_uuid))
}

if ("parent_uuid" %in% names(data_child_protection_ind)) {
  data_child_protection_ind <- data_child_protection_ind |>
    dplyr::mutate(hh_uuid = as.character(parent_uuid))
}

# --------------------------------------------------------------------------------------------------
# 4.1 Household demographics
# --------------------------------------------------------------------------------------------------
if (nrow(data_hh_main) > 0 && all(c("resp_hoh_yn", "resp_gender", "resp_age", "hoh_gender", "hoh_age") %in% names(data_hh_main))) {
  data_hh_main <- add_hoh_final(data_hh_main)
}

# --------------------------------------------------------------------------------------------------
# 4.2 Roster: Washington Group Short Set disability indicators
# --------------------------------------------------------------------------------------------------
wgq_required <- c(
  "ind_age", "wgq_vision", "wgq_hearing", "wgq_mobility",
  "wgq_cognition", "wgq_self_care", "wgq_communication", "hh_uuid"
)

if (nrow(data_roster) > 0 && all(wgq_required %in% names(data_roster))) {
  data_roster <- add_loop_wgq_ss(data_roster)
  data_hh_main <- add_loop_wgq_ss_to_main(
    main = data_hh_main,
    loop = data_roster,
    id_col_main = "hh_uuid",
    id_col_loop = "hh_uuid"
  )
  data_hh_roster <- data_roster
}

# --------------------------------------------------------------------------------------------------
# 4.3 Education repeat and household education composite
# --------------------------------------------------------------------------------------------------
# The final Mozambique XLSForm already calculates edu_ind_age_schooling from the roster.
# We therefore use that field directly rather than re-running age correction with a generic
# school-year start month.

edu_required <- c(
  "edu_ind_age_schooling", "edu_access", "edu_barrier",
  "edu_disrupted_hazards", "edu_disrupted_teacher", "edu_disrupted_displaced",
  "hh_uuid"
)

if (nrow(data_edu_ind) > 0 && all(edu_required %in% names(data_edu_ind))) {
  
  # Access dummies.
  # The package source provided for add_loop_edu_access_d() contains a negation typo in the
  # current implementation, so create the intended binary variables explicitly and use the
  # package aggregation function below.
  data_edu_ind <- data_edu_ind |>
    dplyr::mutate(
      edu_ind_access_d = dplyr::case_when(
        edu_ind_age_schooling == 0 ~ NA_real_,
        edu_ind_age_schooling == 1 & edu_access == "yes" ~ 1,
        edu_ind_age_schooling == 1 & edu_access == "no" ~ 0,
        edu_ind_age_schooling == 1 & edu_access %in% c("dnk", "pnta") ~ NA_real_,
        TRUE ~ NA_real_
      ),
      edu_ind_no_access_d = dplyr::case_when(
        edu_ind_age_schooling == 0 ~ NA_real_,
        edu_ind_age_schooling == 1 & edu_access == "yes" ~ 0,
        edu_ind_age_schooling == 1 & edu_access == "no" ~ 1,
        edu_ind_age_schooling == 1 & edu_access %in% c("dnk", "pnta") ~ NA_real_,
        TRUE ~ NA_real_
      )
    )
  
  # Protection-related education barriers.
  # edu_barrier is select_multiple in the Mozambique tool, while the package individual-level
  # recoder expects one value. Use the repaired split columns to derive the dummy safely.
  edu_protection_choices <- c(
    "protection_at_school",
    "protection_travel_school",
    "child_work_home",
    "child_work_outside",
    "child_armed_group",
    "child_marriage",
    "child_pregnancy",
    "ban",
    "enroll_lack_documentation",
    "discrimination"
  )
  edu_protection_cols <- paste0("edu_barrier_choice_", edu_protection_choices)
  edu_protection_cols <- intersect(edu_protection_cols, names(data_edu_ind))
  
  if (length(edu_protection_cols) > 0) {
    data_edu_ind <- data_edu_ind |>
      dplyr::mutate(
        edu_ind_barrier_protection_d = dplyr::case_when(
          edu_ind_age_schooling == 0 ~ NA_real_,
          dplyr::if_any(dplyr::all_of(edu_protection_cols), ~ .x == 1) ~ 1,
          TRUE ~ 0
        )
      )
  }
  
  # Disruption indicators. The Mozambique tool has hazards, teacher absence and displacement,
  # but no separate attack item, so attack is excluded at this stage.
  data_edu_ind <- add_loop_edu_disrupted_d(
    df = data_edu_ind,
    attack = NULL,
    hazards = "edu_disrupted_hazards",
    displaced = "edu_disrupted_displaced",
    teacher = "edu_disrupted_teacher",
    levels = c("yes", "no", "dnk", "pnta"),
    ind_schooling_age_d = "edu_ind_age_schooling"
  )
  
  # Structural zero placeholder for the absent attack dimension. This is used only so the
  # package aggregation/composite API can ignore that dimension without treating it as missing.
  data_edu_ind <- data_edu_ind |>
    dplyr::mutate(edu_disrupted_attack_d = 0)
  
  # Aggregate education indicators to the household level.
  data_hh_main <- add_loop_edu_ind_schooling_age_d_to_main(
    main = data_hh_main,
    loop = data_edu_ind,
    id_col_main = "hh_uuid",
    id_col_loop = "hh_uuid"
  )
  
  data_hh_main <- add_loop_edu_access_d_to_main(
    main = data_hh_main,
    loop = data_edu_ind,
    id_col_main = "hh_uuid",
    id_col_loop = "hh_uuid"
  )
  
  if ("edu_ind_barrier_protection_d" %in% names(data_edu_ind)) {
    data_hh_main <- add_loop_edu_barrier_protection_d_to_main(
      main = data_hh_main,
      loop = data_edu_ind,
      id_col_main = "hh_uuid",
      id_col_loop = "hh_uuid"
    )
  }
  
  data_hh_main <- add_loop_edu_disrupted_d_to_main(
    main = data_hh_main,
    loop = data_edu_ind,
    attack_d = "edu_disrupted_attack_d",
    hazards_d = "edu_disrupted_hazards_d",
    displaced_d = "edu_disrupted_displaced_d",
    teacher_d = "edu_disrupted_teacher_d",
    id_col_main = "hh_uuid",
    id_col_loop = "hh_uuid"
  )
  
  edu_comp_required <- c(
    "edu_schooling_age_n", "edu_no_access_n", "edu_barrier_protection_n",
    "edu_disrupted_attack_n", "edu_disrupted_hazards_n",
    "edu_disrupted_displaced_n", "edu_disrupted_teacher_n"
  )
  
  if (all(edu_comp_required %in% names(data_hh_main))) {
    data_hh_main <- add_comp_edu(data_hh_main)
  }
}

# --------------------------------------------------------------------------------------------------
# 4.4 Health repeat and household Health composite
# --------------------------------------------------------------------------------------------------
health_required <- c(
  "health_ind_healthcare_needed",
  "health_ind_healthcare_received",
  "hh_uuid"
)

if (nrow(data_health_ind) > 0 && all(health_required %in% names(data_health_ind))) {
  data_health_ind <- add_loop_healthcare_needed_cat(data_health_ind)
  
  data_hh_main <- add_loop_healthcare_needed_cat_to_main(
    main = data_hh_main,
    loop = data_health_ind,
    id_col_main = "hh_uuid",
    id_col_loop = "hh_uuid"
  )
  
  health_comp_required <- c(
    "health_ind_healthcare_needed_no_n",
    "health_ind_healthcare_needed_yes_unmet_n",
    "health_ind_healthcare_needed_yes_met_n"
  )
  
  if (all(health_comp_required %in% names(data_hh_main))) {
    data_hh_main <- add_comp_health(data_hh_main)
  }
}

# Remove the temporary same-name linkage key after all repeat-to-household
# aggregations have been completed. The original uuid/parent_uuid fields remain.
data_hh_main <- data_hh_main |>
  dplyr::select(-dplyr::any_of("hh_uuid"))

data_roster <- data_roster |>
  dplyr::select(-dplyr::any_of("hh_uuid"))
data_hh_roster <- data_roster

data_edu_ind <- data_edu_ind |>
  dplyr::select(-dplyr::any_of("hh_uuid"))

data_health_ind <- data_health_ind |>
  dplyr::select(-dplyr::any_of("hh_uuid"))

data_nutrition_ind <- data_nutrition_ind |>
  dplyr::select(-dplyr::any_of("hh_uuid"))

data_child_protection_ind <- data_child_protection_ind |>
  dplyr::select(-dplyr::any_of("hh_uuid"))

# --------------------------------------------------------------------------------------------------
# 4.5 WASH sub-indicators supported by the final tool
# --------------------------------------------------------------------------------------------------
# HWISE water quantity
hwise_required <- c(
  "wash_hwise_drink", "wash_hwise_hands", "wash_hwise_plans", "wash_hwise_worry"
)

if (all(hwise_required %in% names(data_hh_main))) {
  data_hh_main <- add_hwise(data_hh_main)
}

# Drinking-water source classification.
# The Mozambique tool includes a shaduf response not represented in the package's standard source
# list. It is conservatively treated as undefined here rather than assigning a JMP service level
# without a confirmed methodological mapping.
if ("wash_drinking_water_source" %in% names(data_hh_main)) {
  data_hh_main <- add_drinking_water_source_cat(
    data_hh_main,
    drinking_water_source_cat_undefined = c(
      "dnk", "pnta", "other",
      "shaduf_hand_operated_device_for_lifting_water_usin"
    )
  )
}

# Reconcile water collection time. The package's current add_drinking_water_time_cat() source
# collapses the three <30 minute choice codes through match.arg(), so the Mozambique categories are
# normalized explicitly before using the package threshold/JMP functions.
water_time_required <- c(
  "wash_drinking_water_source",
  "wash_drinking_water_time_yn",
  "wash_drinking_water_time_int",
  "wash_drinking_water_time_sl"
)

if (all(water_time_required %in% names(data_hh_main))) {
  data_hh_main <- data_hh_main |>
    dplyr::mutate(
      wash_drinking_water_time_cat = dplyr::case_when(
        wash_drinking_water_source == "piped_dwelling" ~ "premises",
        wash_drinking_water_time_yn %in% c("water_in_dwelling", "water_in_plot") ~ "premises",
        wash_drinking_water_time_yn == "number_minutes" &
          wash_drinking_water_time_int < 30 ~ "under_30_min",
        wash_drinking_water_time_yn == "number_minutes" &
          wash_drinking_water_time_int >= 30 & wash_drinking_water_time_int < 60 ~ "30min_1hr",
        wash_drinking_water_time_yn == "number_minutes" &
          wash_drinking_water_time_int >= 60 & wash_drinking_water_time_int <= 600 ~ "more_than_1hr",
        wash_drinking_water_time_yn == "dnk" &
          wash_drinking_water_time_sl %in% c("5min_or_less", "5min_15min", "15min_30min") ~ "under_30_min",
        wash_drinking_water_time_yn == "dnk" & wash_drinking_water_time_sl == "30min_1hr" ~ "30min_1hr",
        wash_drinking_water_time_yn == "dnk" & wash_drinking_water_time_sl == "more_than_1hr" ~ "more_than_1hr",
        wash_drinking_water_time_yn %in% c("pnta") ~ "undefined",
        wash_drinking_water_time_yn == "dnk" & wash_drinking_water_time_sl %in% c("dnk", "pnta") ~ "undefined",
        TRUE ~ NA_character_
      )
    )
  
  data_hh_main <- add_drinking_water_time_threshold_cat(data_hh_main)
  
  if (all(c("wash_drinking_water_source_cat", "wash_drinking_water_time_30min_cat") %in% names(data_hh_main))) {
    data_hh_main <- add_drinking_water_quality_jmp_cat(data_hh_main)
  }
}

# Sanitation facility and JMP service level
sanitation_required <- c(
  "wash_sanitation_facility",
  "wash_sanitation_facility_sharing_yn"
)

if (all(sanitation_required %in% names(data_hh_main))) {
  data_hh_main <- add_sanitation_facility_cat(data_hh_main)
  data_hh_main <- add_sharing_sanitation_facility_cat(data_hh_main)
  data_hh_main <- add_sanitation_facility_jmp_cat(data_hh_main)
}

# Handwashing JMP category.
# The Mozambique XLSForm has no modality question and is structured as an in-person observation tool.
# A constant modality variable is therefore added solely for compatibility with the package API;
# the function itself still switches to reported data when observation permission was not granted.
handwashing_required <- c(
  "wash_handwashing_facility",
  "wash_handwashing_facility_reported",
  "wash_handwashing_facility_water_reported_yn",
  "wash_soap_reported_yn",
  "wash_soap_reported_type",
  "wash_handwashing_facility_observed_water_yn",
  "wash_soap_observed_yn",
  "wash_soap_observed_type"
)

if (all(handwashing_required %in% names(data_hh_main))) {
  if (!"survey_modality" %in% names(data_hh_main)) {
    data_hh_main$survey_modality <- "in_person"
  }
  
  data_hh_main <- add_handwashing_facility_cat(
    data_hh_main,
    facility_observed_water = "wash_handwashing_facility_observed_water_yn"
  )
}

# IMPORTANT: Do not call add_sharing_sanitation_facility_n_ind() or add_comp_wash() here.
# The former requires the survey weight, which is not collected in the XLSForm and is expected
# to be added later during weighting. Re-run those WASH steps after the final weight is available.

# --------------------------------------------------------------------------------------------------
# 4.6 Food Security indicators, matrices, and sector composite
# --------------------------------------------------------------------------------------------------
fsl_core_required <- c(
  "fsl_fcs_cereal", "fsl_fcs_legumes", "fsl_fcs_dairy", "fsl_fcs_meat",
  "fsl_fcs_veg", "fsl_fcs_fruit", "fsl_fcs_oil", "fsl_fcs_sugar",
  "fsl_rcsi_lessquality", "fsl_rcsi_borrow", "fsl_rcsi_mealsize",
  "fsl_rcsi_mealadult", "fsl_rcsi_mealnb",
  "fsl_hhs_nofoodhh", "fsl_hhs_nofoodhh_freq",
  "fsl_hhs_sleephungry", "fsl_hhs_sleephungry_freq",
  "fsl_hhs_alldaynight", "fsl_hhs_alldaynight_freq",
  "fsl_lcsi_stress1", "fsl_lcsi_stress2", "fsl_lcsi_stress3", "fsl_lcsi_stress4",
  "fsl_lcsi_crisis1", "fsl_lcsi_crisis2", "fsl_lcsi_crisis3",
  "fsl_lcsi_emergency1", "fsl_lcsi_emergency2", "fsl_lcsi_emergency3"
)

if (all(fsl_core_required %in% names(data_hh_main))) {
  data_hh_main <- add_fcs(data_hh_main, cutoffs = "normal")
  data_hh_main <- add_rcsi(data_hh_main)
  data_hh_main <- add_hhs(data_hh_main)
  data_hh_main <- add_lcsi(data_hh_main)
  data_hh_main <- add_fcm_phase(data_hh_main)
  data_hh_main <- add_fclcm_phase(data_hh_main)
  data_hh_main <- add_comp_foodsec(data_hh_main)
}

# --------------------------------------------------------------------------------------------------
# 4.7 SNFI/HLP indicators and sector composite
# --------------------------------------------------------------------------------------------------
snfi_required <- c(
  "snfi_shelter_type", "snfi_shelter_type_individual", "snfi_shelter_damage",
  "snfi_shelter_issue", "snfi_fds_cooking", "snfi_fds_sleeping", "snfi_fds_storing",
  "energy_lighting_source", "hlp_occupancy", "hlp_risk_eviction"
)

if (all(snfi_required %in% names(data_hh_main))) {
  data_hh_main <- add_shelter_type_cat(
    data_hh_main,
    adequate = c(
      "house",
      "apartment",
      "solid_finished_house_without_corrugated_iron_roofs"
    ),
    inadequate = c(
      "makeshift",
      "unfinished_building",
      "tent",
      "semi_permanent_temporary_grass_roof_bricks",
      "poles_and_tarp_temporary",
      "no_shelter_sleeping_in_the_open"
    )
  )
  
  # Preserve the severity of the explicit individual "no shelter" response.
  data_hh_main <- data_hh_main |>
    dplyr::mutate(
      snfi_shelter_type_cat = dplyr::case_when(
        snfi_shelter_type == "none" ~ "none",
        snfi_shelter_type_individual == "no_shelter_sleeping_in_the_open" ~ "none",
        TRUE ~ snfi_shelter_type_cat
      )
    )
  
  data_hh_main <- add_shelter_damage_cat(
    data_hh_main,
    sep = "/"
  )
  
  data_hh_main <- add_shelter_issue_cat(
    data_hh_main,
    sep = "/"
  )
  
  data_hh_main <- add_fds_cannot_cat(
    data_hh_main,
    lighting_source_undefined = c("pnta", "dnk", "other")
  )
  
  data_hh_main <- add_occupancy_cat(data_hh_main)
  
  snfi_comp_required <- c(
    "snfi_shelter_type_cat", "snfi_shelter_issue_cat", "hlp_tenure_security",
    "snfi_fds_cannot_cat", "snfi_shelter_damage_cat"
  )
  
  if (all(snfi_comp_required %in% names(data_hh_main))) {
    data_hh_main <- add_comp_snfi(
      data_hh_main,
      tenure_security_cat = "hlp_tenure_security"
    )
  }
}

# --------------------------------------------------------------------------------------------------
# 4.8 Protection dimension scores and sector composite
# --------------------------------------------------------------------------------------------------
prot_required <- c(
  "prot_needs_1_services", "prot_needs_1_justice",
  "prot_needs_2_activities", "prot_needs_2_social", "prot_needs_3_movement"
)

if (all(prot_required %in% names(data_hh_main))) {
  data_hh_main <- add_prot_score_rights(
    data_hh_main,
    sep = "/"
  )
  
  data_hh_main <- add_prot_score_practices(
    data_hh_main,
    sep = "/"
  )
  
  data_hh_main <- add_prot_score_movement(
    data_hh_main,
    sep = "/"
  )
  
  data_hh_main <- add_comp_prot(data_hh_main)
}

# --------------------------------------------------------------------------------------------------
# 4.9 Cash/markets expenditure preparation and top-3 expenditure ranking
# --------------------------------------------------------------------------------------------------
# The Mozambique tool has an expanded set of expenditure categories compared with the package's
# proportion helpers. Zeroing and top-3 ranking accept custom vectors and are therefore used;
# the fixed-category proportion functions are intentionally not used here.

freq_expenditure_vars <- c(
  "cm_expenditure_frequent_food",
  "cm_expenditure_frequent_rent",
  "cm_expenditure_frequent_water",
  "cm_expenditure_frequent_nfi_feminine",
  "cm_expenditure_frequent_nfi_other_hygiene",
  "cm_expenditure_frequent_nfi_non_hygiene_non_food",
  "cm_expenditure_frequent_utilities",
  "cm_expenditure_frequent_transportation",
  "cm_expenditure_frequent_fuel",
  "cm_expenditure_frequent_communication",
  "cm_expenditure_frequent_substances_30d",
  "cm_expenditure_frequent_care_services_30d",
  "cm_expenditure_frequent_recreation_30d",
  "cm_expenditure_frequent_pet_care_30d",
  "cm_expenditure_frequent_other"
)

if (all(c("cm_expenditure_frequent", freq_expenditure_vars) %in% names(data_hh_main))) {
  data_hh_main <- add_expenditure_type_zero_freq(
    data_hh_main,
    expenditure_freq = "cm_expenditure_frequent",
    none = "no_expenditure",
    undefined = c("dnk", "pnta"),
    expenditure_freq_types = freq_expenditure_vars
  )
  
  data_hh_main <- add_expenditure_type_freq_rank(
    data_hh_main,
    expenditure_freq_types = freq_expenditure_vars,
    id_col = "uuid"
  )
}

infreq_expenditure_vars <- c(
  "cm_expenditure_infrequent_nfi",
  "cm_expenditure_infrequent_business_assets",
  "cm_expenditure_infrequent_debt",
  "cm_expenditure_infrequent_education",
  "cm_expenditure_infrequent_health",
  "cm_expenditure_infrequent_shelter",
  "cm_expenditure_infrequent_clothing",
  "cm_expenditure_documents_6m",
  "cm_expenditure_legal_protection_6m"
)

if (all(c("cm_expenditure_infrequent", infreq_expenditure_vars) %in% names(data_hh_main))) {
  data_hh_main <- add_expenditure_type_zero_infreq(
    data_hh_main,
    expenditure_infreq = "cm_expenditure_infrequent",
    none = "no_expenditure",
    undefined = c("dnk", "pnta"),
    expenditure_infreq_types = infreq_expenditure_vars
  )
  
  data_hh_main <- add_expenditure_type_infreq_rank(
    data_hh_main,
    expenditure_infreq_types = infreq_expenditure_vars,
    id_col = "uuid"
  )
}

# --------------------------------------------------------------------------------------------------
# 4.10 Functions intentionally deferred/not applicable in Script 1
# --------------------------------------------------------------------------------------------------
# Not called here:
# - add_received_assistance(): the final tool has aap_received_assistance_12m but no assistance-date field.
# - add_barriers_assistance_any(): no aap_barriers_assistance variable in the final tool.
# - income-source functions: only total income is collected; source-specific income amounts are absent.
# - expenditure proportion functions: the Mozambique expenditure modules have expanded/custom categories.
# - add_quantile_interval(): should be run after the final survey weight is available.
# - add_sharing_sanitation_facility_n_ind(): requires the survey weight.
# - add_comp_wash(): depends on the weighted sanitation-sharing indicator above.
# - add_msni(): defer until the final WASH composite (and all final sector scores) has been computed.

# --------------------------------------------------------------------------------------------------
# 4.11 Write processed objects back to raw_data_list and create compatibility aliases
# --------------------------------------------------------------------------------------------------
raw_data_list$Submissions <- data_hh_main
raw_data_list$roster <- data_roster
raw_data_list$edu_ind <- data_edu_ind
raw_data_list$health_ind <- data_health_ind
raw_data_list$nutrition_ind <- data_nutrition_ind
raw_data_list$child_protection_ind <- data_child_protection_ind

# Final Kobo-server linkage alignment for repeat sheets:
#   - repeat uuid = parent household _submission__uuid
#   - repeat parent_uuid/submission_uuid/_submission__uuid = the same parent household UUID
#   - repeat_row_uuid preserves the original repeat-row identifier where available
#   - point_number is copied from the main household sheet into each repeat sheet
raw_data_list <- enrich_repeat_datasets_with_submission_uuid_and_point_number(raw_data_list)

data_hh_main <- raw_data_list$Submissions
data_roster <- raw_data_list$roster
data_hh_roster <- raw_data_list$roster
data_edu_ind <- raw_data_list$edu_ind
data_health_ind <- raw_data_list$health_ind
data_nutrition_ind <- raw_data_list$nutrition_ind
data_child_protection_ind <- raw_data_list$child_protection_ind

data_nut_ind <- data_nutrition_ind
data_prot_ind <- data_child_protection_ind

data <- data_hh_main
raw_data <- data_hh_main

# --------------------------------------------------------------------------------------------------
# 5. Repeat linkage diagnostics
# --------------------------------------------------------------------------------------------------

repeat_linkage_diagnostics <- purrr::imap_dfr(
  raw_data_list,
  function(df, nm) {
    
    if (!is.data.frame(df) || nrow(df) == 0) {
      return(
        tibble::tibble(
          dataset = nm,
          records = 0,
          has_uuid = FALSE,
          has_parent_uuid = FALSE,
          has_repeat_index = FALSE,
          unique_uuid = NA_integer_,
          unique_parent_uuid = NA_integer_
        )
      )
    }
    
    tibble::tibble(
      dataset = nm,
      records = nrow(df),
      has_uuid = "uuid" %in% names(df),
      has_parent_uuid = "parent_uuid" %in% names(df),
      has_repeat_index = "repeat_index" %in% names(df),
      unique_uuid = if ("uuid" %in% names(df)) dplyr::n_distinct(df$uuid, na.rm = TRUE) else NA_integer_,
      unique_parent_uuid = if ("parent_uuid" %in% names(df)) dplyr::n_distinct(df$parent_uuid, na.rm = TRUE) else NA_integer_
    )
  }
)

# --------------------------------------------------------------------------------------------------
# 6. Load logical checks
# --------------------------------------------------------------------------------------------------

checks_sheets <- safe_sheet_names_safe(path_checks)

logical_check_sheet_map <- c(
  Submissions = "household_level",
  roster = "roster",
  edu_ind = "edu_ind",
  health_ind = "health_ind",
  nutrition_ind = "nutrition_ind",
  child_protection_ind = "child_protection_ind"
)

required_check_columns <- c(
  "dataset",
  "check_id",
  "description",
  "check_to_perform",
  "columns_to_clean"
)

read_logical_check_sheet <- function(sheet_name, dataset_name = NULL) {
  
  out <- read_xlsx_sheet_raw_names_safe(path_checks, sheet_name) |>
    janitor::clean_names() |>
    repair_duplicate_column_names()
  
  if ("variables_to_clean" %in% names(out) && !"columns_to_clean" %in% names(out)) {
    out <- out |>
      dplyr::rename(columns_to_clean = variables_to_clean)
  }
  
  if ("columns_to_clean" %in% names(out) && !"variables_to_clean" %in% names(out)) {
    out <- out |>
      dplyr::mutate(variables_to_clean = columns_to_clean)
  }
  
  if (!"columns_to_clean" %in% names(out)) {
    out$columns_to_clean <- NA_character_
  }
  
  if (!"variables_to_clean" %in% names(out)) {
    out$variables_to_clean <- out$columns_to_clean
  }
  
  if (!is.null(dataset_name)) {
    out$dataset <- dataset_name
    out <- out |>
      dplyr::relocate(dataset)
  }
  
  missing_cols <- setdiff(required_check_columns, names(out))
  
  if (length(missing_cols) > 0) {
    stop(
      "Logical check sheet '", sheet_name, "' is missing required column(s): ",
      paste(missing_cols, collapse = ", "),
      call. = FALSE
    )
  }
  
  out |>
    dplyr::mutate(
      dataset = as.character(dataset),
      check_id = as.character(check_id),
      description = as.character(description),
      check_to_perform = as.character(check_to_perform),
      columns_to_clean = as.character(columns_to_clean),
      variables_to_clean = as.character(variables_to_clean)
    ) |>
    dplyr::filter(
      !is.na(check_to_perform),
      trimws(check_to_perform) != ""
    )
}

logical_checks <- list()

logical_check_sheet_diagnostics <- tibble::tibble(
  dataset = names(logical_check_sheet_map),
  logical_check_sheet = unname(logical_check_sheet_map),
  sheet_found = unname(logical_check_sheet_map) %in% checks_sheets
)

for (dataset_nm in names(logical_check_sheet_map)) {
  
  sh <- logical_check_sheet_map[[dataset_nm]]
  
  if (sh %in% checks_sheets) {
    
    tmp <- read_logical_check_sheet(
      sheet_name = sh,
      dataset_name = dataset_nm
    )
    
    logical_checks[[dataset_nm]] <- tmp
    
    if (sh == "household_level") {
      logical_checks$household_level <- tmp
    }
    
  } else {
    
    logical_checks[[dataset_nm]] <- tibble::tibble(
      dataset = character(),
      check_id = character(),
      description = character(),
      check_to_perform = character(),
      columns_to_clean = character(),
      variables_to_clean = character()
    )
  }
}

if ("checks_all" %in% checks_sheets) {
  
  logical_checks$checks_all_raw <- read_logical_check_sheet(
    sheet_name = "checks_all",
    dataset_name = NULL
  )
}

if ("cross_sheet" %in% checks_sheets) {
  
  logical_checks$cross_sheet <- read_logical_check_sheet(
    sheet_name = "cross_sheet",
    dataset_name = "cross_sheet"
  )
}

if ("derived_variables" %in% checks_sheets) {
  
  logical_checks$derived_variables <- read_xlsx_sheet_raw_names_safe(
    path_checks,
    "derived_variables"
  ) |>
    janitor::clean_names() |>
    repair_duplicate_column_names()
}

logical_checks$checks_all <- purrr::imap_dfr(
  logical_checks[names(logical_checks) %in% c(expected_datasets, "cross_sheet")],
  ~ .x
)

logical_check_record_counts <- purrr::imap_dfr(
  logical_checks[names(logical_checks) %in% c(expected_datasets, "cross_sheet")],
  ~ tibble::tibble(
    dataset = .y,
    checks = nrow(.x)
  )
)

# --------------------------------------------------------------------------------------------------
# 7. Raw-data diagnostics
# --------------------------------------------------------------------------------------------------

sheet_record_counts <- purrr::imap_dfr(
  raw_data_list,
  ~ tibble::tibble(
    dataset = .y,
    records = nrow(.x),
    variables = ncol(.x)
  )
)

duplicate_name_diagnostics <- purrr::imap_dfr(
  raw_data_list,
  function(df, nm) {
    
    if (!is.data.frame(df) || ncol(df) == 0) {
      return(
        tibble::tibble(
          dataset = nm,
          duplicate_columns_after_repair = 0,
          duplicate_column_names = NA_character_
        )
      )
    }
    
    dup_names <- names(df)[duplicated(names(df))]
    
    tibble::tibble(
      dataset = nm,
      duplicate_columns_after_repair = length(dup_names),
      duplicate_column_names = if (length(dup_names) == 0) NA_character_ else paste(unique(dup_names), collapse = ", ")
    )
  }
)

cat("\n")
cat(crayon::blue("Kobo export sheets found:\n"))
print(export_sheets)

cat("\n")
cat(crayon::blue("Kobo export sheet mapping:\n"))
print(sheet_map_diagnostics)

cat("\n")
cat(crayon::blue("Raw data record counts:\n"))
print(sheet_record_counts)

cat("\n")
cat(crayon::blue("Repeat linkage diagnostics:\n"))
print(repeat_linkage_diagnostics)

cat("\n")
cat(crayon::blue("Duplicate column diagnostics after repair:\n"))
print(duplicate_name_diagnostics)

cat("\n")
cat(crayon::blue("Logical check sheet mapping:\n"))
print(logical_check_sheet_diagnostics)

cat("\n")
cat(crayon::blue("Logical check counts:\n"))
print(logical_check_record_counts)

if (exists("kobo_column_repair_log") && is.data.frame(kobo_column_repair_log)) {
  
  changed_cols <- kobo_column_repair_log |>
    dplyr::filter(changed)
  
  cat("\n")
  cat(crayon::blue("Kobo column names repaired because of select_multiple split columns or duplicates: "), nrow(changed_cols), "\n", sep = "")
  
  if (nrow(changed_cols) > 0) {
    print(
      changed_cols |>
        dplyr::select(dataset, sheet, original_name, repaired_name) |>
        head(30)
    )
  }
}

cat("\n")
print_step("Data loaded and processed")

# --------------------------------------------------------------------------------------------------
# 8. Report new objects created
# --------------------------------------------------------------------------------------------------

all_dfs_names_after <- Filter(function(x) is(x, "data.frame"), mget(ls())) |> names()
all_lists_names_after <- Filter(function(x) is(x, "list"), mget(ls())) |> names()

if (length(setdiff(all_dfs_names_after, all_dfs_names_before)) > 0) {
  cat(crayon::blue("New dataframes created:\n"))
  cat(paste(setdiff(all_dfs_names_after, all_dfs_names_before), collapse = "\n"), "\n")
}

if (length(setdiff(all_lists_names_after, all_lists_names_before)) > 0) {
  cat(crayon::blue("New lists created:\n"))
  cat(paste(setdiff(all_lists_names_after, all_lists_names_before), collapse = "\n"), "\n")
}