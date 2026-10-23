cat("\014")

# =================================================================================================
# REACH MSNA 2026 Uganda - 3. Do the checks
# Template-structured version covering all final Kobo export sheets
# -------------------------------------------------------------------------------------------------
# This script follows the same broad cleaningtools workflow as the template:
#   1) choose a dataset;
#   2) add percentage-missing / duration where relevant;
#   3) count special values;
#   4) run cleaningtools basic checks: percentage missing, duration, others, special values;
#   5) read the logical-check list for the matching sheet;
#   6) run check_logical_with_list();
#   7) create combined logs;
#   8) add household context variables;
#   9) prepare combined log objects for the processing/export scripts.
#
# Final-tool datasets covered:
#   - Submissions / household_level
#   - roster
#   - edu_ind
#   - health_ind
#   - nutrition_ind / nut_ind
#   - child_protection_ind / prot_ind
# =================================================================================================

all_dfs_names_before <- Filter(function(x) is(x, "data.frame"), mget(ls())) |> names()
all_lists_names_before <- Filter(function(x) is(x, "list"), mget(ls())) |> names()

print_step("3. Running MSNA 2026 cleaningtools-style checks using template structure across all sheets")

# --------------------------------------------------------------------------------------------------
# 0. Small helper functions
# --------------------------------------------------------------------------------------------------

`%||%` <- function(x, y) {
  if (is.null(x)) y else x
}

safe_to_chr <- function(x) {
  if (inherits(x, "POSIXct") || inherits(x, "POSIXt") || inherits(x, "Date")) {
    return(as.character(x))
  }
  as.character(x)
}

safe_numeric <- function(x) {
  suppressWarnings(as.numeric(gsub(",", "", as.character(x))))
}

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

source_dataset_label <- function(x) {
  x <- standardise_dataset_name(x)
  dplyr::case_when(
    x == "Submissions" ~ "household_level",
    TRUE ~ x
  )
}

# Standardise admin2 early so district exports do not create duplicate files for the same district.
# This avoids cases such as "Adjumani", "adjumani", " ADJUMANI ", or multiple spaces being
# treated as different districts in R but as the same/similar workbook path on Windows.
standardise_admin2_value <- function(x) {
  x <- as.character(x)
  x <- stringr::str_squish(x)
  x <- dplyr::na_if(x, "")
  x <- stringr::str_to_lower(x)
  x
}

standardise_admin2_in_df <- function(df) {
  if (!is.data.frame(df) || nrow(df) == 0) {
    return(df)
  }
  if ("admin2" %in% names(df)) {
    df <- df |>
      dplyr::mutate(admin2 = standardise_admin2_value(admin2))
  }
  df
}

find_uuid_col <- function(df, dataset_name = "Submissions") {
  if (!is.data.frame(df) || ncol(df) == 0) return(NA_character_)
  candidates <- if (dataset_name == "Submissions") {
    c("uuid", "_uuid", "_id", "instance_name", "_submission__id", "_submission_id")
  } else {
    # For Kobo server repeat sheets, the parent household UUID is stored in _submission__uuid.
    # Use it as uuid in repeat cleaning logs so reviewers can link flags back to the household submission.
    c("_submission__uuid", "_submission_uuid", "submission_uuid", "parent_uuid", "_parent_uuid", "uuid", "_uuid", "_id", "instance_name", "_submission__id", "_submission_id", "KEY")
  }
  hit <- candidates[candidates %in% names(df)]
  if (length(hit) == 0) NA_character_ else hit[[1]]
}

find_parent_uuid_col <- function(df) {
  if (!is.data.frame(df) || ncol(df) == 0) return(NA_character_)
  candidates <- c(
    "parent_uuid",
    "_parent_uuid",
    "_submission_uuid",
    "_submission__uuid",
    "_submission__id",
    "_submission_id",
    "submission_uuid",
    "submission_id",
    "parent_instance_name",
    "_parent_index"
  )
  hit <- candidates[candidates %in% names(df)]
  if (length(hit) == 0) NA_character_ else hit[[1]]
}

find_repeat_index_col <- function(df) {
  if (!is.data.frame(df) || ncol(df) == 0) return(NA_character_)
  candidates <- c("repeat_index", "_index", "index", "_parent_index", "repeat_position", "KEY")
  hit <- candidates[candidates %in% names(df)]
  if (length(hit) == 0) NA_character_ else hit[[1]]
}

ensure_uuid_cols <- function(df, dataset_name = "Submissions") {
  if (!is.data.frame(df) || nrow(df) == 0) return(tibble::tibble())
  df <- tibble::as_tibble(df)
  
  if (dataset_name != "Submissions") {
    # Preserve the repeat-row identifier separately, then use the parent household UUID as uuid.
    if (!"repeat_row_uuid" %in% names(df)) {
      repeat_row_id_col <- c("uuid", "_id", "instance_name", "KEY", "_submission__id", "_submission_id")
      repeat_row_id_col <- repeat_row_id_col[repeat_row_id_col %in% names(df)]
      df$repeat_row_uuid <- if (length(repeat_row_id_col) > 0) {
        as.character(df[[repeat_row_id_col[[1]]]])
      } else {
        paste0(dataset_name, "_row_", seq_len(nrow(df)))
      }
    }
    
    submission_uuid_col <- c(
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
    submission_uuid_col <- submission_uuid_col[submission_uuid_col %in% names(df)]
    
    if (length(submission_uuid_col) > 0) {
      parent_id <- as.character(df[[submission_uuid_col[[1]]]])
      df$`_submission__uuid` <- parent_id
      df$submission_uuid <- parent_id
      df$parent_uuid <- parent_id
      df$uuid <- parent_id
    } else {
      uuid_col <- find_uuid_col(df, dataset_name)
      if (is.na(uuid_col)) {
        df$uuid <- paste0(dataset_name, "_row_", seq_len(nrow(df)))
      } else {
        df$uuid <- as.character(df[[uuid_col]])
      }
      if (!"parent_uuid" %in% names(df)) df$parent_uuid <- NA_character_
    }
  } else {
    uuid_col <- find_uuid_col(df, dataset_name)
    if (is.na(uuid_col)) {
      df$uuid <- paste0(dataset_name, "_row_", seq_len(nrow(df)))
    } else if (uuid_col != "uuid") {
      df$uuid <- as.character(df[[uuid_col]])
    } else {
      df$uuid <- as.character(df$uuid)
    }
    df$parent_uuid <- NA_character_
    df$submission_uuid <- as.character(df$uuid)
  }
  
  if (!"repeat_index" %in% names(df)) {
    repeat_col <- find_repeat_index_col(df)
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
  
  df
}

empty_log_df <- function() {
  tibble::tibble(
    source_dataset = character(),
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

split_check_vars <- function(x) {
  if (is.null(x) || length(x) == 0 || is.na(x) || trimws(as.character(x)) == "") return(character())
  x <- stringr::str_replace_all(as.character(x), "\\s*[,;]\\s*", ",")
  out <- unlist(strsplit(x, ","))
  out <- trimws(out)
  out <- stringr::str_replace(out, "^[A-Za-z0-9_]+\\.", "")
  out[out != ""] |> unique()
}

format_missingness_value <- function(x) {
  num <- safe_numeric(x)
  dplyr::case_when(
    is.na(num) ~ as.character(x),
    abs(num) <= 1 ~ paste0(round(num * 100, 1), "%"),
    TRUE ~ paste0(round(num, 1), "%")
  )
}

# Extract variables that are actually used inside the R logical expression.
# This is stricter than columns_to_clean and prevents one bad household-level
# check expression from making the whole household-level logical check block fail.
extract_expression_vars <- function(expr) {
  expr <- as.character(expr %||% "")
  if (length(expr) == 0 || is.na(expr) || trimws(expr) == "") return(character())
  
  out <- tryCatch(
    all.vars(parse(text = expr)),
    error = function(e) character()
  )
  
  out <- unique(out)
  out <- out[!is.na(out) & trimws(out) != ""]
  out
}

add_final_tool_derived_check_variables <- function(df, dataset_name = "Submissions") {
  if (!is.data.frame(df) || nrow(df) == 0) return(df)
  if (dataset_name != "Submissions") return(df)
  
  df <- tibble::as_tibble(df)
  
  num_col <- function(x) {
    if (x %in% names(df)) {
      suppressWarnings(as.numeric(as.character(df[[x]])))
    } else {
      rep(NA_real_, nrow(df))
    }
  }
  
  # Preserve the old logical-check workbook where possible by creating the
  # standard food-security derived score variables from final-tool raw variables.
  fcs_vars <- c(
    "fsl_fcs_cereal",
    "fsl_fcs_legumes",
    "fsl_fcs_dairy",
    "fsl_fcs_meat",
    "fsl_fcs_veg",
    "fsl_fcs_fruit",
    "fsl_fcs_oil",
    "fsl_fcs_sugar",
    "fsl_fcs_condiments"
  )
  
  if (!"fsl_fcs_score" %in% names(df) && all(fcs_vars %in% names(df))) {
    df$fsl_fcs_score <-
      num_col("fsl_fcs_cereal") * 2 +
      num_col("fsl_fcs_legumes") * 3 +
      num_col("fsl_fcs_dairy") * 4 +
      num_col("fsl_fcs_meat") * 4 +
      num_col("fsl_fcs_veg") * 1 +
      num_col("fsl_fcs_fruit") * 1 +
      num_col("fsl_fcs_oil") * 0.5 +
      num_col("fsl_fcs_sugar") * 0.5 +
      num_col("fsl_fcs_condiments") * 0
  }
  
  rcsi_vars <- c(
    "fsl_rcsi_lessquality",
    "fsl_rcsi_borrow",
    "fsl_rcsi_mealsize",
    "fsl_rcsi_mealadult",
    "fsl_rcsi_mealnb"
  )
  
  if (!"fsl_rcsi_score" %in% names(df) && all(rcsi_vars %in% names(df))) {
    df$fsl_rcsi_score <-
      num_col("fsl_rcsi_lessquality") * 1 +
      num_col("fsl_rcsi_borrow") * 2 +
      num_col("fsl_rcsi_mealsize") * 1 +
      num_col("fsl_rcsi_mealadult") * 3 +
      num_col("fsl_rcsi_mealnb") * 1
  }
  
  hhs_freq_score <- function(x) {
    y <- tolower(trimws(as.character(x)))
    dplyr::case_when(
      is.na(y) | y == "" ~ NA_real_,
      y %in% c("never", "no", "0", "zero", "not_applicable", "none") ~ 0,
      y %in% c("rarely", "rarely_1_2_times", "1", "1_2", "1-2") ~ 1,
      y %in% c("sometimes", "often", "sometimes_3_10_times", "often_more_than_10_times", "2", "3", "3_10", "more_than_10", "11_plus") ~ 2,
      suppressWarnings(!is.na(as.numeric(y))) ~ dplyr::case_when(
        as.numeric(y) <= 0 ~ 0,
        as.numeric(y) <= 2 ~ 1,
        TRUE ~ 2
      ),
      TRUE ~ NA_real_
    )
  }
  
  hhs_freq_vars <- c(
    "fsl_hhs_nofoodhh_freq",
    "fsl_hhs_sleephungry_freq",
    "fsl_hhs_alldaynight_freq"
  )
  
  if (!"fsl_hhs_score" %in% names(df) && all(hhs_freq_vars %in% names(df))) {
    df$fsl_hhs_score <-
      hhs_freq_score(df$fsl_hhs_nofoodhh_freq) +
      hhs_freq_score(df$fsl_hhs_sleephungry_freq) +
      hhs_freq_score(df$fsl_hhs_alldaynight_freq)
  }
  
  df
}


# --------------------------------------------------------------------------------------------------
# Kobo server repeat linkage helper for Script 3 checked datasets and flags
# --------------------------------------------------------------------------------------------------
main_submission_id_candidates <- c(
  "uuid", "_uuid", "submission_uuid", "_submission_uuid", "_id",
  "instance_name", "_submission__id", "_submission_id", "submission_id"
)

build_main_context_for_repeat_checked_datasets <- function(main_df) {
  if (!is.data.frame(main_df) || nrow(main_df) == 0) return(tibble::tibble())
  
  main_id_cols <- intersect(main_submission_id_candidates, names(main_df))
  context_cols_base <- if (exists("main_context_vars", inherits = TRUE)) main_context_vars else c("point_number")
  context_cols <- intersect(context_cols_base, names(main_df))
  context_cols <- unique(c("point_number", context_cols))
  context_cols <- intersect(context_cols, names(main_df))
  context_cols <- setdiff(context_cols, main_id_cols)
  
  if (length(main_id_cols) == 0) return(tibble::tibble())
  
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

add_point_number_to_repeat_dataset <- function(df, dataset_name, main_context) {
  if (!is.data.frame(df) || nrow(df) == 0) return(df)
  df <- ensure_uuid_cols(df, dataset_name)
  
  if (dataset_name == "Submissions") return(df)
  if (!is.data.frame(main_context) || nrow(main_context) == 0) return(df)
  if (!"_submission__uuid" %in% names(df)) return(df)
  
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
  
  df |>
    dplyr::select(-dplyr::any_of(c(".submission_uuid", paste0(context_from_main, "_main"))))
}

align_repeat_datasets_to_kobo_server_submission_uuid <- function(data_list) {
  if (!is.list(data_list) || !"Submissions" %in% names(data_list)) return(data_list)
  main_context <- build_main_context_for_repeat_checked_datasets(data_list$Submissions)
  purrr::imap(data_list, ~ add_point_number_to_repeat_dataset(.x, .y, main_context))
}


normalise_msna_log <- function(x, dataset_name = NA_character_) {
  if (is.null(x)) return(empty_log_df())
  
  if (is.list(x) && !is.data.frame(x) && "cleaning_log" %in% names(x)) {
    x <- x$cleaning_log
  }
  
  if (is.list(x) && !is.data.frame(x)) {
    parts <- purrr::imap(x, ~ normalise_msna_log(.x, dataset_name = .y))
    parts <- parts[purrr::map_int(parts, nrow) > 0]
    if (length(parts) == 0) return(empty_log_df())
    return(dplyr::bind_rows(parts))
  }
  
  if (!is.data.frame(x) || nrow(x) == 0) return(empty_log_df())
  
  x <- tibble::as_tibble(x) |>
    janitor::clean_names()
  
  if (!"dataset" %in% names(x)) x$dataset <- dataset_name
  x$dataset <- standardise_dataset_name(x$dataset)
  x$dataset <- ifelse(is.na(x$dataset) | x$dataset == "", standardise_dataset_name(dataset_name), x$dataset)
  
  if (!"source_dataset" %in% names(x)) x$source_dataset <- source_dataset_label(x$dataset)
  x$source_dataset <- source_dataset_label(x$source_dataset)
  
  if (!"uuid" %in% names(x)) {
    hit <- c("_uuid", "id", "submission_id", "submission_uuid", "instance_name", "KEY")
    hit <- hit[hit %in% names(x)]
    x$uuid <- if (length(hit) > 0) as.character(x[[hit[[1]]]]) else NA_character_
  }
  if (!"parent_uuid" %in% names(x)) {
    hit <- c("_parent_uuid", "submission_uuid", "parent_id", "_submission__id", "_submission_id")
    hit <- hit[hit %in% names(x)]
    x$parent_uuid <- if (length(hit) > 0) as.character(x[[hit[[1]]]]) else NA_character_
  }
  if (!"repeat_index" %in% names(x)) {
    hit <- c("_index", "index", "repeat_position", "_parent_index")
    hit <- hit[hit %in% names(x)]
    x$repeat_index <- if (length(hit) > 0) as.character(x[[hit[[1]]]]) else NA_character_
  }
  if (!"question" %in% names(x)) {
    hit <- c("question", "question_name", "name", "variable", "variable_name", "variables_to_clean", "columns_to_clean")
    hit <- hit[hit %in% names(x)]
    x$question <- if (length(hit) > 0) as.character(x[[hit[[1]]]]) else NA_character_
  }
  if (!"old_value" %in% names(x)) {
    hit <- c("value", "old", "old_value", "response", "answer")
    hit <- hit[hit %in% names(x)]
    x$old_value <- if (length(hit) > 0) as.character(x[[hit[[1]]]]) else NA_character_
  }
  if (!"new_value" %in% names(x)) x$new_value <- NA_character_
  if (!"change_type" %in% names(x)) x$change_type <- NA_character_
  if (!"issue" %in% names(x)) {
    hit <- c("description", "problem", "comment", "message")
    hit <- hit[hit %in% names(x)]
    x$issue <- if (length(hit) > 0) as.character(x[[hit[[1]]]]) else "Flag generated by automated check."
  }
  if (!"check_id" %in% names(x)) x$check_id <- NA_character_
  if (!"check_to_perform" %in% names(x)) x$check_to_perform <- NA_character_
  if (!"columns_to_clean" %in% names(x)) {
    hit <- c("variables_to_clean", "question", "variable", "variable_name")
    hit <- hit[hit %in% names(x)]
    x$columns_to_clean <- if (length(hit) > 0) as.character(x[[hit[[1]]]]) else as.character(x$question)
  }
  if (!"notes" %in% names(x)) x$notes <- NA_character_
  
  core <- names(empty_log_df())
  for (cc in setdiff(core, names(x))) x[[cc]] <- NA_character_
  
  x |>
    dplyr::mutate(
      dataset = standardise_dataset_name(dataset),
      source_dataset = source_dataset_label(dataset),
      dplyr::across(dplyr::all_of(core), safe_to_chr),
      notes = NA_character_
    ) |>
    dplyr::select(dplyr::all_of(core), dplyr::everything())
}

combine_msna_logs <- function(x) {
  if (is.null(x) || length(x) == 0) return(empty_log_df())
  logs <- purrr::imap(x, ~ normalise_msna_log(.x, .y))
  logs <- logs[purrr::map_int(logs, nrow) > 0]
  if (length(logs) == 0) return(empty_log_df())
  dplyr::bind_rows(logs)
}

# Count of 999 and related special values, kept in the style of your old script.
if (!exists("count_999_by_variable", mode = "function")) {
  count_999_by_variable <- function(df) {
    if (!is.data.frame(df) || nrow(df) == 0) {
      return(tibble::tibble(Variable = character(), `Number of special values` = integer()))
    }
    df |>
      dplyr::mutate(dplyr::across(dplyr::everything(), as.character)) |>
      tidyr::pivot_longer(cols = dplyr::everything(), values_drop_na = TRUE, names_to = "Variable", values_to = "value") |>
      dplyr::filter(value %in% c(999, "999", "999.0", " _999", "_999", -990, "-990", -999, "-999")) |>
      dplyr::count(Variable, name = "Number of special values") |>
      dplyr::arrange(dplyr::desc(`Number of special values`))
  }
}

# --------------------------------------------------------------------------------------------------
# 1. Prepare the final-tool export datasets
# --------------------------------------------------------------------------------------------------

expected_datasets <- c(
  "Submissions",
  "roster",
  "edu_ind",
  "health_ind",
  "nutrition_ind",
  "child_protection_ind"
)

get_dataset_object <- function(dataset_name) {
  if (exists("raw_data_list") && is.list(raw_data_list) && dataset_name %in% names(raw_data_list)) {
    return(raw_data_list[[dataset_name]])
  }
  
  if (dataset_name == "Submissions" && exists("data_hh_main")) return(data_hh_main)
  if (dataset_name == "roster" && exists("data_hh_roster")) return(data_hh_roster)
  if (dataset_name == "roster" && exists("data_roster")) return(data_roster)
  if (dataset_name == "edu_ind" && exists("data_edu_ind")) return(data_edu_ind)
  if (dataset_name == "health_ind" && exists("data_health_ind")) return(data_health_ind)
  if (dataset_name == "nutrition_ind" && exists("data_nutrition_ind")) return(data_nutrition_ind)
  if (dataset_name == "nutrition_ind" && exists("data_nut_ind")) return(data_nut_ind)
  if (dataset_name == "child_protection_ind" && exists("data_child_protection_ind")) return(data_child_protection_ind)
  if (dataset_name == "child_protection_ind" && exists("data_prot_ind")) return(data_prot_ind)
  
  tibble::tibble()
}

raw_data_list <- purrr::map(expected_datasets, get_dataset_object) |>
  purrr::set_names(expected_datasets)

raw_data_list <- purrr::imap(raw_data_list, function(df, dataset_name) {
  if (!is.data.frame(df) || nrow(df) == 0) return(tibble::tibble())
  df <- ensure_uuid_cols(df, dataset_name)
  if (exists("add_msna_derived_variables", mode = "function")) {
    df <- tryCatch(add_msna_derived_variables(df, dataset_name), error = function(e) df)
    df <- ensure_uuid_cols(df, dataset_name)
  }
  df <- standardise_admin2_in_df(df)
  df <- add_final_tool_derived_check_variables(df, dataset_name)
  df
})

raw_data_list <- align_repeat_datasets_to_kobo_server_submission_uuid(raw_data_list)

data_hh_main <- raw_data_list$Submissions %||% tibble::tibble()
data_hh_roster <- raw_data_list$roster %||% tibble::tibble()
data_roster <- raw_data_list$roster %||% tibble::tibble()
data_edu_ind <- raw_data_list$edu_ind %||% tibble::tibble()
data_health_ind <- raw_data_list$health_ind %||% tibble::tibble()
data_nutrition_ind <- raw_data_list$nutrition_ind %||% tibble::tibble()
data_nut_ind <- data_nutrition_ind
data_child_protection_ind <- raw_data_list$child_protection_ind %||% tibble::tibble()
data_prot_ind <- data_child_protection_ind

data <- data_hh_main
raw_data <- data_hh_main

# --------------------------------------------------------------------------------------------------
# 2. Load the predefined logical-check lists
# --------------------------------------------------------------------------------------------------

if (!exists("path_checks")) {
  stop("path_checks object is missing. Define the logical checks workbook path before running this script.", call. = FALSE)
}

if (!file.exists(path_checks)) {
  stop("Logical checks workbook not found: ", path_checks, call. = FALSE)
}

check_workbook_sheets <- readxl::excel_sheets(path_checks)

logical_check_sheet_map <- c(
  Submissions = "household_level",
  roster = "roster",
  edu_ind = "edu_ind",
  health_ind = "health_ind",
  nutrition_ind = "nutrition_ind",
  child_protection_ind = "child_protection_ind"
)

read_check_sheet <- function(sheet_name, dataset_name) {
  if (!sheet_name %in% check_workbook_sheets) {
    return(tibble::tibble(
      dataset = character(),
      check_id = character(),
      description = character(),
      check_to_perform = character(),
      columns_to_clean = character(),
      variables_to_clean = character()
    ))
  }
  
  out <- readxl::read_xlsx(path_checks, sheet = sheet_name, .name_repair = "unique_quiet") |>
    janitor::clean_names()
  
  if ("variables_to_clean" %in% names(out) && !"columns_to_clean" %in% names(out)) {
    out <- out |> dplyr::rename(columns_to_clean = variables_to_clean)
  }
  if ("columns_to_clean" %in% names(out) && !"variables_to_clean" %in% names(out)) {
    out <- out |> dplyr::mutate(variables_to_clean = columns_to_clean)
  }
  
  for (cc in c("check_id", "description", "check_to_perform", "columns_to_clean", "variables_to_clean")) {
    if (!cc %in% names(out)) out[[cc]] <- NA_character_
  }
  
  out |>
    dplyr::mutate(
      dataset = dataset_name,
      source_check_sheet = sheet_name,
      check_id = as.character(check_id),
      description = as.character(description),
      check_to_perform = as.character(check_to_perform),
      columns_to_clean = as.character(columns_to_clean),
      variables_to_clean = as.character(variables_to_clean),
      .before = 1
    ) |>
    dplyr::filter(!is.na(check_to_perform), trimws(as.character(check_to_perform)) != "")
}


# --------------------------------------------------------------------------------------------------
# 2b. Suppress non-sensible repeat-age logical checks
# --------------------------------------------------------------------------------------------------
# The final Kobo tool creates repeat sheets at household-size level. This means repeat sheets can
# contain copied person-level records for people outside the target analytical age band. Those rows are
# normal and should not be exported as cleaning-log flags merely because the repeat record exists.
#
# We therefore suppress the old age-boundary repeat checks:
#   - EDU_001: edu_ind_age outside 3-18
#   - NUT_001: nut_ind_age >= 5
#   - CP_001 : prot_ind_age outside 5-17
#
# Keep substantive checks in those repeat sheets, for example child/labour answers, nutrition answers
# for true under-5s, missing proxy questions for true under-5s, and other response inconsistencies.

is_non_sensible_repeat_age_check <- function(check_df, dataset_name = NA_character_) {
  
  if (!is.data.frame(check_df) || nrow(check_df) == 0) {
    return(logical(0))
  }
  
  check_df <- tibble::as_tibble(check_df)
  
  if (!"dataset" %in% names(check_df)) {
    check_df$dataset <- dataset_name
  }
  if (!"check_id" %in% names(check_df)) {
    check_df$check_id <- NA_character_
  }
  if (!"description" %in% names(check_df)) {
    check_df$description <- NA_character_
  }
  if (!"columns_to_clean" %in% names(check_df) && "variables_to_clean" %in% names(check_df)) {
    check_df <- check_df |> dplyr::rename(columns_to_clean = variables_to_clean)
  }
  if (!"columns_to_clean" %in% names(check_df)) {
    check_df$columns_to_clean <- NA_character_
  }
  
  purrr::map_lgl(seq_len(nrow(check_df)), function(i) {
    
    ds_i <- standardise_dataset_name(check_df$dataset[[i]] %||% dataset_name)
    check_id_i <- as.character(check_df$check_id[[i]] %||% "")
    desc_i <- stringr::str_to_lower(as.character(check_df$description[[i]] %||% ""))
    cols_i <- split_check_vars(check_df$columns_to_clean[[i]] %||% "")
    
    by_exact_id <- (
      (ds_i == "edu_ind" && check_id_i == "EDU_001") ||
        (ds_i == "nutrition_ind" && check_id_i == "NUT_001") ||
        (ds_i == "child_protection_ind" && check_id_i == "CP_001")
    )
    
    by_description_and_column <- (
      (ds_i == "edu_ind" &&
         "edu_ind_age" %in% cols_i &&
         stringr::str_detect(desc_i, "outside|schooling-age|3.*18")) ||
        (ds_i == "nutrition_ind" &&
           "nut_ind_age" %in% cols_i &&
           stringr::str_detect(desc_i, "aged 5|5 years|older|outside")) ||
        (ds_i == "child_protection_ind" &&
           "prot_ind_age" %in% cols_i &&
           stringr::str_detect(desc_i, "outside|5.*17|age range"))
    )
    
    isTRUE(by_exact_id || by_description_and_column)
  })
}

suppress_non_sensible_repeat_age_checks <- function(check_df, dataset_name = NA_character_) {
  
  if (!is.data.frame(check_df) || nrow(check_df) == 0) {
    return(check_df)
  }
  
  suppress_rows <- is_non_sensible_repeat_age_check(check_df, dataset_name)
  
  check_df[!suppress_rows, , drop = FALSE]
}

make_repeat_age_check_exclusion_summary <- function(checks_by_sheet) {
  
  if (!is.list(checks_by_sheet) || length(checks_by_sheet) == 0) {
    return(tibble::tibble(
      dataset = character(),
      check_id = character(),
      columns_to_clean = character(),
      description = character(),
      exclusion_reason = character()
    ))
  }
  
  purrr::imap_dfr(checks_by_sheet, function(check_df, dataset_name) {
    
    if (!is.data.frame(check_df) || nrow(check_df) == 0) {
      return(tibble::tibble())
    }
    
    suppress_rows <- is_non_sensible_repeat_age_check(check_df, dataset_name)
    
    if (!any(suppress_rows)) {
      return(tibble::tibble())
    }
    
    out <- check_df[suppress_rows, , drop = FALSE] |>
      janitor::clean_names()
    
    for (cc in c("check_id", "columns_to_clean", "description")) {
      if (!cc %in% names(out)) {
        out[[cc]] <- NA_character_
      }
    }
    
    out |>
      dplyr::transmute(
        dataset = dataset_name,
        check_id = as.character(check_id),
        columns_to_clean = as.character(columns_to_clean),
        description = as.character(description),
        exclusion_reason = paste0(
          "Suppressed because the final Kobo repeat sheets can include household members outside ",
          "the target age band. The mere presence of a repeat row is normal and should not be flagged."
        )
      )
  })
}

drop_non_sensible_repeat_age_flags <- function(log_df) {
  
  if (!is.data.frame(log_df) || nrow(log_df) == 0) {
    return(log_df)
  }
  
  log_df <- tibble::as_tibble(log_df)
  
  for (cc in c("dataset", "question", "issue", "check_id")) {
    if (!cc %in% names(log_df)) {
      log_df[[cc]] <- NA_character_
    }
  }
  
  suppress_rows <- purrr::pmap_lgl(
    list(log_df$dataset, log_df$question, log_df$issue, log_df$check_id),
    function(dataset, question, issue, check_id) {
      
      ds_i <- standardise_dataset_name(dataset)
      question_i <- as.character(question %||% "")
      issue_i <- stringr::str_to_lower(as.character(issue %||% ""))
      check_id_i <- as.character(check_id %||% "")
      
      (
        ds_i == "edu_ind" &&
          question_i == "edu_ind_age" &&
          (check_id_i == "EDU_001" || stringr::str_detect(issue_i, "outside|schooling-age|3.*18"))
      ) ||
        (
          ds_i == "nutrition_ind" &&
            question_i == "nut_ind_age" &&
            (check_id_i == "NUT_001" || stringr::str_detect(issue_i, "aged 5|5 years|older|outside"))
        ) ||
        (
          ds_i == "child_protection_ind" &&
            question_i == "prot_ind_age" &&
            (check_id_i == "CP_001" || stringr::str_detect(issue_i, "outside|5.*17|age range"))
        )
    }
  )
  
  log_df[!suppress_rows, , drop = FALSE]
}


checks_by_sheet <- list()
for (ds in expected_datasets) {
  if (
    exists("logical_checks") &&
    is.list(logical_checks) &&
    ds %in% names(logical_checks) &&
    is.data.frame(logical_checks[[ds]]) &&
    nrow(logical_checks[[ds]]) > 0
  ) {
    tmp <- logical_checks[[ds]] |>
      janitor::clean_names()
    if ("variables_to_clean" %in% names(tmp) && !"columns_to_clean" %in% names(tmp)) {
      tmp <- tmp |> dplyr::rename(columns_to_clean = variables_to_clean)
    }
    if ("columns_to_clean" %in% names(tmp) && !"variables_to_clean" %in% names(tmp)) {
      tmp <- tmp |> dplyr::mutate(variables_to_clean = columns_to_clean)
    }
    checks_by_sheet[[ds]] <- tmp |> dplyr::mutate(dataset = ds)
  } else {
    checks_by_sheet[[ds]] <- read_check_sheet(logical_check_sheet_map[[ds]], ds)
  }
}

logical_check_exclusion_summary <- make_repeat_age_check_exclusion_summary(checks_by_sheet)
checks_by_sheet <- purrr::imap(checks_by_sheet, suppress_non_sensible_repeat_age_checks)

cross_checks <- if ("cross_sheet" %in% check_workbook_sheets) {
  read_check_sheet("cross_sheet", "cross_sheet")
} else if (exists("logical_checks") && is.list(logical_checks) && "cross_sheet" %in% names(logical_checks)) {
  logical_checks$cross_sheet |> janitor::clean_names()
} else {
  tibble::tibble()
}

filter_logical_checks_for_data <- function(list_of_check, data) {
  if (!is.data.frame(list_of_check) || nrow(list_of_check) == 0 || !is.data.frame(data) || nrow(data) == 0) {
    return(tibble::tibble())
  }
  if (!"columns_to_clean" %in% names(list_of_check) && "variables_to_clean" %in% names(list_of_check)) {
    list_of_check <- list_of_check |> dplyr::rename(columns_to_clean = variables_to_clean)
  }
  if (!"variables_to_clean" %in% names(list_of_check)) {
    list_of_check <- list_of_check |> dplyr::mutate(variables_to_clean = columns_to_clean)
  }
  
  list_of_check |>
    dplyr::rowwise() |>
    dplyr::mutate(
      .vars_to_clean = paste(split_check_vars(columns_to_clean), collapse = ", "),
      .expression_vars = paste(extract_expression_vars(check_to_perform), collapse = ", "),
      .vars_found = {
        vv <- split_check_vars(columns_to_clean)
        length(vv) == 0 || any(vv %in% names(data))
      },
      .missing_vars_to_clean = {
        vv <- split_check_vars(columns_to_clean)
        paste(setdiff(vv, names(data)), collapse = ", ")
      },
      .all_expression_vars_found = {
        vv <- extract_expression_vars(check_to_perform)
        length(vv) == 0 || all(vv %in% names(data))
      },
      .missing_expression_vars = {
        vv <- extract_expression_vars(check_to_perform)
        paste(setdiff(vv, names(data)), collapse = ", ")
      }
    ) |>
    dplyr::ungroup() |>
    dplyr::filter(.vars_found)
}

make_check_variable_availability <- function(check_df, df, dataset_name) {
  if (!is.data.frame(check_df) || nrow(check_df) == 0) return(tibble::tibble())
  if (!"source_check_sheet" %in% names(check_df)) check_df$source_check_sheet <- NA_character_
  check_df |>
    dplyr::rowwise() |>
    dplyr::mutate(
      expected_variables = paste(split_check_vars(columns_to_clean), collapse = ", "),
      expression_variables = paste(extract_expression_vars(check_to_perform), collapse = ", "),
      missing_variables = {
        vv <- split_check_vars(columns_to_clean)
        paste(setdiff(vv, names(df)), collapse = ", ")
      },
      missing_expression_variables = {
        vv <- extract_expression_vars(check_to_perform)
        paste(setdiff(vv, names(df)), collapse = ", ")
      },
      all_columns_to_clean_found = {
        vv <- split_check_vars(columns_to_clean)
        length(vv) == 0 || all(vv %in% names(df))
      },
      any_columns_to_clean_found = {
        vv <- split_check_vars(columns_to_clean)
        length(vv) == 0 || any(vv %in% names(df))
      },
      all_expression_variables_found = {
        vv <- extract_expression_vars(check_to_perform)
        length(vv) == 0 || all(vv %in% names(df))
      }
    ) |>
    dplyr::ungroup() |>
    dplyr::mutate(dataset = dataset_name, .before = 1) |>
    dplyr::select(
      dataset,
      source_check_sheet = dplyr::any_of("source_check_sheet"),
      check_id,
      description,
      columns_to_clean,
      check_to_perform,
      expected_variables,
      expression_variables,
      missing_variables,
      missing_expression_variables,
      all_columns_to_clean_found,
      any_columns_to_clean_found,
      all_expression_variables_found
    )
}

logical_check_variable_availability <- purrr::imap_dfr(
  checks_by_sheet,
  ~ make_check_variable_availability(.x, raw_data_list[[.y]], .y)
)

# --------------------------------------------------------------------------------------------------
# 3. Missingness helpers: keep both template-style and high-missingness threshold checks
# --------------------------------------------------------------------------------------------------

missingness_threshold_percent <- 50

get_questionnaire_cols_for_missingness <- function(df, dataset_name = "Submissions") {
  if (!is.data.frame(df) || ncol(df) == 0) return(character())
  
  survey_source <- if (exists("df_survey_clean") && is.data.frame(df_survey_clean)) {
    df_survey_clean
  } else if (exists("df_survey") && is.data.frame(df_survey)) {
    janitor::clean_names(df_survey)
  } else {
    tibble::tibble()
  }
  
  if (is.data.frame(survey_source) && nrow(survey_source) > 0 && all(c("type", "name") %in% names(survey_source))) {
    questionnaire_cols <- survey_source |>
      dplyr::mutate(type = as.character(type), name = as.character(name)) |>
      dplyr::filter(
        !is.na(name),
        name != "",
        !stringr::str_detect(type, "^begin[_ ]"),
        !stringr::str_detect(type, "^end[_ ]"),
        !stringr::str_detect(type, "^calculate$"),
        !stringr::str_detect(type, "^note$"),
        !stringr::str_detect(type, "^acknowledge$"),
        !stringr::str_detect(type, "^geopoint$"),
        !stringr::str_detect(type, "^start$|^end$|^today$")
      ) |>
      dplyr::pull(name) |>
      intersect(names(df))
    if (length(questionnaire_cols) >= 3) return(unique(questionnaire_cols))
  }
  
  metadata_cols <- c(
    "uuid", "_uuid", "_id", "id", "instance_name", "parent_uuid", "_parent_uuid",
    "repeat_index", "_index", "index", "_submission_time", "_submitted_by",
    "_submission__id", "_submission_id", "_submission__uuid", "_submission_uuid",
    "submission_id", "submission_uuid", "validation_status", "status", "tags", "notes",
    "version", "start", "end", "today", "duration", "duration_minutes", "percentage_missing",
    "percentage_missing_msna", "percentage_missing_msna_percent", "log_created_at"
  )
  
  fallback_cols <- setdiff(names(df), metadata_cols)
  fallback_cols <- fallback_cols[
    !stringr::str_detect(fallback_cols, "^_") &
      !stringr::str_detect(fallback_cols, "^calc_|_calc$|^calculate_|_score$|_category$")
  ]
  unique(fallback_cols)
}

add_msna_missingness_percent <- function(df, dataset_name = "Submissions") {
  if (!is.data.frame(df) || nrow(df) == 0) return(df)
  cols_to_check <- get_questionnaire_cols_for_missingness(df, dataset_name)
  if (length(cols_to_check) == 0) {
    df$percentage_missing_msna <- NA_real_
    return(df)
  }
  
  missing_matrix <- df |>
    dplyr::select(dplyr::all_of(cols_to_check)) |>
    dplyr::mutate(dplyr::across(dplyr::everything(), ~ is.na(.x) | trimws(as.character(.x)) == ""))
  
  df$percentage_missing_msna <- rowMeans(as.data.frame(missing_matrix), na.rm = FALSE) * 100
  df
}

make_missingness_50_log <- function(df, dataset_name, uuid_col = "uuid", threshold = missingness_threshold_percent) {
  if (!is.data.frame(df) || nrow(df) == 0) return(empty_log_df())
  if (!"percentage_missing_msna" %in% names(df)) df <- add_msna_missingness_percent(df, dataset_name)
  if (!"percentage_missing_msna" %in% names(df)) return(empty_log_df())
  
  if (!uuid_col %in% names(df)) uuid_col <- find_uuid_col(df, dataset_name)
  if (is.na(uuid_col) || !uuid_col %in% names(df)) {
    df$uuid_tmp_missingness <- paste0(dataset_name, "_row_", seq_len(nrow(df)))
    uuid_col <- "uuid_tmp_missingness"
  }
  
  flagged <- df |>
    dplyr::filter(!is.na(percentage_missing_msna), percentage_missing_msna >= threshold)
  
  if (nrow(flagged) == 0) return(empty_log_df())
  
  tibble::tibble(
    source_dataset = source_dataset_label(dataset_name),
    dataset = dataset_name,
    uuid = as.character(flagged[[uuid_col]]),
    parent_uuid = if ("parent_uuid" %in% names(flagged)) as.character(flagged$parent_uuid) else NA_character_,
    repeat_index = if ("repeat_index" %in% names(flagged)) as.character(flagged$repeat_index) else NA_character_,
    question = "percentage_missing",
    old_value = paste0(round(as.numeric(flagged$percentage_missing_msna), 1), "%"),
    new_value = NA_character_,
    change_type = NA_character_,
    issue = paste0("High missingness: ", threshold, "% or more of expected questionnaire responses are missing in this record."),
    check_id = paste0(dataset_name, "_MISS_", threshold),
    check_to_perform = paste0("percentage_missing_msna >= ", threshold),
    columns_to_clean = "percentage_missing",
    notes = NA_character_
  )
}

clarify_standard_missingness_flags <- function(log_df) {
  if (!is.data.frame(log_df) || nrow(log_df) == 0) return(log_df)
  log_df <- tibble::as_tibble(log_df)
  if (!"question" %in% names(log_df)) return(log_df)
  if (!"issue" %in% names(log_df)) log_df$issue <- NA_character_
  if (!"old_value" %in% names(log_df)) log_df$old_value <- NA_character_
  if (!"notes" %in% names(log_df)) log_df$notes <- NA_character_
  
  is_missingness_pattern <- log_df$question == "percentage_missing" &
    !stringr::str_detect(as.character(log_df$issue), "High missingness")
  
  log_df$old_value[is_missingness_pattern] <- format_missingness_value(log_df$old_value[is_missingness_pattern])
  log_df$issue[is_missingness_pattern] <- "Unusual missingness pattern: this record differs from the missingness pattern of other records in the same dataset."
  log_df$notes[is_missingness_pattern] <- NA_character_
  
  log_df
}

# --------------------------------------------------------------------------------------------------
# 4. Dataset-level template workflow
# --------------------------------------------------------------------------------------------------

get_other_columns_to_check <- function(data) {
  survey_for_other <- if (exists("df_survey_clean") && is.data.frame(df_survey_clean)) {
    df_survey_clean
  } else if (exists("df_survey") && is.data.frame(df_survey)) {
    janitor::clean_names(df_survey)
  } else {
    tibble::tibble()
  }
  
  if (!is.data.frame(survey_for_other) || nrow(survey_for_other) == 0 || !all(c("type", "name") %in% names(survey_for_other))) {
    return(character())
  }
  
  survey_for_other |>
    dplyr::filter(type == "text", stringr::str_detect(name, "other|specify|comment|comments|explain")) |>
    dplyr::filter(name %in% names(data)) |>
    dplyr::pull(name) |>
    unique()
}

run_basic_cleaningtools_checks <- function(data, dataset_name, uuid_col) {
  if (!is.data.frame(data) || nrow(data) == 0) return(list(checked_dataset = data, cleaning_log = empty_log_df()))
  
  other_columns_to_check <- get_other_columns_to_check(data)
  duration_col <- if (dataset_name == "Submissions" && "duration" %in% names(data)) {
    "duration"
  } else if (dataset_name == "Submissions" && "duration_minutes" %in% names(data)) {
    "duration_minutes"
  } else {
    NA_character_
  }
  
  run_missingness_checks <- dataset_name == "Submissions"
  
  required_funs <- c("check_value", "create_combined_log")
  if (run_missingness_checks) required_funs <- c("check_percentage_missing", required_funs)
  if (!is.na(duration_col)) required_funs <- c(required_funs, "check_duration")
  if (length(other_columns_to_check) > 0) required_funs <- c(required_funs, "check_others")
  
  have_funs <- all(vapply(required_funs, exists, logical(1), mode = "function"))
  if (!have_funs) {
    missing_funs <- required_funs[!vapply(required_funs, exists, logical(1), mode = "function")]
    return(list(
      checked_dataset = data,
      cleaning_log = tibble::tibble(
        source_dataset = source_dataset_label(dataset_name),
        dataset = dataset_name,
        uuid = NA_character_,
        parent_uuid = NA_character_,
        repeat_index = NA_character_,
        question = NA_character_,
        old_value = NA_character_,
        new_value = NA_character_,
        change_type = NA_character_,
        issue = paste0("BASIC CHECKS COULD NOT RUN: missing function(s): ", paste(missing_funs, collapse = ", ")),
        check_id = paste0(dataset_name, "_BASIC_FUNCTION_MISSING"),
        check_to_perform = "cleaningtools basic checks",
        columns_to_clean = NA_character_,
        notes = NA_character_
      )
    ))
  }
  
  clog_basic <- tryCatch({
    # This follows the template structure. Percentage-missing checks are run only
    # at household level; repeat sheets should not generate missingness flags.
    clog <- data
    
    if (run_missingness_checks) {
      clog <- clog |>
        check_percentage_missing(
          uuid_column = uuid_col,
          column_to_check = "percentage_missing",
          strongness_factor = 2
        )
    }
    
    if (!is.na(duration_col)) {
      clog <- clog |>
        check_duration(
          column_to_check = duration_col,
          uuid_column = uuid_col,
          lower_bound = 10,
          higher_bound = 1440
        )
    }
    
    if (length(other_columns_to_check) > 0) {
      clog <- clog |>
        check_others(
          uuid_column = uuid_col,
          columns_to_check = other_columns_to_check
        )
    }
    
    clog |>
      check_value(
        uuid_column = uuid_col,
        element_name = "checked_dataset",
        values_to_look = c(999, " _999", -990, "-990", -999, "-999", "999", "999.0")
      )
  }, error = function(e) {
    tibble::tibble(
      source_dataset = source_dataset_label(dataset_name),
      dataset = dataset_name,
      uuid = NA_character_,
      parent_uuid = NA_character_,
      repeat_index = NA_character_,
      question = NA_character_,
      old_value = NA_character_,
      new_value = NA_character_,
      change_type = NA_character_,
      issue = paste0("BASIC CHECKS COULD NOT RUN: ", conditionMessage(e)),
      check_id = paste0(dataset_name, "_BASIC_CHECK_ERROR"),
      check_to_perform = "household check_percentage_missing where dataset == Submissions + check_duration/check_others/check_value",
      columns_to_clean = NA_character_,
      notes = NA_character_
    )
  })
  
  if (is.list(clog_basic) && "checked_dataset" %in% names(clog_basic)) {
    checked_dataset <- ensure_uuid_cols(clog_basic$checked_dataset, dataset_name)
  } else {
    checked_dataset <- data
  }
  
  combined_basic <- tryCatch(
    clog_basic |> create_combined_log(),
    error = function(e) list(checked_dataset = checked_dataset, cleaning_log = clog_basic)
  )
  
  combined_basic$checked_dataset <- checked_dataset
  combined_basic$cleaning_log <- combined_basic$cleaning_log |>
    normalise_msna_log(dataset_name) |>
    clarify_standard_missingness_flags()
  
  combined_basic
}


# Manual logical-check engine.
# This is intentionally used as the primary logical-check engine because it directly
# evaluates each check_to_perform expression against the data frame. This prevents
# empty cleaning logs when cleaningtools::check_logical_with_list() silently returns
# no rows or fails to interpret a valid expression.
# Return the actual value of one variable, without prefixing it with the variable name.
# Example: old_value should be "999", not "wash_sanitation_facility_sharing_n=999".
make_actual_old_value_for_question <- function(df, question_col) {
  if (!is.data.frame(df) || nrow(df) == 0 || is.na(question_col) || !question_col %in% names(df)) {
    return(rep(NA_character_, if (is.data.frame(df)) nrow(df) else 0))
  }
  
  x <- df[[question_col]]
  
  if (inherits(x, "POSIXct") || inherits(x, "POSIXt") || inherits(x, "Date")) {
    return(as.character(x))
  }
  
  if (is.list(x)) {
    return(vapply(x, function(z) paste(as.character(z), collapse = " "), character(1)))
  }
  
  as.character(x)
}

# Build one cleaning-log row per flagged submission and per question/variable.
# This avoids multi-variable cells such as "var1, var2" in question and avoids
# "var=value" strings in old_value.
make_one_question_rows_for_check <- function(flagged,
                                             dataset_name,
                                             uuid_col,
                                             question_cols,
                                             check_id_i,
                                             description_i,
                                             expr_i,
                                             notes_i) {
  if (!is.data.frame(flagged) || nrow(flagged) == 0) {
    return(empty_log_df())
  }
  
  question_cols <- unique(as.character(question_cols))
  question_cols <- question_cols[!is.na(question_cols) & trimws(question_cols) != ""]
  question_cols <- question_cols[question_cols %in% names(flagged)]
  
  if (length(question_cols) == 0) {
    question_cols <- NA_character_
  }
  
  purrr::map_dfr(question_cols, function(question_i) {
    
    old_value_i <- if (!is.na(question_i) && question_i %in% names(flagged)) {
      make_actual_old_value_for_question(flagged, question_i)
    } else {
      rep(NA_character_, nrow(flagged))
    }
    
    tibble::tibble(
      source_dataset = source_dataset_label(dataset_name),
      dataset = dataset_name,
      uuid = as.character(flagged[[uuid_col]]),
      parent_uuid = if ("parent_uuid" %in% names(flagged)) as.character(flagged$parent_uuid) else NA_character_,
      repeat_index = if ("repeat_index" %in% names(flagged)) as.character(flagged$repeat_index) else NA_character_,
      question = question_i,
      old_value = old_value_i,
      new_value = NA_character_,
      change_type = NA_character_,
      issue = description_i,
      check_id = check_id_i,
      check_to_perform = expr_i,
      columns_to_clean = question_i,
      notes = NA_character_
    )
  })
}

evaluate_logical_expression_vector <- function(data, expr) {
  expr <- as.character(expr %||% "")
  
  if (!is.data.frame(data) || nrow(data) == 0 || is.na(expr) || trimws(expr) == "") {
    return(list(
      ok = FALSE,
      flag = rep(FALSE, if (is.data.frame(data)) nrow(data) else 0),
      error = "Empty data or empty check_to_perform expression."
    ))
  }
  
  out <- tryCatch(
    eval(parse(text = expr), envir = data, enclos = parent.frame()),
    error = function(e) e
  )
  
  if (inherits(out, "error")) {
    return(list(
      ok = FALSE,
      flag = rep(FALSE, nrow(data)),
      error = conditionMessage(out)
    ))
  }
  
  if (length(out) == 0) {
    return(list(
      ok = FALSE,
      flag = rep(FALSE, nrow(data)),
      error = "Expression returned length 0."
    ))
  }
  
  if (length(out) == 1) {
    out <- rep(out, nrow(data))
  }
  
  if (length(out) != nrow(data)) {
    return(list(
      ok = FALSE,
      flag = rep(FALSE, nrow(data)),
      error = paste0("Expression returned length ", length(out), " but data has ", nrow(data), " rows.")
    ))
  }
  
  out <- as.logical(out)
  out[is.na(out)] <- FALSE
  
  list(
    ok = TRUE,
    flag = out,
    error = NA_character_
  )
}

run_manual_logical_checks <- function(data, dataset_name, list_of_check, uuid_col = "uuid") {
  if (!is.data.frame(data) || nrow(data) == 0) {
    return(empty_log_df())
  }
  
  list_of_check <- filter_logical_checks_for_data(list_of_check, data)
  
  if (!is.data.frame(list_of_check) || nrow(list_of_check) == 0) {
    return(empty_log_df())
  }
  
  data <- tibble::as_tibble(data)
  
  if (!uuid_col %in% names(data)) {
    uuid_col <- find_uuid_col(data, dataset_name)
  }
  
  if (is.na(uuid_col) || !uuid_col %in% names(data)) {
    data$uuid_manual_logical_check <- paste0(dataset_name, "_row_", seq_len(nrow(data)))
    uuid_col <- "uuid_manual_logical_check"
  }
  
  purrr::map_dfr(seq_len(nrow(list_of_check)), function(ii) {
    
    chk <- list_of_check[ii, , drop = FALSE]
    
    check_id_i <- as.character(chk$check_id[[1]] %||% paste0(dataset_name, "_logical_", ii))
    description_i <- as.character(chk$description[[1]] %||% "Logical consistency issue.")
    expr_i <- as.character(chk$check_to_perform[[1]] %||% "")
    cols_i <- split_check_vars(chk$columns_to_clean[[1]] %||% "")
    cols_found_i <- cols_i[cols_i %in% names(data)]
    
    eval_i <- evaluate_logical_expression_vector(data, expr_i)
    
    if (!isTRUE(eval_i$ok)) {
      # Keep an explicit diagnostic row instead of silently dropping the check.
      error_question_cols <- unique(cols_i)
      error_question_cols <- error_question_cols[!is.na(error_question_cols) & trimws(error_question_cols) != ""]
      if (length(error_question_cols) == 0) error_question_cols <- NA_character_
      
      return(purrr::map_dfr(error_question_cols, function(question_i) {
        tibble::tibble(
          source_dataset = source_dataset_label(dataset_name),
          dataset = dataset_name,
          uuid = NA_character_,
          parent_uuid = NA_character_,
          repeat_index = NA_character_,
          question = question_i,
          old_value = NA_character_,
          new_value = NA_character_,
          change_type = NA_character_,
          issue = paste0("LOGICAL CHECK COULD NOT RUN: ", eval_i$error),
          check_id = paste0(check_id_i, "_ERROR"),
          check_to_perform = expr_i,
          columns_to_clean = question_i,
          notes = NA_character_
        )
      }))
    }
    
    flagged <- data[eval_i$flag %in% TRUE, , drop = FALSE]
    
    if (!is.data.frame(flagged) || nrow(flagged) == 0) {
      return(empty_log_df())
    }
    
    make_one_question_rows_for_check(
      flagged = flagged,
      dataset_name = dataset_name,
      uuid_col = uuid_col,
      question_cols = cols_found_i,
      check_id_i = check_id_i,
      description_i = description_i,
      expr_i = expr_i,
      notes_i = NA_character_
    )
  }) |>
    normalise_msna_log(dataset_name)
}

run_logical_cleaningtools_checks <- function(data, dataset_name, list_of_check, uuid_col) {
  if (!is.data.frame(data) || nrow(data) == 0) {
    return(list(checked_dataset = data, cleaning_log = empty_log_df()))
  }
  
  # Primary engine: manual vectorised evaluation of check_to_perform.
  # This avoids empty logs caused by cleaningtools expression parsing/format issues.
  manual_logical_log <- run_manual_logical_checks(
    data = data,
    dataset_name = dataset_name,
    list_of_check = list_of_check,
    uuid_col = uuid_col
  )
  
  list(
    checked_dataset = data,
    cleaning_log = manual_logical_log
  )
}

run_one_dataset_template_structure <- function(dataset_name) {
  cat("\n#==============================================================================\n", sep = "")
  cat("Running checks for dataset: ", dataset_name, "\n", sep = "")
  
  data <- raw_data_list[[dataset_name]]
  if (!is.data.frame(data) || nrow(data) == 0) {
    cat("Skipping dataset: ", dataset_name, " | records: 0\n", sep = "")
    return(list(
      checked_dataset = tibble::tibble(),
      cleaning_log = empty_log_df(),
      basic_log = empty_log_df(),
      logical_log = empty_log_df(),
      missingness_50_log = empty_log_df()
    ))
  }
  
  data <- ensure_uuid_cols(data, dataset_name)
  uuid_col <- find_uuid_col(data, dataset_name)
  if (is.na(uuid_col) || !uuid_col %in% names(data)) uuid_col <- "uuid"
  
  # Add columns to the df: percentage_missing follows the cleaningtools template.
  if (exists("add_percentage_missing", mode = "function")) {
    data <- tryCatch(data |> add_percentage_missing(), error = function(e) data)
  } else if (exists("add_percentage_missing_msna", mode = "function")) {
    data <- tryCatch(data |> add_percentage_missing_msna(), error = function(e) data)
  }
  
  # Add duration only on the household-level sheet.
  if (dataset_name == "Submissions") {
    if (exists("add_duration", mode = "function")) {
      data <- tryCatch(data |> add_duration(), error = function(e) data)
    } else if (exists("add_duration_msna", mode = "function")) {
      data <- tryCatch(data |> add_duration_msna(), error = function(e) data)
    }
    if (!"duration" %in% names(data) && "duration_minutes" %in% names(data)) data$duration <- data$duration_minutes
  }
  
  # Add the explicit MSNA high-missingness percentage used for the 50% threshold flag.
  # This is intentionally restricted to household-level submissions only.
  if (dataset_name == "Submissions") {
    data <- add_msna_missingness_percent(data, dataset_name)
  }
  data <- ensure_uuid_cols(data, dataset_name)
  raw_data_list[[dataset_name]] <<- data
  
  # Count 999.0 / special values for this dataset.
  special_counts_by_dataset[[dataset_name]] <<- count_999_by_variable(data) |>
    dplyr::mutate(dataset = dataset_name, .before = 1)
  
  # Check percentage missing, duration, specific values and others using cleaningtools.
  combined_basic <- run_basic_cleaningtools_checks(data, dataset_name, uuid_col)
  data_checked_basic <- ensure_uuid_cols(combined_basic$checked_dataset, dataset_name)
  raw_data_list[[dataset_name]] <<- data_checked_basic
  
  # Additional high-missingness threshold flag. Run only at household level.
  missingness_50_log <- if (dataset_name == "Submissions") {
    make_missingness_50_log(
      df = data_checked_basic,
      dataset_name = dataset_name,
      uuid_col = uuid_col,
      threshold = missingness_threshold_percent
    )
  } else {
    empty_log_df()
  }
  
  # Check the data with the predefined logical-check list.
  list_of_check <- checks_by_sheet[[dataset_name]] %||% tibble::tibble()
  
  # Checking with logical list.
  combined_logical <- run_logical_cleaningtools_checks(
    data = data_checked_basic,
    dataset_name = dataset_name,
    list_of_check = list_of_check,
    uuid_col = uuid_col
  )
  
  checked_dataset_final <- ensure_uuid_cols(combined_logical$checked_dataset, dataset_name)
  
  dataset_cleaning_log <- combine_msna_logs(list(
    basic = combined_basic$cleaning_log,
    missingness_50 = missingness_50_log,
    logical = combined_logical$cleaning_log
  )) |>
    dplyr::distinct(dataset, uuid, parent_uuid, repeat_index, question, issue, check_id, .keep_all = TRUE)
  
  list(
    checked_dataset = checked_dataset_final,
    cleaning_log = dataset_cleaning_log,
    basic_log = normalise_msna_log(combined_basic$cleaning_log, dataset_name),
    logical_log = normalise_msna_log(combined_logical$cleaning_log, dataset_name),
    missingness_50_log = normalise_msna_log(missingness_50_log, dataset_name)
  )
}

# --------------------------------------------------------------------------------------------------
# 5. Run the template workflow for all final-tool sheets
# --------------------------------------------------------------------------------------------------

special_counts_by_dataset <- list()
results_by_dataset <- purrr::map(expected_datasets, run_one_dataset_template_structure) |>
  purrr::set_names(expected_datasets)

checked_datasets_by_dataset <- purrr::map(results_by_dataset, "checked_dataset")
checked_datasets_by_dataset <- purrr::map(checked_datasets_by_dataset, standardise_admin2_in_df)
checked_datasets_by_dataset <- align_repeat_datasets_to_kobo_server_submission_uuid(checked_datasets_by_dataset)
combined_checked_datasets_by_dataset <- checked_datasets_by_dataset

raw_data_list <- checked_datasets_by_dataset

data_hh_main <- raw_data_list$Submissions %||% tibble::tibble()
data_hh_roster <- raw_data_list$roster %||% tibble::tibble()
data_roster <- raw_data_list$roster %||% tibble::tibble()
data_edu_ind <- raw_data_list$edu_ind %||% tibble::tibble()
data_health_ind <- raw_data_list$health_ind %||% tibble::tibble()
data_nutrition_ind <- raw_data_list$nutrition_ind %||% tibble::tibble()
data_nut_ind <- data_nutrition_ind
data_child_protection_ind <- raw_data_list$child_protection_ind %||% tibble::tibble()
data_prot_ind <- data_child_protection_ind

data <- data_hh_main
raw_data <- data_hh_main

basic_logs <- purrr::map(results_by_dataset, "basic_log")
logical_logs <- purrr::map(results_by_dataset, "logical_log")
missingness_50_logs <- purrr::map(results_by_dataset, "missingness_50_log")

# --------------------------------------------------------------------------------------------------
# 6. Optional cross-sheet checks
# --------------------------------------------------------------------------------------------------

cross_sheet_log <- empty_log_df()

if (is.data.frame(cross_checks) && nrow(cross_checks) > 0) {
  if (exists("run_cross_sheet_checks", mode = "function")) {
    cross_sheet_log <- normalise_msna_log(
      run_cross_sheet_checks(raw_data_list, cross_checks),
      "cross_sheet"
    )
  } else {
    cross_sheet_log <- tibble::tibble(
      source_dataset = "cross_sheet",
      dataset = "cross_sheet",
      uuid = NA_character_,
      parent_uuid = NA_character_,
      repeat_index = NA_character_,
      question = NA_character_,
      old_value = NA_character_,
      new_value = NA_character_,
      change_type = NA_character_,
      issue = "Cross-sheet checks could not run because run_cross_sheet_checks() is not loaded.",
      check_id = "CROSS_SHEET_FUNCTION_MISSING",
      check_to_perform = "run_cross_sheet_checks(raw_data_list, cross_checks)",
      columns_to_clean = NA_character_,
      notes = NA_character_
    )
  }
}

soft_duplicate_log_norm <- if (exists("soft_duplicate_log")) {
  normalise_msna_log(soft_duplicate_log, "Submissions")
} else {
  empty_log_df()
}

# --------------------------------------------------------------------------------------------------
# 7. Create one combined log and add household context
# --------------------------------------------------------------------------------------------------

combined_log_df <- combine_msna_logs(c(
  purrr::set_names(purrr::map(results_by_dataset, "cleaning_log"), names(results_by_dataset)),
  list(cross_sheet = cross_sheet_log),
  list(soft_duplicates = soft_duplicate_log_norm)
)) |>
  dplyr::mutate(log_created_at = Sys.time()) |>
  dplyr::distinct(dataset, uuid, parent_uuid, repeat_index, question, issue, check_id, .keep_all = TRUE)

# Final safeguard: remove age-boundary repeat flags that are known to be false positives
# under the final Kobo repeat design.
combined_log_df <- drop_non_sensible_repeat_age_flags(combined_log_df)

# Final cleaning-log policy: leave notes blank in the cleaning log.
# The issue, check_id and check_to_perform columns carry the review guidance.
if (is.data.frame(combined_log_df) && nrow(combined_log_df) > 0) {
  if (!"notes" %in% names(combined_log_df)) {
    combined_log_df$notes <- NA_character_
  }
  combined_log_df$notes <- NA_character_
}

main_context_vars <- c(
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

build_main_context_by_key <- function(main_df) {
  if (!is.data.frame(main_df) || nrow(main_df) == 0) return(tibble::tibble())
  main_id_cols <- intersect(c("uuid", "_uuid", "_id", "instance_name", "_submission__id", "_submission_id"), names(main_df))
  context_cols <- intersect(main_context_vars, names(main_df))
  context_cols <- setdiff(context_cols, main_id_cols)
  if (length(main_id_cols) == 0) return(tibble::tibble())
  
  tmp <- main_df |>
    dplyr::select(dplyr::any_of(c(main_id_cols, context_cols))) |>
    dplyr::mutate(dplyr::across(dplyr::all_of(main_id_cols), as.character))
  
  purrr::map_dfr(main_id_cols, function(id_col) {
    tmp |>
      dplyr::filter(!is.na(.data[[id_col]]), trimws(.data[[id_col]]) != "") |>
      dplyr::transmute(parent_uuid = .data[[id_col]], dplyr::across(dplyr::all_of(context_cols)))
  }) |>
    dplyr::distinct(parent_uuid, .keep_all = TRUE)
}

coalesce_context_suffix <- function(df, suffix) {
  for (cc in main_context_vars) {
    joined <- paste0(cc, suffix)
    if (joined %in% names(df)) {
      if (cc %in% names(df)) {
        df[[cc]] <- dplyr::coalesce(as.character(df[[cc]]), as.character(df[[joined]]))
      } else {
        df[[cc]] <- as.character(df[[joined]])
      }
    }
  }
  df |>
    dplyr::select(-dplyr::any_of(paste0(main_context_vars, suffix)))
}

if (nrow(data_hh_main) > 0 && nrow(combined_log_df) > 0) {
  main_context_by_parent_key <- build_main_context_by_key(data_hh_main)
  if (nrow(main_context_by_parent_key) > 0 && "parent_uuid" %in% names(combined_log_df)) {
    combined_log_df <- combined_log_df |>
      dplyr::mutate(parent_uuid = as.character(parent_uuid)) |>
      dplyr::left_join(main_context_by_parent_key, by = "parent_uuid", suffix = c("", "_parent")) |>
      coalesce_context_suffix("_parent")
  }
  
  if ("uuid" %in% names(data_hh_main) && "uuid" %in% names(combined_log_df)) {
    submission_context_cols <- intersect(main_context_vars, names(data_hh_main))
    submission_context <- data_hh_main |>
      dplyr::mutate(uuid = as.character(uuid)) |>
      dplyr::select(dplyr::any_of(c("uuid", submission_context_cols))) |>
      dplyr::distinct(uuid, .keep_all = TRUE)
    
    combined_log_df <- combined_log_df |>
      dplyr::mutate(uuid = as.character(uuid)) |>
      dplyr::left_join(submission_context, by = "uuid", suffix = c("", "_main")) |>
      coalesce_context_suffix("_main")
  }
}

# Standardise district values in the final cleaning log as well, so Script 5 exports each district once.
combined_log_df <- standardise_admin2_in_df(combined_log_df)

# Add more detail in the description of the duration issue, as in the template.
if (nrow(combined_log_df) > 0 && "issue" %in% names(combined_log_df)) {
  suppressWarnings({
    duration_value <- safe_numeric(stringr::word(combined_log_df$old_value, 1))
    combined_log_df$issue <- ifelse(
      stringr::str_detect(as.character(combined_log_df$issue), "Duration|duration") &
        !is.na(duration_value) &
        duration_value > 120,
      "The survey took more than 120 minutes",
      combined_log_df$issue
    )
    combined_log_df$issue <- ifelse(
      stringr::str_detect(as.character(combined_log_df$issue), "Duration|duration") &
        !is.na(duration_value) &
        duration_value < 10,
      "The survey took less than 10 minutes",
      combined_log_df$issue
    )
  })
}

# Remove duplicates in the same spirit as the template, but keep dataset/repeat identity.
combined_log_df <- combined_log_df |>
  dplyr::distinct(dataset, uuid, parent_uuid, repeat_index, question, issue, check_id, .keep_all = TRUE)

if (is.data.frame(combined_log_df) && nrow(combined_log_df) > 0) {
  if (!"notes" %in% names(combined_log_df)) {
    combined_log_df$notes <- NA_character_
  }
  combined_log_df$notes <- NA_character_
}

# --------------------------------------------------------------------------------------------------
# 8. Summaries and objects for later scripts
# --------------------------------------------------------------------------------------------------

combined_log_by_dataset <- if (nrow(combined_log_df) > 0) {
  tmp <- combined_log_df |>
    dplyr::group_split(dataset)
  names(tmp) <- combined_log_df |>
    dplyr::distinct(dataset) |>
    dplyr::pull(dataset)
  tmp
} else {
  list()
}

check_summary <- combined_log_df |>
  dplyr::count(source_dataset, dataset, check_id, issue, name = "n_flags") |>
  dplyr::arrange(source_dataset, dataset, dplyr::desc(n_flags))

cleaning_dashboard <- combined_log_df |>
  dplyr::count(source_dataset, dataset, issue, name = "n_flags") |>
  dplyr::arrange(source_dataset, dataset, dplyr::desc(n_flags))

# Missingness summary is also restricted to household-level data, matching the flagging rule.
missingness_summary <- purrr::imap_dfr(raw_data_list["Submissions"], function(df, nm) {
  if (!is.data.frame(df) || nrow(df) == 0) {
    return(tibble::tibble(dataset = nm, variable = NA_character_, percent_missing = NA_real_))
  }
  df |>
    dplyr::summarise(dplyr::across(dplyr::everything(), ~ mean(is.na(.x) | trimws(as.character(.x)) == "") * 100)) |>
    tidyr::pivot_longer(cols = dplyr::everything(), names_to = "variable", values_to = "percent_missing") |>
    dplyr::mutate(dataset = nm, .before = 1) |>
    dplyr::arrange(dataset, dplyr::desc(percent_missing))
})

repeat_count_summary <- if (exists("make_repeat_count_summary", mode = "function")) {
  make_repeat_count_summary(raw_data_list)
} else {
  purrr::imap_dfr(raw_data_list, function(df, nm) {
    tibble::tibble(
      dataset = nm,
      records = nrow(df),
      unique_parent_uuid = if ("parent_uuid" %in% names(df)) dplyr::n_distinct(df$parent_uuid, na.rm = TRUE) else NA_integer_,
      unique_uuid = if ("uuid" %in% names(df)) dplyr::n_distinct(df$uuid, na.rm = TRUE) else NA_integer_
    )
  })
}

special_value_summary <- dplyr::bind_rows(special_counts_by_dataset)

logical_check_summary <- logical_check_variable_availability |>
  dplyr::count(
    dataset,
    all_columns_to_clean_found,
    any_columns_to_clean_found,
    all_expression_variables_found,
    name = "n_checks"
  ) |>
  dplyr::arrange(dataset, all_columns_to_clean_found, any_columns_to_clean_found, all_expression_variables_found)



logical_check_run_summary <- purrr::imap_dfr(
  results_by_dataset,
  function(res, nm) {
    
    check_df <- checks_by_sheet[[nm]] %||% tibble::tibble()
    df <- raw_data_list[[nm]] %||% tibble::tibble()
    
    check_df_filtered <- filter_logical_checks_for_data(
      list_of_check = check_df,
      data = df
    )
    
    tibble::tibble(
      dataset = nm,
      n_checks_in_workbook = if (is.data.frame(check_df)) nrow(check_df) else 0L,
      n_checks_after_variable_filter = if (is.data.frame(check_df_filtered)) nrow(check_df_filtered) else 0L,
      n_logical_flags = if (is.data.frame(res$logical_log)) nrow(res$logical_log) else 0L,
      n_total_flags = if (is.data.frame(res$cleaning_log)) nrow(res$cleaning_log) else 0L
    )
  }
)




admin2_export_summary <- if (is.data.frame(data_hh_main) && nrow(data_hh_main) > 0 && "admin2" %in% names(data_hh_main)) {
  data_hh_main |>
    dplyr::mutate(admin2 = standardise_admin2_value(admin2)) |>
    dplyr::filter(!is.na(admin2), trimws(as.character(admin2)) != "") |>
    dplyr::count(admin2, name = "n_submissions") |>
    dplyr::arrange(admin2)
} else {
  tibble::tibble(admin2 = character(), n_submissions = integer())
}

cleaning_log_all <- combined_log_df
cleaning_log <- cleaning_log_all
cleaning_log_template <- cleaning_log_all
cleaning_logs_by_dataset <- combined_log_by_dataset

cleaning_log_workbook_sheets <- c(
  list(
    cleaning_log_all = cleaning_log_all,
    check_summary = check_summary,
    dashboard = cleaning_dashboard,
    repeat_count_summary = repeat_count_summary,
    missingness_summary = missingness_summary,
    special_value_summary = special_value_summary,
    logical_check_summary = logical_check_summary,
    logical_check_run_summary = logical_check_run_summary,
    logical_check_exclusion_summary = logical_check_exclusion_summary,
    logical_check_variable_availability = logical_check_variable_availability,
    admin2_export_summary = admin2_export_summary
  ),
  cleaning_logs_by_dataset
)

logbook_to_export <- cleaning_log_workbook_sheets
combined_log_to_export <- cleaning_log_workbook_sheets

combined_log <- list(
  checked_dataset = data_hh_main,
  cleaning_log = cleaning_log_all
)

combined_log_cleaningtools_style <- combined_log

cat("\nCleaning logs prepared in memory only; no Excel export was run in this script.\n", sep = "")
cat("Objects ready for the next script:\n")
cat(" - combined_log / combined_log_cleaningtools_style: cleaningtools-style list\n")
cat(" - cleaning_log_all / cleaning_log / cleaning_log_template: one combined data-frame cleaning log\n")
cat(" - cleaning_logs_by_dataset / combined_log_by_dataset: separate logs by dataset/repeat\n")
cat(" - checked_datasets_by_dataset / combined_checked_datasets_by_dataset: checked data by sheet\n")
cat(" - logical_check_variable_availability: check-variable validation table\n")
cat(" - logical_check_run_summary: checks loaded, checks usable, and logical flags by dataset\n")
cat(" - admin2_export_summary: canonical district values used by Script 5\n")
cat("Total flags generated: ", nrow(cleaning_log_all), "\n", sep = "")

if (nrow(check_summary) > 0) {
  cat("\nFlags by dataset:\n")
  print(
    check_summary |>
      dplyr::group_by(source_dataset, dataset) |>
      dplyr::summarise(flags = sum(n_flags), .groups = "drop")
  )
}

if (nrow(logical_check_summary) > 0) {
  cat("\nLogical-check variable availability summary:\n")
  print(logical_check_summary)
}

cat("\n\n\n\n\n\n\n")
cat(green$bold("Checks completed"))
cat("\n\n\n")

# --------------------------------------------------------------------------------------------------
# 9. Report new objects created
# --------------------------------------------------------------------------------------------------

all_dfs_names_after <- Filter(function(x) is(x, "data.frame"), mget(ls())) |> names()
all_lists_names_after <- Filter(function(x) is(x, "list"), mget(ls())) |> names()

if (length(setdiff(all_dfs_names_after, all_dfs_names_before)) > 0) {
  cat(blue("The new dataframes created are:"))
  cat("\n")
  for (df in setdiff(all_dfs_names_after, all_dfs_names_before)) {
    cat(df)
    cat("\n")
  }
}

if (length(setdiff(all_lists_names_after, all_lists_names_before)) > 0) {
  cat(blue("The new lists created are:"))
  cat("\n")
  for (df in setdiff(all_lists_names_after, all_lists_names_before)) {
    cat(df)
    cat("\n")
  }
}