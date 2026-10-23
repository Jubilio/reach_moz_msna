cat("\014")

all_dfs_names_before <- Filter(function(x) is(x, "data.frame"), mget(ls())) |> names()
all_lists_names_before <- Filter(function(x) is(x, "list"), mget(ls())) |> names()

# --------------------------------------------------------------------------------------------------
# 5. EXPORT THE LOGBOOKS - MSNA 2026 UGANDA
# --------------------------------------------------------------------------------------------------
# Purpose:
#   Export one cleaning log workbook for each district/admin2.
#
# Structure followed from the cleaningtools template:
#   1) Prepare a cleaningtools-style combined_log object.
#   2) Export using cleaningtools::create_xlsx_cleaning_log().
#   3) Use cleaning_log_name = "cleaning_log".
#   4) Use column_for_color = "issue".
#   5) Print completion message and report new objects created.
#
# MSNA-specific additions kept:
#   - One district-specific cleaning log workbook per admin2.
#   - No national workbook.
#   - No export index workbook.
#   - README sheet added only to each district cleaning-log workbook.
#   - Active dropdown validation on cleaning_log$change_type.
#   - change_type left blank for reviewers to select manually.
#   - Final cleaning_log export column order:
#       source_dataset, dataset, uuid, enum_id, point_number, question, issue,
#       old_value, new_value, notes, change_type, admin1, admin2, admin3,
#       admin3_setlement, admin4, hoh_type, parent_uuid, repeat_index.
# --------------------------------------------------------------------------------------------------

print_step("5. Exporting district-specific MSNA 2026 Uganda cleaning logbooks using cleaningtools structure")

# --------------------------------------------------------------------------------------------------
# 0. Safety checks
# --------------------------------------------------------------------------------------------------

if (!exists("path_cleaning_folder_today")) {
  stop(
    "Object 'path_cleaning_folder_today' was not found. Run the Master cleaning script first.",
    call. = FALSE
  )
}

if (!dir.exists(path_cleaning_folder_today)) {
  dir.create(path_cleaning_folder_today, recursive = TRUE)
}

required_packages <- c(
  "dplyr",
  "tibble",
  "purrr",
  "tidyr",
  "stringr",
  "openxlsx",
  "janitor",
  "cleaningtools"
)

for (pkg in required_packages) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    stop("Package '", pkg, "' is required for Script 5.", call. = FALSE)
  }
}

if (!exists("green") && requireNamespace("crayon", quietly = TRUE)) green <- crayon::green
if (!exists("blue") && requireNamespace("crayon", quietly = TRUE)) blue <- crayon::blue

# --------------------------------------------------------------------------------------------------
# 1. Validation guide and helper functions
# --------------------------------------------------------------------------------------------------

change_type_dropdown_options <- c(
  "change_response",
  "blank_response",
  "remove_survey",
  "no_action"
)

make_change_type_readme <- function() {
  tibble::tibble(
    change_type_validation = c(
      "change_response",
      "blank_response",
      "remove_survey",
      "no_action"
    ),
    description = c(
      "Change the response to new.value",
      "Remove and NA the response",
      "Delete the survey",
      "No action to take."
    )
  )
}

change_type_readme <- make_change_type_readme()

empty_cleaning_log_export <- function() {
  tibble::tibble(
    source_dataset = character(),
    dataset = character(),
    uuid = character(),
    enum_id = character(),
    point_number = character(),
    question = character(),
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
    check_id = character(),
    check_to_perform = character(),
    columns_to_clean = character()
  )
}

as_cleaning_log_df <- function(x) {
  
  if (is.null(x)) {
    return(empty_cleaning_log_export())
  }
  
  if (is.list(x) && !is.data.frame(x) && "cleaning_log" %in% names(x)) {
    return(tibble::as_tibble(x$cleaning_log))
  }
  
  if (is.data.frame(x)) {
    return(tibble::as_tibble(x))
  }
  
  empty_cleaning_log_export()
}

safe_df_for_excel <- function(df) {
  
  if (is.null(df) || !is.data.frame(df)) {
    return(tibble::tibble(message = "No records"))
  }
  
  df <- tibble::as_tibble(df)
  
  if (ncol(df) == 0) {
    return(tibble::tibble(message = "No records"))
  }
  
  df |>
    dplyr::mutate(
      dplyr::across(
        dplyr::everything(),
        ~ {
          if (inherits(.x, "POSIXct") || inherits(.x, "POSIXt") || inherits(.x, "Date")) {
            as.character(.x)
          } else if (is.list(.x)) {
            as.character(.x)
          } else {
            .x
          }
        }
      )
    )
}


make_valid_sheet_names <- function(x) {
  
  x <- as.character(x)
  x <- stringr::str_replace_all(x, "[\\[\\]\\*\\?/\\\\:]", "_")
  x <- stringr::str_replace_all(x, "^\\s+|\\s+$", "")
  x <- ifelse(is.na(x) | x == "", "sheet", x)
  x <- substr(x, 1, 31)
  
  out <- character(length(x))
  used <- character()
  
  for (i in seq_along(x)) {
    
    candidate <- x[[i]]
    
    if (!candidate %in% used) {
      out[[i]] <- candidate
      used <- c(used, candidate)
    } else {
      
      suffix_id <- 2
      
      repeat {
        suffix <- paste0("_", suffix_id)
        base <- substr(candidate, 1, 31 - nchar(suffix))
        candidate2 <- paste0(base, suffix)
        
        if (!candidate2 %in% used) {
          out[[i]] <- candidate2
          used <- c(used, candidate2)
          break
        }
        
        suffix_id <- suffix_id + 1
      }
    }
  }
  
  out
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

source_dataset_label <- function(x) {
  
  x <- as.character(x)
  
  dplyr::case_when(
    is.na(x) | trimws(x) == "" ~ NA_character_,
    x %in% c("Submissions", "household", "household_level", "data_hh_main") ~ "household_level",
    x %in% c("roster", "data_hh_roster", "data_roster") ~ "roster",
    x %in% c("edu_ind", "data_edu_ind") ~ "edu_ind",
    x %in% c("health_ind", "data_health_ind") ~ "health_ind",
    x %in% c("nutrition_ind", "nut_ind", "data_nutrition_ind", "data_nut_ind") ~ "nutrition_ind",
    x %in% c("child_protection_ind", "prot_ind", "data_child_protection_ind", "data_prot_ind") ~ "child_protection_ind",
    x %in% c("cross_sheet", "soft_duplicates") ~ x,
    TRUE ~ x
  )
}

# --------------------------------------------------------------------------------------------------
# 2. Cleaning-log and checked-dataset standardisation
# --------------------------------------------------------------------------------------------------

standardise_checked_dataset <- function(df, dataset_name = NA_character_) {
  
  if (is.null(df) || !is.data.frame(df)) {
    return(tibble::tibble())
  }
  
  df <- tibble::as_tibble(df)
  
  if (!"uuid" %in% names(df)) {
    uuid_candidates <- c("uuid", "_uuid", "_id", "instance_name", "_submission__id", "_submission_id")
    uuid_hit <- uuid_candidates[uuid_candidates %in% names(df)][1]
    df$uuid <- if (!is.na(uuid_hit)) as.character(df[[uuid_hit]]) else NA_character_
  } else {
    df$uuid <- as.character(df$uuid)
  }
  
  if (!"parent_uuid" %in% names(df)) {
    parent_candidates <- c(
      "parent_uuid",
      "_parent_uuid",
      "_submission_uuid",
      "_submission__uuid",
      "_submission__id",
      "_submission_id",
      "parent_instance_name"
    )
    parent_hit <- parent_candidates[parent_candidates %in% names(df)][1]
    df$parent_uuid <- if (!is.na(parent_hit)) as.character(df[[parent_hit]]) else NA_character_
  } else {
    df$parent_uuid <- as.character(df$parent_uuid)
  }
  
  if (!"source_dataset" %in% names(df)) {
    df$source_dataset <- source_dataset_label(dataset_name)
  }
  
  df
}

standardise_log_for_export <- function(log_df) {
  
  log_df <- as_cleaning_log_df(log_df)
  
  if (nrow(log_df) == 0 && ncol(log_df) == 0) {
    return(empty_cleaning_log_export())
  }
  
  log_df <- log_df |>
    janitor::clean_names()
  
  if (!"dataset" %in% names(log_df)) {
    if ("source_dataset" %in% names(log_df)) {
      log_df$dataset <- as.character(log_df$source_dataset)
    } else {
      log_df$dataset <- "Submissions"
    }
  }
  
  if (!"source_dataset" %in% names(log_df)) {
    log_df$source_dataset <- as.character(log_df$dataset)
  }
  
  # Sometimes the input cleaning log already contains submitted_by after janitor cleaning.
  if (!"enum_id" %in% names(log_df)) {
    log_df$enum_id <- NA_character_
  }
  
  if ("enumerator_id" %in% names(log_df)) {
    log_df$enum_id <- dplyr::coalesce(
      as.character(log_df$enum_id),
      as.character(log_df$enumerator_id)
    )
  }
  
  if ("username" %in% names(log_df)) {
    log_df$enum_id <- dplyr::coalesce(
      as.character(log_df$enum_id),
      as.character(log_df$username)
    )
  }
  
  if ("submitted_by" %in% names(log_df)) {
    log_df$enum_id <- dplyr::coalesce(
      as.character(log_df$enum_id),
      as.character(log_df$submitted_by)
    )
  }
  
  required_log_cols <- c(
    "source_dataset",
    "dataset",
    "uuid",
    "enum_id",
    "point_number",
    "question",
    "issue",
    "old_value",
    "new_value",
    "notes",
    "change_type",
    "admin1",
    "admin2",
    "admin3",
    "admin3_setlement",
    "admin4",
    "hoh_type",
    "parent_uuid",
    "repeat_index",
    "check_id",
    "check_to_perform",
    "columns_to_clean"
  )
  
  for (cc in required_log_cols) {
    if (!cc %in% names(log_df)) {
      log_df[[cc]] <- NA_character_
    }
  }
  
  log_df |>
    dplyr::mutate(
      dplyr::across(dplyr::any_of(required_log_cols), as.character),
      dataset = dplyr::case_when(
        is.na(dataset) | trimws(dataset) == "" ~ dplyr::if_else(
          !is.na(source_dataset) & trimws(source_dataset) != "",
          source_dataset,
          "Submissions"
        ),
        TRUE ~ dataset
      ),
      dataset = source_dataset_label(dataset),
      source_dataset = source_dataset_label(source_dataset),
      source_dataset = dplyr::case_when(
        is.na(source_dataset) | trimws(source_dataset) == "" ~ source_dataset_label(dataset),
        TRUE ~ source_dataset
      )
    )
}

format_cleaning_log_for_export <- function(log_df) {
  
  log_df <- standardise_log_for_export(log_df)
  
  export_cols <- c(
    "source_dataset",
    "dataset",
    "uuid",
    "enum_id",
    "point_number",
    "question",
    "issue",
    "old_value",
    "new_value",
    "notes",
    "change_type",
    "admin1",
    "admin2",
    "admin3",
    "admin3_setlement",
    "admin4",
    "hoh_type",
    "parent_uuid",
    "repeat_index"
  )
  
  for (cc in export_cols) {
    if (!cc %in% names(log_df)) {
      log_df[[cc]] <- NA_character_
    }
  }
  
  # Important: leave change_type blank in all exported cleaning logs.
  # Reviewers will select the action manually using the active dropdown.
  log_df$change_type <- NA_character_
  
  log_df |>
    dplyr::mutate(
      dplyr::across(dplyr::any_of(export_cols), as.character)
    ) |>
    dplyr::arrange(source_dataset, dataset, uuid, question, issue) |>
    dplyr::select(dplyr::all_of(export_cols))
}

build_household_context_lookup <- function(main_data) {
  
  if (!is.data.frame(main_data) || nrow(main_data) == 0) {
    return(tibble::tibble(.join_key = character()))
  }
  
  main_data <- tibble::as_tibble(main_data)
  
  main_id_cols <- intersect(
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
    names(main_data)
  )
  
  if (length(main_id_cols) == 0) {
    return(tibble::tibble(.join_key = character()))
  }
  
  context_cols <- intersect(
    c(
      "admin1",
      "admin2",
      "admin3",
      "admin3_setlement",
      "admin4",
      "point_number",
      "enum_id",
      "enum_gender",
      "username",
      "_submitted_by",
      "submitted_by",
      "_submission_time",
      "submission_time",
      "start",
      "end",
      "today",
      "hoh_type",
      "resp_gender",
      "resp_age",
      "hh_size"
    ),
    names(main_data)
  )
  
  purrr::map_dfr(
    main_id_cols,
    function(id_col) {
      
      main_data |>
        dplyr::mutate(.join_key = as.character(.data[[id_col]])) |>
        dplyr::filter(!is.na(.join_key), trimws(.join_key) != "") |>
        dplyr::select(.join_key, dplyr::any_of(context_cols))
    }
  ) |>
    dplyr::distinct(.join_key, .keep_all = TRUE)
}

fill_joined_context_cols <- function(df, suffix) {
  
  context_cols <- c(
    "admin1",
    "admin2",
    "admin3",
    "admin3_setlement",
    "admin4",
    "point_number",
    "enum_id",
    "enum_gender",
    "username",
    "_submitted_by",
    "submitted_by",
    "_submission_time",
    "submission_time",
    "start",
    "end",
    "today",
    "hoh_type",
    "resp_gender",
    "resp_age",
    "hh_size"
  )
  
  for (cc in context_cols) {
    
    joined_cc <- paste0(cc, suffix)
    
    if (joined_cc %in% names(df)) {
      
      if (cc %in% names(df)) {
        df[[cc]] <- dplyr::coalesce(
          as.character(df[[cc]]),
          as.character(df[[joined_cc]])
        )
      } else {
        df[[cc]] <- as.character(df[[joined_cc]])
      }
    }
  }
  
  df |>
    dplyr::select(-dplyr::any_of(paste0(context_cols, suffix)))
}

attach_household_context_to_log <- function(log_df, main_data) {
  
  log_df <- standardise_log_for_export(log_df)
  
  if (!is.data.frame(main_data) || nrow(main_data) == 0) {
    return(format_cleaning_log_for_export(log_df))
  }
  
  household_context <- build_household_context_lookup(main_data)
  
  if (nrow(household_context) == 0) {
    return(format_cleaning_log_for_export(log_df))
  }
  
  # Add direct context for household-level flags by uuid.
  log_df <- log_df |>
    dplyr::mutate(uuid = as.character(uuid)) |>
    dplyr::left_join(
      household_context,
      by = c("uuid" = ".join_key"),
      suffix = c("", "_hh_uuid")
    ) |>
    fill_joined_context_cols("_hh_uuid")
  
  # Add household context for repeat-level flags by parent_uuid.
  log_df <- log_df |>
    dplyr::mutate(parent_uuid = as.character(parent_uuid)) |>
    dplyr::left_join(
      household_context,
      by = c("parent_uuid" = ".join_key"),
      suffix = c("", "_hh_parent")
    ) |>
    fill_joined_context_cols("_hh_parent")
  
  if (!"enum_id" %in% names(log_df)) {
    log_df$enum_id <- NA_character_
  }
  
  if ("enumerator_id" %in% names(log_df)) {
    log_df$enum_id <- dplyr::coalesce(
      as.character(log_df$enum_id),
      as.character(log_df$enumerator_id)
    )
  }
  
  if ("username" %in% names(log_df)) {
    log_df$enum_id <- dplyr::coalesce(
      as.character(log_df$enum_id),
      as.character(log_df$username)
    )
  }
  
  if ("submitted_by" %in% names(log_df)) {
    log_df$enum_id <- dplyr::coalesce(
      as.character(log_df$enum_id),
      as.character(log_df$submitted_by)
    )
  }
  
  if (!"point_number" %in% names(log_df)) {
    log_df$point_number <- NA_character_
  }
  
  format_cleaning_log_for_export(log_df)
}

make_check_summary <- function(log_df) {
  
  log_df <- standardise_log_for_export(log_df)
  
  if (nrow(log_df) == 0) {
    return(tibble::tibble(message = "No records"))
  }
  
  log_df |>
    dplyr::count(source_dataset, dataset, check_id, issue, name = "n_flags") |>
    dplyr::arrange(source_dataset, dataset, dplyr::desc(n_flags))
}

filter_checked_dataset_for_district <- function(df, dataset_name, district_name, district_keys) {
  
  if (!is.data.frame(df) || nrow(df) == 0) {
    return(tibble::tibble())
  }
  
  df <- tibble::as_tibble(df)
  
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
      "submission_id"
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

# --------------------------------------------------------------------------------------------------
# 3. Workbook post-processing after cleaningtools export
# --------------------------------------------------------------------------------------------------

append_extra_sheets_to_cleaningtools_workbook <- function(output_path, extra_sheets = list()) {
  
  if (!file.exists(output_path)) {
    stop("Cannot append sheets. File not found: ", output_path, call. = FALSE)
  }
  
  if (!is.list(extra_sheets) || length(extra_sheets) == 0) {
    return(invisible(output_path))
  }
  
  # Do not append README here. cleaningtools/post-formatting handles README.
  extra_sheets <- extra_sheets[tolower(names(extra_sheets)) != "readme"]
  
  if (length(extra_sheets) == 0) {
    return(invisible(output_path))
  }
  
  wb <- openxlsx::loadWorkbook(output_path)
  
  extra_sheets <- purrr::map(extra_sheets, safe_df_for_excel)
  names(extra_sheets) <- make_valid_sheet_names(names(extra_sheets))
  
  for (sheet_name in names(extra_sheets)) {
    
    existing_match <- names(wb)[tolower(trimws(names(wb))) == tolower(trimws(sheet_name))]
    
    if (length(existing_match) > 0) {
      for (ss in existing_match) {
        openxlsx::removeWorksheet(wb, sheet = ss)
      }
    }
    
    openxlsx::addWorksheet(wb, sheetName = sheet_name, gridLines = TRUE)
    openxlsx::writeData(
      wb,
      sheet = sheet_name,
      x = extra_sheets[[sheet_name]],
      rowNames = FALSE,
      keepNA = FALSE
    )
  }
  
  openxlsx::saveWorkbook(wb, output_path, overwrite = TRUE)
  
  invisible(output_path)
}

apply_impact_format_readme_and_dropdown <- function(output_path,
                                                    include_readme = TRUE,
                                                    include_change_type_dropdown = TRUE,
                                                    cleaning_log_sheet = "cleaning_log",
                                                    change_type_col = "change_type",
                                                    validation_extra_rows = 1000) {
  
  if (!file.exists(output_path)) {
    stop("Cannot format workbook. File not found: ", output_path, call. = FALSE)
  }
  
  wb <- openxlsx::loadWorkbook(output_path)
  
  get_workbook_sheet_names <- function(wb) {
    out <- tryCatch(openxlsx::sheets(wb), error = function(e) NULL)
    if (is.null(out)) out <- tryCatch(names(wb), error = function(e) NULL)
    if (is.null(out)) out <- tryCatch(wb$sheet_names, error = function(e) NULL)
    as.character(out)
  }
  
  # README handling:
  # cleaningtools often creates README already. Do not add a duplicate README.
  # If README exists, reuse it. If it does not exist, create it once.
  if (isTRUE(include_readme)) {
    
    existing_sheets <- get_workbook_sheet_names(wb)
    readme_sheet <- existing_sheets[tolower(trimws(existing_sheets)) == "readme"][1]
    
    if (length(readme_sheet) == 0 || is.na(readme_sheet)) {
      readme_sheet <- "README"
      
      tryCatch(
        {
          openxlsx::addWorksheet(wb, sheetName = readme_sheet, gridLines = TRUE)
        },
        error = function(e) {
          # If Excel/openxlsx says README already exists but it was not detected above,
          # continue and write to README rather than stopping the whole district loop.
          if (!grepl("already exists", conditionMessage(e), ignore.case = TRUE)) {
            stop(e)
          }
        }
      )
    }
    
    try(
      openxlsx::writeData(
        wb,
        sheet = readme_sheet,
        x = change_type_readme,
        rowNames = FALSE,
        keepNA = FALSE
      ),
      silent = TRUE
    )
  }
  
  header_style <- openxlsx::createStyle(
    fontName = "Arial Narrow",
    fontSize = 11,
    fontColour = "white",
    border = "TopBottomLeftRight",
    borderColour = "black",
    halign = "center",
    fgFill = "#E34443",
    textDecoration = "bold",
    wrapText = TRUE,
    valign = "center"
  )
  
  table_body_style <- openxlsx::createStyle(
    fontName = "Arial Narrow",
    fontSize = 7,
    border = "TopBottomLeftRight",
    borderColour = "grey26",
    borderStyle = "thin",
    wrapText = FALSE
  )
  
  wrap_body_style <- openxlsx::createStyle(
    fontName = "Arial Narrow",
    fontSize = 7,
    border = "TopBottomLeftRight",
    borderColour = "grey26",
    borderStyle = "thin",
    wrapText = TRUE,
    valign = "top"
  )
  
  dropdown_style <- openxlsx::createStyle(
    fontName = "Arial Narrow",
    fontSize = 7,
    border = "TopBottomLeftRight",
    borderColour = "grey26",
    borderStyle = "thin",
    fgFill = "#FFF2CC",
    wrapText = FALSE
  )
  
  current_sheets <- get_workbook_sheet_names(wb)
  current_sheets <- current_sheets[!is.na(current_sheets) & current_sheets != ""]
  
  for (sheet_name in current_sheets) {
    
    df <- tryCatch(
      openxlsx::readWorkbook(output_path, sheet = sheet_name),
      error = function(e) tibble::tibble()
    )
    
    if (!is.data.frame(df) || ncol(df) == 0) {
      next
    }
    
    n_cols <- ncol(df)
    n_rows <- nrow(df)
    
    openxlsx::setColWidths(wb, sheet = sheet_name, cols = 1:n_cols, widths = rep(25, n_cols))
    openxlsx::setRowHeights(wb, sheet = sheet_name, rows = 1, heights = 30)
    
    openxlsx::addStyle(
      wb,
      sheet = sheet_name,
      style = header_style,
      rows = 1,
      cols = 1:n_cols,
      gridExpand = TRUE,
      stack = TRUE
    )
    
    if (n_rows > 0) {
      openxlsx::addStyle(
        wb,
        sheet = sheet_name,
        style = table_body_style,
        rows = 2:(n_rows + 1),
        cols = 1:n_cols,
        gridExpand = TRUE,
        stack = TRUE
      )
      
      text_cols <- intersect(
        c("issue", "question", "old_value", "new_value", "check_to_perform", "columns_to_clean", "notes", "description"),
        names(df)
      )
      
      if (length(text_cols) > 0) {
        openxlsx::addStyle(
          wb,
          sheet = sheet_name,
          style = wrap_body_style,
          rows = 2:(n_rows + 1),
          cols = match(text_cols, names(df)),
          gridExpand = TRUE,
          stack = TRUE
        )
      }
    }
    
    try(openxlsx::freezePane(wb, sheet = sheet_name, firstRow = TRUE), silent = TRUE)
    try(openxlsx::addFilter(wb, sheet = sheet_name, rows = 1, cols = 1:n_cols), silent = TRUE)
    
    if (
      isTRUE(include_change_type_dropdown) &&
      sheet_name == cleaning_log_sheet &&
      change_type_col %in% names(df)
    ) {
      
      change_type_col_index <- which(names(df) == change_type_col)[[1]]
      validation_end_row <- max(n_rows + validation_extra_rows + 1, 1001)
      validation_rows <- 2:validation_end_row
      validation_values <- paste0('"', paste(change_type_dropdown_options, collapse = ","), '"')
      
      openxlsx::dataValidation(
        wb,
        sheet = sheet_name,
        cols = change_type_col_index,
        rows = validation_rows,
        type = "list",
        value = validation_values
      )
      
      openxlsx::addStyle(
        wb,
        sheet = sheet_name,
        style = dropdown_style,
        rows = validation_rows,
        cols = change_type_col_index,
        gridExpand = TRUE,
        stack = TRUE
      )
      
      openxlsx::setColWidths(
        wb,
        sheet = sheet_name,
        cols = change_type_col_index,
        widths = 20
      )
    }
  }
  
  openxlsx::saveWorkbook(wb, output_path, overwrite = TRUE)
  
  invisible(output_path)
}


# Force-write a worksheet after cleaningtools export.
# This guarantees that the workbook receives the exact district-level cleaning_log object
# prepared in Script 4, even if cleaningtools creates an empty cleaning_log sheet.
write_or_replace_workbook_sheet <- function(output_path, sheet_name, df) {
  
  if (!file.exists(output_path)) {
    stop("Cannot write worksheet. File not found: ", output_path, call. = FALSE)
  }
  
  wb <- openxlsx::loadWorkbook(output_path)
  
  existing_sheets <- tryCatch(
    openxlsx::sheets(wb),
    error = function(e) names(wb)
  )
  
  existing_hit <- existing_sheets[tolower(existing_sheets) == tolower(sheet_name)]
  
  if (length(existing_hit) > 0) {
    for (existing_sheet in existing_hit) {
      openxlsx::removeWorksheet(wb, sheet = existing_sheet)
    }
  }
  
  openxlsx::addWorksheet(wb, sheetName = sheet_name, gridLines = TRUE)
  openxlsx::writeData(
    wb,
    sheet = sheet_name,
    x = safe_df_for_excel(df),
    rowNames = FALSE,
    keepNA = FALSE
  )
  
  openxlsx::saveWorkbook(wb, output_path, overwrite = TRUE)
  
  invisible(output_path)
}

export_one_district_cleaningtools_logbook <- function(district_combined_log,
                                                      output_path,
                                                      extra_sheets = list()) {
  
  if (!is.list(district_combined_log) ||
      !"checked_dataset" %in% names(district_combined_log) ||
      !"cleaning_log" %in% names(district_combined_log)) {
    stop(
      "district_combined_log must be a cleaningtools-style list with checked_dataset and cleaning_log.",
      call. = FALSE
    )
  }
  
  # This is the same core export structure as the cleaningtools template.
  cleaningtools::create_xlsx_cleaning_log(
    district_combined_log,
    cleaning_log_name = "cleaning_log",
    output_path = output_path,
    column_for_color = "issue"
  )
  
  # Critical safeguard:
  # After cleaningtools creates the workbook, force-write the exact district-level
  # cleaning_log and checked_dataset objects. This prevents empty cleaning_log sheets.
  write_or_replace_workbook_sheet(
    output_path = output_path,
    sheet_name = "checked_dataset",
    df = district_combined_log$checked_dataset
  )
  
  write_or_replace_workbook_sheet(
    output_path = output_path,
    sheet_name = "cleaning_log",
    df = district_combined_log$cleaning_log
  )
  
  append_extra_sheets_to_cleaningtools_workbook(
    output_path = output_path,
    extra_sheets = extra_sheets
  )
  
  apply_impact_format_readme_and_dropdown(
    output_path = output_path,
    include_readme = TRUE,
    include_change_type_dropdown = TRUE,
    cleaning_log_sheet = "cleaning_log",
    change_type_col = "change_type",
    validation_extra_rows = 1000
  )
  
  invisible(output_path)
}

# --------------------------------------------------------------------------------------------------
# 4. Recover district-level cleaning logs prepared by Script 4
# --------------------------------------------------------------------------------------------------

if (!exists("district_combined_logs_by_admin2") ||
    !is.list(district_combined_logs_by_admin2) ||
    length(district_combined_logs_by_admin2) == 0) {
  stop(
    paste0(
      "district_combined_logs_by_admin2 was not found or is empty. ",
      "Run the updated '4.Process the CLs and DFs.R' first so that district-level cleaning logs are split after Script 4."
    ),
    call. = FALSE
  )
}

if (!exists("district_extra_sheets_by_admin2") || !is.list(district_extra_sheets_by_admin2)) {
  district_extra_sheets_by_admin2 <- list()
}

if (!exists("district_workbook_sheets_by_admin2") || !is.list(district_workbook_sheets_by_admin2)) {
  district_workbook_sheets_by_admin2 <- list()
}

# Use all district-specific cleaning-log objects created after Script 4.
district_values <- names(district_combined_logs_by_admin2)
district_values <- district_values[!is.na(district_values) & trimws(as.character(district_values)) != ""]
district_values <- sort(unique(as.character(district_values)))

if (length(district_values) == 0) {
  stop("No district names were found in district_combined_logs_by_admin2.", call. = FALSE)
}

# Combined log for summary/compatibility only.
cleaning_log_all <- purrr::map_dfr(
  district_combined_logs_by_admin2[district_values],
  function(x) {
    if (is.list(x) && "cleaning_log" %in% names(x) && is.data.frame(x$cleaning_log)) {
      return(tibble::as_tibble(x$cleaning_log))
    }
    tibble::tibble()
  }
)

# --------------------------------------------------------------------------------------------------
# 5. Export paths - district workbooks only
# --------------------------------------------------------------------------------------------------

export_time <- format(Sys.time(), "%Y-%m-%d_at_%Hh%M")

district_export_root <- file.path(
  path_cleaning_folder_today,
  "District cleaning logs"
)

if (!dir.exists(district_export_root)) {
  dir.create(district_export_root, recursive = TRUE)
}

# --------------------------------------------------------------------------------------------------
# 6. Export district-specific workbooks by looping through each district cleaning log
# --------------------------------------------------------------------------------------------------

district_export_paths <- list()
district_combined_logs <- list()

for (current_district in district_values) {
  
  current_district_safe <- make_safe_file_name(current_district)
  
  current_district_dir <- file.path(
    district_export_root,
    current_district_safe
  )
  
  if (!dir.exists(current_district_dir)) {
    dir.create(current_district_dir, recursive = TRUE)
  }
  
  district_combined_log <- district_combined_logs_by_admin2[[current_district]]
  
  if (!is.list(district_combined_log) ||
      !"checked_dataset" %in% names(district_combined_log) ||
      !"cleaning_log" %in% names(district_combined_log)) {
    warning("Skipping district '", current_district, "': object does not contain checked_dataset and cleaning_log.")
    next
  }
  
  district_source_n_cleaning_flags <- if (is.data.frame(district_combined_log$cleaning_log)) {
    nrow(district_combined_log$cleaning_log)
  } else {
    0L
  }
  
  if (district_source_n_cleaning_flags == 0L) {
    warning(
      "District '", current_district, "' has zero rows in district_combined_log$cleaning_log before export. ",
      "Check district_export_manifest from Script 4."
    )
  }
  
  district_combined_log$checked_dataset <- safe_df_for_excel(district_combined_log$checked_dataset)
  district_combined_log$cleaning_log <- format_cleaning_log_for_export(district_combined_log$cleaning_log)
  district_combined_log$cleaning_log <- safe_df_for_excel(district_combined_log$cleaning_log)
  
  if ("change_type" %in% names(district_combined_log$cleaning_log)) {
    district_combined_log$cleaning_log$change_type <- NA_character_
  }
  
  district_extra_sheets <- district_extra_sheets_by_admin2[[current_district]]
  
  if (is.null(district_extra_sheets)) {
    district_extra_sheets <- list()
  }
  
  district_file <- file.path(
    current_district_dir,
    paste0(
      "Cleaning_log_MSNA2026_Uganda_",
      current_district_safe,
      "_",
      export_time,
      ".xlsx"
    )
  )
  
  export_one_district_cleaningtools_logbook(
    district_combined_log = district_combined_log,
    output_path = district_file,
    extra_sheets = district_extra_sheets
  )
  
  district_export_paths[[current_district]] <- district_file
  district_combined_logs[[current_district]] <- district_combined_log
  district_workbook_sheets_by_admin2[[current_district]] <- c(
    district_combined_log,
    district_extra_sheets
  )
}

# --------------------------------------------------------------------------------------------------
# 7. Final object compatibility
# --------------------------------------------------------------------------------------------------

logbook_to_export <- district_workbook_sheets_by_admin2
combined_log_to_export <- cleaning_log_all

# Do not create/export these sheets/workbooks.
cleaning_log_no_deletion <- NULL
deletion_log <- NULL
cleaning_dashboard <- NULL
cl_name_to_export <- NULL
export_index_path <- NULL
logbook_export_index <- NULL

# --------------------------------------------------------------------------------------------------
# 8. Console summary
# --------------------------------------------------------------------------------------------------

cat("\n")
cat("National logbook exported: FALSE\n")
cat("Export index exported: FALSE\n")
cat("District-specific logbooks exported: ", length(district_export_paths), "\n", sep = "")
cat("Total cleaning-log records available across district logs: ", nrow(cleaning_log_all), "\n", sep = "")
cat("Export function used: cleaningtools::create_xlsx_cleaning_log\n")
cat("District logs source: district_combined_logs_by_admin2 from Script 4\n")
cat("Loop level: one export per district cleaning log\n")
cat("change_type dropdown added: TRUE\n")
cat("change_type prefilled: FALSE\n")
cat("\n")

if (length(district_export_paths) > 0) {
  cat("District-specific exports:\n")
  print(
    tibble::tibble(
      admin2 = names(district_export_paths),
      file_path = unlist(district_export_paths, use.names = FALSE)
    )
  )
}


cat("\n")
cat("\n")
cat("\n")
cat("\n")
cat("\n")
cat("\n")
cat("\n")
cat(green$bold("District-specific logbook export completed"))
cat("\n")
cat("\n")
cat("\n")

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

