# =================================================================================================
# REACH MSNA 2026 Mozambique - Data cleaning helper functions
# Updated for current Kobo export structure: Submissions + roster/edu/health/nutrition/child protection repeats
# =================================================================================================

load_package <- function(pkg, required = TRUE) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    msg <- paste0("Package '", pkg, "' is not installed.")
    if (required) stop(msg, call. = FALSE) else { warning(msg, call. = FALSE); return(FALSE) }
  }
  suppressPackageStartupMessages(library(pkg, character.only = TRUE))
  TRUE
}

print_step <- function(x) {
  if (requireNamespace("crayon", quietly = TRUE)) {
    cat("\n", crayon::green$bold(x), "\n", sep = "")
  } else {
    cat("\n", x, "\n", sep = "")
  }
}

print_warn <- function(x) {
  if (requireNamespace("crayon", quietly = TRUE)) {
    cat("\n", crayon::yellow$bold(x), "\n", sep = "")
  } else {
    warning(x, call. = FALSE)
  }
}

safe_sheet_names <- function(path) {
  if (!file.exists(path)) stop("File not found: ", path, call. = FALSE)
  readxl::excel_sheets(path)
}

safe_read_xlsx_sheet <- function(path, sheet, guess_max = 100000) {
  if (!file.exists(path)) stop("File not found: ", path, call. = FALSE)
  sheets <- readxl::excel_sheets(path)
  if (!sheet %in% sheets) {
    warning("Sheet '", sheet, "' not found in ", basename(path), call. = FALSE)
    return(tibble::tibble())
  }
  readxl::read_xlsx(path, sheet = sheet, guess_max = guess_max) |> janitor::clean_names()
}

read_xlsx_sheet_raw_names <- function(path, sheet, guess_max = 100000) {
  if (!file.exists(path)) stop("File not found: ", path, call. = FALSE)
  sheets <- readxl::excel_sheets(path)
  if (!sheet %in% sheets) {
    warning("Sheet '", sheet, "' not found in ", basename(path), call. = FALSE)
    return(tibble::tibble())
  }
  readxl::read_xlsx(path, sheet = sheet, guess_max = guess_max, .name_repair = "unique_quiet")
}

standardise_column_names <- function(df) {
  # Preserve Kobo system fields that begin with underscores; clean other syntactic issues only lightly.
  names(df) <- stringr::str_replace_all(names(df), "\\s+", "_")
  names(df) <- stringr::str_replace_all(names(df), "[/]", "__")
  names(df) <- stringr::str_replace_all(names(df), "[^A-Za-z0-9_]+", "_")
  names(df) <- stringr::str_replace_all(names(df), "_+", "_")
  df
}

first_existing_col <- function(df, candidates) {
  hit <- candidates[candidates %in% names(df)]
  if (length(hit) == 0) NA_character_ else hit[[1]]
}

coalesce_cols_chr <- function(df, cols) {
  cols <- cols[cols %in% names(df)]
  if (length(cols) == 0) return(rep(NA_character_, nrow(df)))
  out <- rep(NA_character_, nrow(df))
  for (cc in cols) {
    val <- as.character(df[[cc]])
    out <- dplyr::if_else(is.na(out) | out == "", val, out, missing = out)
  }
  out
}

standardise_uuid_columns <- function(df, dataset_name = "Submissions") {
  if (nrow(df) == 0) return(df)
  if (dataset_name == "Submissions") {
    if (!"uuid" %in% names(df)) {
      uuid_col <- first_existing_col(df, c("_uuid", "uuid", "instance_name", "_id"))
      if (!is.na(uuid_col)) df$uuid <- as.character(df[[uuid_col]])
    }
    if (!"parent_uuid" %in% names(df)) df$parent_uuid <- NA_character_
  } else {
    if (!"uuid" %in% names(df)) {
      uuid_col <- first_existing_col(df, c("_uuid", "uuid", "_id"))
      if (!is.na(uuid_col)) df$uuid <- as.character(df[[uuid_col]])
    }
    if (!"parent_uuid" %in% names(df)) {
      df$parent_uuid <- coalesce_cols_chr(df, c("_parent_uuid", "_submission_uuid", "_submission__uuid", "_submission__id", "parent_instance_name"))
    }
  }
  df
}

filter_test_submissions <- function(df, pattern = NULL) {
  if (is.null(pattern) || is.na(pattern) || pattern == "" || nrow(df) == 0) return(df)
  cols <- c("_submitted_by", "_submission__submitted_by", "username", "enumerator_id")
  cols <- cols[cols %in% names(df)]
  if (length(cols) == 0) return(df)
  keep <- rep(TRUE, nrow(df))
  for (cc in cols) {
    keep <- keep & !stringr::str_detect(as.character(df[[cc]]), stringr::regex(pattern, ignore_case = TRUE))
  }
  df[keep, , drop = FALSE]
}

safe_numeric <- function(x) {
  suppressWarnings(as.numeric(stringr::str_replace_all(as.character(x), ",", "")))
}

safe_datetime <- function(x) {
  if (inherits(x, "POSIXct")) return(x)
  if (inherits(x, "Date")) return(as.POSIXct(x))
  suppressWarnings(lubridate::parse_date_time(as.character(x), orders = c("Ymd HMS", "Ymd HM", "Ymd", "ymd HMS", "ymd HM", "ymd", "dmy HMS", "dmy HM", "dmy"), tz = "Africa/Kampala"))
}

add_percentage_missing_msna <- function(df, ignore_cols = c("uuid", "parent_uuid")) {
  if (nrow(df) == 0) return(df)
  check_cols <- setdiff(names(df), ignore_cols)
  if (length(check_cols) == 0) {
    df$percentage_missing <- 0
  } else {
    df$percentage_missing <- apply(df[, check_cols, drop = FALSE], 1, function(z) mean(is.na(z) | z == "") * 100)
  }
  df
}

add_duration_msna <- function(df) {
  if (!all(c("start", "end") %in% names(df))) return(df)
  start_dt <- safe_datetime(df$start)
  end_dt <- safe_datetime(df$end)
  df$duration_minutes <- as.numeric(difftime(end_dt, start_dt, units = "mins"))
  df
}

score_hhs <- function(yes_no, freq) {
  yn <- as.character(yes_no)
  fr <- as.character(freq)
  dplyr::case_when(
    is.na(yn) | yn == "" ~ NA_real_,
    yn == "no" ~ 0,
    yn == "yes" & fr %in% c("rarely_1_2_times", "rarely", "1_2", "1_2_times") ~ 1,
    yn == "yes" & fr %in% c("sometimes_3_10_times", "sometimes", "3_10", "3_10_times") ~ 1,
    yn == "yes" & fr %in% c("often_more_than_10_times", "often", "more_than_10") ~ 2,
    yn == "yes" ~ 1,
    TRUE ~ NA_real_
  )
}

cat_from_score <- function(score, breaks, labels) {
  cut(score, breaks = breaks, labels = labels, include.lowest = TRUE, right = TRUE)
}

add_msna_food_security_scores <- function(df) {
  if (nrow(df) == 0) return(df)
  # FCS using current tool variable names if present.
  fcs_vars <- c("fcs_cereals", "fcs_legumes", "fcs_dairy", "fcs_meat_fish_eggs", "fcs_vegetables", "fcs_fruits", "fcs_oil", "fcs_sugar")
  if (all(fcs_vars %in% names(df))) {
    df <- df |> dplyr::mutate(
      dplyr::across(dplyr::all_of(fcs_vars), safe_numeric),
      fcs_score = 2*fcs_cereals + 3*fcs_legumes + 4*fcs_dairy + 4*fcs_meat_fish_eggs +
        1*fcs_vegetables + 1*fcs_fruits + 0.5*fcs_oil + 0.5*fcs_sugar,
      fcs_category = dplyr::case_when(
        is.na(fcs_score) ~ NA_character_,
        fcs_score <= 28 ~ "poor",
        fcs_score <= 42 ~ "borderline",
        fcs_score > 42 ~ "acceptable"
      )
    )
  }
  # rCSI.
  rcsi_vars <- c("fsl_rcsi_lessquality", "fsl_rcsi_borrow", "fsl_rcsi_mealsize", "fsl_rcsi_mealadult", "fsl_rcsi_mealnb")
  if (all(rcsi_vars %in% names(df))) {
    df <- df |> dplyr::mutate(
      dplyr::across(dplyr::all_of(rcsi_vars), safe_numeric),
      rcsi_score = 1*fsl_rcsi_lessquality + 2*fsl_rcsi_borrow + 1*fsl_rcsi_mealsize + 3*fsl_rcsi_mealadult + 1*fsl_rcsi_mealnb
    )
  }
  # HHS.
  hhs_vars <- c("hhs_lev1_nofoodhh", "hhs_lev1_nofoodhh_freq", "hhs_lev1_sleephungry", "hhs_lev1_sleephungry_freq", "hhs_lev1_alldaynight", "hhs_lev1_alldaynight_freq")
  if (all(hhs_vars %in% names(df))) {
    df <- df |> dplyr::mutate(
      hhs_score = score_hhs(hhs_lev1_nofoodhh, hhs_lev1_nofoodhh_freq) +
        score_hhs(hhs_lev1_sleephungry, hhs_lev1_sleephungry_freq) +
        score_hhs(hhs_lev1_alldaynight, hhs_lev1_alldaynight_freq),
      hhs_category = dplyr::case_when(
        is.na(hhs_score) ~ NA_character_,
        hhs_score <= 1 ~ "little_to_no_hunger",
        hhs_score <= 3 ~ "moderate_hunger",
        hhs_score >= 4 ~ "severe_hunger"
      )
    )
  }
  df
}

add_msna_derived_variables <- function(df, dataset_name = "Submissions") {
  if (nrow(df) == 0) return(df)
  if (dataset_name == "Submissions") {
    df <- df |> add_duration_msna() |> add_msna_food_security_scores()
    if (all(c("ind_f_adult_n", "ind_m_adult_n") %in% names(df))) {
      df <- df |> dplyr::mutate(num_adults = safe_numeric(ind_f_adult_n) + safe_numeric(ind_m_adult_n))
    }
  }
  age_vars <- intersect(c("ind_age", "edu_ind_age", "health_ind_age", "nut_ind_age", "child_ind_age"), names(df))
  if (length(age_vars) > 0) df <- df |> dplyr::mutate(dplyr::across(dplyr::all_of(age_vars), safe_numeric))
  df
}

extract_other_columns_from_survey <- function(df_survey, dataset_df) {
  if (nrow(df_survey) == 0) return(character())
  name_col <- if ("name" %in% names(df_survey)) "name" else NA_character_
  type_col <- if ("type" %in% names(df_survey)) "type" else NA_character_
  if (is.na(name_col) || is.na(type_col)) return(character())
  df_survey |>
    dplyr::filter(.data[[type_col]] == "text", stringr::str_detect(.data[[name_col]], "other|specify|comment")) |>
    dplyr::pull(.data[[name_col]]) |>
    intersect(names(dataset_df))
}

split_columns_to_clean <- function(x) {
  if (is.null(x) || length(x) == 0 || is.na(x) || trimws(x) == "") return(character())
  x <- stringr::str_replace_all(as.character(x), "\\s*[,;]\\s*", ",")
  out <- unlist(strsplit(x, ","))
  out <- trimws(out)
  out <- stringr::str_replace(out, "^[A-Za-z0-9_]+\\.", "")
  out[out != ""]
}

value_string_for_columns <- function(row_df, cols) {
  cols <- cols[cols %in% names(row_df)]
  if (length(cols) == 0) return(NA_character_)
  paste(paste0(cols, "=", as.character(row_df[1, cols, drop = TRUE])), collapse = " | ")
}


# ---- Cleaning-log schema and safe bind helpers ----
empty_cleaning_log <- function() {
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

log_core_columns <- names(empty_cleaning_log())

is_probably_log_df <- function(x) {
  is.data.frame(x) && any(c("issue", "check_id", "question", "old_value", "columns_to_clean", "change_type") %in% names(x))
}

normalise_cleaning_log <- function(x, object_name = NA_character_) {
  if (is.null(x)) return(empty_cleaning_log())
  
  # Lists can occur when a helper returns a named list of logs. Flatten only log-like
  # data frames and skip raw datasets accidentally passed into the combiner.
  if (is.list(x) && !is.data.frame(x)) {
    parts <- purrr::imap(x, ~ normalise_cleaning_log(.x, .y))
    parts <- parts[purrr::map_int(parts, nrow) > 0]
    if (length(parts) == 0) return(empty_cleaning_log())
    return(dplyr::bind_rows(parts))
  }
  
  if (!is.data.frame(x) || nrow(x) == 0) return(empty_cleaning_log())
  
  if (!is_probably_log_df(x)) {
    warning("Skipped a non-log object while combining cleaning logs: ", object_name, call. = FALSE)
    return(empty_cleaning_log())
  }
  
  x <- tibble::as_tibble(x)
  
  # Harmonise common alternative column names produced by cleaningtools or older scripts.
  if (!"uuid" %in% names(x)) {
    hit <- first_existing_col(x, c("_uuid", "id", "submission_id", "submission_uuid"))
    x$uuid <- if (!is.na(hit)) as.character(x[[hit]]) else NA_character_
  }
  if (!"parent_uuid" %in% names(x)) {
    hit <- first_existing_col(x, c("_parent_uuid", "_submission_uuid", "submission_uuid", "parent_id"))
    x$parent_uuid <- if (!is.na(hit)) as.character(x[[hit]]) else NA_character_
  }
  if (!"repeat_index" %in% names(x)) {
    hit <- first_existing_col(x, c("_index", "index", "repeat_position"))
    x$repeat_index <- if (!is.na(hit)) as.character(x[[hit]]) else NA_character_
  }
  if (!"question" %in% names(x)) {
    hit <- first_existing_col(x, c("name", "variable", "columns_to_clean"))
    x$question <- if (!is.na(hit)) as.character(x[[hit]]) else NA_character_
  }
  if (!"old_value" %in% names(x)) {
    hit <- first_existing_col(x, c("value", "old", "response", "answer"))
    x$old_value <- if (!is.na(hit)) as.character(x[[hit]]) else NA_character_
  }
  if (!"new_value" %in% names(x)) x$new_value <- NA_character_
  if (!"change_type" %in% names(x)) x$change_type <- NA_character_
  if (!"issue" %in% names(x)) {
    hit <- first_existing_col(x, c("description", "problem", "comment"))
    x$issue <- if (!is.na(hit)) as.character(x[[hit]]) else "Flag generated by automated cleaning check."
  }
  if (!"check_id" %in% names(x)) x$check_id <- if (!is.na(object_name) && object_name != "") as.character(object_name) else NA_character_
  if (!"check_to_perform" %in% names(x)) x$check_to_perform <- NA_character_
  if (!"columns_to_clean" %in% names(x)) x$columns_to_clean <- as.character(x$question)
  if (!"dataset" %in% names(x)) {
    ds_guess <- stringr::str_remove(as.character(object_name), "_(duplicate|missingness|special|duration)$")
    x$dataset <- if (!is.na(ds_guess) && ds_guess != "") ds_guess else NA_character_
  }
  if (!"notes" %in% names(x)) x$notes <- NA_character_
  
  missing_core <- setdiff(log_core_columns, names(x))
  for (cc in missing_core) x[[cc]] <- NA_character_
  
  x |>
    dplyr::mutate(dplyr::across(dplyr::all_of(log_core_columns), as.character)) |>
    dplyr::select(dplyr::all_of(log_core_columns), dplyr::everything())
}

combine_cleaning_logs <- function(...) {
  objects <- list(...)
  if (length(objects) == 1 && is.list(objects[[1]]) && !is.data.frame(objects[[1]])) {
    objects <- objects[[1]]
  }
  logs <- purrr::imap(objects, ~ normalise_cleaning_log(.x, .y))
  logs <- logs[purrr::map_int(logs, nrow) > 0]
  if (length(logs) == 0) return(empty_cleaning_log())
  dplyr::bind_rows(logs)
}

make_log_rows <- function(flagged_df, dataset_name, check_id, description, check_to_perform, columns_to_clean, uuid_col = "uuid") {
  if (is.null(flagged_df) || nrow(flagged_df) == 0) return(tibble::tibble())
  cols <- split_columns_to_clean(columns_to_clean)
  uuid_use <- if (uuid_col %in% names(flagged_df)) uuid_col else first_existing_col(flagged_df, c("uuid", "_uuid", "_id", "instance_name"))
  if (is.na(uuid_use)) flagged_df$uuid_tmp_log <- seq_len(nrow(flagged_df)); uuid_use <- ifelse(is.na(uuid_use), "uuid_tmp_log", uuid_use)
  info_cols <- intersect(c("parent_uuid", "enumerator_id", "district_name", "sub_county_division", "refugee_settlement", "hoh_type", "_submission_time", "username"), names(flagged_df))
  base <- tibble::tibble(
    dataset = dataset_name,
    uuid = as.character(flagged_df[[uuid_use]]),
    parent_uuid = if ("parent_uuid" %in% names(flagged_df)) as.character(flagged_df$parent_uuid) else NA_character_,
    repeat_index = if ("_index" %in% names(flagged_df)) as.character(flagged_df$`_index`) else NA_character_,
    question = columns_to_clean,
    old_value = vapply(seq_len(nrow(flagged_df)), function(i) value_string_for_columns(flagged_df[i, , drop = FALSE], cols), character(1)),
    new_value = NA_character_,
    change_type = NA_character_,
    issue = description,
    check_id = check_id,
    check_to_perform = check_to_perform,
    columns_to_clean = columns_to_clean,
    notes = NA_character_
  )
  for (cc in info_cols) base[[cc]] <- as.character(flagged_df[[cc]])
  base
}

run_one_check <- function(df, check_row, dataset_name, uuid_col = "uuid") {
  expr_txt <- as.character(check_row$check_to_perform[[1]])
  if (is.na(expr_txt) || trimws(expr_txt) == "") return(tibble::tibble())
  res <- tryCatch({
    expr <- rlang::parse_expr(expr_txt)
    val <- rlang::eval_tidy(expr, data = df)
    if (is.logical(val)) {
      if (length(val) == 1) val <- rep(val, nrow(df))
      if (length(val) != nrow(df)) stop("Logical check returned ", length(val), " values for ", nrow(df), " rows.")
      val[is.na(val)] <- FALSE
      df[val, , drop = FALSE]
    } else if (is.data.frame(val)) {
      val
    } else if (is.list(val)) {
      stop("Logical check returned a list, not a logical vector or data frame.")
    } else {
      tibble::tibble()
    }
  }, error = function(e) {
    tibble::tibble(.check_error = conditionMessage(e))
  })
  if (".check_error" %in% names(res)) {
    return(tibble::tibble(
      dataset = dataset_name,
      uuid = NA_character_,
      parent_uuid = NA_character_,
      repeat_index = NA_character_,
      question = as.character(check_row$columns_to_clean[[1]]),
      old_value = NA_character_,
      new_value = NA_character_,
      change_type = NA_character_,
      issue = paste0("CHECK COULD NOT RUN: ", res$.check_error[[1]]),
      check_id = as.character(check_row$check_id[[1]]),
      check_to_perform = expr_txt,
      columns_to_clean = as.character(check_row$columns_to_clean[[1]]),
      notes = "Review expression or missing variables."
    ))
  }
  make_log_rows(
    flagged_df = res,
    dataset_name = dataset_name,
    check_id = as.character(check_row$check_id[[1]]),
    description = as.character(check_row$description[[1]]),
    check_to_perform = expr_txt,
    columns_to_clean = as.character(check_row$columns_to_clean[[1]]),
    uuid_col = uuid_col
  )
}

run_check_list <- function(df, checks_df, dataset_name, uuid_col = "uuid") {
  if (nrow(df) == 0 || nrow(checks_df) == 0) return(tibble::tibble())
  required_cols <- c("check_id", "description", "check_to_perform", "columns_to_clean")
  miss <- setdiff(required_cols, names(checks_df))
  if (length(miss) > 0) stop("Missing check columns: ", paste(miss, collapse = ", "), call. = FALSE)
  logs <- purrr::map(seq_len(nrow(checks_df)), function(i) run_one_check(df, checks_df[i, ], dataset_name, uuid_col = uuid_col))
  combine_cleaning_logs(logs)
}

run_cross_sheet_checks <- function(data_list, checks_df) {
  if (nrow(checks_df) == 0) return(empty_cleaning_log())
  env <- new.env(parent = globalenv())
  for (nm in names(data_list)) assign(nm, data_list[[nm]], envir = env)
  logs <- vector("list", nrow(checks_df))
  for (i in seq_len(nrow(checks_df))) {
    expr_txt <- as.character(checks_df$check_to_perform[[i]])
    res <- tryCatch(eval(parse(text = expr_txt), envir = env), error = function(e) tibble::tibble(.check_error = conditionMessage(e)))
    if (is.list(res) && !is.data.frame(res)) {
      res <- tibble::tibble(.check_error = "Cross-sheet expression returned a list. It must return a data frame of flagged records.")
    }
    if (!is.data.frame(res)) {
      res <- tibble::tibble(.check_error = paste0("Cross-sheet expression returned unsupported type: ", paste(class(res), collapse = "/")))
    }
    if (".check_error" %in% names(res)) {
      logs[[i]] <- tibble::tibble(
        dataset = "cross_sheet",
        uuid = NA_character_,
        parent_uuid = NA_character_,
        repeat_index = NA_character_,
        question = as.character(checks_df$columns_to_clean[[i]]),
        old_value = NA_character_,
        new_value = NA_character_,
        change_type = NA_character_,
        issue = paste0("CHECK COULD NOT RUN: ", res$.check_error[[1]]),
        check_id = as.character(checks_df$check_id[[i]]),
        check_to_perform = expr_txt,
        columns_to_clean = as.character(checks_df$columns_to_clean[[i]]),
        notes = "Cross-sheet expression failed. It should return a data frame of flagged records."
      )
    } else {
      logs[[i]] <- make_log_rows(as.data.frame(res), "cross_sheet", checks_df$check_id[[i]], checks_df$description[[i]], expr_txt, checks_df$columns_to_clean[[i]])
    }
  }
  combine_cleaning_logs(logs)
}

basic_duplicate_check <- function(df, dataset_name, uuid_col = "uuid") {
  if (nrow(df) == 0 || !uuid_col %in% names(df)) return(tibble::tibble())
  flagged <- df[duplicated(df[[uuid_col]]) | duplicated(df[[uuid_col]], fromLast = TRUE), , drop = FALSE]
  make_log_rows(flagged, dataset_name, paste0(dataset_name, "_DUP_UUID"), "Duplicate UUID/ID in dataset.", paste0("duplicated(", uuid_col, ")"), uuid_col, uuid_col = uuid_col)
}

basic_missingness_check <- function(df, dataset_name, uuid_col = "uuid", threshold = 80) {
  if (nrow(df) == 0) return(tibble::tibble())
  df <- add_percentage_missing_msna(df)
  flagged <- df |> dplyr::filter(.data$percentage_missing >= threshold)
  make_log_rows(flagged, dataset_name, paste0(dataset_name, "_MISS_80"), paste0("Row has >= ", threshold, "% missing values."), "percentage_missing >= threshold", "percentage_missing", uuid_col = uuid_col)
}

basic_duration_check <- function(df, dataset_name = "Submissions", uuid_col = "uuid", lower_bound = 10, upper_bound = 180) {
  if (nrow(df) == 0 || !"duration_minutes" %in% names(df)) return(tibble::tibble())
  flagged <- df |> dplyr::filter(!is.na(duration_minutes), duration_minutes < lower_bound | duration_minutes > upper_bound)
  make_log_rows(flagged, dataset_name, "SUB_DURATION", paste0("Survey duration outside ", lower_bound, "-", upper_bound, " minutes."), "duration_minutes < lower_bound | duration_minutes > upper_bound", "start, end, duration_minutes", uuid_col = uuid_col)
}

basic_special_value_check <- function(df, dataset_name, uuid_col = "uuid", values_to_look = c("999", "_999", "-999", "999.0")) {
  if (nrow(df) == 0) return(tibble::tibble())
  char_df <- df |> dplyr::mutate(dplyr::across(dplyr::everything(), as.character))
  long <- char_df |>
    dplyr::mutate(.row_id_tmp = dplyr::row_number()) |>
    tidyr::pivot_longer(cols = -dplyr::all_of(c(".row_id_tmp", intersect(c(uuid_col, "parent_uuid"), names(char_df)))), names_to = "question", values_to = "old_value", values_drop_na = TRUE) |>
    dplyr::filter(old_value %in% values_to_look)
  if (nrow(long) == 0) return(tibble::tibble())
  long |>
    dplyr::transmute(dataset = dataset_name, uuid = if (uuid_col %in% names(long)) as.character(.data[[uuid_col]]) else NA_character_, parent_uuid = if ("parent_uuid" %in% names(long)) as.character(parent_uuid) else NA_character_, repeat_index = NA_character_, question, old_value, new_value = NA_character_, change_type = NA_character_, issue = "Special missing/unknown code found in data.", check_id = paste0(dataset_name, "_SPECIAL_VALUE"), check_to_perform = "value %in% special missing codes", columns_to_clean = question, notes = NA_character_)
}

other_specify_check <- function(df, dataset_name, other_cols, uuid_col = "uuid") {
  if (nrow(df) == 0 || length(other_cols) == 0) return(tibble::tibble())
  logs <- list()
  for (oc in other_cols) {
    # Empty 'other specify' fields are not automatically flagged here because the triggering parent variable differs by question.
    flagged <- df |> dplyr::filter(!is.na(.data[[oc]]), trimws(as.character(.data[[oc]])) %in% c("", "other", "n/a", "na", "none"))
    logs[[oc]] <- make_log_rows(flagged, dataset_name, paste0(dataset_name, "_OTHER_TEXT"), "Other/specify field contains weak, blank, or non-informative text.", paste0(oc, " is weak/blank"), oc, uuid_col = uuid_col)
  }
  combine_cleaning_logs(logs)
}

translation_flag_log <- function(df, dataset_name, uuid_col = "uuid", pattern = "[^ -~]") {
  if (nrow(df) == 0) return(tibble::tibble())
  id_cols <- intersect(c(uuid_col, "parent_uuid", "_index", "_id", "_uuid"), names(df))
  char_cols <- names(df)[vapply(df, is.character, logical(1))]
  char_cols <- setdiff(char_cols, id_cols)
  if (length(char_cols) == 0) return(tibble::tibble())
  long <- df |>
    dplyr::select(dplyr::all_of(c(id_cols, char_cols))) |>
    tidyr::pivot_longer(cols = dplyr::all_of(char_cols), names_to = "question", values_to = "old_value", values_drop_na = TRUE) |>
    dplyr::filter(stringr::str_detect(old_value, pattern))
  if (nrow(long) == 0) return(tibble::tibble())
  long |>
    dplyr::transmute(dataset = dataset_name, uuid = if (uuid_col %in% names(long)) as.character(.data[[uuid_col]]) else NA_character_, parent_uuid = if ("parent_uuid" %in% names(long)) as.character(parent_uuid) else NA_character_, repeat_index = if ("_index" %in% names(long)) as.character(.data$`_index`) else NA_character_, question, old_value, new_value = NA_character_, change_type = "change_response", issue = "Translation or non-standard text review needed", check_id = paste0(dataset_name, "_TRANSLATION"), check_to_perform = "non-ASCII text detected", columns_to_clean = question, notes = NA_character_)
}

export_excel_list <- function(df_list, export_file_name, export_directory = "") {
  if (!dir.exists(export_directory) && export_directory != "") dir.create(export_directory, recursive = TRUE)
  wb <- openxlsx::createWorkbook()
  header_style <- openxlsx::createStyle(fontName = "Arial Narrow", fontSize = 11, fontColour = "white", border = "TopBottomLeftRight", borderColour = "black", halign = "center", fgFill = "#7030A0", textDecoration = "bold", wrapText = TRUE, valign = "center")
  body_style <- openxlsx::createStyle(fontName = "Arial Narrow", fontSize = 9, border = "TopBottomLeftRight", borderColour = "grey70", borderStyle = "thin", wrapText = FALSE)
  for (nm in names(df_list)) {
    df <- df_list[[nm]]
    if (!is.data.frame(df)) next
    sn <- substr(gsub("[\\[\\]\\*\\?/\\\\:]", "_", nm), 1, 31)
    openxlsx::addWorksheet(wb, sn, gridLines = TRUE)
    if (nrow(df) == 0 && ncol(df) == 0) df <- tibble::tibble(message = "No records")
    df <- df |> dplyr::mutate(dplyr::across(where(lubridate::is.POSIXt), as.character))
    openxlsx::writeData(wb, sn, df, rowNames = FALSE)
    openxlsx::addStyle(wb, sn, header_style, rows = 1, cols = seq_len(ncol(df)), gridExpand = TRUE)
    if (nrow(df) > 0) openxlsx::addStyle(wb, sn, body_style, rows = 2:(nrow(df) + 1), cols = seq_len(ncol(df)), gridExpand = TRUE)
    openxlsx::setColWidths(wb, sn, cols = seq_len(ncol(df)), widths = "auto")
    openxlsx::freezePane(wb, sn, firstRow = TRUE)
  }
  out_path <- file.path(export_directory, export_file_name)
  openxlsx::saveWorkbook(wb, out_path, overwrite = TRUE)
  out_path
}

bind_cleaning_logs <- function(paths, sheet_candidates = c("cleaning_log", "combined_log", "checks", "Sheet1")) {
  if (length(paths) == 0) return(tibble::tibble())
  logs <- list()
  for (p in paths) {
    sheets <- tryCatch(readxl::excel_sheets(p), error = function(e) character())
    sh <- sheet_candidates[sheet_candidates %in% sheets][1]
    if (is.na(sh)) next
    logs[[p]] <- readxl::read_xlsx(p, sheet = sh, .name_repair = "unique_quiet") |> janitor::clean_names()
  }
  dplyr::bind_rows(logs)
}

apply_cleaning_log_to_dataset <- function(df, log_df, dataset_name, uuid_col = "uuid") {
  if (nrow(df) == 0 || nrow(log_df) == 0) return(df)
  lg <- log_df |> dplyr::filter(.data$dataset == dataset_name | is.na(.data$dataset) | .data$dataset == "")
  if (nrow(lg) == 0) return(df)
  # Remove surveys first.
  if ("change_type" %in% names(lg)) {
    remove_ids <- lg |> dplyr::filter(change_type == "remove_survey") |> dplyr::pull(uuid) |> unique()
    if (length(remove_ids) > 0 && uuid_col %in% names(df)) df <- df |> dplyr::filter(!.data[[uuid_col]] %in% remove_ids)
  }
  # Apply value changes and blanking.
  actionable <- lg |> dplyr::filter(change_type %in% c("change_response", "blank_response"), !is.na(question), question %in% names(df))
  if (nrow(actionable) == 0) return(df)
  for (i in seq_len(nrow(actionable))) {
    q <- actionable$question[[i]]
    uid <- actionable$uuid[[i]]
    if (!uuid_col %in% names(df)) next
    idx <- which(as.character(df[[uuid_col]]) == as.character(uid))
    if (length(idx) == 0) next
    if (actionable$change_type[[i]] == "blank_response") {
      df[idx, q] <- NA
    } else {
      df[idx, q] <- actionable$new_value[[i]]
    }
  }
  df
}

review_cleaning_log_basic <- function(raw_data_list, cleaning_log) {
  if (nrow(cleaning_log) == 0) return(tibble::tibble())
  problems <- list()
  for (i in seq_len(nrow(cleaning_log))) {
    row <- cleaning_log[i, ]
    ds <- if ("dataset" %in% names(row) && !is.na(row$dataset[[1]]) && row$dataset[[1]] != "") row$dataset[[1]] else "Submissions"
    if (!ds %in% names(raw_data_list)) {
      problems[[length(problems)+1]] <- tibble::tibble(row_number = i, dataset = ds, uuid = as.character(row$uuid[[1]]), question = as.character(row$question[[1]]), issue = "Dataset named in cleaning log was not found in raw data list")
      next
    }
    df <- raw_data_list[[ds]]
    q <- as.character(row$question[[1]])
    if (!is.na(q) && q != "" && !q %in% names(df) && !stringr::str_detect(q, ",")) {
      problems[[length(problems)+1]] <- tibble::tibble(row_number = i, dataset = ds, uuid = as.character(row$uuid[[1]]), question = q, issue = "Question/variable not found in dataset")
    }
    if ("change_type" %in% names(row) && row$change_type[[1]] == "change_response" && (is.na(row$new_value[[1]]) || row$new_value[[1]] == "")) {
      problems[[length(problems)+1]] <- tibble::tibble(row_number = i, dataset = ds, uuid = as.character(row$uuid[[1]]), question = q, issue = "change_response has missing new_value")
    }
  }
  dplyr::bind_rows(problems)
}

drop_and_log <- function(data, dataset_name, vars_to_drop, reason, deletion_log_env = parent.frame()) {
  if (nrow(data) == 0) return(data)
  vars_selected <- tidyselect::eval_select(rlang::expr(c(!!!vars_to_drop)), data = data)
  dropped_vars <- names(vars_selected)
  if (length(dropped_vars) > 0) {
    newlog <- tibble::tibble(dataset_name = dataset_name, variable_name = dropped_vars, deletion_rule = paste(vapply(vars_to_drop, rlang::expr_text, character(1)), collapse = ", "), deletion_reason = reason, timestamp = Sys.time())
    if (!exists("variable_deletion_log", envir = deletion_log_env)) assign("variable_deletion_log", tibble::tibble(), envir = deletion_log_env)
    assign("variable_deletion_log", dplyr::bind_rows(get("variable_deletion_log", envir = deletion_log_env), newlog), envir = deletion_log_env)
    data <- data |> dplyr::select(-dplyr::all_of(dropped_vars))
  }
  data
}

make_variable_tracker <- function(df_survey, data_list) {
  if (nrow(df_survey) == 0) return(tibble::tibble())
  nm_col <- "name"; type_col <- "type"
  if (!all(c(nm_col, type_col) %in% names(df_survey))) return(tibble::tibble())
  all_data_vars <- purrr::imap_dfr(data_list, ~tibble::tibble(dataset = .y, variable_name = names(.x)))
  df_survey |>
    dplyr::filter(!is.na(.data[[nm_col]]), .data[[nm_col]] != "") |>
    dplyr::transmute(variable_name = .data[[nm_col]], type = .data[[type_col]], label = dplyr::coalesce(.data[[intersect(c("label::english (en)", "label_english_en", "label"), names(df_survey))[1]]], "")) |>
    dplyr::left_join(all_data_vars, by = "variable_name") |>
    dplyr::mutate(in_export = !is.na(dataset))
}

make_coverage_summary <- function(main_df) {
  if (nrow(main_df) == 0) return(list())
  date_col <- first_existing_col(main_df, c("_submission_time", "submission_time", "today", "start"))
  df <- main_df
  if (!is.na(date_col)) df$submission_date <- as.Date(safe_datetime(df[[date_col]]))
  out <- list()
  if (all(c("district_name", "submission_date") %in% names(df))) {
    out$coverage_by_district_date <- df |> dplyr::count(district_name, submission_date, name = "n_forms") |> tidyr::pivot_wider(names_from = submission_date, values_from = n_forms, values_fill = 0)
  }
  if (all(c("district_name", "sub_county_division") %in% names(df))) {
    out$coverage_by_location <- df |> dplyr::count(district_name, sub_county_division, refugee_settlement = if ("refugee_settlement" %in% names(df)) refugee_settlement else NA_character_, name = "n_forms") |> dplyr::arrange(district_name, sub_county_division)
  }
  if ("enumerator_id" %in% names(df)) {
    out$coverage_by_enumerator <- df |> dplyr::count(enumerator_id, name = "n_forms") |> dplyr::arrange(dplyr::desc(n_forms))
  }
  if (all(c("district_name", "enumerator_id") %in% names(df))) {
    out$enumerator_by_district <- df |> dplyr::count(enumerator_id, district_name, name = "n_forms") |> tidyr::pivot_wider(names_from = district_name, values_from = n_forms, values_fill = 0)
  }
  out
}

make_repeat_count_summary <- function(data_list) {
  purrr::imap_dfr(data_list, function(df, nm) {
    tibble::tibble(dataset = nm, records = nrow(df), unique_parent_uuid = if ("parent_uuid" %in% names(df)) dplyr::n_distinct(df$parent_uuid, na.rm = TRUE) else NA_integer_)
  })
}





export_in_reach_format_list <- function(df_list, export_file_name, export_directory = "") {
  if (!is.list(df_list) || length(df_list) == 0) {
    stop("df_list must be a non-empty list of dataframes")
  }
  
  wb <- createWorkbook()
  
  header_style <- createStyle(
    fontName = "Arial Narrow", fontSize = 11, fontColour = "white", 
    border = "TopBottomLeftRight", borderColour = "black", 
    halign = "center", fgFill = "#E34443", textDecoration = "bold",
    wrapText = T, valign = "center"
  )
  
  table_body_style <- createStyle(
    fontName = "Arial Narrow", fontSize = 7, 
    border = "TopBottomLeftRight", borderColour = "grey26", borderStyle = "thin", 
    wrapText = FALSE
  )
  
  for (df_name in names(df_list)) {
    df <- df_list[[df_name]]
    
    if (!is.data.frame(df)) next  # Skip non-dataframe elements
    
    if (any(sapply(df, inherits, "POSIXct"))) {
      df <- df %>% mutate(across(where(is.POSIXct), as.character))
    }
    
    addWorksheet(wb, sheetName = df_name)
    writeData(wb, sheet = df_name, df, rowNames = FALSE)
    setColWidths(wb, sheet = df_name, cols = 1:ncol(df), widths = rep(25, ncol(df)))
    setRowHeights(wb, sheet = df_name, rows = 1, heights = 30)
    
    addStyle(wb, sheet = df_name, header_style, rows = 1, cols = 1:ncol(df), gridExpand = TRUE)
    addStyle(wb, sheet = df_name, table_body_style, rows = 2:(nrow(df) + 1), cols = 1:ncol(df), gridExpand = TRUE)
  }
  
  saveWorkbook(wb, file.path(export_directory, export_file_name), overwrite = TRUE)
}





# #Count of 999
# count_999_by_variable <- function(df){
#   
#   df <- df %>% pivot_longer(cols = everything() , cols_vary = "slowest", values_drop_na = T, values_transform = as.character, names_to = "Variable") %>%
#     filter(grepl("^999$|_999|^_999$", value)) %>% group_by(Variable) %>% 
#     summarise(`Number of 999` = n()) %>% arrange(desc(`Number of 999`))
#   
#   return(df)
#   
# }
# 
# 
# #Function to remove 999 and get compute outliers. It will only return the outliers, not the checked dataset
# get_outliers <- function(df, cols_not_to_check_outliers){
#   
#   df <- df %>% mutate(across(everything(), ~ifelse(. %in% c(999, -999, "999"), NA, .)))
#   clog_outliers <- df %>% check_outliers(uuid_column = "_uuid", sm_separator = "/", 
#                                          columns_not_to_check = cols_not_to_check_outliers, 
#                                          kobo_survey = df_survey, kobo_choices = df_choices)
#   
#   return(clog_outliers$potential_outliers)
#   
# }
# 
# 
# 
# 
# 
# #Get count after grouping, just to save time....
# 
# get_count <- function(df, ...){
#   col <- enquo(col)
#   df <- df %>% group_by(...) %>% 
#     summarise(count = n()) %>% 
#     arrange(desc(count))
#   
#   return(df)
#   
#   
# }
# 
# 
# 
# 
# what_turned_na <- function(df){
#   suppressWarnings({
#     df %>%  
#       pivot_longer(cols = everything(), values_transform = as.character, values_drop_na = T) %>% 
#       mutate(value_num = as.numeric(value)) %>% 
#       filter(is.na(value_num)) %>% select(-value_num) %>% 
#       print()
#   })
# }
# 









export_in_reach_format_list <- function(df_list, export_file_name, export_directory = "") {
  if (!is.list(df_list) || length(df_list) == 0) {
    stop("df_list must be a non-empty list of dataframes")
  }
  
  wb <- createWorkbook()
  
  header_style <- createStyle(
    fontName = "Arial Narrow", fontSize = 11, fontColour = "white", 
    border = "TopBottomLeftRight", borderColour = "black", 
    halign = "center", fgFill = "#E34443", textDecoration = "bold",
    wrapText = T, valign = "center"
  )
  
  table_body_style <- createStyle(
    fontName = "Arial Narrow", fontSize = 7, 
    border = "TopBottomLeftRight", borderColour = "grey26", borderStyle = "thin", 
    wrapText = FALSE
  )
  
  for (df_name in names(df_list)) {
    df <- df_list[[df_name]]
    
    if (!is.data.frame(df)) next  # Skip non-dataframe elements
    
    if (any(sapply(df, inherits, "POSIXct"))) {
      df <- df %>% mutate(across(where(is.POSIXct), as.character))
    }
    
    addWorksheet(wb, sheetName = df_name)
    writeData(wb, sheet = df_name, df, rowNames = FALSE)
    setColWidths(wb, sheet = df_name, cols = 1:ncol(df), widths = rep(25, ncol(df)))
    setRowHeights(wb, sheet = df_name, rows = 1, heights = 30)
    
    addStyle(wb, sheet = df_name, header_style, rows = 1, cols = 1:ncol(df), gridExpand = TRUE)
    addStyle(wb, sheet = df_name, table_body_style, rows = 2:(nrow(df) + 1), cols = 1:ncol(df), gridExpand = TRUE)
  }
  
  saveWorkbook(wb, file.path(export_directory, export_file_name), overwrite = TRUE)
}






export_in_reach_format_multiple_sheets<-function(file_R_1,  sheet_name_1 = "sheet_1",
                                                 file_R_2 = NULL,sheet_name_2= "2",
                                                 file_R_3= NULL,  sheet_name_3= "3",
                                                 file_R_4= NULL,sheet_name_4= "4",
                                                 file_R_5= NULL,  sheet_name_5= "5",
                                                 file_R_6= NULL,sheet_name_6= "6",
                                                 file_R_7= NULL,sheet_name_7= "7",
                                                 file_R_8= NULL,sheet_name_8= "8",
                                                 file_R_9= NULL,sheet_name_9= "9",
                                                 file_R_10= NULL,sheet_name_10= "10",
                                                 file_R_11= NULL,sheet_name_11= "11",
                                                 file_R_12= NULL,sheet_name_12= "12",
                                                 file_R_13= NULL,sheet_name_13= "13",
                                                 file_R_14= NULL,sheet_name_14= "14",
                                                 file_R_15= NULL,sheet_name_15= "15",
                                                 file_R_16= NULL,sheet_name_16= "16",
                                                 file_R_17= NULL,sheet_name_17= "17",
                                                 file_R_18= NULL,sheet_name_18= "18",
                                                 file_R_19= NULL,sheet_name_19= "19",
                                                 file_R_20= NULL,sheet_name_20= "20",
                                                 file_R_21= NULL,sheet_name_21= "21",
                                                 file_R_22= NULL,sheet_name_22= "22",
                                                 file_R_23= NULL,sheet_name_23= "23",
                                                 file_R_24= NULL,sheet_name_24= "23",
                                                 export_file_name,
                                                 export_directory = ""){
  
  #set dates back to characther so Excel shows them correctly
  if(length(names(Filter(is.POSIXct,file_R_1 )))>0){
    file_R_1<-file_R_1 %>% mutate_if(is.POSIXct,as.character)
  }
  
  # Create workbook
  wb <- createWorkbook()
  #for file_R_1
  # Add a worksheets
  addWorksheet(wb, sheet = sheet_name_1, gridLines = T)
  # write data
  writeData(wb, sheet = sheet_name_1, file_R_1, rowNames = F)
  # set column widths
  setColWidths(wb, sheet = sheet_name_1, cols = 1:ncol(file_R_1), widths = rep(15,ncol(file_R_1)))
  setRowHeights(wb, sheet = sheet_name_1,rows = 1, heights = 17)
  
  # header style
  header_style <- 
    createStyle(fontName = "Arial Narrow", fontSize = 11, fontColour = "white",  border = "TopBottomLeftRight ", borderColour = "black", halign = "center", fgFill = "#E34443", textDecoration = "bold")
  addStyle(wb, sheet = sheet_name_1, header_style, rows = 1, cols = 1:ncol(file_R_1), gridExpand = TRUE)
  
  # table body style
  table_body_style1 <-
    createStyle(fontName = "Arial Narrow",fontSize = 7, border = "TopBottomLeftRight ", borderColour = "grey26", borderStyle = "thin", wrapText = F)
  addStyle(wb, sheet = sheet_name_1, table_body_style1, rows = 2:(nrow(file_R_1)+1), cols = 1:ncol(file_R_1), gridExpand = TRUE)
  
  
  if(!is.null(file_R_2)){
    
    #set dates back to characther so Excel shows them correctly
    if(length(names(Filter(is.POSIXct,file_R_2 )))>0){
      file_R_2<-file_R_2 %>% mutate_if(is.POSIXct,as.character)
    }
    
    # Add a worksheets
    addWorksheet(wb, sheet = sheet_name_2, gridLines = T)
    # write data
    writeData(wb, sheet = sheet_name_2, file_R_2, rowNames = F)
    # set column widths
    setColWidths(wb, sheet = sheet_name_2, cols = 1:ncol(file_R_2), widths = rep(15,ncol(file_R_2)))
    setRowHeights(wb, sheet = sheet_name_2,rows = 1, heights = 17)
    
    # header style
    addStyle(wb, sheet = sheet_name_2, header_style, rows = 1, cols = 1:ncol(file_R_2), gridExpand = TRUE)
    
    # table body style
    addStyle(wb, sheet = sheet_name_2, table_body_style1, rows = 2:(nrow(file_R_2)+1), cols = 1:ncol(file_R_2), gridExpand = TRUE)
  }
  
  if(!is.null(file_R_3)){
    #set dates back to characther so Excel shows them correctly
    if(length(names(Filter(is.POSIXct,file_R_3 )))>0){
      file_R_3<-file_R_3 %>% mutate_if(is.POSIXct,as.character)
    }
    # Add a worksheets
    addWorksheet(wb, sheet = sheet_name_3, gridLines = T)
    # write data
    writeData(wb, sheet = sheet_name_3, file_R_3, rowNames = F)
    # set column widths
    setColWidths(wb, sheet = sheet_name_3, cols = 1:ncol(file_R_3), widths = rep(15,ncol(file_R_3)))
    setRowHeights(wb, sheet = sheet_name_3,rows = 1, heights = 17)
    
    # header style
    addStyle(wb, sheet = sheet_name_3, header_style, rows = 1, cols = 1:ncol(file_R_3), gridExpand = TRUE)
    
    # table body style
    addStyle(wb, sheet = sheet_name_3, table_body_style1, rows = 2:(nrow(file_R_3)+1), cols = 1:ncol(file_R_3), gridExpand = TRUE)
  }
  
  if(!is.null(file_R_4)){
    #set dates back to characther so Excel shows them correctly
    if(length(names(Filter(is.POSIXct,file_R_4 )))>0){
      file_R_4<-file_R_4 %>% mutate_if(is.POSIXct,as.character)
    }
    # Add a worksheets
    addWorksheet(wb, sheet = sheet_name_4, gridLines = T)
    # write data
    writeData(wb, sheet = sheet_name_4, file_R_4, rowNames = F)
    # set column widths
    setColWidths(wb, sheet = sheet_name_4, cols = 1:ncol(file_R_4), widths = rep(15,ncol(file_R_4)))
    setRowHeights(wb, sheet = sheet_name_4,rows = 1, heights = 17)
    
    # header style
    addStyle(wb, sheet = sheet_name_4, header_style, rows = 1, cols = 1:ncol(file_R_4), gridExpand = TRUE)
    
    # table body style
    addStyle(wb, sheet = sheet_name_4, table_body_style1, rows = 2:(nrow(file_R_4)+1), cols = 1:ncol(file_R_4), gridExpand = TRUE)
  }
  
  if(!is.null(file_R_5)){
    #set dates back to characther so Excel shows them correctly
    if(length(names(Filter(is.POSIXct,file_R_5 )))>0){
      file_R_5<-file_R_5 %>% mutate_if(is.POSIXct,as.character)
    }
    # Add a worksheets
    addWorksheet(wb, sheet = sheet_name_5, gridLines = T)
    # write data
    writeData(wb, sheet = sheet_name_5, file_R_5, rowNames = F)
    # set column widths
    setColWidths(wb, sheet = sheet_name_5, cols = 1:ncol(file_R_5), widths = rep(15,ncol(file_R_5)))
    setRowHeights(wb, sheet = sheet_name_5,rows = 1, heights = 17)
    
    # header style
    addStyle(wb, sheet = sheet_name_5, header_style, rows = 1, cols = 1:ncol(file_R_5), gridExpand = TRUE)
    
    # table body style
    addStyle(wb, sheet = sheet_name_5, table_body_style1, rows = 2:(nrow(file_R_5)+1), cols = 1:ncol(file_R_5), gridExpand = TRUE)
  }
  
  if(!is.null(file_R_6)){
    #set dates back to characther so Excel shows them correctly
    if(length(names(Filter(is.POSIXct,file_R_6 )))>0){
      file_R_6<-file_R_6 %>% mutate_if(is.POSIXct,as.character)
    }
    # Add a worksheets
    addWorksheet(wb, sheet = sheet_name_6, gridLines = T)
    # write data
    writeData(wb, sheet = sheet_name_6, file_R_6, rowNames = F)
    # set column widths
    setColWidths(wb, sheet = sheet_name_6, cols = 1:ncol(file_R_6), widths = rep(15,ncol(file_R_6)))
    setRowHeights(wb, sheet = sheet_name_6,rows = 1, heights = 17)
    
    # header style
    addStyle(wb, sheet = sheet_name_6, header_style, rows = 1, cols = 1:ncol(file_R_6), gridExpand = TRUE)
    
    # table body style
    addStyle(wb, sheet = sheet_name_6, table_body_style1, rows = 2:(nrow(file_R_6)+1), cols = 1:ncol(file_R_6), gridExpand = TRUE)
  }
  
  if(!is.null(file_R_7)){
    #set dates back to characther so Excel shows them correctly
    if(length(names(Filter(is.POSIXct,file_R_7 )))>0){
      file_R_7<-file_R_7 %>% mutate_if(is.POSIXct,as.character)
    }
    # Add a worksheets
    addWorksheet(wb, sheet = sheet_name_7, gridLines = T)
    # write data
    writeData(wb, sheet = sheet_name_7, file_R_7, rowNames = F)
    # set column widths
    setColWidths(wb, sheet = sheet_name_7, cols = 1:ncol(file_R_7), widths = rep(15,ncol(file_R_7)))
    setRowHeights(wb, sheet = sheet_name_7,rows = 1, heights = 17)
    
    # header style
    addStyle(wb, sheet = sheet_name_7, header_style, rows = 1, cols = 1:ncol(file_R_7), gridExpand = TRUE)
    
    # table body style
    addStyle(wb, sheet = sheet_name_7, table_body_style1, rows = 2:(nrow(file_R_7)+1), cols = 1:ncol(file_R_7), gridExpand = TRUE)
  }
  
  if(!is.null(file_R_8)){
    #set dates back to characther so Excel shows them correctly
    if(length(names(Filter(is.POSIXct,file_R_8 )))>0){
      file_R_8<-file_R_8 %>% mutate_if(is.POSIXct,as.character)
    }
    # Add a worksheets
    addWorksheet(wb, sheet = sheet_name_8, gridLines = T)
    # write data
    writeData(wb, sheet = sheet_name_8, file_R_8, rowNames = F)
    # set column widths
    setColWidths(wb, sheet = sheet_name_8, cols = 1:ncol(file_R_8), widths = rep(15,ncol(file_R_8)))
    setRowHeights(wb, sheet = sheet_name_8,rows = 1, heights = 17)
    
    # header style
    addStyle(wb, sheet = sheet_name_8, header_style, rows = 1, cols = 1:ncol(file_R_8), gridExpand = TRUE)
    
    # table body style
    addStyle(wb, sheet = sheet_name_8, table_body_style1, rows = 2:(nrow(file_R_8)+1), cols = 1:ncol(file_R_8), gridExpand = TRUE)
  }
  
  if(!is.null(file_R_9)){
    #set dates back to characther so Excel shows them correctly
    if(length(names(Filter(is.POSIXct,file_R_9 )))>0){
      file_R_9<-file_R_9 %>% mutate_if(is.POSIXct,as.character)
    }
    # Add a worksheets
    addWorksheet(wb, sheet = sheet_name_9, gridLines = T)
    # write data
    writeData(wb, sheet = sheet_name_9, file_R_9, rowNames = F)
    # set column widths
    setColWidths(wb, sheet = sheet_name_9, cols = 1:ncol(file_R_9), widths = rep(15,ncol(file_R_9)))
    setRowHeights(wb, sheet = sheet_name_9,rows = 1, heights = 17)
    
    # header style
    addStyle(wb, sheet = sheet_name_9, header_style, rows = 1, cols = 1:ncol(file_R_9), gridExpand = TRUE)
    
    # table body style
    addStyle(wb, sheet = sheet_name_9, table_body_style1, rows = 2:(nrow(file_R_9)+1), cols = 1:ncol(file_R_9), gridExpand = TRUE)
  }
  
  if(!is.null(file_R_10)){
    #set dates back to characther so Excel shows them correctly
    if(length(names(Filter(is.POSIXct,file_R_10 )))>0){
      file_R_10<-file_R_10 %>% mutate_if(is.POSIXct,as.character)
    }
    # Add a worksheets
    addWorksheet(wb, sheet = sheet_name_10, gridLines = T)
    # write data
    writeData(wb, sheet = sheet_name_10, file_R_10, rowNames = F)
    # set column widths
    setColWidths(wb, sheet = sheet_name_10, cols = 1:ncol(file_R_10), widths = rep(15,ncol(file_R_10)))
    setRowHeights(wb, sheet = sheet_name_10,rows = 1, heights = 17)
    
    # header style
    addStyle(wb, sheet = sheet_name_10, header_style, rows = 1, cols = 1:ncol(file_R_10), gridExpand = TRUE)
    
    # table body style
    addStyle(wb, sheet = sheet_name_10, table_body_style1, rows = 2:(nrow(file_R_10)+1), cols = 1:ncol(file_R_10), gridExpand = TRUE)
  }
  
  
  if(!is.null(file_R_11)){
    #set dates back to characther so Excel shows them correctly
    if(length(names(Filter(is.POSIXct,file_R_11 )))>0){
      file_R_11<-file_R_11 %>% mutate_if(is.POSIXct,as.character)
    }
    # Add a worksheets
    addWorksheet(wb, sheet = sheet_name_11, gridLines = T)
    # write data
    writeData(wb, sheet = sheet_name_11, file_R_11, rowNames = F)
    # set column widths
    setColWidths(wb, sheet = sheet_name_11, cols = 1:ncol(file_R_11), widths = rep(15,ncol(file_R_11)))
    setRowHeights(wb, sheet = sheet_name_11,rows = 1, heights = 17)
    
    # header style
    addStyle(wb, sheet = sheet_name_11, header_style, rows = 1, cols = 1:ncol(file_R_11), gridExpand = TRUE)
    
    # table body style
    addStyle(wb, sheet = sheet_name_11, table_body_style1, rows = 2:(nrow(file_R_11)+1), cols = 1:ncol(file_R_11), gridExpand = TRUE)
  }
  
  
  if(!is.null(file_R_12)){
    #set dates back to characther so Excel shows them correctly
    if(length(names(Filter(is.POSIXct,file_R_12 )))>0){
      file_R_12<-file_R_12 %>% mutate_if(is.POSIXct,as.character)
    }
    # Add a worksheets
    addWorksheet(wb, sheet = sheet_name_12, gridLines = T)
    # write data
    writeData(wb, sheet = sheet_name_12, file_R_12, rowNames = F)
    # set column widths
    setColWidths(wb, sheet = sheet_name_12, cols = 1:ncol(file_R_12), widths = rep(15,ncol(file_R_12)))
    setRowHeights(wb, sheet = sheet_name_12,rows = 1, heights = 17)
    
    # header style
    addStyle(wb, sheet = sheet_name_12, header_style, rows = 1, cols = 1:ncol(file_R_12), gridExpand = TRUE)
    
    # table body style
    addStyle(wb, sheet = sheet_name_12, table_body_style1, rows = 2:(nrow(file_R_12)+1), cols = 1:ncol(file_R_12), gridExpand = TRUE)
  }
  
  
  if(!is.null(file_R_13)){
    #set dates back to characther so Excel shows them correctly
    if(length(names(Filter(is.POSIXct,file_R_13 )))>0){
      file_R_13<-file_R_13 %>% mutate_if(is.POSIXct,as.character)
    }
    # Add a worksheets
    addWorksheet(wb, sheet = sheet_name_13, gridLines = T)
    # write data
    writeData(wb, sheet = sheet_name_13, file_R_13, rowNames = F)
    # set column widths
    setColWidths(wb, sheet = sheet_name_13, cols = 1:ncol(file_R_13), widths = rep(15,ncol(file_R_13)))
    setRowHeights(wb, sheet = sheet_name_13,rows = 1, heights = 17)
    
    # header style
    addStyle(wb, sheet = sheet_name_13, header_style, rows = 1, cols = 1:ncol(file_R_13), gridExpand = TRUE)
    
    # table body style
    addStyle(wb, sheet = sheet_name_13, table_body_style1, rows = 2:(nrow(file_R_13)+1), cols = 1:ncol(file_R_13), gridExpand = TRUE)
  }
  
  
  if(!is.null(file_R_14)){
    #set dates back to characther so Excel shows them correctly
    if(length(names(Filter(is.POSIXct,file_R_14 )))>0){
      file_R_14<-file_R_14 %>% mutate_if(is.POSIXct,as.character)
    }
    # Add a worksheets
    addWorksheet(wb, sheet = sheet_name_14, gridLines = T)
    # write data
    writeData(wb, sheet = sheet_name_14, file_R_14, rowNames = F)
    # set column widths
    setColWidths(wb, sheet = sheet_name_14, cols = 1:ncol(file_R_14), widths = rep(15,ncol(file_R_14)))
    setRowHeights(wb, sheet = sheet_name_14,rows = 1, heights = 17)
    
    # header style
    addStyle(wb, sheet = sheet_name_14, header_style, rows = 1, cols = 1:ncol(file_R_14), gridExpand = TRUE)
    
    # table body style
    addStyle(wb, sheet = sheet_name_14, table_body_style1, rows = 2:(nrow(file_R_14)+1), cols = 1:ncol(file_R_14), gridExpand = TRUE)
  }
  
  
  if(!is.null(file_R_15)){
    #set dates back to characther so Excel shows them correctly
    if(length(names(Filter(is.POSIXct,file_R_15 )))>0){
      file_R_15<-file_R_15 %>% mutate_if(is.POSIXct,as.character)
    }
    # Add a worksheets
    addWorksheet(wb, sheet = sheet_name_15, gridLines = T)
    # write data
    writeData(wb, sheet = sheet_name_15, file_R_15, rowNames = F)
    # set column widths
    setColWidths(wb, sheet = sheet_name_15, cols = 1:ncol(file_R_15), widths = rep(15,ncol(file_R_15)))
    setRowHeights(wb, sheet = sheet_name_15,rows = 1, heights = 17)
    
    # header style
    addStyle(wb, sheet = sheet_name_15, header_style, rows = 1, cols = 1:ncol(file_R_15), gridExpand = TRUE)
    
    # table body style
    addStyle(wb, sheet = sheet_name_15, table_body_style1, rows = 2:(nrow(file_R_15)+1), cols = 1:ncol(file_R_15), gridExpand = TRUE)
  }
  
  
  if(!is.null(file_R_16)){
    #set dates back to characther so Excel shows them correctly
    if(length(names(Filter(is.POSIXct,file_R_16 )))>0){
      file_R_16<-file_R_16 %>% mutate_if(is.POSIXct,as.character)
    }
    # Add a worksheets
    addWorksheet(wb, sheet = sheet_name_16, gridLines = T)
    # write data
    writeData(wb, sheet = sheet_name_16, file_R_16, rowNames = F)
    # set column widths
    setColWidths(wb, sheet = sheet_name_16, cols = 1:ncol(file_R_16), widths = rep(15,ncol(file_R_16)))
    setRowHeights(wb, sheet = sheet_name_16,rows = 1, heights = 17)
    
    # header style
    addStyle(wb, sheet = sheet_name_16, header_style, rows = 1, cols = 1:ncol(file_R_16), gridExpand = TRUE)
    
    # table body style
    addStyle(wb, sheet = sheet_name_16, table_body_style1, rows = 2:(nrow(file_R_16)+1), cols = 1:ncol(file_R_16), gridExpand = TRUE)
  }
  
  
  if(!is.null(file_R_17)){
    #set dates back to characther so Excel shows them correctly
    if(length(names(Filter(is.POSIXct,file_R_17 )))>0){
      file_R_17<-file_R_17 %>% mutate_if(is.POSIXct,as.character)
    }
    # Add a worksheets
    addWorksheet(wb, sheet = sheet_name_17, gridLines = T)
    # write data
    writeData(wb, sheet = sheet_name_17, file_R_17, rowNames = F)
    # set column widths
    setColWidths(wb, sheet = sheet_name_17, cols = 1:ncol(file_R_17), widths = rep(15,ncol(file_R_17)))
    setRowHeights(wb, sheet = sheet_name_17,rows = 1, heights = 17)
    
    # header style
    addStyle(wb, sheet = sheet_name_17, header_style, rows = 1, cols = 1:ncol(file_R_17), gridExpand = TRUE)
    
    # table body style
    addStyle(wb, sheet = sheet_name_17, table_body_style1, rows = 2:(nrow(file_R_17)+1), cols = 1:ncol(file_R_17), gridExpand = TRUE)
  }
  
  
  if(!is.null(file_R_18)){
    #set dates back to characther so Excel shows them correctly
    if(length(names(Filter(is.POSIXct,file_R_18 )))>0){
      file_R_18<-file_R_18 %>% mutate_if(is.POSIXct,as.character)
    }
    # Add a worksheets
    addWorksheet(wb, sheet = sheet_name_18, gridLines = T)
    # write data
    writeData(wb, sheet = sheet_name_18, file_R_18, rowNames = F)
    # set column widths
    setColWidths(wb, sheet = sheet_name_18, cols = 1:ncol(file_R_18), widths = rep(15,ncol(file_R_18)))
    setRowHeights(wb, sheet = sheet_name_18,rows = 1, heights = 17)
    
    # header style
    addStyle(wb, sheet = sheet_name_18, header_style, rows = 1, cols = 1:ncol(file_R_18), gridExpand = TRUE)
    
    # table body style
    addStyle(wb, sheet = sheet_name_18, table_body_style1, rows = 2:(nrow(file_R_18)+1), cols = 1:ncol(file_R_18), gridExpand = TRUE)
  }
  
  
  if(!is.null(file_R_19)){
    #set dates back to characther so Excel shows them correctly
    if(length(names(Filter(is.POSIXct,file_R_19 )))>0){
      file_R_19<-file_R_19 %>% mutate_if(is.POSIXct,as.character)
    }
    # Add a worksheets
    addWorksheet(wb, sheet = sheet_name_19, gridLines = T)
    # write data
    writeData(wb, sheet = sheet_name_19, file_R_19, rowNames = F)
    # set column widths
    setColWidths(wb, sheet = sheet_name_19, cols = 1:ncol(file_R_19), widths = rep(15,ncol(file_R_19)))
    setRowHeights(wb, sheet = sheet_name_19,rows = 1, heights = 17)
    
    # header style
    addStyle(wb, sheet = sheet_name_19, header_style, rows = 1, cols = 1:ncol(file_R_19), gridExpand = TRUE)
    
    # table body style
    addStyle(wb, sheet = sheet_name_19, table_body_style1, rows = 2:(nrow(file_R_19)+1), cols = 1:ncol(file_R_19), gridExpand = TRUE)
  }
  
  
  if(!is.null(file_R_20)){
    #set dates back to characther so Excel shows them correctly
    if(length(names(Filter(is.POSIXct,file_R_20 )))>0){
      file_R_20<-file_R_20 %>% mutate_if(is.POSIXct,as.character)
    }
    # Add a worksheets
    addWorksheet(wb, sheet = sheet_name_20, gridLines = T)
    # write data
    writeData(wb, sheet = sheet_name_20, file_R_20, rowNames = F)
    # set column widths
    setColWidths(wb, sheet = sheet_name_20, cols = 1:ncol(file_R_20), widths = rep(15,ncol(file_R_20)))
    setRowHeights(wb, sheet = sheet_name_20,rows = 1, heights = 17)
    
    # header style
    addStyle(wb, sheet = sheet_name_20, header_style, rows = 1, cols = 1:ncol(file_R_20), gridExpand = TRUE)
    
    # table body style
    addStyle(wb, sheet = sheet_name_20, table_body_style1, rows = 2:(nrow(file_R_20)+1), cols = 1:ncol(file_R_20), gridExpand = TRUE)
  }
  
  
  if(!is.null(file_R_21)){
    #set dates back to characther so Excel shows them correctly
    if(length(names(Filter(is.POSIXct,file_R_21 )))>0){
      file_R_21<-file_R_21 %>% mutate_if(is.POSIXct,as.character)
    }
    # Add a worksheets
    addWorksheet(wb, sheet = sheet_name_21, gridLines = T)
    # write data
    writeData(wb, sheet = sheet_name_21, file_R_21, rowNames = F)
    # set column widths
    setColWidths(wb, sheet = sheet_name_21, cols = 1:ncol(file_R_21), widths = rep(15,ncol(file_R_21)))
    setRowHeights(wb, sheet = sheet_name_21,rows = 1, heights = 17)
    
    # header style
    addStyle(wb, sheet = sheet_name_21, header_style, rows = 1, cols = 1:ncol(file_R_21), gridExpand = TRUE)
    
    # table body style
    addStyle(wb, sheet = sheet_name_21, table_body_style1, rows = 2:(nrow(file_R_21)+1), cols = 1:ncol(file_R_21), gridExpand = TRUE)
  }
  
  
  if(!is.null(file_R_22)){
    #set dates back to characther so Excel shows them correctly
    if(length(names(Filter(is.POSIXct,file_R_22 )))>0){
      file_R_22<-file_R_22 %>% mutate_if(is.POSIXct,as.character)
    }
    # Add a worksheets
    addWorksheet(wb, sheet = sheet_name_22, gridLines = T)
    # write data
    writeData(wb, sheet = sheet_name_22, file_R_22, rowNames = F)
    # set column widths
    setColWidths(wb, sheet = sheet_name_22, cols = 1:ncol(file_R_22), widths = rep(15,ncol(file_R_22)))
    setRowHeights(wb, sheet = sheet_name_22,rows = 1, heights = 17)
    
    # header style
    addStyle(wb, sheet = sheet_name_22, header_style, rows = 1, cols = 1:ncol(file_R_22), gridExpand = TRUE)
    
    # table body style
    addStyle(wb, sheet = sheet_name_22, table_body_style1, rows = 2:(nrow(file_R_22)+1), cols = 1:ncol(file_R_22), gridExpand = TRUE)
  }
  
  
  if(!is.null(file_R_23)){
    #set dates back to characther so Excel shows them correctly
    if(length(names(Filter(is.POSIXct,file_R_23 )))>0){
      file_R_23<-file_R_23 %>% mutate_if(is.POSIXct,as.character)
    }
    # Add a worksheets
    addWorksheet(wb, sheet = sheet_name_23, gridLines = T)
    # write data
    writeData(wb, sheet = sheet_name_23, file_R_23, rowNames = F)
    # set column widths
    setColWidths(wb, sheet = sheet_name_23, cols = 1:ncol(file_R_23), widths = rep(15,ncol(file_R_23)))
    setRowHeights(wb, sheet = sheet_name_23,rows = 1, heights = 17)
    
    # header style
    addStyle(wb, sheet = sheet_name_23, header_style, rows = 1, cols = 1:ncol(file_R_23), gridExpand = TRUE)
    
    # table body style
    addStyle(wb, sheet = sheet_name_23, table_body_style1, rows = 2:(nrow(file_R_23)+1), cols = 1:ncol(file_R_23), gridExpand = TRUE)
  }
  
  if(!is.null(file_R_24)){
    #set dates back to characther so Excel shows them correctly
    if(length(names(Filter(is.POSIXct,file_R_24 )))>0){
      file_R_24 <- file_R_24 %>% mutate_if(is.POSIXct,as.character)
    }
    # Add a worksheets
    addWorksheet(wb, sheet = sheet_name_24, gridLines = T)
    # write data
    writeData(wb, sheet = sheet_name_24, file_R_24, rowNames = F)
    # set column widths
    setColWidths(wb, sheet = sheet_name_24, cols = 1:ncol(file_R_24), widths = rep(15,ncol(file_R_24)))
    setRowHeights(wb, sheet = sheet_name_24,rows = 1, heights = 17)
    
    # header style
    addStyle(wb, sheet = sheet_name_24, header_style, rows = 1, cols = 1:ncol(file_R_24), gridExpand = TRUE)
    
    # table body style
    addStyle(wb, sheet = sheet_name_24, table_body_style1, rows = 2:(nrow(file_R_24)+1), cols = 1:ncol(file_R_24), gridExpand = TRUE)
  }
  
  
  saveWorkbook(wb, paste0(export_directory,export_file_name,".xlsx"), overwrite = TRUE)
}




