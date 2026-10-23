cat("\014")

# ==================================================================================================
# REACH MSNA 2026 Uganda - 7. Generate AO quality-check analysis
# ==================================================================================================
# PURPOSE
#   Reproduce the indicator requests in "Uganda_MSNA_Quality Checks_AOs.xlsx" using the FINAL
#   Uganda 2026 Kobo XLSForm and the processed datasets created by the existing cleaning pipeline.
#
# RECOMMENDED RUN POSITION
#   Run after Script 4 ("4.Process the CLs and DFs...") so checked_datasets_by_dataset is available.
#   The script also works after Script 1 because it falls back to raw_data_list / data_* objects.
#
# IMPORTANT ANALYTICAL RULES
#   1) This is a DATA-QUALITY / FIELD-MONITORING analysis, therefore estimates are UNWEIGHTED.
#   2) "Displacement" in the AO request is operationalised as the final-tool population group
#      variable hoh_type: refugee vs host_community. Overall results are also reported.
#   3) DK/PNA/special numeric codes are excluded from denominators and summary statistics.
#   4) Repeat-sheet indicators are linked back to the household via the parent/submission UUID and
#      inherit hoh_type and enumerator context from the household submission.
#   5) select_multiple questions are handled whether Kobo exports the original space-separated field,
#      split columns (question/choice -> question_choice_choice), or both.
#   6) The AO workbook contains several outdated/incorrect question codes. This script corrects them
#      against the final XLSForm, while preserving the original request text and row order.
#
# OUTPUT
#   A copy of the AO workbook with added columns:
#      H Overall result
#      I Refugee result
#      J Host community result
#      K Overall N
#      L Refugee N
#      M Host community N
#      N Technical note
#   Plus detail sheets for numeric results, enumerator "other" counts, time tracking and tool validation.
# ==================================================================================================

# --------------------------------------------------------------------------------------------------
# 0. Packages and paths
# --------------------------------------------------------------------------------------------------
required_packages <- c("dplyr", "tidyr", "purrr", "stringr", "readxl", "openxlsx", "janitor", "tibble")
for (pkg in required_packages) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    stop("Package '", pkg, "' is required for Script 7.", call. = FALSE)
  }
}

`%||%` <- function(x, y) if (is.null(x)) y else x


# --------------------------------------------------------------------------------------------------
# DEFINE INPUT FILE PATHS
# --------------------------------------------------------------------------------------------------

path_analysis_request <- 
  "C:/Users/Administrator/Documents/UG tasks/01_data_cleaning/MSN_U/MSNA_cleaning/inputs/Uganda_MSNA_Quality Checks_AOs.xlsx"




first_existing_file <- function(paths) {
  hit <- paths[file.exists(paths)]
  if (length(hit) == 0) NA_character_ else hit[[1]]
}

if (!exists("path_analysis_request")) {
  path_analysis_request <- first_existing_file(c(
    "inputs/Uganda_MSNA_Quality Checks_AOs.xlsx",
    "Uganda_MSNA_Quality Checks_AOs.xlsx"
  ))
}

if (is.na(path_analysis_request) || !file.exists(path_analysis_request)) {
  stop(
    "AO analysis request workbook not found. Set path_analysis_request to Uganda_MSNA_Quality Checks_AOs.xlsx.",
    call. = FALSE
  )
}

if (!exists("path_tool")) {
  path_tool <- first_existing_file(c(
    "inputs/REACH_MSNA2026_uganda_Kobo_final_1_14_aug_2026.xlsx",
    "REACH_MSNA2026_uganda_Kobo_final_1_14_aug_2026.xlsx"
  ))
}

if (is.na(path_tool) || !file.exists(path_tool)) {
  stop("Final Kobo XLSForm not found. Set path_tool to the final Uganda 2026 XLSForm.", call. = FALSE)
}

if (!exists("path_analysis_output")) {
  output_dir <- if (exists("path_cleaning_folder_today") && dir.exists(path_cleaning_folder_today)) {
    path_cleaning_folder_today
  } else {
    "outputs"
  }
  if (!dir.exists(output_dir)) dir.create(output_dir, recursive = TRUE)
  path_analysis_output <- file.path(
    output_dir,
    paste0("Uganda_MSNA_Quality_Checks_AOs_RESULTS_", format(Sys.Date(), "%Y%m%d"), ".xlsx")
  )
}

# --------------------------------------------------------------------------------------------------
# 1. Read AO request and final XLSForm metadata
# --------------------------------------------------------------------------------------------------
ao_request <- readxl::read_xlsx(
  path_analysis_request,
  sheet = "Data Analysis Request",
  .name_repair = "minimal"
) |>
  tibble::as_tibble()

if (nrow(ao_request) != 63) {
  warning(
    "The supplied AO request currently has ", nrow(ao_request),
    " indicator rows; this script was mapped to the 63-row version supplied for Uganda MSNA 2026."
  )
}

ao_request <- ao_request |>
  dplyr::mutate(
    request_id = dplyr::row_number(),
    excel_row = request_id + 1L
  )

survey_meta <- readxl::read_xlsx(path_tool, "survey", .name_repair = "minimal") |>
  janitor::clean_names() |>
  tibble::as_tibble()

choices_meta <- readxl::read_xlsx(path_tool, "choices", .name_repair = "minimal") |>
  janitor::clean_names() |>
  tibble::as_tibble()

# --------------------------------------------------------------------------------------------------
# 2. Get the most processed versions of all survey datasets
# --------------------------------------------------------------------------------------------------
get_msna_dataset <- function(dataset_name) {
  if (exists("checked_datasets_by_dataset") && is.list(checked_datasets_by_dataset) &&
      dataset_name %in% names(checked_datasets_by_dataset)) {
    x <- checked_datasets_by_dataset[[dataset_name]]
    if (is.data.frame(x)) return(tibble::as_tibble(x))
  }

  if (exists("raw_data_list") && is.list(raw_data_list) && dataset_name %in% names(raw_data_list)) {
    x <- raw_data_list[[dataset_name]]
    if (is.data.frame(x)) return(tibble::as_tibble(x))
  }

  object_candidates <- switch(
    dataset_name,
    Submissions = c("data_hh_main", "data", "raw_data"),
    roster = c("data_hh_roster", "data_roster"),
    edu_ind = c("data_edu_ind"),
    health_ind = c("data_health_ind"),
    nutrition_ind = c("data_nutrition_ind", "data_nut_ind"),
    child_protection_ind = c("data_child_protection_ind", "data_prot_ind"),
    character()
  )

  for (nm in object_candidates) {
    if (exists(nm, inherits = TRUE)) {
      x <- get(nm, inherits = TRUE)
      if (is.data.frame(x)) return(tibble::as_tibble(x))
    }
  }

  tibble::tibble()
}

hh     <- get_msna_dataset("Submissions")
roster <- get_msna_dataset("roster")
edu    <- get_msna_dataset("edu_ind")
health <- get_msna_dataset("health_ind")
nut    <- get_msna_dataset("nutrition_ind")
cp     <- get_msna_dataset("child_protection_ind")

if (!is.data.frame(hh) || nrow(hh) == 0) {
  stop("No household-level data were found. Run Script 1 (and preferably Scripts 2-4) first.", call. = FALSE)
}

# --------------------------------------------------------------------------------------------------
# 3. Core helpers: IDs, context, special values, select_multiple, formatting
# --------------------------------------------------------------------------------------------------
first_existing_col <- function(df, candidates) {
  hit <- candidates[candidates %in% names(df)]
  if (length(hit) == 0) NA_character_ else hit[[1]]
}

household_uuid_col <- first_existing_col(
  hh,
  c("uuid", "_uuid", "submission_uuid", "_submission__uuid", "_id", "instance_name", "_submission_id")
)
if (is.na(household_uuid_col)) stop("No household UUID column found.", call. = FALSE)

hh <- hh |>
  dplyr::mutate(.hh_uuid = as.character(.data[[household_uuid_col]]))

# Final-tool population group used for the AO workbook's "Displacement" disaggregation.
if (!"hoh_type" %in% names(hh)) {
  stop("hoh_type is missing from the household dataset; cannot produce the requested displacement/population-group disaggregation.", call. = FALSE)
}

if (!"enum_id" %in% names(hh)) hh$enum_id <- NA_character_

hh <- hh |>
  dplyr::mutate(
    hoh_type = stringr::str_to_lower(stringr::str_squish(as.character(hoh_type))),
    enum_id = as.character(enum_id)
  )

household_context <- hh |>
  dplyr::transmute(
    .hh_uuid,
    hoh_type_hh = hoh_type,
    enum_id_hh = enum_id,
    admin2_hh = if ("admin2" %in% names(hh)) as.character(admin2) else NA_character_,
    point_number_hh = if ("point_number" %in% names(hh)) as.character(point_number) else NA_character_
  ) |>
  dplyr::distinct(.hh_uuid, .keep_all = TRUE)

find_repeat_parent_col <- function(df) {
  first_existing_col(
    df,
    c("uuid", "parent_uuid", "submission_uuid", "_submission__uuid", "_submission_uuid",
      "_parent_uuid", "_submission__id", "_submission_id", "parent_instance_name")
  )
}

enrich_repeat_context <- function(df) {
  if (!is.data.frame(df) || nrow(df) == 0) return(tibble::as_tibble(df))

  parent_col <- find_repeat_parent_col(df)
  if (is.na(parent_col)) {
    warning("Repeat dataset has no usable parent/submission UUID and could not be linked to household context.")
    return(tibble::as_tibble(df))
  }

  out <- tibble::as_tibble(df) |>
    dplyr::mutate(.hh_uuid = as.character(.data[[parent_col]])) |>
    dplyr::left_join(household_context, by = ".hh_uuid")

  if (!"hoh_type" %in% names(out)) out$hoh_type <- out$hoh_type_hh
  else out$hoh_type <- dplyr::coalesce(as.character(out$hoh_type), out$hoh_type_hh)

  if (!"enum_id" %in% names(out)) out$enum_id <- out$enum_id_hh
  else out$enum_id <- dplyr::coalesce(as.character(out$enum_id), out$enum_id_hh)

  out |>
    dplyr::mutate(
      hoh_type = stringr::str_to_lower(stringr::str_squish(as.character(hoh_type))),
      enum_id = as.character(enum_id)
    ) |>
    dplyr::select(-dplyr::any_of(c("hoh_type_hh", "enum_id_hh", "admin2_hh", "point_number_hh")))
}

roster <- enrich_repeat_context(roster)
edu    <- enrich_repeat_context(edu)
health <- enrich_repeat_context(health)
nut    <- enrich_repeat_context(nut)
cp     <- enrich_repeat_context(cp)

analysis_groups <- c("Overall", "Refugee", "Host community")

filter_population_group <- function(df, group_name) {
  if (!is.data.frame(df) || nrow(df) == 0) return(df)
  if (group_name == "Overall") return(df)
  if (!"hoh_type" %in% names(df)) return(df[0, , drop = FALSE])

  code <- dplyr::case_when(
    group_name == "Refugee" ~ "refugee",
    group_name == "Host community" ~ "host_community",
    TRUE ~ NA_character_
  )
  df |>
    dplyr::filter(hoh_type == code)
}

clean_numeric <- function(x, special_values = c(-999, -990, 999)) {
  y <- suppressWarnings(as.numeric(gsub(",", "", as.character(x))))
  y[y %in% special_values] <- NA_real_
  y[!is.finite(y)] <- NA_real_
  y
}

valid_response <- function(x) {
  y <- stringr::str_to_lower(stringr::str_squish(as.character(x)))
  !is.na(y) & y != "" & !y %in% c("dnk", "don't know", "don’t know", "pnta", "prefer not to answer")
}

# Handles both a space-separated select_multiple field and Kobo split columns repaired by Script 1.
selected_vec <- function(df, variable, choice) {
  n <- nrow(df)
  if (n == 0) return(logical())
  out <- rep(FALSE, n)

  if (variable %in% names(df)) {
    x <- stringr::str_squish(as.character(df[[variable]]))
    pat <- paste0("(^|\\s)", choice, "(\\s|$)")
    out <- out | (!is.na(x) & stringr::str_detect(x, pat))
  }

  choice_clean <- janitor::make_clean_names(choice)
  split_candidates <- unique(c(
    paste0(variable, "_choice_", choice_clean),
    paste0(variable, "_", choice_clean),
    paste0(variable, "_choice_", choice),
    paste0(variable, "_", choice)
  ))
  split_candidates <- split_candidates[split_candidates %in% names(df)]

  if (length(split_candidates) > 0) {
    for (sc in split_candidates) {
      z <- stringr::str_to_lower(stringr::str_squish(as.character(df[[sc]])))
      out <- out | z %in% c("1", "true", "yes", "selected", choice, choice_clean)
    }
  }

  out[is.na(out)] <- FALSE
  out
}

fmt_number <- function(x, digits = 2) {
  if (length(x) == 0 || is.na(x)) return(NA_character_)
  format(round(x, digits), nsmall = digits, trim = TRUE, big.mark = ",", scientific = FALSE)
}

fmt_integerish <- function(x) {
  if (length(x) == 0 || is.na(x)) return(NA_character_)
  format(round(x, 0), nsmall = 0, trim = TRUE, big.mark = ",", scientific = FALSE)
}

fmt_percent <- function(x, digits = 1) {
  if (length(x) == 0 || is.na(x)) return(NA_character_)
  paste0(format(round(100 * x, digits), nsmall = digits, trim = TRUE, scientific = FALSE), "%")
}

result_object <- function(display = NA_character_, n = NA_integer_, detail = NULL, note = NA_character_) {
  if (is.null(detail)) detail <- tibble::tibble(statistic = NA_character_, value = NA_real_)
  list(display = display, n = as.integer(n), detail = detail, note = note)
}

result_numeric_stat <- function(df, variable, statistic,
                                special_values = c(-999, -990, 999), digits = 2,
                                currency_style = FALSE) {
  if (!variable %in% names(df)) return(result_object(note = paste0("Variable not found: ", variable)))
  x <- clean_numeric(df[[variable]], special_values)
  x <- x[!is.na(x)]
  if (length(x) == 0) return(result_object(n = 0L, note = "No valid observations"))

  value <- switch(
    statistic,
    mean = mean(x),
    median = stats::median(x),
    min = min(x),
    max = max(x),
    stop("Unsupported statistic: ", statistic)
  )

  display <- if (currency_style) fmt_integerish(value) else fmt_number(value, digits)
  result_object(
    display = display,
    n = length(x),
    detail = tibble::tibble(statistic = statistic, value = as.numeric(value))
  )
}

result_multi_numeric <- function(df, variable, stats = c("min", "median", "max"),
                                 special_values = c(-999, -990, 999), digits = 2) {
  if (!variable %in% names(df)) return(result_object(note = paste0("Variable not found: ", variable)))
  x <- clean_numeric(df[[variable]], special_values)
  x <- x[!is.na(x)]
  if (length(x) == 0) return(result_object(n = 0L, note = "No valid observations"))

  vals <- purrr::map_dbl(stats, function(s) {
    switch(s, mean = mean(x), median = stats::median(x), min = min(x), max = max(x))
  })
  names(vals) <- stats

  display <- paste(
    paste0(stringr::str_to_title(names(vals)), ": ", purrr::map_chr(vals, ~ fmt_number(.x, digits))),
    collapse = " | "
  )

  result_object(
    display = display,
    n = length(x),
    detail = tibble::tibble(statistic = names(vals), value = as.numeric(vals))
  )
}

result_pct_one <- function(df, variable, numerator_codes, denominator_codes = NULL) {
  if (!variable %in% names(df)) return(result_object(note = paste0("Variable not found: ", variable)))
  x <- stringr::str_to_lower(stringr::str_squish(as.character(df[[variable]])))

  if (is.null(denominator_codes)) denom <- valid_response(x)
  else denom <- !is.na(x) & x %in% denominator_codes

  numerator <- denom & x %in% numerator_codes
  n <- sum(denom, na.rm = TRUE)
  if (n == 0) return(result_object(n = 0L, note = "No valid denominator"))
  p <- sum(numerator, na.rm = TRUE) / n

  result_object(
    display = fmt_percent(p),
    n = n,
    detail = tibble::tibble(statistic = "percent", value = p)
  )
}

result_pct_multiple <- function(df, variable, numerator_choices,
                                denominator_exclude_choices = c("dnk", "pnta")) {
  if (nrow(df) == 0) return(result_object(n = 0L, note = "No records"))

  excluded <- rep(FALSE, nrow(df))
  for (ch in denominator_exclude_choices) excluded <- excluded | selected_vec(df, variable, ch)

  # If the original select_multiple variable exists, a blank response is not a valid denominator.
  if (variable %in% names(df)) {
    base_nonblank <- !is.na(df[[variable]]) & stringr::str_squish(as.character(df[[variable]])) != ""
  } else {
    # With split-only exports, consider any non-missing split response as an answered question.
    prefix <- paste0("^", variable, "(_choice_)?")
    split_cols <- names(df)[stringr::str_detect(names(df), prefix)]
    base_nonblank <- if (length(split_cols) > 0) {
      apply(df[, split_cols, drop = FALSE], 1, function(z) any(!is.na(z) & as.character(z) != ""))
    } else {
      rep(TRUE, nrow(df))
    }
  }

  denom <- base_nonblank & !excluded
  numerator <- rep(FALSE, nrow(df))
  for (ch in numerator_choices) numerator <- numerator | selected_vec(df, variable, ch)
  numerator <- numerator & denom

  n <- sum(denom, na.rm = TRUE)
  if (n == 0) return(result_object(n = 0L, note = "No valid denominator"))
  p <- sum(numerator, na.rm = TRUE) / n

  result_object(
    display = fmt_percent(p),
    n = n,
    detail = tibble::tibble(statistic = "percent", value = p)
  )
}

result_pct_numeric_positive <- function(df, variable) {
  if (!variable %in% names(df)) return(result_object(note = paste0("Variable not found: ", variable)))
  x <- clean_numeric(df[[variable]], special_values = c(-999, -990, 999))
  denom <- !is.na(x)
  n <- sum(denom)
  if (n == 0) return(result_object(n = 0L, note = "No valid denominator"))
  p <- mean(x[denom] > 0)
  result_object(
    display = fmt_percent(p),
    n = n,
    detail = tibble::tibble(statistic = "percent", value = p)
  )
}

# --------------------------------------------------------------------------------------------------
# 4. Derived variables required by the AO workbook
# --------------------------------------------------------------------------------------------------
# 4.1 Number of children <18 per household from roster
if (nrow(roster) > 0 && "ind_age" %in% names(roster)) {
  children_by_hh <- roster |>
    dplyr::mutate(ind_age_num = clean_numeric(ind_age, special_values = c(999, -999, -990))) |>
    dplyr::group_by(.hh_uuid) |>
    dplyr::summarise(
      n_children_under18 = sum(!is.na(ind_age_num) & ind_age_num < 18, na.rm = TRUE),
      .groups = "drop"
    )

  hh <- hh |>
    dplyr::left_join(children_by_hh, by = ".hh_uuid") |>
    dplyr::mutate(n_children_under18 = dplyr::coalesce(n_children_under18, 0L))
} else {
  hh$n_children_under18 <- NA_real_
}

# 4.2 Household expenditure totals from the actual amount fields in the final XLSForm
freq_amount_map <- c(
  food = "cm_expenditure_frequent_food",
  rent = "cm_expenditure_frequent_rent",
  water = "cm_expenditure_frequent_water",
  feminine_hygiene_products = "cm_expenditure_frequent_nfi_feminine",
  other_hygiene_items_personal_and_domestic_e_g_laun = "cm_expenditure_frequent_nfi_other_hygiene",
  nfi = "cm_expenditure_frequent_nfi_non_hygiene_non_food",
  utilities = "cm_expenditure_frequent_utilities",
  transportation = "cm_expenditure_frequent_transportation",
  energy_firewood_charcoal_fuel_etc_i = "cm_expenditure_frequent_fuel",
  communications = "cm_expenditure_frequent_communication",
  substances_such_as_alcohol_tobacco_mairungi_etc = "cm_expenditure_frequent_substances_30d",
  childcare_elderly_care_care_for_disabled_people_e = "cm_expenditure_frequent_care_services_30d",
  recreation_sport_and_culture = "cm_expenditure_frequent_recreation_30d",
  pet_care_pet_food_veterinary_care_etc = "cm_expenditure_frequent_pet_care_30d",
  other = "cm_expenditure_frequent_other"
)

infreq_amount_map <- c(
  clothing = "cm_expenditure_infrequent_clothing",
  nfi = "cm_expenditure_infrequent_nfi",
  health = "cm_expenditure_infrequent_health",
  education = "cm_expenditure_infrequent_education",
  debt = "cm_expenditure_infrequent_debt",
  shelter = "cm_expenditure_infrequent_shelter",
  issuing_documents_related_to_refugee_registration = "cm_expenditure_documents_6m",
  livelihoods_or_business_assets_agricultural_equipm = "cm_expenditure_infrequent_business_assets",
  legal_assistance_psychosocial_support_disability = "cm_expenditure_legal_protection_6m"
)

calculate_expenditure_total <- function(df, select_variable, amount_map, unmeasured_other = FALSE) {
  n <- nrow(df)
  out <- rep(0, n)
  invalid <- rep(FALSE, n)
  any_valid_category <- rep(FALSE, n)

  for (choice in names(amount_map)) {
    amount_var <- amount_map[[choice]]
    selected <- selected_vec(df, select_variable, choice)
    any_valid_category <- any_valid_category | selected

    if (!amount_var %in% names(df)) {
      invalid <- invalid | selected
      next
    }

    amount <- clean_numeric(df[[amount_var]], special_values = c(-999, -990))
    invalid <- invalid | (selected & is.na(amount))
    out <- out + ifelse(selected & !is.na(amount), amount, 0)
  }

  no_expenditure <- selected_vec(df, select_variable, "no_expenditure")
  unknown <- selected_vec(df, select_variable, "dnk") | selected_vec(df, select_variable, "pnta")

  if (unmeasured_other) {
    # Final tool captures the text description of "other" infrequent expenditure but no amount field.
    invalid <- invalid | selected_vec(df, select_variable, "other")
  }

  out[no_expenditure] <- 0
  invalid[no_expenditure] <- FALSE
  invalid <- invalid | unknown | (!any_valid_category & !no_expenditure)
  out[invalid] <- NA_real_
  out
}

hh$cm_expenditure_frequent_total_30d <- calculate_expenditure_total(
  hh, "cm_expenditure_frequent", freq_amount_map, unmeasured_other = FALSE
)

hh$cm_expenditure_infrequent_total_6m <- calculate_expenditure_total(
  hh, "cm_expenditure_infrequent", infreq_amount_map, unmeasured_other = TRUE
)

# 4.3 Identify all final-tool "specify other" text variables and count filled entries per enumerator
other_question_names <- survey_meta |>
  dplyr::filter(
    stringr::str_to_lower(as.character(type)) == "text",
    stringr::str_detect(
      stringr::str_to_lower(as.character(label_english_en)),
      "other.*specify|specify.*other"
    )
  ) |>
  dplyr::pull(name) |>
  as.character() |>
  unique()

collect_other_entries <- function(df, dataset_name) {
  if (!is.data.frame(df) || nrow(df) == 0) return(tibble::tibble())
  vars <- intersect(other_question_names, names(df))
  if (length(vars) == 0) return(tibble::tibble())

  id_cols <- intersect(c(".hh_uuid", "hoh_type", "enum_id"), names(df))
  df |>
    dplyr::select(dplyr::all_of(id_cols), dplyr::all_of(vars)) |>
    tidyr::pivot_longer(
      cols = dplyr::all_of(vars),
      names_to = "question",
      values_to = "response"
    ) |>
    dplyr::mutate(
      response = stringr::str_squish(as.character(response)),
      dataset = dataset_name
    ) |>
    dplyr::filter(!is.na(response), response != "", !stringr::str_to_lower(response) %in% c("na", "nan", "null", "n/a"))
}

other_entries <- dplyr::bind_rows(
  collect_other_entries(hh, "Submissions"),
  collect_other_entries(roster, "roster"),
  collect_other_entries(edu, "edu_ind"),
  collect_other_entries(health, "health_ind"),
  collect_other_entries(nut, "nutrition_ind"),
  collect_other_entries(cp, "child_protection_ind")
)

# --------------------------------------------------------------------------------------------------
# 5. AO-request-specific result functions
# --------------------------------------------------------------------------------------------------
result_mean_others_per_enumerator <- function(group_name) {
  d <- filter_population_group(other_entries, group_name)
  if (nrow(d) == 0) return(result_object(n = 0L, note = "No filled specify-other responses"))

  # Include enumerators with zero 'other' responses by using all enumerators in the group as denominator.
  hh_group <- filter_population_group(hh, group_name)
  enums <- hh_group |>
    dplyr::filter(!is.na(enum_id), stringr::str_squish(enum_id) != "") |>
    dplyr::distinct(enum_id)

  counts <- d |>
    dplyr::filter(!is.na(enum_id), stringr::str_squish(enum_id) != "") |>
    dplyr::count(enum_id, name = "n_other")

  counts <- enums |>
    dplyr::left_join(counts, by = "enum_id") |>
    dplyr::mutate(n_other = dplyr::coalesce(n_other, 0L))

  if (nrow(counts) == 0) return(result_object(n = 0L, note = "No valid enumerator IDs"))
  m <- mean(counts$n_other)
  result_object(
    display = fmt_number(m, 2),
    n = nrow(counts),
    detail = tibble::tibble(statistic = "mean_other_responses_per_enumerator", value = m),
    note = paste0("Across ", nrow(counts), " enumerators; all final-tool 'specify other' text fields included.")
  )
}

result_infrequent_other_per_enumerator <- function(group_name) {
  d <- filter_population_group(hh, group_name)
  if (nrow(d) == 0) return(result_object(n = 0L))

  d <- d |>
    dplyr::mutate(selected_other = selected_vec(d, "cm_expenditure_infrequent", "other"))

  enums <- d |>
    dplyr::filter(!is.na(enum_id), stringr::str_squish(enum_id) != "") |>
    dplyr::group_by(enum_id) |>
    dplyr::summarise(n_other_infrequent = sum(selected_other, na.rm = TRUE), .groups = "drop")

  if (nrow(enums) == 0) return(result_object(n = 0L, note = "No valid enumerator IDs"))

  total <- sum(enums$n_other_infrequent)
  mean_e <- mean(enums$n_other_infrequent)
  max_e <- max(enums$n_other_infrequent)

  result_object(
    display = paste0("Total: ", total, " | Mean/enum: ", fmt_number(mean_e, 2), " | Max/enum: ", max_e),
    n = nrow(enums),
    detail = tibble::tibble(
      statistic = c("total_other_infrequent", "mean_per_enumerator", "max_per_enumerator"),
      value = c(total, mean_e, max_e)
    )
  )
}

infrequent_choice_labels <- c(
  clothing = "Clothing",
  nfi = "Non-food household items for infrequent purchase",
  health = "Health-related expenditures",
  education = "Education-related expenditures",
  debt = "Debt repayment",
  shelter = "Shelter",
  issuing_documents_related_to_refugee_registration = "Refugee-registration/document expenditures",
  livelihoods_or_business_assets_agricultural_equipm = "Livelihood/business assets",
  legal_assistance_psychosocial_support_disability = "Legal/psychosocial/disability/special-needs",
  other = "Other infrequent expenditure"
)

result_infrequent_type_percentages <- function(group_name) {
  d <- filter_population_group(hh, group_name)
  if (nrow(d) == 0) return(result_object(n = 0L))

  no_exp <- selected_vec(d, "cm_expenditure_infrequent", "no_expenditure")
  dnk <- selected_vec(d, "cm_expenditure_infrequent", "dnk")
  pnta <- selected_vec(d, "cm_expenditure_infrequent", "pnta")
  any_exp <- rep(FALSE, nrow(d))
  for (ch in names(infrequent_choice_labels)) any_exp <- any_exp | selected_vec(d, "cm_expenditure_infrequent", ch)
  denom <- any_exp & !no_exp & !dnk & !pnta
  n <- sum(denom)
  if (n == 0) return(result_object(n = 0L, note = "No households with a valid infrequent expenditure category"))

  percentages <- purrr::map_dbl(names(infrequent_choice_labels), function(ch) {
    sum(selected_vec(d, "cm_expenditure_infrequent", ch) & denom) / n
  })
  names(percentages) <- names(infrequent_choice_labels)

  display <- paste(
    paste0(unname(infrequent_choice_labels), ": ", purrr::map_chr(percentages, fmt_percent)),
    collapse = "\n"
  )

  result_object(
    display = display,
    n = n,
    detail = tibble::tibble(
      statistic = paste0("percent_", names(percentages)),
      value = as.numeric(percentages)
    ),
    note = "Denominator = households reporting at least one valid infrequent-expenditure category; multiple responses allowed."
  )
}

result_mean_children <- function(group_name) {
  d <- filter_population_group(hh, group_name)
  result_numeric_stat(d, "n_children_under18", "mean", special_values = numeric(0), digits = 2)
}

# --------------------------------------------------------------------------------------------------
# 6. Corrected final-tool mapping for all 63 AO request rows
# --------------------------------------------------------------------------------------------------
# Each handler returns the requested result for one population group.
handlers <- vector("list", nrow(ao_request))
actual_variable <- rep(NA_character_, nrow(ao_request))
actual_dataset <- rep(NA_character_, nrow(ao_request))
mapping_note <- rep(NA_character_, nrow(ao_request))

set_handler <- function(id, dataset, variable, fun, note = NA_character_) {
  if (id <= length(handlers)) {
    handlers[[id]] <<- fun
    actual_dataset[[id]] <<- dataset
    actual_variable[[id]] <<- variable
    mapping_note[[id]] <<- note
  }
}

# Demographics ------------------------------------------------------------------------------------
set_handler(1, "Submissions", "hh_size", function(g) {
  result_numeric_stat(filter_population_group(hh, g), "hh_size", "mean", special_values = numeric(0), digits = 2)
})
set_handler(2, "Submissions", "resp_gender", function(g) {
  result_pct_one(filter_population_group(hh, g), "resp_gender", "female", c("male", "female"))
}, "Final-tool response code is female (label: Female / woman).")
set_handler(3, "Submissions", "resp_age", function(g) {
  result_numeric_stat(filter_population_group(hh, g), "resp_age", "mean", special_values = 999, digits = 1)
})
set_handler(4, "Submissions", "resp_hoh_yn", function(g) {
  result_pct_one(filter_population_group(hh, g), "resp_hoh_yn", "no", c("yes", "no"))
})
set_handler(5, "roster -> household derived", "n_children_under18", result_mean_children,
            "Derived by counting roster members with valid ind_age < 18 for each household.")
set_handler(6, "all datasets", "all final-tool specify-other text fields", result_mean_others_per_enumerator,
            "Derived dynamically from XLSForm text questions labelled 'If other, please specify'.")

# Cash and Markets --------------------------------------------------------------------------------
set_handler(7, "Submissions", "cm_income_total", function(g) {
  result_numeric_stat(filter_population_group(hh, g), "cm_income_total", "max", special_values = -999, digits = 0, currency_style = TRUE)
})
set_handler(8, "Submissions", "cm_income_total", function(g) {
  result_numeric_stat(filter_population_group(hh, g), "cm_income_total", "min", special_values = -999, digits = 0, currency_style = TRUE)
})
set_handler(9, "Submissions", "cm_income_total", function(g) {
  result_numeric_stat(filter_population_group(hh, g), "cm_income_total", "median", special_values = -999, digits = 0, currency_style = TRUE)
})

set_handler(10, "Submissions", "cm_expenditure_frequent_total_30d", function(g) {
  result_numeric_stat(filter_population_group(hh, g), "cm_expenditure_frequent_total_30d", "max", special_values = numeric(0), digits = 0, currency_style = TRUE)
}, "Corrected: cm_expenditure_frequent is a select_multiple selector; total expenditure is derived from all selected 30-day amount fields.")
set_handler(11, "Submissions", "cm_expenditure_frequent_total_30d", function(g) {
  result_numeric_stat(filter_population_group(hh, g), "cm_expenditure_frequent_total_30d", "median", special_values = numeric(0), digits = 0, currency_style = TRUE)
}, "Corrected to derived 30-day total expenditure.")
set_handler(12, "Submissions", "cm_expenditure_frequent_total_30d", function(g) {
  result_numeric_stat(filter_population_group(hh, g), "cm_expenditure_frequent_total_30d", "min", special_values = numeric(0), digits = 0, currency_style = TRUE)
}, "Corrected to derived 30-day total expenditure.")

set_handler(13, "Submissions", "cm_expenditure_infrequent_total_6m", function(g) {
  result_numeric_stat(filter_population_group(hh, g), "cm_expenditure_infrequent_total_6m", "max", special_values = numeric(0), digits = 0, currency_style = TRUE)
}, "Corrected: derived from 6-month amount fields. If 'other' is selected, total is set NA because the final tool captures its description but no amount.")
set_handler(14, "Submissions", "cm_expenditure_infrequent_total_6m", function(g) {
  result_numeric_stat(filter_population_group(hh, g), "cm_expenditure_infrequent_total_6m", "median", special_values = numeric(0), digits = 0, currency_style = TRUE)
}, "Corrected to derived 6-month total expenditure.")
set_handler(15, "Submissions", "cm_expenditure_infrequent_total_6m", function(g) {
  result_numeric_stat(filter_population_group(hh, g), "cm_expenditure_infrequent_total_6m", "min", special_values = numeric(0), digits = 0, currency_style = TRUE)
}, "Corrected to derived 6-month total expenditure.")
set_handler(16, "Submissions", "cm_expenditure_infrequent = other", result_infrequent_other_per_enumerator,
            "Enumerator-level count of households selecting the 'other' infrequent-expenditure category.")
set_handler(17, "Submissions", "cm_expenditure_infrequent", result_infrequent_type_percentages,
            "Uses every actual infrequent-expenditure choice in the final XLSForm, including categories omitted from the AO request text.")

# Education repeat ---------------------------------------------------------------------------------
set_handler(18, "edu_ind", "edu_access", function(g) {
  result_pct_one(filter_population_group(edu, g), "edu_access", "no", c("yes", "no"))
}, "Person-level denominator: children for whom edu_access is asked (tool relevance: age 3-18), excluding DK/PNA.")
set_handler(19, "edu_ind", "edu_disrupted_displaced", function(g) {
  result_pct_one(filter_population_group(edu, g), "edu_disrupted_displaced", "yes", c("yes", "no"))
}, "Asked only when edu_access = yes; denominator follows tool relevance and excludes DK/PNA.")
set_handler(20, "edu_ind", "edu_disrupted_teacher", function(g) {
  result_pct_one(filter_population_group(edu, g), "edu_disrupted_teacher", "yes", c("yes", "no"))
})
set_handler(21, "edu_ind", "edu_disrupted_hazards", function(g) {
  result_pct_one(filter_population_group(edu, g), "edu_disrupted_hazards", "yes", c("yes", "no"))
})

# Health / Washington Group questions are in the roster repeat -----------------------------------
set_handler(22, "roster", "wgq_vision", function(g) {
  result_pct_one(filter_population_group(roster, g), "wgq_vision",
                 c("a_lot_of_difficulty", "cannot_do_at_all"),
                 c("no_difficulty", "some_difficulty", "a_lot_of_difficulty", "cannot_do_at_all"))
}, "Corrected AO typo: the first duplicate wgq_hearing row is the missing Washington Group vision domain (wgq_vision).")
set_handler(23, "roster", "wgq_hearing", function(g) {
  result_pct_one(filter_population_group(roster, g), "wgq_hearing",
                 c("a_lot_of_difficulty", "cannot_do_at_all"),
                 c("no_difficulty", "some_difficulty", "a_lot_of_difficulty", "cannot_do_at_all"))
})
set_handler(24, "roster", "wgq_mobility", function(g) {
  result_pct_one(filter_population_group(roster, g), "wgq_mobility",
                 c("a_lot_of_difficulty", "cannot_do_at_all"),
                 c("no_difficulty", "some_difficulty", "a_lot_of_difficulty", "cannot_do_at_all"))
})
set_handler(25, "roster", "wgq_cognition", function(g) {
  result_pct_one(filter_population_group(roster, g), "wgq_cognition",
                 c("a_lot_of_difficulty", "cannot_do_at_all"),
                 c("no_difficulty", "some_difficulty", "a_lot_of_difficulty", "cannot_do_at_all"))
})
set_handler(26, "roster", "wgq_self_care", function(g) {
  result_pct_one(filter_population_group(roster, g), "wgq_self_care",
                 c("a_lot_of_difficulty", "cannot_do_at_all"),
                 c("no_difficulty", "some_difficulty", "a_lot_of_difficulty", "cannot_do_at_all"))
})
set_handler(27, "roster", "wgq_communication", function(g) {
  result_pct_one(filter_population_group(roster, g), "wgq_communication",
                 c("a_lot_of_difficulty", "cannot_do_at_all"),
                 c("no_difficulty", "some_difficulty", "a_lot_of_difficulty", "cannot_do_at_all"))
})

# SNFI ------------------------------------------------------------------------------------------------
set_handler(28, "Submissions", "snfi_shelter_type", function(g) {
  result_pct_one(filter_population_group(hh, g), "snfi_shelter_type", "collective_center",
                 c("collective_center", "individual_shelter", "hosted", "hosting", "none"))
})
set_handler(29, "Submissions", "hlp_occupancy", function(g) {
  result_pct_one(filter_population_group(hh, g), "hlp_occupancy", "no_agreement",
                 c("ownership", "rented", "hosted_free", "no_agreement", "other"))
}, "Final-tool response code for squatting is no_agreement.")
set_handler(30, "Submissions", "snfi_shelter_damage", function(g) {
  result_pct_multiple(filter_population_group(hh, g), "snfi_shelter_damage", "none")
}, "select_multiple; 'none' = No damage and is exclusive by XLSForm constraint.")
set_handler(31, "Submissions", "snfi_shelter_issue", function(g) {
  result_pct_multiple(filter_population_group(hh, g), "snfi_shelter_issue", "none")
}, "Corrected AO question code: final tool uses snfi_shelter_issue; 'none' = No noticeable issue.")
set_handler(32, "Submissions", "snfi_essential_items_missing", function(g) {
  result_pct_multiple(filter_population_group(hh, g), "snfi_essential_items_missing", "none")
})

# WASH ------------------------------------------------------------------------------------------------
set_handler(33, "Submissions", "wash_hwise_drink", function(g) {
  result_pct_one(filter_population_group(hh, g), "wash_hwise_drink", c("often", "always"),
                 c("never", "rarely", "sometimes", "often", "always"))
})
set_handler(34, "Submissions", "wash_drinking_water_source", function(g) {
  result_pct_one(
    filter_population_group(hh, g),
    "wash_drinking_water_source",
    c("unprotected_well", "unprotected_spring", "surface_water", "tank_truck", "cart_tank", "bottled_water"),
    c("piped_dwelling", "piped_compound", "piped_neighbour", "tap", "borehole", "protected_well",
      "unprotected_well", "well_spring", "unprotected_spring", "rainwater_collection", "tank_truck",
      "cart_tank", "kiosk", "bottled_water", "sachet_water", "surface_water",
      "shaduf_hand_operated_device_for_lifting_water_usin", "other")
  )
}, "Tracks exactly the AO workbook's listed source categories; this is not a full JMP drinking-water classification.")
set_handler(35, "Submissions", "wash_sanitation_facility", function(g) {
  result_pct_one(
    filter_population_group(hh, g),
    "wash_sanitation_facility",
    c("flush_open_drain", "flush_dnk_where", "pit_latrine_wo_slab", "twin_pit_latrine_wo_slab",
      "bucket", "other_container", "hanging_toilet", "none"),
    c("flush_piped_sewer", "flush_septic_tank", "flush_pit_latrine", "flush_open_drain", "flush_dnk_where",
      "pit_latrine_slab", "pit_latrine_wo_slab", "twin_pit_latrine_slab", "twin_pit_latrine_wo_slab",
      "compost", "bucket", "other_container", "hanging_toilet", "none", "other")
  )
}, "Tracks the AO workbook's listed unsafe toilet categories.")
set_handler(36, "Submissions", "wash_sanitation_facility_sharing_yn", function(g) {
  result_pct_one(filter_population_group(hh, g), "wash_sanitation_facility_sharing_yn", "yes", c("yes", "no"))
}, "Numerator set to 'yes'; AO workbook omitted the response option.")
set_handler(37, "Submissions", "wash_handwashing_facility", function(g) {
  result_pct_one(filter_population_group(hh, g), "wash_handwashing_facility", "no_permission",
                 c("available_fixed_in_dwelling", "available_fixed_in_plot", "available_mobile", "none", "no_permission", "other"))
})
set_handler(38, "Submissions", "wash_soap_reported_yn", function(g) {
  result_pct_one(filter_population_group(hh, g), "wash_soap_reported_yn", "no", c("yes", "no"))
}, "This question is relevant only when wash_handwashing_facility = no_permission, so the denominator is only households asked the question.")

# Food security: final-tool FCS variables are fsl_fcs_* -------------------------------------------
fcs_vars <- c(
  "fsl_fcs_cereal", "fsl_fcs_legumes", "fsl_fcs_dairy", "fsl_fcs_meat", "fsl_fcs_veg",
  "fsl_fcs_fruit", "fsl_fcs_oil", "fsl_fcs_sugar", "fsl_fcs_condiments"
)
for (j in seq_along(fcs_vars)) {
  id <- 38 + j
  var <- fcs_vars[[j]]
  set_handler(id, "Submissions", var, local({
    v <- var
    function(g) result_multi_numeric(filter_population_group(hh, g), v, c("min", "median", "max"),
                                     special_values = numeric(0), digits = 1)
  }), "Corrected AO question-code prefix from fcs_* to final-tool fsl_fcs_*. Indicator text says min, median and max; those three statistics are reported.")
}

rcsi_vars <- c("fsl_rcsi_lessquality", "fsl_rcsi_borrow", "fsl_rcsi_mealsize", "fsl_rcsi_mealadult", "fsl_rcsi_mealnb")
for (j in seq_along(rcsi_vars)) {
  id <- 47 + j
  var <- rcsi_vars[[j]]
  set_handler(id, "Submissions", var, local({
    v <- var
    function(g) result_pct_numeric_positive(filter_population_group(hh, g), v)
  }), "Percent using the strategy = valid responses with number of days > 0; denominator includes valid 0-7 day responses.")
}

hhs_vars <- c("fsl_hhs_nofoodhh", "fsl_hhs_sleephungry", "fsl_hhs_alldaynight")
for (j in seq_along(hhs_vars)) {
  id <- 52 + j
  var <- hhs_vars[[j]]
  set_handler(id, "Submissions", var, local({
    v <- var
    function(g) result_pct_one(filter_population_group(hh, g), v, "yes", c("yes", "no"))
  }), "Numerator = yes; denominator = yes/no responses.")
}

# Time tracking ------------------------------------------------------------------------------------
time_mapping <- c(
  "duration_demographics_roster_min",
  "duration_health_min",
  "duration_cash_markets_min",
  "duration_protection_min"
)
time_notes <- c(
  "Individual loop mapped to final-tool Demographics and household roster section duration.",
  "Health mapped to duration_health_min.",
  "Expenditures mapped to final-tool Cash and markets duration.",
  "Protection mapped to duration_protection_min."
)
for (j in seq_along(time_mapping)) {
  id <- 55 + j
  var <- time_mapping[[j]]
  set_handler(id, "Submissions", var, local({
    v <- var
    function(g) result_numeric_stat(filter_population_group(hh, g), v, "min", special_values = numeric(0), digits = 2)
  }), time_notes[[j]])
}

# The last four AO rows say only "Minimal number of minutes spent on the section" and do not name a
# section. Do NOT guess. They are intentionally left blank in the main sheet; every available section
# duration is exported in the dedicated "Time Tracking All Sections" sheet below.
for (id in 60:min(63, nrow(ao_request))) {
  set_handler(id, "Submissions", NA_character_, function(g) {
    result_object(
      display = "NOT SPECIFIED",
      n = NA_integer_,
      note = "The AO request row does not identify a section. See 'Time Tracking All Sections' for all final-tool section minima."
    )
  }, "AO workbook does not specify which section this row refers to; no section was guessed.")
}

# --------------------------------------------------------------------------------------------------
# 7. Run every AO request for Overall, Refugee and Host community
# --------------------------------------------------------------------------------------------------
run_one_request <- function(id) {
  handler <- handlers[[id]]
  if (is.null(handler)) {
    return(tibble::tibble(
      request_id = id,
      disaggregation_value = analysis_groups,
      display = NA_character_,
      n = NA_integer_,
      technical_note = "No handler mapped"
    ))
  }

  purrr::map_dfr(analysis_groups, function(grp) {
    ans <- tryCatch(
      handler(grp),
      error = function(e) result_object(note = paste0("ERROR: ", conditionMessage(e)))
    )

    detail <- ans$detail
    if (is.null(detail) || nrow(detail) == 0) detail <- tibble::tibble(statistic = NA_character_, value = NA_real_)

    detail |>
      dplyr::mutate(
        request_id = id,
        disaggregation_value = grp,
        display = ans$display,
        n = ans$n,
        technical_note = ans$note,
        .before = 1
      )
  })
}

analysis_long <- purrr::map_dfr(seq_len(nrow(ao_request)), run_one_request)

analysis_cell <- analysis_long |>
  dplyr::distinct(request_id, disaggregation_value, display, n, technical_note)

analysis_wide <- analysis_cell |>
  dplyr::select(request_id, disaggregation_value, display, n) |>
  tidyr::pivot_wider(
    names_from = disaggregation_value,
    values_from = c(display, n),
    names_glue = "{.value}__{disaggregation_value}"
  )

row_notes <- tibble::tibble(
  request_id = seq_len(nrow(ao_request)),
  mapping_note = mapping_note
) |>
  dplyr::left_join(
    analysis_cell |>
      dplyr::filter(!is.na(technical_note), technical_note != "") |>
      dplyr::group_by(request_id) |>
      dplyr::summarise(runtime_note = paste(unique(technical_note), collapse = " | "), .groups = "drop"),
    by = "request_id"
  ) |>
  dplyr::mutate(
    technical_note = dplyr::case_when(
      !is.na(mapping_note) & !is.na(runtime_note) ~ paste(mapping_note, runtime_note, sep = " | "),
      !is.na(mapping_note) ~ mapping_note,
      TRUE ~ runtime_note
    )
  ) |>
  dplyr::select(request_id, technical_note)

output_rows <- ao_request |>
  dplyr::left_join(analysis_wide, by = "request_id") |>
  dplyr::left_join(row_notes, by = "request_id")

# --------------------------------------------------------------------------------------------------
# 8. Detailed enumerator outputs
# --------------------------------------------------------------------------------------------------
# All specify-other counts per enumerator/population group
enumerator_other_detail <- if (nrow(other_entries) > 0) {
  hh |>
    dplyr::filter(!is.na(enum_id), stringr::str_squish(enum_id) != "") |>
    dplyr::distinct(enum_id, hoh_type) |>
    dplyr::left_join(
      other_entries |>
        dplyr::filter(!is.na(enum_id), stringr::str_squish(enum_id) != "") |>
        dplyr::count(enum_id, hoh_type, name = "n_specify_other_responses"),
      by = c("enum_id", "hoh_type")
    ) |>
    dplyr::mutate(n_specify_other_responses = dplyr::coalesce(n_specify_other_responses, 0L))
} else {
  tibble::tibble(enum_id = character(), hoh_type = character(), n_specify_other_responses = integer())
}

enumerator_infrequent_other_detail <- hh |>
  dplyr::mutate(selected_infrequent_other = selected_vec(hh, "cm_expenditure_infrequent", "other")) |>
  dplyr::filter(!is.na(enum_id), stringr::str_squish(enum_id) != "") |>
  dplyr::group_by(enum_id, hoh_type) |>
  dplyr::summarise(
    n_households = dplyr::n(),
    n_infrequent_other = sum(selected_infrequent_other, na.rm = TRUE),
    .groups = "drop"
  )

enumerator_detail <- dplyr::full_join(
  enumerator_other_detail,
  enumerator_infrequent_other_detail,
  by = c("enum_id", "hoh_type")
) |>
  dplyr::mutate(
    n_specify_other_responses = dplyr::coalesce(n_specify_other_responses, 0L),
    n_infrequent_other = dplyr::coalesce(n_infrequent_other, 0L)
  ) |>
  dplyr::arrange(hoh_type, dplyr::desc(n_specify_other_responses), enum_id)

# --------------------------------------------------------------------------------------------------
# 9. All section time minima from final XLSForm
# --------------------------------------------------------------------------------------------------
duration_meta <- survey_meta |>
  dplyr::filter(
    stringr::str_to_lower(as.character(type)) == "calculate",
    stringr::str_detect(as.character(name), "^duration_.*_min$")
  ) |>
  dplyr::transmute(
    variable = as.character(name),
    section = as.character(label_english_en)
  )

time_tracking_all_sections <- purrr::map_dfr(seq_len(nrow(duration_meta)), function(i) {
  var <- duration_meta$variable[[i]]
  section <- duration_meta$section[[i]]

  purrr::map_dfr(analysis_groups, function(grp) {
    d <- filter_population_group(hh, grp)
    ans <- result_numeric_stat(d, var, "min", special_values = numeric(0), digits = 2)
    tibble::tibble(
      variable = var,
      section = section,
      population_group = grp,
      minimum_minutes = if (nrow(ans$detail) > 0) ans$detail$value[[1]] else NA_real_,
      n_valid = ans$n,
      note = ans$note
    )
  })
})

# --------------------------------------------------------------------------------------------------
# 10. Validate AO question codes against final XLSForm
# --------------------------------------------------------------------------------------------------
requested_code <- as.character(ao_request[["Question code"]])
requested_code <- stringr::str_squish(requested_code)

# Tool lookup for final mapped variable; derived variables are explicitly marked.
derived_variables <- c(
  "n_children_under18",
  "all final-tool specify-other text fields",
  "cm_expenditure_frequent_total_30d",
  "cm_expenditure_infrequent_total_6m",
  "cm_expenditure_infrequent = other"
)

validation <- tibble::tibble(
  request_id = seq_len(nrow(ao_request)),
  excel_row = seq_len(nrow(ao_request)) + 1L,
  module = as.character(ao_request[["Module"]]),
  indicator = as.character(ao_request[["Indicator"]]),
  requested_question_code = requested_code,
  actual_dataset = actual_dataset,
  actual_variable = actual_variable,
  mapping_note = mapping_note
) |>
  dplyr::rowwise() |>
  dplyr::mutate(
    final_tool_found = dplyr::case_when(
      is.na(actual_variable) ~ FALSE,
      actual_variable %in% derived_variables ~ TRUE,
      actual_variable == "cm_expenditure_infrequent" ~ actual_variable %in% survey_meta$name,
      TRUE ~ actual_variable %in% survey_meta$name
    ),
    final_tool_label = if (!is.na(actual_variable) && actual_variable %in% survey_meta$name) {
      as.character(survey_meta$label_english_en[match(actual_variable, survey_meta$name)])
    } else if (!is.na(actual_variable) && actual_variable %in% derived_variables) {
      "Derived variable (see mapping note)"
    } else {
      NA_character_
    }
  ) |>
  dplyr::ungroup()

# --------------------------------------------------------------------------------------------------
# 11. Export: preserve original AO sheet and append results
# --------------------------------------------------------------------------------------------------
wb <- openxlsx::loadWorkbook(path_analysis_request)
main_sheet <- "Data Analysis Request"

# Write result headers and values into H:N, preserving original A:G exactly.
result_headers <- c(
  "Overall result", "Refugee result", "Host community result",
  "Overall N", "Refugee N", "Host community N", "Technical note"
)
openxlsx::writeData(wb, main_sheet, t(result_headers), startCol = 8, startRow = 1, colNames = FALSE)

result_matrix <- output_rows |>
  dplyr::transmute(
    overall_result = .data[["display__Overall"]],
    refugee_result = .data[["display__Refugee"]],
    host_result = .data[["display__Host community"]],
    overall_n = .data[["n__Overall"]],
    refugee_n = .data[["n__Refugee"]],
    host_n = .data[["n__Host community"]],
    technical_note
  )

openxlsx::writeData(wb, main_sheet, result_matrix, startCol = 8, startRow = 2, colNames = FALSE)

header_style <- openxlsx::createStyle(
  fontColour = "#FFFFFF", fgFill = "#1F4E78", textDecoration = "bold",
  halign = "center", valign = "center", wrapText = TRUE,
  border = "TopBottomLeftRight", borderColour = "#D9E2F3"
)
body_style <- openxlsx::createStyle(valign = "top", wrapText = TRUE)

openxlsx::addStyle(wb, main_sheet, header_style, rows = 1, cols = 8:14, gridExpand = TRUE, stack = TRUE)
openxlsx::addStyle(wb, main_sheet, body_style, rows = 2:(nrow(ao_request) + 1), cols = 8:14, gridExpand = TRUE, stack = TRUE)
openxlsx::setColWidths(wb, main_sheet, cols = 8:10, widths = 32)
openxlsx::setColWidths(wb, main_sheet, cols = 11:13, widths = 12)
openxlsx::setColWidths(wb, main_sheet, cols = 14, widths = 55)
openxlsx::freezePane(wb, main_sheet, firstRow = TRUE)

# Helper to replace a detail sheet if it already exists.
replace_sheet <- function(wb, sheet_name, data) {
  if (sheet_name %in% names(wb)) openxlsx::removeWorksheet(wb, sheet_name)
  openxlsx::addWorksheet(wb, sheet_name)
  openxlsx::writeDataTable(wb, sheet_name, data, tableStyle = "TableStyleMedium2")
  openxlsx::freezePane(wb, sheet_name, firstRow = TRUE)
  if (ncol(data) > 0) openxlsx::setColWidths(wb, sheet_name, cols = seq_len(ncol(data)), widths = "auto")
}

analysis_long_export <- analysis_long |>
  dplyr::left_join(
    ao_request |>
      dplyr::transmute(
        request_id,
        excel_row,
        module = as.character(.data[["Module"]]),
        indicator = as.character(.data[["Indicator"]]),
        requested_question_code = as.character(.data[["Question code"]])
      ),
    by = "request_id"
  ) |>
  dplyr::left_join(
    tibble::tibble(
      request_id = seq_len(nrow(ao_request)),
      actual_dataset = actual_dataset,
      actual_variable = actual_variable
    ),
    by = "request_id"
  ) |>
  dplyr::select(
    request_id, excel_row, module, indicator, requested_question_code,
    actual_dataset, actual_variable, disaggregation_value,
    statistic, value, display, n, technical_note
  ) |>
  dplyr::arrange(request_id, factor(disaggregation_value, levels = analysis_groups), statistic)

replace_sheet(wb, "Analysis Results Long", analysis_long_export)
replace_sheet(wb, "Enumerator Other Detail", enumerator_detail)
replace_sheet(wb, "Time Tracking All Sections", time_tracking_all_sections)
replace_sheet(wb, "Tool Validation", validation)

openxlsx::saveWorkbook(wb, path_analysis_output, overwrite = TRUE)

# --------------------------------------------------------------------------------------------------
# 12. Console diagnostics
# --------------------------------------------------------------------------------------------------
cat("\nAO quality analysis exported successfully:\n  ", normalizePath(path_analysis_output, winslash = "/", mustWork = FALSE), "\n", sep = "")
cat("\nPopulation-group disaggregation used: hoh_type = refugee / host_community.\n")
cat("Unweighted field-quality analysis: yes.\n")
cat("Request rows processed: ", nrow(ao_request), "\n", sep = "")
cat("Final-tool mapping rows found/derived: ", sum(validation$final_tool_found, na.rm = TRUE), " / ", nrow(validation), "\n", sep = "")
cat("\nNote: the final four generic Time Tracking rows in the AO workbook do not name a section.\n")
cat("They are not guessed; use the 'Time Tracking All Sections' sheet for the complete duration analysis.\n")

