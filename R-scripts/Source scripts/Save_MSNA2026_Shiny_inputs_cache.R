cat("\014")

# =================================================================================================
# REACH MSNA 2026 Uganda - Save Shiny App Input Cache
# Purpose: Save all data objects already loaded in the R environment into the Shiny app inputs folder.
#
# This script DOES NOT run the data collection tracker and DOES NOT export Excel.
# It only saves the required objects as an .rds cache that the Shiny app can read directly.
#
# Expected to run AFTER the data loading/checking pipeline, for example:
#   source("R-scripts/Source scripts/1.Load and process the data.R")
#   source("R-scripts/Source scripts/2.Soft duplicate checks.R")
#   source("R-scripts/Source scripts/3.Do the checks.R")
#   source("R-scripts/Source scripts/4.Process the CLs and DFs.R")
#   source("R-scripts/Source scripts/Save Shiny app data cache.R")
#
# Before sourcing this script, set path_shiny_app_folder if your Shiny folder has a different name:
#   path_shiny_inputs_folder <- "C:/Users/Administrator/Documents/UG tasks/01_data_cleaning/MSN_U/MSNA_cleaning/MSNA2026_Uganda_Field_Tracker_Shiny/inputs"
# =================================================================================================

all_dfs_names_before <- Filter(function(x) is(x, "data.frame"), mget(ls())) |> names()
all_lists_names_before <- Filter(function(x) is(x, "list"), mget(ls())) |> names()

suppressPackageStartupMessages({
  library(dplyr)
  library(tibble)
  library(purrr)
  library(stringr)
  library(crayon)
})

# -------------------------------------------------------------------------------------------------
# 1. Helper functions
# -------------------------------------------------------------------------------------------------

`%||%` <- function(x, y) if (!is.null(x)) x else y

safe_get_object <- function(object_name, env = .GlobalEnv) {
  if (exists(object_name, envir = env, inherits = TRUE)) {
    get(object_name, envir = env, inherits = TRUE)
  } else {
    NULL
  }
}

safe_is_dataframe <- function(x) {
  is.data.frame(x)
}

safe_is_nonempty_dataframe <- function(x) {
  is.data.frame(x) && nrow(x) > 0
}

safe_is_list <- function(x) {
  is.list(x) && !is.data.frame(x)
}

make_empty_df <- function() {
  tibble::tibble()
}

object_nrow <- function(x) {
  if (is.data.frame(x)) nrow(x) else NA_integer_
}

object_ncol <- function(x) {
  if (is.data.frame(x)) ncol(x) else NA_integer_
}

object_size_mb <- function(x) {
  round(as.numeric(utils::object.size(x)) / 1024^2, 3)
}

select_first_dataframe <- function(object_names, env = .GlobalEnv) {
  for (nm in object_names) {
    obj <- safe_get_object(nm, env = env)
    if (is.data.frame(obj) && nrow(obj) > 0) return(obj)
  }
  NULL
}

select_first_list <- function(object_names, env = .GlobalEnv) {
  for (nm in object_names) {
    obj <- safe_get_object(nm, env = env)
    if (is.list(obj) && !is.data.frame(obj)) return(obj)
  }
  NULL
}

standardise_raw_data_list <- function(main_df) {
  raw_list <- safe_get_object("raw_data_list")
  if (is.list(raw_list) && !is.data.frame(raw_list)) {
    if (!"Submissions" %in% names(raw_list) && is.data.frame(main_df)) {
      raw_list$Submissions <- main_df
    }
    return(raw_list)
  }
  
  list(
    Submissions = main_df %||% make_empty_df(),
    roster = select_first_dataframe(c("data_hh_roster", "data_roster", "roster", "hh_roster")) %||% make_empty_df(),
    edu_ind = select_first_dataframe(c("data_edu_ind", "edu_ind", "education_ind", "data_education_ind")) %||% make_empty_df(),
    health_ind = select_first_dataframe(c("data_health_ind", "health_ind")) %||% make_empty_df(),
    nutrition_ind = select_first_dataframe(c("data_nutrition_ind", "nutrition_ind", "data_child_nutrition", "data_child_nutrition_u5")) %||% make_empty_df(),
    child_protection_ind = select_first_dataframe(c("data_child_protection_ind", "child_protection_ind", "data_cp_ind")) %||% make_empty_df()
  )
}

make_cache_inventory <- function(cache) {
  purrr::imap_dfr(cache, function(obj, nm) {
    if (is.data.frame(obj)) {
      tibble::tibble(
        object_name = nm,
        object_type = "data.frame",
        rows = nrow(obj),
        columns = ncol(obj),
        list_length = NA_integer_,
        size_mb = object_size_mb(obj)
      )
    } else if (is.list(obj)) {
      tibble::tibble(
        object_name = nm,
        object_type = "list",
        rows = NA_integer_,
        columns = NA_integer_,
        list_length = length(obj),
        size_mb = object_size_mb(obj)
      )
    } else {
      tibble::tibble(
        object_name = nm,
        object_type = paste(class(obj), collapse = "; "),
        rows = NA_integer_,
        columns = NA_integer_,
        list_length = NA_integer_,
        size_mb = object_size_mb(obj)
      )
    }
  })
}

make_raw_list_inventory <- function(raw_list) {
  if (!is.list(raw_list)) return(tibble::tibble())
  purrr::imap_dfr(raw_list, function(df, nm) {
    tibble::tibble(
      sheet = nm,
      is_dataframe = is.data.frame(df),
      rows = if (is.data.frame(df)) nrow(df) else NA_integer_,
      columns = if (is.data.frame(df)) ncol(df) else NA_integer_
    )
  })
}

resolve_shiny_inputs_folder <- function() {
  # Priority 1: direct user-defined inputs folder.
  # This is the preferred option for your local setup.
  if (exists("path_shiny_inputs_folder", inherits = TRUE)) {
    candidate <- get("path_shiny_inputs_folder", inherits = TRUE)
    if (is.character(candidate) && length(candidate) == 1 && nzchar(candidate)) {
      return(candidate)
    }
  }

  # Priority 2: user-defined Shiny app folder; save inside its inputs folder.
  if (exists("path_shiny_app_folder", inherits = TRUE)) {
    candidate <- get("path_shiny_app_folder", inherits = TRUE)
    if (is.character(candidate) && length(candidate) == 1 && nzchar(candidate)) {
      return(file.path(candidate, "inputs"))
    }
  }

  # Priority 3: current working directory if it is the Shiny app folder.
  if (file.exists(file.path(getwd(), "app.R")) || dir.exists(file.path(getwd(), "www"))) {
    return(file.path(getwd(), "inputs"))
  }

  # Priority 4: your stated Windows Shiny inputs folder.
  windows_inputs <- "C:/Users/Administrator/Documents/UG tasks/01_data_cleaning/MSN_U/MSNA_cleaning/MSNA2026_Uganda_Field_Tracker_Shiny/inputs"
  if (dir.exists(dirname(windows_inputs)) || dir.exists(windows_inputs)) {
    return(windows_inputs)
  }

  # Priority 5: common local folder names inside the current project folder.
  candidates <- c(
    "MSNA2026_Uganda_Field_Tracker_Shiny",
    "MSNA2026_Uganda_Field_Tracker_Shiny_cached",
    "MSNA2026_Uganda_Field_Tracker_Shiny_env_filters",
    "MSNA2026_Uganda_Field_Tracker_Shiny_v4_field_team",
    "MSNA2026_Uganda_Field_Tracker_Shiny_v3_standard"
  )
  existing <- candidates[dir.exists(candidates)]
  if (length(existing) > 0) return(file.path(existing[[1]], "inputs"))

  # If nothing is found, use the standard local Shiny folder name and create inputs under it.
  file.path("MSNA2026_Uganda_Field_Tracker_Shiny", "inputs")
}

# -------------------------------------------------------------------------------------------------
# 2. Resolve Shiny inputs/cache folder
# -------------------------------------------------------------------------------------------------

path_shiny_inputs_cache <- normalizePath(resolve_shiny_inputs_folder(), winslash = "/", mustWork = FALSE)
path_shiny_app_folder_resolved <- normalizePath(dirname(path_shiny_inputs_cache), winslash = "/", mustWork = FALSE)

if (!dir.exists(path_shiny_inputs_cache)) {
  dir.create(path_shiny_inputs_cache, recursive = TRUE)
}

if (!dir.exists(path_shiny_inputs_cache)) {
  stop(
    "The Shiny inputs folder could not be created or found: ", path_shiny_inputs_cache, "\n",
    "Set it explicitly before running this script, for example:\n",
    "path_shiny_inputs_folder <- 'C:/Users/Administrator/Documents/UG tasks/01_data_cleaning/MSN_U/MSNA_cleaning/MSNA2026_Uganda_Field_Tracker_Shiny/inputs'\n",
    "source('R-scripts/Source scripts/Save Shiny app data cache.R')",
    call. = FALSE
  )
}

# -------------------------------------------------------------------------------------------------
# 3. Collect main, repeat, and checking objects from the R environment
# -------------------------------------------------------------------------------------------------

main_df <- select_first_dataframe(c(
  "clean_df",
  "clean_data",
  "clean_dataset",
  "data_hh_main",
  "data_collected",
  "data"
))

if (is.null(main_df)) {
  raw_candidate <- safe_get_object("raw_data_list")
  if (is.list(raw_candidate) && "Submissions" %in% names(raw_candidate) && is.data.frame(raw_candidate$Submissions)) {
    main_df <- raw_candidate$Submissions
  }
}

if (is.null(main_df) || !is.data.frame(main_df) || nrow(main_df) == 0) {
  stop(
    "No main MSNA dataset found in the environment. Expected one of: clean_df, clean_data, clean_dataset, data_hh_main, data_collected, data, or raw_data_list$Submissions.",
    call. = FALSE
  )
}

raw_data_list_cache <- standardise_raw_data_list(main_df)

# Make sure common repeat objects are available even if raw_data_list exists but misses some names.
repeat_defaults <- list(
  roster = select_first_dataframe(c("data_hh_roster", "data_roster", "roster", "hh_roster")) %||% make_empty_df(),
  edu_ind = select_first_dataframe(c("data_edu_ind", "edu_ind", "education_ind", "data_education_ind")) %||% make_empty_df(),
  health_ind = select_first_dataframe(c("data_health_ind", "health_ind")) %||% make_empty_df(),
  nutrition_ind = select_first_dataframe(c("data_nutrition_ind", "nutrition_ind", "data_child_nutrition", "data_child_nutrition_u5")) %||% make_empty_df(),
  child_protection_ind = select_first_dataframe(c("data_child_protection_ind", "child_protection_ind", "data_cp_ind")) %||% make_empty_df()
)

for (nm in names(repeat_defaults)) {
  if (!nm %in% names(raw_data_list_cache) || !is.data.frame(raw_data_list_cache[[nm]])) {
    raw_data_list_cache[[nm]] <- repeat_defaults[[nm]]
  }
}
raw_data_list_cache$Submissions <- main_df

# -------------------------------------------------------------------------------------------------
# 4. Build the Shiny cache object
# -------------------------------------------------------------------------------------------------

cache_created_at <- Sys.time()

msna2026_shiny_cache <- list(
  cache_created_at = cache_created_at,
  generated_at = cache_created_at,
  source_working_directory = getwd(),
  shiny_app_folder = path_shiny_app_folder_resolved,
  shiny_inputs_folder = path_shiny_inputs_cache,
  
  # Main datasets
  data_collected = main_df,
  data_hh_main = main_df,
  data = main_df,
  clean_df = safe_get_object("clean_df"),
  clean_data = safe_get_object("clean_data"),
  clean_dataset = safe_get_object("clean_dataset"),
  raw_data_list = raw_data_list_cache,
  
  # Repeat-sheet datasets as direct objects for app/helper compatibility
  data_hh_roster = raw_data_list_cache$roster,
  roster = raw_data_list_cache$roster,
  data_edu_ind = raw_data_list_cache$edu_ind,
  edu_ind = raw_data_list_cache$edu_ind,
  data_health_ind = raw_data_list_cache$health_ind,
  health_ind = raw_data_list_cache$health_ind,
  data_nutrition_ind = raw_data_list_cache$nutrition_ind,
  nutrition_ind = raw_data_list_cache$nutrition_ind,
  data_child_protection_ind = raw_data_list_cache$child_protection_ind,
  child_protection_ind = raw_data_list_cache$child_protection_ind,
  
  # Cleaning and checking logs
  combined_log = safe_get_object("combined_log"),
  cleaning_log = safe_get_object("cleaning_log"),
  cleaning_log_template = safe_get_object("cleaning_log_template"),
  final_cleaning_log_no_dl = safe_get_object("final_cleaning_log_no_dl"),
  cleaning_log_no_dl = safe_get_object("cleaning_log_no_dl"),
  deletion_log = safe_get_object("deletion_log"),
  soft_duplicate_log = safe_get_object("soft_duplicate_log"),
  combined_log_by_dataset = safe_get_object("combined_log_by_dataset"),
  cleaning_logs_by_dataset = safe_get_object("cleaning_logs_by_dataset"),
  deletion_log_by_dataset = safe_get_object("deletion_log_by_dataset"),
  
  # Summary objects
  cleaning_dashboard = safe_get_object("cleaning_dashboard"),
  dashboard = safe_get_object("dashboard"),
  check_summary = safe_get_object("check_summary"),
  repeat_count_summary = safe_get_object("repeat_count_summary"),
  missingness_summary = safe_get_object("missingness_summary"),
  special_value_summary = safe_get_object("special_value_summary"),
  sheet_record_counts = safe_get_object("sheet_record_counts"),
  
  # Kobo form/reference objects
  df_survey = safe_get_object("df_survey"),
  df_survey_clean = safe_get_object("df_survey_clean"),
  df_choices = safe_get_object("df_choices"),
  df_settings = safe_get_object("df_settings"),
  logical_checks = safe_get_object("logical_checks"),
  
  # Useful path objects, if defined
  path_cleaning_folder_today = safe_get_object("path_cleaning_folder_today"),
  path_data_raw = safe_get_object("path_data_raw"),
  path_checks = safe_get_object("path_checks"),
  path_tool = safe_get_object("path_tool")
)

# Remove NULL entries only where safe. Keep missing optional objects out of the file rather than saving many NULLs.
msna2026_shiny_cache <- msna2026_shiny_cache[!vapply(msna2026_shiny_cache, is.null, logical(1))]

cache_inventory <- make_cache_inventory(msna2026_shiny_cache)
raw_list_inventory <- make_raw_list_inventory(msna2026_shiny_cache$raw_data_list)

msna2026_shiny_cache$cache_inventory <- cache_inventory
msna2026_shiny_cache$raw_data_list_inventory <- raw_list_inventory

# -------------------------------------------------------------------------------------------------
# 5. Save cache files inside the Shiny app folder
# -------------------------------------------------------------------------------------------------

latest_cache_path <- file.path(path_shiny_inputs_cache, "latest_msna2026_tracker_cache.rds")
latest_generic_cache_path <- file.path(path_shiny_inputs_cache, "latest_msna2026_shiny_cache.rds")
timestamped_cache_path <- file.path(
  path_shiny_inputs_cache,
  paste0("msna2026_shiny_cache_", format(cache_created_at, "%Y%m%d_%H%M%S"), ".rds")
)
manifest_path <- file.path(path_shiny_inputs_cache, "latest_msna2026_cache_manifest.csv")
raw_manifest_path <- file.path(path_shiny_inputs_cache, "latest_msna2026_raw_data_list_manifest.csv")

saveRDS(msna2026_shiny_cache, latest_cache_path, compress = "gzip")
saveRDS(msna2026_shiny_cache, latest_generic_cache_path, compress = "gzip")
saveRDS(msna2026_shiny_cache, timestamped_cache_path, compress = "gzip")
utils::write.csv(cache_inventory, manifest_path, row.names = FALSE)
utils::write.csv(raw_list_inventory, raw_manifest_path, row.names = FALSE)

# -------------------------------------------------------------------------------------------------
# 6. Console output
# -------------------------------------------------------------------------------------------------

cat("\n")
cat(green$bold("MSNA 2026 Shiny data cache saved successfully"))
cat("\n\n")
cat("Shiny app folder: ", path_shiny_app_folder_resolved, "\n", sep = "")
cat("Shiny inputs folder: ", path_shiny_inputs_cache, "\n", sep = "")
cat("Latest cache for app: ", latest_cache_path, "\n", sep = "")
cat("Generic cache copy: ", latest_generic_cache_path, "\n", sep = "")
cat("Timestamped backup: ", timestamped_cache_path, "\n", sep = "")
cat("Cache manifest: ", manifest_path, "\n", sep = "")
cat("Raw-data-list manifest: ", raw_manifest_path, "\n", sep = "")
cat("\n")
cat(blue$bold("Main dataset saved:"), " ", nrow(main_df), " rows and ", ncol(main_df), " columns\n", sep = "")
cat(blue$bold("Raw/repeat sheets saved:"), "\n", sep = "")
print(raw_list_inventory)
cat("\n")

# -------------------------------------------------------------------------------------------------
# 7. Object tracking output, same style as the cleaning scripts
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
