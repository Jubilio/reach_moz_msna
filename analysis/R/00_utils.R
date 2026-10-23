# =============================================================================
# 00_utils.R — Data Loading & Analysis Pipeline Helpers (English)
# =============================================================================

suppressPackageStartupMessages({
  library(dplyr)
  library(readr)
  library(fs)
  library(here)
})

# Configure Pandoc path if running outside of RStudio IDE
if (Sys.getenv("RSTUDIO_PANDOC") == "") {
  rstudio_pandoc <- "C:/Program Files/RStudio/resources/app/bin/quarto/bin/tools"
  if (file.exists(rstudio_pandoc)) {
    Sys.setenv(RSTUDIO_PANDOC = rstudio_pandoc)
  }
}

#' Locates and loads the most recent cleaned dataset from the Mozambique cleaning pipeline
load_latest_clean_data <- function(outputs_dir = here::here("outputs", "moz")) {
  moz_runs <- sort(fs::dir_ls(outputs_dir, type = "directory"))
  if (length(moz_runs) == 0) {
    stop("No outputs found in: ", outputs_dir,
         "\nPlease run 'source(\"R-scripts/moz/run.R\")' first.")
  }
  latest_run <- tail(moz_runs, 1)
  message("--> Loading data from latest clean run: ", latest_run)

  rds_path <- file.path(latest_run, "candidate_data.rds")
  if (!file.exists(rds_path)) {
    stop("File 'candidate_data.rds' not found in: ", latest_run)
  }

  data_tables <- readRDS(rds_path)

  # Retrieve HHS frequency questions from raw_snapshot if not already in Submissions
  raw_snap_path <- file.path(latest_run, "raw_snapshot.rds")
  if (file.exists(raw_snap_path)) {
    raw_snap <- readRDS(raw_snap_path)
    subm_raw <- raw_snap$Submissions
    subm <- data_tables$Submissions

    if (!"m2755_hh_hhs_no_food_frequency" %in% names(subm) && ncol(subm_raw) >= 155) {
      subm$m2755_hh_hhs_no_food_frequency      <- subm_raw[[151]]
      subm$m2757_hh_hhs_sleep_hungry_frequency <- subm_raw[[153]]
      subm$m2759_hh_hhs_no_day_night_frequency <- subm_raw[[155]]
      data_tables$Submissions <- subm
    }
  }

  return(data_tables)
}

#' Safe numeric conversion (suppresses warnings and coerces non-finite values to NA)
to_numeric <- function(x) {
  num_x <- suppressWarnings(as.numeric(as.character(x)))
  num_x[!is.finite(num_x)] <- NA_real_
  num_x
}

#' Value mask for recall days indicators (0 to 7 days for FCS and rCSI)
mask_days <- function(x) {
  val <- to_numeric(x)
  val[val < 0 | val > 7 | val != floor(val)] <- NA_real_
  val
}

#' Verifies that all required columns exist in the data.frame
assert_columns <- function(df, required_cols, df_name = "dataset") {
  missing_cols <- setdiff(required_cols, names(df))
  if (length(missing_cols) > 0) {
    warning("Missing required columns in '", df_name, "': ", paste(missing_cols, collapse = ", "))
    return(FALSE)
  }
  return(TRUE)
}

#' Applies human-readable labels in English to data.frame column names
apply_human_readable_labels <- function(df, labels_path = here::here("analysis", "config", "variable_labels_en.csv")) {
  if (is.null(df) || !is.data.frame(df) || ncol(df) == 0) return(df)
  if (file.exists(labels_path)) {
    dict <- readr::read_csv(labels_path, show_col_types = FALSE)
    map <- stats::setNames(dict$label_en, dict$raw_name)
    current_names <- names(df)
    new_names <- current_names
    for (i in seq_along(current_names)) {
      nm <- current_names[i]
      if (nm %in% names(map)) {
        new_names[i] <- map[[nm]]
      }
    }
    names(df) <- new_names
  }
  return(df)
}
